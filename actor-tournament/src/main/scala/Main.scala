import org.apache.pekko
import pekko.actor.{Actor, ActorLogging, ActorRef, Props, ActorSystem}
import scala.util.Random

case class Init(n: Int, k: Int)
case object Piłeczka
case object Punkt
case object Start
case class Wygrana(zawodnik: ActorRef)
case class GrajZ(przeciwnik: ActorRef)

class Organizator extends Actor with ActorLogging {
  def receive: Receive = {
    case Init(n: Int, k: Int) =>
      val zawodnicy = (1 to n).map { i =>
        context.actorOf(Props(classOf[Zawodnik], k, self), s"zawodnik$i")
      }
      context.become(active(zawodnicy, k))
      self ! Start

    case other =>
      log.warning(s"Unexpected message in receive: $other")
  }

  def active(zawodnicy: Seq[ActorRef], k: Int): Receive = {
    case Start =>
      val losowi = Random.shuffle(zawodnicy)
      val pary = losowi.grouped(2).collect {
        case Seq(a, b) => (a, b)
      }.toList

      val wolny = if (losowi.size % 2 == 1) Some(losowi.last) else None
      val kontynuuj = wolny.toList

      context.become(oczekiwanie(pary.size, kontynuuj, k))

      pary.foreach { case (a, b) =>
        log.info(s"Rozpoczynam mecz pomiędzy ${a.path.name} i ${b.path.name}")
        a ! GrajZ(b)
        b ! GrajZ(a)
      }
    case other =>
      
  }

  def oczekiwanie(oczekiwani: Int, zwyciezcy: Seq[ActorRef], k: Int): Receive = {
    case Wygrana(zawodnik) =>
      log.info(s"Otrzymano Wygrana od ${zawodnik.path.name}")
      val nowi = zawodnik +: zwyciezcy
      if (nowi.size == oczekiwani) {
        if (nowi.size == 1) {
          log.info(s"${zawodnik.path.name} wygrał turniej!")
          context.system.terminate()
        } else {
          context.become(active(nowi, k))
          self ! Start
        }
      } else {
        context.become(oczekiwanie(oczekiwani, nowi, k))
      }

    case other =>
      
  }
}

class Zawodnik(k: Int, organizator: ActorRef) extends Actor with ActorLogging {
  def receive: Receive = {
    case GrajZ(przeciwnik) =>
      log.info(s"Start ${self.path.name} odbił piłkę")
      context.become(gra(0, przeciwnik))
      przeciwnik ! Piłeczka

    case other =>
      
  }

  def gra(punkty: Int, przeciwnik: ActorRef): Receive = {
    case Piłeczka =>
      if (Random.nextDouble() <= 0.75) {
      log.info(s"${self.path.name} odbił piłkę")
      przeciwnik ! Piłeczka
      } else {
        log.info(s"${self.path.name} nie odbił piłki – punkt dla przeciwnika")
        przeciwnik ! Punkt
      }

    case Punkt =>
    val nowePunkty = punkty + 1
    log.info(s"${self.path.name} zdobywa punkt: $nowePunkty")
    if (nowePunkty >= k) {
      log.info(s"${self.path.name} wygrał mecz")
      organizator ! Wygrana(self)
    } else {
      context.become(gra(nowePunkty, przeciwnik))
      przeciwnik ! Piłeczka
    }


    case other =>
      
  }
}

@main def playTournament(): Unit = {
  val system = ActorSystem("Turniej")
  val organizator = system.actorOf(Props[Organizator](), "organizator")
  organizator ! Init(n = 8, k = 3)
}

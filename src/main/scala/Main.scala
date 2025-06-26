import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props, PoisonPill, Terminated}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

case class InitZamek(przeciwnik: ActorRef)
case class Strzal(ileObroncow: Int)
case class InitSila(z1: ActorRef, z2: ActorRef)
case object Trafiony
case object Rozkaz

class Zamek extends Actor with ActorLogging {

  def receive: Receive = {
    case InitZamek(przeciwnik) =>
      val obroncy = (1 to 100).map { i =>
        val obronca = context.actorOf(Props[Obronca](), s"obronca-$i")
        context.watch(obronca)
        obronca
      }.toSet

      context.become(active(przeciwnik, obroncy))
  }

  def active(przeciwnik: ActorRef, obroncy: Set[ActorRef]): Receive = {
    case Rozkaz =>
      if (obroncy.nonEmpty) {
        przeciwnik ! Strzal(obroncy.size)
      }

    case Strzal(ile) =>
      obroncy.foreach(_ ! Strzal(ile)) 

    case Trafiony =>
      log.info(s"${sender().path.name} trafiony — umiera")
      sender() ! PoisonPill

    case Terminated(actor) =>
      val nowiObroncy = obroncy - actor
      log.info(s"${actor.path.name} zginął. Pozostało: ${nowiObroncy.size}")
      if (nowiObroncy.isEmpty) {
        log.info(s"${self.path.name} przegral bitwę!")
        context.system.terminate()
      } else {
        context.become(active(przeciwnik, nowiObroncy))
      }
  }
}


class Obronca extends Actor with ActorLogging {
  def receive: Receive = {
    case Strzal(ile) =>
      val r = scala.util.Random.nextDouble()
      val szansa = ile / 200.0
      if (r < szansa) {
        sender() ! Trafiony
      }
  }
}

class SilaWyzsza extends Actor {
  def receive: Receive = {
    case InitSila(z1, z2) =>
      z1 ! InitZamek(z2)
      z2 ! InitZamek(z1)
      context.become(active(z1, z2))
  }

  def active(z1: ActorRef, z2: ActorRef): Receive = {
    case Rozkaz =>
      z1 ! Rozkaz
      z2 ! Rozkaz
  }
}

@main
def mainProg: Unit = {
  val system = ActorSystem("Bitwa")
  val silaWyzsza = system.actorOf(Props[SilaWyzsza](), "SilaWyzsza")
  val zamek1 = system.actorOf(Props[Zamek](), "Zamek1")
  val zamek2 = system.actorOf(Props[Zamek](), "Zamek2")

  silaWyzsza ! InitSila(zamek1, zamek2)

  system.scheduler.scheduleAtFixedRate(10.millis, 1000.millis, silaWyzsza, Rozkaz)
}

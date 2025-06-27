package lab14
package WyrzutniaTorped

import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props, PoisonPill, Terminated}
import lab14.Statek.Trafiony

case object StrzałTorpeda
case class InitTorpeda(przeciwnik:ActorRef)

class WyrzutniaTorped extends Actor with ActorLogging {
  def receive: Receive = {
    case InitTorpeda(przeciwnik) =>
      context.become(active(przeciwnik, 100))
  }

    def active(przeciwnik: ActorRef, hp: Int) : Receive = {
    case StrzałTorpeda if hp > 0 =>
      przeciwnik ! Trafiony(50, true)

    case Trafiony(damage, _) =>
      val newHp = (hp - damage).max(0)
      if (newHp == 0){
        log.info(s"${self.path.name} zniszczone!")
        context.become(active(przeciwnik, newHp))
      } 
  }
}
package lab14
package DzialoLaserowe

import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props, PoisonPill, Terminated}
import lab14.Statek.Trafiony

case object StrzałLaser
case class InitLaser(przeciwnik:ActorRef)

class DzialoLaserowe extends Actor with ActorLogging {
  def receive: Receive = {
    case InitLaser(przeciwnik) =>
      context.become(active(przeciwnik, 100))
  }

  def active(przeciwnik: ActorRef, hp: Int) : Receive = {
    case StrzałLaser if hp > 0 =>
      przeciwnik ! Trafiony(20, false)

    case Trafiony(damage, _) =>
      val newHp = (hp - damage).max(0)
      if (newHp == 0){
        log.info(s"${self.path.name} zniszczone!")
        context.become(active(przeciwnik, newHp))
      } 
  }
}
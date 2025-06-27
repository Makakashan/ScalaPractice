package lab14
package DzialoJonowe

import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props, PoisonPill, Terminated}
import lab14.Statek.Trafiony

case object StrzałJon
case class InitJon(przeciwnik:ActorRef)

class DzialoJonowe extends Actor with ActorLogging {
  def receive: Receive = {
    case InitJon(przeciwnik) =>
      context.become(active(przeciwnik, 100, 0))
  }

  def active(przeciwnik: ActorRef, hp: Int, cooldown: Int) : Receive = {
    case StrzałJon if hp > 0 && cooldown == 0 =>
      przeciwnik ! Trafiony(40, false)

    case StrzałJon if cooldown > 0 =>
      context.become(active(przeciwnik, hp, cooldown - 1))

    case Trafiony(damage, _) =>
      val newHp = (hp - damage).max(0)
      val newCooldown = cooldown + 1
      if (newHp == 0){
        log.info(s"${self.path.name} zniszczone!")
        context.become(active(przeciwnik, newHp, newCooldown))
      } 
  }
}


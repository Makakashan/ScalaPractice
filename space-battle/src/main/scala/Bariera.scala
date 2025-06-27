package lab14
package Bariera

import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props, PoisonPill, Terminated}
import lab14.Statek.* 
import lab14.Rozkaz

case class Init(statek: ActorRef)

class Bariera extends Actor with ActorLogging {
  def receive: Receive = {
    case Init(statek) =>
      context.become(active(1000, statek))
  }

  def active(barrierHp: Int, statek: ActorRef): Receive = {
    case Rozkaz =>
      val newBarrierHp = (barrierHp + 100).min(1000)
      log.info(s"Bariera sie regeneruje: $newBarrierHp HP")
      context.become(active(newBarrierHp, statek))

    case Trafiony(damage, false) =>
      val newHp = (barrierHp - damage).max(0)
      if (newHp == 0) {
        log.info("Bariera zniszczona!")
        statek ! Trafiony(damage, true) 
      } else {
        log.info(s"Bariera trafiona za $damage, zostalo $newHp")
        context.become(active(newHp, statek))
      }
      
  }

  def destroyed: Receive = {
        case _ => log.info("1234ty")
  }
}

package lab14
package SilaWyzsza

import org.apache.pekko
import pekko.actor.{Actor, ActorLogging, ActorRef, Props}
import lab14.Statek.*
import lab14.Rozkaz



case class Init(statek1: ActorRef, statek2: ActorRef)

class SilaWyzsza extends Actor with ActorLogging {
  def receive: Receive = {
    case Init(s1, s2) =>
      s1 ! lab14.Statek.Init(s2)
      s2 ! lab14.Statek.Init(s1)
      context.become(active(s1, s2))
  }

  def active(s1: ActorRef, s2: ActorRef): Receive = {
    case Rozkaz =>
      s1 ! Rozkaz
      s2 ! Rozkaz
  }
}

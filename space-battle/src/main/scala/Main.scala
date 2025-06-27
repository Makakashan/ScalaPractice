package lab14

import org.apache.pekko
import pekko.actor.{ActorSystem, Actor, ActorRef, Props}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
case object Rozkaz

@main
def mainProg: Unit = {
  val system = ActorSystem("Symulacja")
  val siłaWyższa = system.actorOf(Props[SilaWyzsza.SilaWyzsza](), "SilaWyzsza")
  val statek1 = system.actorOf(Props[Statek.Statek](), "Statek1")
  val statek2 = system.actorOf(Props[Statek.Statek](), "Statek2")

  siłaWyższa ! SilaWyzsza.Init(statek1,statek2)

  system.scheduler.scheduleAtFixedRate(10.millis, 1000.millis, siłaWyższa, Rozkaz)
}
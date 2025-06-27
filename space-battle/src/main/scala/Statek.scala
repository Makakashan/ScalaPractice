package lab14
package Statek

import org.apache.pekko
import pekko.actor.{Actor, ActorLogging, ActorRef, Props}
import lab14.Bariera.*
import lab14.DzialoJonowe.*
import lab14.DzialoLaserowe.*
import lab14.WyrzutniaTorped.*
import lab14.Rozkaz


case class Init(przeciwnik: ActorRef)
case class Trafiony(damage: Int, damageOnStatek: Boolean)

class Statek extends Actor with ActorLogging {
  def receive: Receive = {
    case Init(przeciwnik) =>
      val hp = 1000
      val barrier = context.actorOf(Props[Bariera](), "barrier")
      barrier ! Bariera.Init(self)
      val lasery = (1 to 6).map { i =>
        val laser = context.actorOf(Props[DzialoLaserowe](), s"laser-$i")
        laser
      }
      val jony = (1 to 4).map{ i =>
        val jon = context.actorOf(Props[DzialoJonowe](), s"jon-$i")
        jon
      }

      val torpedy = (1 to 2).map{ i =>
        val torpeda = context.actorOf(Props[WyrzutniaTorped](), s"torpeda-$i")
        torpeda
      }
      
      val moduly = lasery ++ jony ++ torpedy

      moduly.foreach {
        case ref if ref.path.name.startsWith("laser")   => ref ! InitLaser(przeciwnik)
        case ref if ref.path.name.startsWith("jon")     => ref ! InitJon(przeciwnik)
        case ref if ref.path.name.startsWith("torpeda") => ref ! InitTorpeda(przeciwnik)
      }

      context.become(active(przeciwnik, hp, barrier, moduly))
  }

  def active(przeciwnik: ActorRef, hp: Int, barrier: ActorRef, moduly: Iterable[ActorRef]): Receive = {
    case Rozkaz =>
      barrier ! Rozkaz
      moduly.foreach {
          case ref if ref.path.name.startsWith("laser")   => ref ! StrzałLaser
          case ref if ref.path.name.startsWith("jon")     => ref ! StrzałJon
          case ref if ref.path.name.startsWith("torpeda") => ref ! StrzałTorpeda
        }


    case Trafiony(damage, false) =>
      barrier ! Trafiony(damage, false)

  case Trafiony(damage, true) =>
    log.info(s"Bariera przebita! Rozwazam uszkodzenie modulu albo kadluba.")
    val szansa = 5
    val modulyList = moduly.toList
    val r = util.Random.nextInt(100)

    if (r < szansa && modulyList.nonEmpty) {
      val index = (r / szansa).min(modulyList.size - 1)
      val cel = modulyList(index)
      log.info(s"Uszkadzam modul: ${cel.path.name}")
      cel ! Trafiony(damage, true)
    } else {
      val newHp = (hp - damage).max(0)
      if (newHp <= 0) {
        log.info(s"Statek zniszczony! HP spadlo do 0.")
        context.system.terminate()
      } else {
        log.info(s"Statek otrzymal $damage obrazen, pozostalo $newHp HP.")
        context.become(active(przeciwnik, newHp, barrier, moduly))
      }
    }
  }
}
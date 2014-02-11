package ch.epfl.lara.synthesis.stringsolver.actors

import akka.actor.Actor
import akka.actor.Props
 
class HelloWorld extends Actor {
 
  override def preStart(): Unit = {
    // create the greeter actor
    val greeter = context.actorOf(Props[Greeter], "greeter")
    // tell it to perform the greeting
    greeter ! Greeter.Greet
  }
 
  def receive = {
    // when the greeter is done, stop this actor and with it the application
    case Greeter.Done => context.stop(self)
  }
}

object Greeter {
  case object Greet
  case object Done
}
 
class Greeter extends Actor {
  def receive = {
    case Greeter.Greet =>
      println("Hello World!")
      sender ! Greeter.Done
  }
}

object Commands {
  case class CommandLine(as: Array[String])
}

/**
 * 
 */
class AutomaticRenaming extends Actor{
  import Commands._
  def receive = {
    case CommandLine(as) =>
      
  }
}
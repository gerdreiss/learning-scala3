package com.github.gerdreiss.learningscala3

object Traits:

  trait Talker(subject: String):
    def talkTo(another: Talker): String = ""

  class Person(name: String) extends Talker("rock music"):
    override def talkTo(another: Talker) = ""

  class RockFan extends Talker("rock music")
  class RockFanatic extends RockFan with Talker // would not compile '("heavy metal")'

  // derived traits will NOT pass constructor to parent traits
  trait BrokenRecord extends Talker

  class AnnoyingFriend extends BrokenRecord with Talker("Politics")

  // super traits
  super trait Paintable
  trait Color
  case object Red extends Color with Paintable
  case object Green extends Color with Paintable
  case object Blue extends Color with Paintable

  val color = if ( 42 > 2 ) Red else Blue
  
  /*
   * super trait examples:
   * 
   * scala.Product
   * java.lang.Comparable
   * java.lang.Serializable
   * 
   */
package lectures.section5

trait Instrumentalist:
  def play(): Unit

trait Singer:
  self: Instrumentalist => // self type
  def sing(): Unit

class LeadSinger extends Singer with Instrumentalist:
  override def play(): Unit = ???
  override def sing(): Unit = ???

// this ain't compiling
// class Vocalist extends Singer:
//   override def sing(): Unit = ???

val jamesHet = new Singer with Instrumentalist:
  override def play(): Unit = ???
  override def sing(): Unit = ???

class Guitarist extends Instrumentalist:
  override def play(): Unit = println("(guitar solo)")

val ericClapton = new Guitarist with Singer:
  override def sing(): Unit = ???

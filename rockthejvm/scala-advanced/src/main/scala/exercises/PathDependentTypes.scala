package exercises

trait ItemLike:
  type Key

trait Item[K] extends ItemLike:
  type Key = K

trait IntItem extends Item[Int]
trait StringItem extends Item[String]

// shit ain't compiling
// @main def PathDependentTypesExercise(): Unit =
//   def get[ItemType <: ItemLike](key: ItemType#Key): ItemType = ???
//   get[IntItem](42) // ok
//   get[StringItem]("home") // ok
//   get[IntItem]("scala") // not ok

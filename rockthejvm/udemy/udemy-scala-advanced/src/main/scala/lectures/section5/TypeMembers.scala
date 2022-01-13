package lectures.section5

object TypeMembers:

  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class AnimalCollection:
    type AnimalType // abstract type member
    type BoundedAnimal <: Animal
    type SuperBoundedAnimal >: Dog <: Animal
    type AnimalC = Cat

  val ac = new AnimalCollection
  val dog: ac.AnimalType = ???
  val cat: ac.AnimalC = new Cat

  trait MyList:
    type T
    def add(element: T): MyList

  class NonEmptyList(value: Int) extends MyList:
    override type T = Int
    def add(element: Int): MyList = ???

  // .type
  type CatsType = cat.type
  val newCat: CatsType = cat

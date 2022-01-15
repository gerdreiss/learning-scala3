package lectures.section5

import lectures.section2.Recap.Crocodile

object RecursiveAndFBoundedTypes extends App:

  object FBoundedPolymorphism:

    /** F-Bounded Polymorphism */
    trait Animal[A <: Animal[A]]:
      def breed: List[A]

    trait Cat extends Animal[Cat]:
      override def breed: List[Cat]

    trait Dog extends Animal[Dog]:
      override def breed: List[Dog]

    // this compiles, but it shouldn't
    class Crocodile extends Animal[Dog]:
      override def breed: List[Dog] = ???

    trait Entity[E <: Entity[E]] // ORM
    class Person extends Comparable[Person]:
      override def compareTo(o: Person): Int = ???

  end FBoundedPolymorphism

  object FBoundedPolymorphismWithSelfTypes:

    /** F-Bounded Polymorphism + self types */
    trait Animal[A <: Animal[A]]:
      self: A =>
      def breed: List[A]

    trait Cat extends Animal[Cat]:
      override def breed: List[Cat]

    trait Dog extends Animal[Dog]:
      override def breed: List[Dog]

    // now this doesn't compile anymore
    // class Crocodile extends Animal[Dog]:
    //   override def breed: List[Dog] = ???

    trait Fish extends Animal[Fish]
    trait Cod extends Fish:
      override def breed: List[Fish] // this is a problem, we could return another Fish, say Salmon

  end FBoundedPolymorphismWithSelfTypes

  object TypeClassSolution:

    // solve the limitation of F-Bounded Types with Type Classes (FPFTW!)
    trait Animal

    trait CanBreed[A <: Animal]:
      def breed(a: A): List[A]

    case class Dog(name: String) extends Animal
    object Dog:
      given CanBreed[Dog] with
        def breed(dog: Dog): List[Dog] =
          List(Dog("Golfo"), Dog("Snoopy"), Dog("Lassy"))

    extension [A <: Animal](animal: A)
      def breed(using canBreed: CanBreed[A]): List[A] =
        canBreed.breed(animal)

    val dog = Dog("Breeder")
    println(dog.breed)

  end TypeClassSolution

  object SimplerTypeClassSolution:

    trait Animal[A]:
      def breed(a: A): List[A]

    class Dog
    object Dog:
      given Animal[Dog] with
        def breed(dog: Dog): List[Dog] = ???

    extension [A](animal: A)
      def breed(using a: Animal[A]): List[A] =
        a.breed(animal)

  end SimplerTypeClassSolution

  val dog = new SimplerTypeClassSolution.Dog
  dog.breed

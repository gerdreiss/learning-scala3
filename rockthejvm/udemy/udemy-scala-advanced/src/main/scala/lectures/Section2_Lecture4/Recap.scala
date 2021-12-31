package lectures.Section2_Lecture4

import scala.annotation.tailrec

object Recap extends App:

  @tailrec def factorial(n: Int, acc: Int): Int =
    if n <= 0 then acc
    else factorial(n - 1, n * acc)

  println(factorial(5, 1))

  class Animal
  class Dog extends Animal

  trait Carnivore:
    def eat(a: Animal): Unit

  class Crocodile extends Animal with Carnivore:
    override def eat(a: Animal): Unit =
      println("Crunchidy crunch!")

  val aDog = Dog()
  val aCroc = Crocodile()

  // dot notation
  aCroc.eat(aDog)
  // infix notation
  aCroc eat aDog

  // anonymous class
  val aCarnivore = new Carnivore:
    override def eat(a: Animal): Unit =
      println("Anonymous crunch!")

  // or like this:
  val anotherCarnivore: Carnivore =
    new:
      override def eat(a: Animal): Unit =
        println("Another carnivorous crunch!")

  // or even like this:
  val yetAnotherCarnivore: Carnivore =
    (a: Animal) => println("Yet another carnivorous crunch!")

  // functions a just classes with method `apply`:
  val inc = new Function[Int, Int]:
    override def apply(v1: Int): Int = v1 + 1
  // or
  val anonymousInc: Function1[Int, Int] =
    new:
      override def apply(v1: Int): Int = v1 + 1
  // or even
  val lambdaInc: Int => Int =
    (v1: Int) => v1 + 1

  println(inc(10))

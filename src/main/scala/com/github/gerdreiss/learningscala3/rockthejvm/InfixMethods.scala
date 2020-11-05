package com.github.gerdreiss
package learningscala3
package rockthejvm

import scala.annotation.infix

object InfixMethods extends App:

  // permissive with method names, e.g. ::, ++, -->
  // infix methods

  case class Person(name: String):
    @infix // only required for alphanumeric names
    def likes(movie: String): String = s"$name likes $movie"

  val person = Person("Mary")
  person.likes("Forrest Gump")
  person likes "Forrest Gump" // this is going to be deprecated in Scala 3.1

  @infix
  def (person: Person).enjoys(musicGenre: String): String =
    s"${person.name} listens to $musicGenre"

  extension (person: Person)
    @infix
    def enjoys2(musicGenre: String): String  =
      s"${person.name} listens to $musicGenre"

  person enjoys "Rock"
  person enjoys2 "Rock"
  
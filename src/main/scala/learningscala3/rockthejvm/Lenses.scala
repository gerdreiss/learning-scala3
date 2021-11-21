package com.github.gerdreiss.learningscala3
package rockthejvm

object Lenses extends App:

  case class Guitar(make: String, model: String)
  case class Guitarist(name: String, favoriteGuitar: Guitar)
  case class Rockband(name: String, yearFormed: Int, leadGuitarist: Guitarist)

  val metallica = Rockband("Metallica", 1991, Guitarist("Kirk Hammett", Guitar("ESP", "M II")))

  // update guitar model without lenses
  val metallicaFixed =
    metallica.copy(
      leadGuitarist = metallica.leadGuitarist.copy(
        favoriteGuitar = metallica.leadGuitarist.favoriteGuitar.copy(
          model = metallica.leadGuitarist.favoriteGuitar.model.replace(" ", "-").nn
        )
      )
    )

  val kirksFavGuitar = metallica.leadGuitarist.favoriteGuitar

  import monocle.Lens
  import monocle.macros.GenLens

  // lenses
  val guitarModelLens: Lens[Guitar, String] = GenLens[Guitar](_.model)

  // inspect a field
  val kirksGuitarModel = guitarModelLens.get(kirksFavGuitar) // M II
  // modify a field
  val formattedGuitar  =
    guitarModelLens.modify(_.replace(" ", "-").nn)(kirksFavGuitar) // Guitar("ESP", "M-II")

  val leadGuitaristLens  = GenLens[Rockband](_.leadGuitarist)
  val favoriteGuitarLens = GenLens[Guitarist](_.favoriteGuitar)
  val composedLens       = leadGuitaristLens.andThen(favoriteGuitarLens).andThen(guitarModelLens)

  val kirksGuitarModel2 = composedLens.get(metallica)
  val metallicaFixed2   = composedLens.modify(_.replace(" ", "-").nn)(metallica)

  println(s"Initial guitar: $kirksFavGuitar")
  println(s"Updated guitar: $formattedGuitar")
  println(s"Kirk's fav guitar model: $kirksGuitarModel2")
  println(s"Metallica lensefixed: $metallicaFixed2")

// prisms

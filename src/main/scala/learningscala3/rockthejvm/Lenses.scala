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
  println(s"Metallica lensfixed: $metallicaFixed2")

  // prisms
  sealed trait Shape
  case class Circle(radius: Double)                    extends Shape
  case class Rectangle(w: Double, h: Double)           extends Shape
  case class Triangle(a: Double, b: Double, c: Double) extends Shape

  val circle       = Circle(20)
  val rectangle    = Rectangle(10, 20)
  val triangle     = Triangle(3, 4, 5)
  val shape: Shape = circle

  // increase size without prisms
  val newShape = shape match
    case Circle(r)         => Circle(r * 2)
    case Rectangle(w, h)   => Rectangle(w * 2, h)
    case Triangle(a, b, c) => Triangle(a + 1, b + 1, c + 1)

  import monocle.Prism

  val circlePrism = Prism[Shape, Double] {
    case Circle(r) => Some(r)
    case _         => None
  }(Circle.apply)

  val circle2  = circlePrism(40)                 // Circle(40)
  val noRadius = circlePrism.getOption(triangle) // None
  val radius   = circlePrism.getOption(circle2)  // Some(40)

  case class Icon(background: String, shape: Shape)
  case class Logo(color: String)
  case class BrandIdentity(logo: Logo, icon: Icon)

  val brandIdentityIconLens             = GenLens[BrandIdentity](_.icon)
  val iconShapeLens                     = GenLens[Icon](_.shape)
  val brandIdentityIconCircleRadiusLens =
    brandIdentityIconLens.andThen(iconShapeLens).andThen(circlePrism)

  val brand    = BrandIdentity(Logo("red"), Icon("white", Circle(40)))
  val modified = brandIdentityIconCircleRadiusLens.modify(_ + 10)(brand)

  println(s"Original brand: $brand")
  println(s"Modified brand: $modified")

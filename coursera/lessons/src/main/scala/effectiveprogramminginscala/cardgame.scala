
enum Shape:
  case Diamond, Squiggle, Oval

enum Number:
  case One, Two, Three

enum Shading:
  case Solid, Striped, Open

enum Color:
  case Red, Green, Purple

case class Card(shape: Shape, number: Number, shading: Shading, color: Color)

val deck = (
  Card(Shape.Oval, Number.One, Shading.Open, Color.Red),
  Card(Shape.Diamond, Number.Two, Shading.Solid, Color.Green),
  Card(Shape.Squiggle, Number.Three, Shading.Striped, Color.Purple)
)

// Three cards make up a set if all the following properties are satisfied.
// 1. The three cards all have the same number of shapes, or they have three different numbers of shapes.
// 2. The three cards all have the same shape or they have three different shapes.
// 3. Then three cards all have the same shading, or they have three different shadings,
// 4. The three cards all have the same color or they have three different colors.

def checkShapes(deck: (Card, Card, Card)): Boolean =
  validSet(Set(deck._1.shape, deck._2.shape, deck._3.shape))

def checkNumbers(deck: (Card, Card, Card)): Boolean =
  validSet(Set(deck._1.number, deck._2.number, deck._3.number))

def checkShadings(deck: (Card, Card, Card)): Boolean =
  validSet(Set(deck._1.shading, deck._2.shading, deck._3.shading))

def checkColors(deck: (Card, Card, Card)): Boolean =
  validSet(Set(deck._1.color, deck._2.color, deck._3.color))

def validSet[A](set: Set[A]): Boolean =
  set.size == 1 || set.size == 3

def isValidSet(deck: (Card, Card, Card)): Boolean =
  checkShapes(deck) &&
    checkNumbers(deck) &&
    checkShadings(deck) &&
    checkColors(deck)

@main def cards(): Unit =
  println(isValidSet(deck))

enum Shape:
  case Circle(r: Int)
  case Triangle(a: Int, b: Int, c: Int)
  case Rectangle(a: Int, b: Int)

object Quadrat:
  def unapply(shape: Shape): Option[Int] =
    shape match
      case Shape.Rectangle(a, b) if a == b => Some(a)
      case _                               => None

object Equilateral:
  def unapply(shape: Shape): Option[Int] =
    shape match
      case Shape.Triangle(a, b, c) if a == b && b == c => Some(a)
      case _                                           => None

object Shape:
  def area(shape: Shape): Double =
    shape match
      case Circle(r)         => r * r * 3.14
      case Triangle(a, b, c) => a * b * c / 2
      case Equilateral(a)    => a * a * a / 2
      case Rectangle(a, b)   => a * b
      case Quadrat(a)        => a * a

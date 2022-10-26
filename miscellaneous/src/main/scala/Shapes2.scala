object Shapes2 extends App:

  //  this code is intentionally messed up, the coding problem is to go through it and cleanup
  trait Shape:
    def area: Int
    def width: Int

  case class Circle(radius: Int, pi: BigDecimal = math.Pi) extends Shape:
    override val area: Int  = (radius * radius * pi).toInt
    override val width: Int = radius

  case class Rectangle(height: Int, width: Int) extends Shape:
    override val area: Int = height * width

  case class Square(override val width: Int) extends Shape:
    override val area: Int = width * width

  def scale(toScale: Shape, factor: Int): Shape =
    // if (toScale.isInstanceOf[Circle]) {
    //  toScale.asInstanceOf[Circle].withRadius(toScale.width * factor)
    //  toScale
    // } else if (toScale.isInstanceOf[Square]) {
    //  new Square(toScale.width * factor)
    //  new Square
    // } else new Rectangle(toScale.asInstanceOf[Rectangle].heigth * factor, toScale.getWidth * factor)
    toScale match
      case Circle(radius, pi)       => Circle(radius * factor, pi)
      case Rectangle(height, width) => Rectangle(height * factor, width * factor)
      case Square(width)            => Square(width * factor)

  // create a list of Shapes with :
  // Circle(1), Rectangle(1,2), Square(1)
  val shapes = Circle(1) :: Rectangle(1, 2) :: Square(1) :: Nil
  // Display the Sum of the areas of the shapes in thev list
  println(shapes.map(_.area).sum)
  // Display the Sum of the areas of the sahpes in a List Scaled with a Factor *2
  println(shapes.map(scale(_, 2)).map(_.area).sum)
  // Display the Sum of the areas of the sahpes in a List Scaled with a Factor *4
  println(shapes.map(scale(_, 4)).map(_.area).sum)

import java.util.UUID

object ValueClasses:

  case class Product(code: String, desc: String)

  trait Backend:
    def findByCode(code: String): Option[Product]
    def findByDesc(desc: String): List[Product]

  val code = UUID.randomUUID.toString
  val desc = "a cool product"

  val backend =
    new Backend:
      def findByCode(code: String): Option[Product] = ???
      def findByDesc(desc: String): List[Product]   = ???

  backend.findByCode(code) // this is correct
  backend.findByDesc(code) // this is wrong

  // solution to the problem above
  // solution 1 - case classes with companion objects with constructors

  case class BarCode private (code: String)
  object BarCode:
    def make(code: String): Either[String, BarCode] =
      Either.cond(
        code.matches(
          """^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$"""
        ),
        BarCode(code),
        "Invalid code"
      )

  case class Desc private (desc: String)
  object Desc:
    def make(desc: String): Either[String, Desc] =
      Either.cond(desc.length >= 10, Desc(desc), "Invalid description")

  trait BackendV2:
    def findByCode(code: BarCode): Option[Product]
    def findByDesc(code: Desc): Option[Product]

  val backendV2 =
    new BackendV2:
      override def findByCode(code: BarCode): Option[Product] = ???
      override def findByDesc(code: Desc): Option[Product]    = ???

  // backendV2.findByCode(BarCode(code)) // better
  // backendV2.findByCode(BarCode(desc)) // still wrong!

  // solution 2 - extend AnyVal
  // extends AnyVal gives us:
  // - no runtime overhead
  // restrictions:
  // - exactly ONE val constructor argument
  // - no other vals inside, just methods (defs)
  // - cannot be extended
  // - can only extend universal traits (traits with just defs and without initializations)
  case class BarCodeVC(code: String) extends AnyVal:
    def countryCode: Char = code.head

  def show[T](arg: T): String = arg.toString

  // d1 this causes an instantiation of BarCode case class
  // because of type erasure of show[T]
  show(BarCodeVC(UUID.randomUUID.toString))

  // d2
  val barCodes = Array[BarCodeVC](
    BarCodeVC(UUID.randomUUID.toString), // these will
    BarCodeVC(UUID.randomUUID.toString), // also be
    BarCodeVC(UUID.randomUUID.toString)  // instantiated
  )

  // d3 in this case BarCodeVC will also be instantiated because of pattern matching
  BarCodeVC(UUID.randomUUID.toString) match
    case BarCodeVC(code) => println(code)

  // solution 3: newtype lib - solves all problems
  // For Scala 2:
  import io.estatico.newtype.macros.newtype
  import io.estatico.newtype.ops.*

  @newtype case class BarCodeNewtypeS2(code: String)

  object BarCodeNewtypeS2:
    def make(code: String): Either[String, BarCodeNewtypeS2] =
      Either.cond(
        code.matches(
          """^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$"""
        ),
        // code.coerce, <- for this to work, org.scalamacros:paradise compiler plugins is needed
        BarCodeNewtypeS2(code),
        "Invalid code"
      )

  // For Scala 3 use opaque type
  opaque type BarCodeOpaque = String

  object BarCodeOpaque:
    def make(code: String): Either[String, BarCodeOpaque] =
      Either.cond(
        code.matches(
          """^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$"""
        ),
        code, // nothing else necessary - how cool is that?
        "Invalid code"
      )

  object Usage:
    val barcodeString = UUID.randomUUID.toString
    val barcode       = BarCodeOpaque
      .make(barcodeString)
      .getOrElse(throw new java.lang.IllegalArgumentException("Invalid code"))

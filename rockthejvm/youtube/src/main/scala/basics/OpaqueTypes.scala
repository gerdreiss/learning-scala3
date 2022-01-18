package basics

object OpaqueTypes extends App:

  //   case class Name(value: String) {
  //     // some additional logic
  //   }

  object SocialNetwork: // "domain"

    opaque type Name = String
    // inside the scope, Name = String

    val name: Name = "G"

    // 1 = companion object
    object Name:
      def fromString(s: String): Option[Name] =
        if s.isEmpty || s.head.isLower then None else Some(s)

    extension (n: Name) def length: Int = n.length

  end SocialNetwork

  import SocialNetwork.*

  // outside of scope, Name != String
  // val name: Name = "G" // does not compile

  val maybeName = Name.fromString("G") // Some("G")
  maybeName.foreach(println)
  maybeName.map(_.length).foreach(println)

  object Graphics:

    opaque type Color                = Int // in hex
    opaque type ColorFilter <: Color = Int

    val Red: Color   = 0xff000000
    val Green: Color = 0x00ff0000
    val Blue: Color  = 0x0000ff00
    val halfTransparency: ColorFilter = 0x88 // 50% transparency

  end Graphics

  import Graphics.*

  case class OverlayFilter(c: Color)

  val fadeLayer = OverlayFilter(halfTransparency) // ColorFilter extends Color

// https://softwaremill.com/fancy-strings-in-scala-3/
import scala.compiletime.{ error, requireConst }

opaque type NonEmptyString = String
opaque type LowerCased     = String

object NonEmptyString:
  def apply(s: String): Option[NonEmptyString] =
    if s.isEmpty then None
    else Some(s)

  inline def from(inline s: String): NonEmptyString =
    requireConst(s)
    inline if s == "" then error("got an empty string") else s

  given Conversion[NonEmptyString, String] with
    inline def apply(nes: NonEmptyString): String = nes

end NonEmptyString

object LowerCased:
  def apply(s: String): LowerCased = s.toLowerCase().nn

  given Conversion[LowerCased, String] with
    inline def apply(lc: LowerCased): String = lc

  inline def from(inline s: String): LowerCased = ${ fromImpl('s) }

  import scala.quoted.*
  private def fromImpl(s: Expr[String])(using Quotes): Expr[NonEmptyString] =
    import quotes.reflect.*

    s.asTerm match
      case Inlined(_, _, Literal(StringConstant(str))) =>
        if LowerCased(str) == str then s
        else report.errorAndAbort(s"got a string which is not all lower case: $str")
      case _                                           =>
        report.errorAndAbort(s"got a value which is not a constant string: ${s.show}")

end LowerCased

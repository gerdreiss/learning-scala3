import scala.util.parsing.combinator.RegexParsers

final case class WordFreq(word: String, count: Int):
  override def toString = s"Word <$word> occurs with frequency $count"

object SimpleParser extends RegexParsers:
  def word: Parser[String]   = """[a-z]+""".r
  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def freq: Parser[WordFreq] = word ~ number ^^ { case wd ~ fr => WordFreq(wd, fr) }

@main def SimpleParserExe(): Unit =
  import SimpleParser.*
  parse(freq, "johnny 121") match
    case Success(matched, _) => println(matched)
    case Failure(msg, _)     => println(s"FAILURE: $msg")
    case Error(msg, _)       => println(s"ERROR: $msg")

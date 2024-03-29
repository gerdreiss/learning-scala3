import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.*

inline def summonAll[T <: Tuple]: List[Eq[?]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[Eq[t]] :: summonAll[ts]

trait Eq[T]:
  def eqv(x: T, y: T): Boolean

object Eq:
  given Eq[Int] with
    def eqv(x: Int, y: Int): Boolean = x == y

  def check(elem: Eq[?])(x: Any, y: Any): Boolean =
    elem.asInstanceOf[Eq[Any]].eqv(x, y)

  def iterator[T](p: T): Iterator[Any] =
    p.asInstanceOf[Product].productIterator

  def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[?]]): Eq[T] =
    (x: T, y: T) =>
      val ordx = s.ordinal(x)
      (s.ordinal(y) == ordx) && check(elems(ordx))(x, y)

  def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[?]]): Eq[T] =
    (x: T, y: T) =>
      iterator(x)
        .zip(iterator(y))
        .zip(elems.iterator)
        .forall { case ((x, y), elem) =>
          check(elem)(x, y)
        }

  inline given derived[T](using m: Mirror.Of[T]): Eq[T] =
    lazy val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)

end Eq

enum Opt[+T] derives Eq:
  case Sm(t: T)
  case Nn

@main def EqDerivation(): Unit =
  import Opt.*
  val eqoi = summon[Eq[Opt[Int]]]
  assert(eqoi.eqv(Sm(23), Sm(23)))
  assert(!eqoi.eqv(Sm(23), Sm(13)))
  assert(!eqoi.eqv(Sm(23), Nn))

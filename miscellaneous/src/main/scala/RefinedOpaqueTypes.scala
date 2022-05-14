import eu.timepit.refined.*
import eu.timepit.refined.api.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.boolean.*
import eu.timepit.refined.collection.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.string.*
import io.estatico.newtype.macros.*

object RefinedOpaqueTypes extends App:

  type Rgx = "^[a-zA-Z]+(([',. -][a-zA-Z ])?[a-zA-Z]*)*$"

  opaque type CardNameT       = String Refined MatchesRegex[Rgx]
  opaque type CardNumberT     = String Refined (Size[16] And ValidLong)
  opaque type CardExpirationT = String Refined (Size[4] And ValidInt)
  opaque type CardCVVT        = String Refined (Size[3] And ValidInt)

  @newtype case class CardName(name: CardNameT)
  @newtype case class CardNumber(number: CardNumberT)
  @newtype case class CardExpiration(expiration: CardExpirationT)
  @newtype case class CardCVV(cvv: CardCVVT)

  object CardNameT:
    def apply(name: String): Either[String, CardName] =
      refineV[MatchesRegex[Rgx]](name).map(CardName(_))

  // these don't work - this kind of functionality is better done with zio-prelude's Newtype[T]
  // object CardNumberT:
  //   def apply(number: String): Either[String, CardNumber] =
  //     refineV[Size[16] And ValidLong](number).map(CardNumber(_))
  // object CardExpirationT:
  //   def apply(expiration: String): Either[String, CardExpiration] =
  //     refineV[Size[4] And ValidInt](expiration).map(CardExpiration(_))
  // object CardCVVT:
  //   def apply(cvv: String): Either[String, CardCVV] =
  //     refineV[Size[3] And ValidInt](cvv).map(CardCVV(_))

  case class Card(
      name: CardName,
      number: CardNumber,
      expiration: CardExpiration,
      ccv: CardCVV
  )

  // this does not work (yet?) in Scala 3
  // see https://github.com/fthomas/refined/issues/932
  // val card = Card("Max Muster", 1000000000000000L, "0223", 999)
  println // (card)

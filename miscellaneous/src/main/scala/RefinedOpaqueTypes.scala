import eu.timepit.refined.*
import eu.timepit.refined.api.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.boolean.*
import eu.timepit.refined.collection.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.string.*
import io.estatico.newtype.macros.*

object RefinedOpaqueTypes extends App :

  type Rgx = "^[a-zA-Z]+(([',. -][a-zA-Z ])?[a-zA-Z]*)*$"

  opaque type CardNameT       = String Refined MatchesRegex[Rgx]
  opaque type CardNumberT     = Long Refined Size[16]
  opaque type CardExpirationT = String Refined (Size[4] And ValidInt)
  opaque type CardCVVT        = Int Refined Size[3]

  @newtype case class CardName(name: CardNameT)
  @newtype case class CardNumber(number: CardNumberT)
  @newtype case class CardExpiration(expiration: CardExpirationT)
  @newtype case class CardCVV(cvv: CardCVVT)

  case class Card(
    name: CardName,
    number: CardNumber,
    expiration: CardExpiration,
    ccv: CardCVV
  )

  // this does not work (yet?) in Scala 3
  // see https://github.com/fthomas/refined/issues/932
  // val card = Card("Max Muster", 1000000000000000L, "0223", 999)
  println

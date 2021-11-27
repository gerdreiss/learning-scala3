import sbt._

object Dependencies {
  case object dev {
    case object optics {
      val `monocle-core` =
        "dev.optics" %% "monocle-core" % "3.1.0"
      val `monocle-macro` =
        "dev.optics" %% "monocle-macro" % "3.1.0"
    }
  }
  case object eu  {
    case object timepit {
      val refined =
        "eu.timepit" %% "refined" % "0.9.27"
    }
  }
  case object io  {
    case object estatico {
      val newtype =
        "io.estatico" %% "newtype" % "0.4.4"
    }
  }

  case object org {
    case object scalatest {
      val scalatest =
        "org.scalatest" %% "scalatest" % "3.2.10"
    }

    case object scalatestplus {
      val `scalacheck-1-15` =
        "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0"
    }

    case object tpolecat {
      val typename =
        "org.tpolecat" %% "typename" % "1.0.0"
    }

    case object typelevel {
      val `cats-core` =
        "org.typelevel" %% "cats-core" % "2.6.1"
      val kittens =
        "org.typelevel" %% "kittens" % "3.0.0-M1"
    }
  }
}

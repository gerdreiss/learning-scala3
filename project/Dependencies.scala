import sbt._

object Dependencies {
  case object io {
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
  }
}

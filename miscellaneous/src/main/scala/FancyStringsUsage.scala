// https://softwaremill.com/fancy-strings-in-scala-3/
object Usage:
  // compilation fails:
  // Found:    ("abc" : String)
  // Required: NonEmptyString
  // val x0: NonEmptyString = "abc"

  // compilation succeeds
  val x1: NonEmptyString = NonEmptyString.from("abc")

  // compilation fails:
  // got an empty string
  // val x2: NonEmptyString = NonEmptyString.from("")

  // compilation fails:
  // expected a constant value but found: Usage.
  val z2 = "x" * 10
  // val x3: NonEmptyString = NonEmptyString.from(z2)

  // implicit conversion at work
  def test(param: String) = s"Got: $param"
  println(test(x1))

  // compilation succeeds
  val p1: LowerCased = LowerCased("Abc")

  // compilation succeeds
  LowerCased.from("abc")

  // compilation fails:
  // got a string which is not all lower case: Abc
  // LowerCased.from("Abc")

  // compilation fails:
  // got a value which is not a constant string: Usage.z
  val z1 = "x" * 10
  // LowerCased.from(z1)

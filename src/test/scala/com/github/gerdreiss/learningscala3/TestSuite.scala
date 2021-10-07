package com.github.gerdreiss
package learningscala3

import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.adhocExtensions

trait TestSuite
    extends AnyFunSuite
       with should.Matchers
       with GivenWhenThen
       with BeforeAndAfterAll
       with BeforeAndAfterEach
       with ScalaCheckPropertyChecks:
  final protected type Arbitrary[A] =
    org.scalacheck.Arbitrary[A]

  final protected val Arbitrary =
    org.scalacheck.Arbitrary

  final protected type Assertion =
    org.scalatest.compatible.Assertion

  final protected type Gen[+A] =
    org.scalacheck.Gen[A]

  final protected val Gen =
    org.scalacheck.Gen

package com.github.gerdreiss
package learningscala3
package rockthejvm

import java.io.File

object NewTypes extends App:

  // 1 - literal types
  val aNumber = 3
  val three: 3 = 3

  def passNumber(n: Int) = println(n)

  passNumber(aNumber)
  passNumber(three)

  def passStrict(n: 3) = println(n)
  
  passStrict(three) // OK
  // passStrict(aNumber) // does not compile

  val pi: 3.14 = 3.14
  val truth: true = true
  val myFavLang: "Haskell" = "Haskell" // would not compile: "Scala"

  def meaningOfLife(meaning: Option[42]) = meaning.foreach(println)

  // 2 - union types
  def ambivalentMethod(arg: String | Int) =
    arg match
      case _: String => println(s"String: $arg")
      case _: Int    => println(s"Int: $arg")

  ambivalentMethod(aNumber)
  ambivalentMethod(myFavLang)

  type ErrorOr[T] = T | "error"

  def handleResource(file: ErrorOr[File]): Unit = ()

  val stringOrInt: ErrorOr[Int] =
   if (aNumber < 0) "error" else aNumber

  // 3 - intersection types
  trait Camera:
      def takePhoto() = println("snap")
  trait Phone:
      def makeCall() = println("ring")

  def useSmartDevice(device: Camera & Phone) =
      device.takePhoto()
      device.makeCall()

  class SmartPhone extends Camera with Phone

  useSmartDevice(new SmartPhone)


  trait HostConfig
  trait HostController:
      def get: Option[HostConfig]

  trait PortConfig
  trait PortController:
      def get: Option[PortConfig]

  def getConfigs(
      controller: HostController & PortController
    ): Option[HostConfig & PortConfig] = controller.get


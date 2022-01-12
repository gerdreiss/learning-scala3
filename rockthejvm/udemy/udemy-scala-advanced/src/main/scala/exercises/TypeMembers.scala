package exercises

trait MList:
  type A
  def head: A
  def tail: MList

trait NumberList:
  type A <: Number

// class CustomList(h: String, t: CustomList) extends MList with NumberList:
//   type A = String
//   def head: String = h
//   def tail: CustomList = t

class IntList(h: Int, t: IntList) extends MList:
  override type A = Int
  def head: A = h
  def tail: IntList = t

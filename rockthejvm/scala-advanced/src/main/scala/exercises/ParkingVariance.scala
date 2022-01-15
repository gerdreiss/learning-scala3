package exercises

class InvariantParking[T](vehicles: List[T]):
  def park(vehicle: T): InvariantParking[T] = ???
  def impound(vehicles: List[T]): InvariantParking[T] = ???
  def checkVehicles(conditions: String): List[T] = ???

class CovariantParking[+T](vehicles: List[T]):
  def park[S >: T](vehicle: S): CovariantParking[S] = ???
  def impound[S >: T](vehicle: S): CovariantParking[S] = ???
  def checkVehicles(conditions: String): List[T] = ???

class ContravariantParking[-T](vehicles: List[T]):
  def park(vehicle: T): ContravariantParking[T] = ???
  def impound(vehicle: T): ContravariantParking[T] = ???
  def checkVehicles[S <: T](conditions: String): List[S] = ???

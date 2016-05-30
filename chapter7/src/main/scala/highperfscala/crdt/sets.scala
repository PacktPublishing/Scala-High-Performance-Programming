package highperfscala.crdt

class OperationBasedGSet[A](set: Set[A]) {

  def value = set

  def contains(a: A) = set.contains(a)

  def update(a: AddElement[A]): (OperationBasedGSet[A], AddElement[A]) =
    (new OperationBasedGSet(set + a.a), a)

  def merge(other: AddElement[A]): OperationBasedGSet[A] =
    new OperationBasedGSet(set + other.a)

}

case class AddElement[A](a: A)

case class GSetState[A](set: Set[A])

class StateBasedGSet[A](set: Set[A]) {

  def value: Set[A] = set

  def contains(a: A): Boolean = set.contains(a)

  def update(a: AddElement[A]): (StateBasedGSet[A], GSetState[A]) = {
    val newSet = new StateBasedGSet(set + a.a)
    (newSet, GSetState(newSet.value))
  }

  def merge(other: GSetState[A]): StateBasedGSet[A] = {
    new StateBasedGSet(set ++ other.set)
  }

}
package proteinrefinery.ui

import nutcracker.IncSet
import nutcracker.IncSet.IncSetRef

import scalaz.Show

abstract class Goal {
  type A

  def ref: IncSetRef[A]
  def value: IncSet[A]
  def show: Show[A]
}

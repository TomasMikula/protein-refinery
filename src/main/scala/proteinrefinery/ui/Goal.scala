package proteinrefinery.ui

import nutcracker.{DRef, IncSet}

import scalaz.Show

abstract class Goal {
  type A

  def ref: DRef[IncSet[A]]
  def value: IncSet[A]
  def show: Show[A]
}

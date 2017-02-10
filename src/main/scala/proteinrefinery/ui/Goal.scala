package proteinrefinery.ui

import nutcracker.IncSet

import scalaz.Show

abstract class Goal[Ref[_]] {
  type A

  def ref: Ref[IncSet[A]]
  def value: IncSet[A]
  def show: Show[A]
}

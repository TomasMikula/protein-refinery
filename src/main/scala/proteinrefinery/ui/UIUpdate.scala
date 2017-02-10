package proteinrefinery.ui

import nutcracker.{Dom, IncSet}

import scalaz.Show

trait UIUpdate[M[_], Ref[_]] {
  def initGoal[A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], desc: String): M[Unit]
  def addSolution[A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): M[Unit]
  def updateSolution[A[_[_]]](t: GoalType[A], goalRef: Ref[IncSet[Ref[A[Ref]]]], solRef: Ref[A[Ref]], sol: A[Ref])(implicit dom: Dom[A[Ref]], show: Show[A[Ref]]): M[Unit]
  def newFact[A[_[_]]](t: FactType[A], fact: A[Ref])(implicit A: Show[A[Ref]]): M[Unit]
}
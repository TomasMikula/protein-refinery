package proteinrefinery.ui

import nutcracker.{Dom, IncSet}

import scalaz.Show

trait UIUpdate[M[_], Ref[_]] {
  def initGoal[A](t: GoalType[A], goalRef: Ref[IncSet[Ref[A]]], desc: String): M[Unit]
  def addSolution[A](t: GoalType[A], goalRef: Ref[IncSet[Ref[A]]], solRef: Ref[A], sol: A)(implicit dom: Dom[A], show: Show[A]): M[Unit]
  def updateSolution[A](t: GoalType[A], goalRef: Ref[IncSet[Ref[A]]], solRef: Ref[A], sol: A)(implicit dom: Dom[A], show: Show[A]): M[Unit]
  def newFact[A](t: FactType[A], fact: A)(implicit A: Show[A]): M[Unit]
}
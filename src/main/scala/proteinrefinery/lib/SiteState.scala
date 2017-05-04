package proteinrefinery.lib

import nutcracker.data.Promise
import nutcracker.util.{DeepEqual, DeepShow, IsEqual, MonadObjectOutput}
import proteinrefinery.util.{HomSet, Unification}

import scalaz.{Equal, Show}
import scalaz.syntax.equal._

case class StateLabel(value: String) extends AnyVal {
  override def toString = value
}

object StateLabel {
  implicit val equalInstance: Equal[StateLabel] = new Equal[StateLabel] {
    def equal(s1: StateLabel, a2: StateLabel): Boolean = s1.value == a2.value
  }

  implicit val showInstance: Show[StateLabel] = new Show[StateLabel] {
    override def shows(s: StateLabel): String = s.value
  }

  implicit def deepEqualInstance[Ptr1[_], Ptr2[_]]: DeepEqual[StateLabel, StateLabel, Ptr1, Ptr2] =
    new DeepEqual[StateLabel, StateLabel, Ptr1, Ptr2] {
      def equal(a1: StateLabel, a2: StateLabel): IsEqual[Ptr1, Ptr2] = IsEqual(equalInstance.equal(a1, a2))
    }

  implicit def deepShowInstance[Ptr[_]]: DeepShow[StateLabel,Ptr] = new DeepShow.FromSerialize[StateLabel, Ptr] {
    def serialize[M[_]](a: StateLabel)(implicit ev: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      ev.write(a.value)
  }
}

object SiteState {
  type SiteState = Promise[StateLabel]
  type Update = Promise.Update[StateLabel]
  type Delta = Promise.Delta[StateLabel]

  def apply(label: String): SiteState = Promise.completed(StateLabel(label))

  def meet(s1: SiteState, s2: SiteState): SiteState =
    Promise.meet(s1, s2)

  implicit def homSet: HomSet.Aux[SiteState, List[Unit]] = new HomSet[SiteState] {
    type HomSet = List[Unit]

    def homSet(s1: SiteState, s2: SiteState): HomSet =
      if (s1 === s2) List(())
      else Nil
  }

  implicit def unificationInstance: Unification.Aux[SiteState, Update, Delta] =
    Unification.promiseUnification[StateLabel]
}
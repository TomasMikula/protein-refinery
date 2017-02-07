package proteinrefinery.lib

import scala.language.higherKinds
import nutcracker.util.{DeepEqual, DeepShow, IsEqual, MonadObjectOutput}

import scalaz.{Equal, Show}

case class SiteLabel(name: String) {
  override def toString = name
}

object SiteLabel {

  implicit def equalInstance: Equal[SiteLabel] = new Equal[SiteLabel] {
    def equal(s1: SiteLabel, s2: SiteLabel): Boolean = s1.name == s2.name
  }

  implicit def deepEqualInstance[Ptr1[_], Ptr2[_]]: DeepEqual[SiteLabel, SiteLabel, Ptr1, Ptr2] =
    new DeepEqual[SiteLabel, SiteLabel, Ptr1, Ptr2] {
      def equal(s1: SiteLabel, s2: SiteLabel): IsEqual[Ptr1, Ptr2] = IsEqual(s1.name == s2.name)
    }

  implicit def showInstance: Show[SiteLabel] = new Show[SiteLabel] {
    override def shows(s: SiteLabel): String = s.name
  }

  implicit def deepShowInstance[Ptr[_]]: DeepShow[SiteLabel, Ptr] = new DeepShow.FromSerialize[SiteLabel, Ptr] {
    def serialize[M[_]](a: SiteLabel)(implicit ev: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      ev.write(a.name)
  }
}


package proteinrefinery

import nutcracker.Dom
import proteinrefinery.util.{Identification, Unification}

trait ImplicitConversions extends ImplicitConversions0

trait ImplicitConversions0 {
  implicit def identificationToUnification[A](implicit I: Identification[A]): Unification.Aux[A, I.Update, I.Delta, I.F] = I.unification
  implicit def identificationToDom[A](implicit I: Identification[A]): Dom.Aux[A, I.Update, I.Delta] = I.unification.dom
}

trait ImplicitConversions1 {
  implicit def unificationToDom[A](implicit U: Unification[A]): Dom.Aux[A, U.Update, U.Delta] = U.dom
}
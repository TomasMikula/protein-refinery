package proteinrefinery.util

/** Identification that is reflexive, i.e.
  *   ∀a necessarilySame(a, a)
  */
trait ReflexiveIdentification[A] extends Identification[A]

package protein.mechanism

import protein.capability.{Binding, BindingPartner}

case class CompetitiveBinding(
  binding: Binding,
  competingLeft: BindingPartner
) {
  override def toString = s"Binding ${Binding(competingLeft, binding.right)} competes with binding $binding"
}

package protein.capability

case class Binding(left: BindingPartner, right: BindingPartner) {
  def flip: Binding = Binding(right, left)

  override def toString = s"${left.p}>${left.s} - ${right.s}<${right.p}"
}

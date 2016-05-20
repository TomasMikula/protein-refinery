package protein.capability

import protein.mechanism.Site

case class BindingPartnerPattern(p: ProteinPattern, s: Site) {
  def overlaps(that: BindingPartnerPattern): Boolean = (this.s == that.s) && (this.p isCompatibleWith that.p)
}

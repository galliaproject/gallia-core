package gallia
package vldt

import meta._

// ===========================================================================
@NumberAbstraction
object MetaValidationCompatibility {

  // TODO: handle some aliases, eg Nes,...
  def compatible(x: Cls, y: Cls): Boolean = x == y

  def compatible(x: Containee, y: Containee): Boolean = x == y // FIXME

  // ---------------------------------------------------------------------------
  def compatible(x: Info, y: Info, mode: SpecialCardiMode): Boolean =
    if (mode == SpecialCardiMode.IgnoreAltogether)
      x.toRequired.toNonMultiple ==
      y.toRequired.toNonMultiple

    // ---------------------------------------------------------------------------
    else if (mode == SpecialCardiMode.IgnoreRequiredness)
      x.toRequired ==
      y.toRequired

    // ---------------------------------------------------------------------------
    else
      x == y // FIXME


}

// ===========================================================================

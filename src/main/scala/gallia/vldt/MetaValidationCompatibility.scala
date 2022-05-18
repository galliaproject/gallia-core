package gallia
package vldt

import meta._

// ===========================================================================
object MetaValidationCompatibility {

  def compatible(x: Info, y: Info, mode: SpecialCardiMode): Boolean =
         if (mode == SpecialCardiMode.IgnoreAltogether)   compatible(x.toRequired.toSingle, y.toRequired.toSingle)
    else if (mode == SpecialCardiMode.IgnoreRequiredness) compatible(x.toRequired,          y.toRequired)
    else                                                  compatible(x,                     y)

  // ===========================================================================
  def compatible(x: Info, y: Info1, mode: SpecialCardiMode): Boolean =
         if (mode == SpecialCardiMode.IgnoreAltogether)                               oneCompatible(x.toSingle.union, y.toSingle.subInfo)
    else if (mode == SpecialCardiMode.IgnoreRequiredness)                             oneCompatible(x         .union, y         .subInfo)
    else                                                  x.optional == y.optional && oneCompatible(x         .union, y         .subInfo)

  // ===========================================================================
  def oneCompatible(xs: Seq[SubInfo], y: SubInfo): Boolean =
      xs.exists(compatible(_, y))

    // ---------------------------------------------------------------------------
    def compatible(x: Cls ,    y: Cls)    : Boolean = x == y // TODO: handle some aliases, eg Nes,...
    def compatible(x: Info,    y: Info)   : Boolean = x == y // FIXME
    def compatible(x: SubInfo, y: SubInfo): Boolean = x == y || (x.isEnm && y.isEnm /* 220506101842 */) // FIXME
}

// ===========================================================================

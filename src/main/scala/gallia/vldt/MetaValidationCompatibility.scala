package gallia
package vldt

import meta._

// ===========================================================================
object MetaValidationCompatibility {

  def compatible(x: Ofni, y: Ofni, mode: SpecialCardiMode): Boolean =
         if (mode == SpecialCardiMode.IgnoreAltogether)   compatible(x.toRequired.toSingle, y.toRequired.toSingle)
    else if (mode == SpecialCardiMode.IgnoreRequiredness) compatible(x.toRequired,          y.toRequired)
    else                                                  compatible(x,                     y)

  // ===========================================================================
  def compatible(x: Ofni, y: Ofnu, mode: SpecialCardiMode): Boolean =
         if (mode == SpecialCardiMode.IgnoreAltogether)                               oneCompatible(x.toSingle.infos, y.toSingle.info)
    else if (mode == SpecialCardiMode.IgnoreRequiredness)                             oneCompatible(x         .infos, y         .info)
    else                                                  x.optional == y.optional && oneCompatible(x         .infos, y         .info)

  // ===========================================================================
  def oneCompatible(xs: Seq[Info], y: Info): Boolean =
      xs.exists(compatible(_, y))

    // ---------------------------------------------------------------------------
    def compatible(x: Cls      , y: Cls)      : Boolean = x == y // TODO: handle some aliases, eg Nes,...
    def compatible(x: Ofni     , y: Ofni)     : Boolean = x == y // FIXME
    def compatible(x: Info     , y: Info)     : Boolean = x == y // FIXME
    def compatible(x: Containee, y: Containee): Boolean = x == y // FIXME

}

// ===========================================================================

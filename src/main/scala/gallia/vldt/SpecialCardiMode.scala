package gallia
package vldt

import enumeratum.{Enum, EnumEntry}

// ===========================================================================
sealed trait SpecialCardiMode extends EnumEntry { /* eg for remove conditionally */
    def isNormal             : Boolean = this == SpecialCardiMode.Normal
    def isIgnoreRequiredness : Boolean = this == SpecialCardiMode.IgnoreRequiredness
    def isIgnoreAltogether   : Boolean = this == SpecialCardiMode.IgnoreAltogether }

  // ---------------------------------------------------------------------------
  object SpecialCardiMode extends Enum[SpecialCardiMode] {
    val values = findValues

    case object Normal             extends SpecialCardiMode
    case object IgnoreRequiredness extends SpecialCardiMode // still needed?
    case object IgnoreAltogether   extends SpecialCardiMode
  }

// ===========================================================================
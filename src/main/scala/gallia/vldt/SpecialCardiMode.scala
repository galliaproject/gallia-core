package gallia
package vldt

import enumeratum.{Enum, EnumEntry}

// ===========================================================================
sealed trait SpecialCardiMode extends EnumEntry /* eg for remove conditionally */

  // ---------------------------------------------------------------------------
  object SpecialCardiMode extends Enum[SpecialCardiMode] {
    val values = findValues

    case object Normal             extends SpecialCardiMode
    case object IgnoreRequiredness extends SpecialCardiMode // still needed?
    case object IgnoreAltogether   extends SpecialCardiMode
  }

// ===========================================================================
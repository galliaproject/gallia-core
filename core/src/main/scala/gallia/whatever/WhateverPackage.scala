package gallia

// ===========================================================================
package object whatever {
  private[whatever] val StringOrdering = implicitly[Ordering[String]]

  // ---------------------------------------------------------------------------
  /** else causes issues with scala 3 if used directly in Whatever.capitalize */
  private[whatever] def capitalize(value: String): String = value.capitalize }

// ===========================================================================

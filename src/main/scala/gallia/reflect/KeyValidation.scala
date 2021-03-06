package gallia.reflect

// ===========================================================================
object KeyValidation { // TODO: t210127133546 - move (also used by meta validation)
  private val ValidKeyChars = aptus.utils.CharUtils.AlphaNumericalSet ++ Set('-', '_', '@', ':', '.')
  private val MaxKeySize = 100

  // ---------------------------------------------------------------------------
  def isValid(key: Symbol): Boolean = isValid(key.name)

  def isValid(key: String): Boolean =
    key.size >  0 &&
    key.size <= MaxKeySize &&
    key.forall(ValidKeyChars.contains)
}

// ===========================================================================
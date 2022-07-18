package gallia

// ===========================================================================
package object data {
  type BasicType = reflect.BasicType
  val  BasicType = reflect.BasicType

  type PathPair = domain.PathPair
  val  PathPair = domain.PathPair

  // ---------------------------------------------------------------------------
  private[data] val Base64StringPrefix = "base64:"
}

// ===========================================================================

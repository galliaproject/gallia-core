package gallia

// ===========================================================================
package object vldt {
  private[vldt] val  TypeNode = gallia.reflect.TypeNode
  private[vldt] type TypeNode = gallia.reflect.TypeNode

  private[vldt] val  Container = gallia.reflect.Container
  private[vldt] type Container = gallia.reflect.Container

  private[vldt] val  BasicType = gallia.reflect.BasicType
  private[vldt] type BasicType = gallia.reflect.BasicType

  // ===========================================================================
  case class Err private (details: Any) { def format = s"ERROR: ${details}" } //TODO: more structure
}

// ===========================================================================

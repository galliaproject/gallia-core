package gallia

// ===========================================================================
package object vldt {
  private[vldt] val  TypeNode = gallia.reflect.TypeNode
  private[vldt] type TypeNode = gallia.reflect.TypeNode

  private[vldt] val  Container = gallia.reflect.Container
  private[vldt] type Container = gallia.reflect.Container

  private[vldt] val  BasicType = gallia.meta.basic.BasicType
  private[vldt] type BasicType = gallia.meta.basic.BasicType

  // ===========================================================================
  case class Err private[gallia] (details: Any) { def format = s"ERROR: ${details}" } //TODO: more structure
}

// ===========================================================================

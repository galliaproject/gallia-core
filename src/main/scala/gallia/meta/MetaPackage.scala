package gallia

// ===========================================================================
package object meta {
  private[meta] type Container = gallia.reflect.Container
  private[meta] val  Container = gallia.reflect.Container

  private[meta] type BasicType = gallia.reflect.BasicType
  private[meta] val  BasicType = gallia.reflect.BasicType

  private[meta] type TypeNode  = gallia.reflect.TypeNode
  private[meta] val  TypeNode  = gallia.reflect.TypeNode

  // ===========================================================================
  type NumericalType       = gallia.reflect.NumericalType
    type UnboundedNumber   = gallia.reflect.UnboundedNumber
    type   BoundedNumber   = gallia.reflect.  BoundedNumber
      type IntegerLikeType = gallia.reflect.IntegerLikeType
      type RealLikeType    = gallia.reflect.   RealLikeType  

}

// ===========================================================================

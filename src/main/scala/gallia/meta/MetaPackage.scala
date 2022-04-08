package gallia

import aptus.Seq_

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

  // ===========================================================================
  case class FldPair(field1: Fld, field2: Fld) {
    override def toString: String = formatDefault
      def formatDefault: String = Seq(field1.formatDefault, field2.formatDefault).section
  }
  
  // ===========================================================================
  /** PNF = Potentially Nested Field */
  case class PNF(path: KPath, info: Info) extends FldLike {
                override val key = path.key
  
      protected override val _container:      Container  = info.container
      protected override val _containee:      Containee  = info.containee
      protected override val _containees: Seq[Containee] = Seq(_containee) // see t210125111338 (union types)    
  }
      
}

// ===========================================================================

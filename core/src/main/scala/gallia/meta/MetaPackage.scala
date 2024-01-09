package gallia

import aptus.Seq_

// ===========================================================================
package object meta {

  object MetaKey {
    val _fields   = "fields"
      val _key      = "key"
      val _info     = "info"
        val _optional = "optional"
        val _union    = "union"
          val _multiple  = "multiple"
          val _valueType = "valueType" }

  // ===========================================================================
  private[gallia] type Container = gallia.reflect.Container
  private[gallia] val  Container = gallia.reflect.Container

  private[gallia] type BasicType = gallia.meta.basic.BasicType
  private[gallia] val  BasicType = gallia.meta.basic.BasicType

  private[meta] type TypeNode  = gallia.reflect.TypeNode
  private[meta] val  TypeNode  = gallia.reflect.TypeNode

  // ===========================================================================
  type NumericalType       = gallia.meta.basic.NumericalType
    type UnboundedNumber   = gallia.meta.basic.UnboundedNumber
    type   BoundedNumber   = gallia.meta.basic.  BoundedNumber
      type IntegerLikeType = gallia.meta.basic.IntegerLikeType
      type RealLikeType    = gallia.meta.basic.   RealLikeType

  // ===========================================================================
  case class FldPair(field1: Fld, field2: Fld) {
    override def toString: String = formatDefault
      def formatDefault: String = Seq(field1.formatDefault, field2.formatDefault).section
  }

  // ===========================================================================
  /** PNF = Potentially Nested Field */
  // see t210125111338 (union types) - adapt
  case class PNF(path: KPath, optional: Optional, multiple: Multiple, valueType: ValueType) extends HasKey with Info1Like {
    final override           val  key  : Key   = path.key
    final override protected val _info1: Info1 = Info1(optional, multiple, valueType)

    // ---------------------------------------------------------------------------
    def info: Info = Info(optional, Seq(SubInfo(multiple, valueType)))

    // ---------------------------------------------------------------------------
    override def toString = formatDefault
      def formatDefault: String =
        s"${path.formatDefault}:${optional}:${multiple}:${valueType}"
  }

}

// ===========================================================================

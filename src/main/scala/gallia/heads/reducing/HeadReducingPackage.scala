package gallia
package heads

// ===========================================================================
package object reducing extends _heads {
  type Values = List[Option[gallia.AnyValue]]

  type Fld = gallia.meta.Fld
  val  Fld = gallia.meta.Fld

  type BasicType = gallia.meta.basic.BasicType
  val  BasicType = gallia.meta.basic.BasicType

  type NumericalType = gallia.meta.basic.NumericalType }

// ===========================================================================
package gallia
package heads

// ===========================================================================
package object reducing extends _heads {
  type Values = List[Option[gallia.AnyValue]]

  type Fld = gallia.meta.Fld
  val  Fld = gallia.meta.Fld

  type BasicType = gallia.reflect.BasicType
  val  BasicType = gallia.reflect.BasicType

  type NumericalType = gallia.reflect.NumericalType
}

// ===========================================================================
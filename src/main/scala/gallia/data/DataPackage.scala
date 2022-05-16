package gallia

// ===========================================================================
package object data {
  type BasicType = reflect.BasicType
  val  BasicType = reflect.BasicType

  // ---------------------------------------------------------------------------
  trait HasTaxOpt { def valueOpt(c: Cls): Option[Obj => Obj] }
}

// ===========================================================================

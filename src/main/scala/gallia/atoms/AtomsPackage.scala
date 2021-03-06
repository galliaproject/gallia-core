package gallia

// ===========================================================================
package object atoms {
  type _ff11 = gallia.FunctionWrappers._ff11
  type _ff21 = gallia.FunctionWrappers._ff21

  // ---------------------------------------------------------------------------
  val ErrorId       = gallia.vldt.ErrorId
  val _Error = gallia.vldt._Error

  // ---------------------------------------------------------------------------
  type Obj  = gallia.data.single  .Obj
  type Objs = gallia.data.multiple.Objs

  // ---------------------------------------------------------------------------
  type Key  = gallia.Key
  type AnyValue = gallia.AnyValue

  // ---------------------------------------------------------------------------
  type StringSplitter = gallia.domain.StringSplitter

  // ---------------------------------------------------------------------------
  type _Nested = gallia.atoms.AtomsOthers._Nested
  val  _Nested = gallia.atoms.AtomsOthers._Nested

  type _Rename = gallia.atoms.AtomsUUVeryBasics._Rename
  val  _Rename = gallia.atoms.AtomsUUVeryBasics._Rename
}

// ===========================================================================
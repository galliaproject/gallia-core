package gallia
package selection

// ===========================================================================
package object typed {
  type TargetQuery[T] = gallia.trgt.TargetQuery[T]
  val  TargetQuery    = gallia.trgt.TargetQuery

  // ---------------------------------------------------------------------------
  type TypeNode = gallia.reflect.TypeNode

  // ---------------------------------------------------------------------------
  type TqKey    = TargetQuery[Key]
  type TqRen    = TargetQuery[Ren]
  type TqKeyz   = TargetQuery[Keyz]
  type TqKPath  = TargetQuery[KPath]
  type TqRPathz = TargetQuery[RPathz]

  // ---------------------------------------------------------------------------
  val TsBoilerplate = TsBase
}

// ===========================================================================
package gallia.selection

import gallia._

// ===========================================================================
package object typed {
  type TargetQuery[T] = gallia.target.TargetQuery[T]
  val  TargetQuery    = gallia.target.TargetQuery

  // ---------------------------------------------------------------------------
  type TypeNode = gallia.reflect.TypeNode

  // ---------------------------------------------------------------------------
  type TQKey    = TargetQuery[Key]
  type TQRen    = TargetQuery[Ren]
  type TqKeyz   = TargetQuery[Keyz]
  type TqKPath  = TargetQuery[KPath]
  type TqRPathz = TargetQuery[RPathz]

  // ---------------------------------------------------------------------------
  private[typed] def node[T: gallia.WTT] = gallia.reflect.TypeNode.parse[T]

  // ---------------------------------------------------------------------------
  val TsBoilerplate = TsBase
}

// ===========================================================================
package gallia.heads

// ===========================================================================
package object common extends _heads {
  val      TargetQueryUtils = gallia.target.utils.     TargetQueryUtils
  val TypedTargetQueryUtils = gallia.target.utils.TypedTargetQueryUtils

  // ---------------------------------------------------------------------------
  type StringSplitter = gallia.domain.StringSplitter

  // ---------------------------------------------------------------------------
  val pwrap = gallia.FunctionWrappers.pwrap _

  // ===========================================================================
  type Grab       [T] = TSL.Squash.TSelector[T]
  type Squash     [T] = TSL.Squash.TSelector[T]
  type Pivot      [T] = TSL.Squash.TSelector[T] // t200924162200 - pivot must be compatible with grab/squash's

  type AssertData [T] = TSL.AssertData.TSelector[T]

  type Fuse       [T] = TSL.FuseFission.TSelector[T]
  type Fission    [T] = TSL.FuseFission.TSelector[T]

  type Generate1  [T] = TSL.Generate1.TSelector[T]
  type Generate2  [T] = TSL.Generate2.TSelector[T]

  type RemoveIf   [T] = TSL.RemoveIf   .TSelector[T]
  type Cotransform[T] = TSL.Cotransform.TSelector[T]
  type Transform  [T] = TSL.Transform  .TSelector[T]
}

// ===========================================================================

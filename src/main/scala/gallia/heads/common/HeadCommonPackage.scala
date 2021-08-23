package gallia.heads

import gallia.KPathW

// ===========================================================================
package object common extends _heads {
  val      TargetQueryUtils = gallia.target.utils.     TargetQueryUtils
  val TypedTargetQueryUtils = gallia.target.utils.TypedTargetQueryUtils

  // ---------------------------------------------------------------------------
  type StringSplitter = gallia.domain.StringSplitter

  // ---------------------------------------------------------------------------
  val pwrap = gallia.FunctionWrappers.pwrap _

  // ===========================================================================
  import gallia.domain._
  
  // ---------------------------------------------------------------------------  
  def kpaths2(path1: KPathW, path2: KPathW)                = KPaths2(path1.value, path2.value)
  def kpaths3(path1: KPathW, path2: KPathW, path3: KPathW) = KPaths3(path1.value, path2.value, path3.value)

  // ---------------------------------------------------------------------------  
  def tkpath[T: WTT](path : KPathW) = gallia.TKPath(path.value, gallia.reflect.TypeNode.parse[T])
  
    def tkpaths2[T1: WTT, T2: WTT]         (path1: KPathW, path2: KPathW)                = TKPaths2(tkpath[T1](path1), tkpath[T2](path2))
    def tkpaths3[T1: WTT, T2: WTT, T3: WTT](path1: KPathW, path2: KPathW, path3: KPathW) = TKPaths3(tkpath[T1](path1), tkpath[T2](path2), tkpath[T3](path3))
  
  // ===========================================================================
  type Grab       [T] = TSL.Squash.TSelector[T]
  type Squash     [T] = TSL.Squash.TSelector[T]
  type Pivot      [T] = TSL.Squash.TSelector[T] // t200924162200 - pivot must be compatible with grab/squash's

  type AssertData [T] = TSL.AssertData.TSelector[T]

  type Fusion     [T] = TSL.FuseFission.TSelector[T]
  type Fission    [T] = TSL.FuseFission.TSelector[T]

  type Generate1  [T] = TSL.Generate1.TSelector[T]
  type Generate2  [T] = TSL.Generate2.TSelector[T]

  type RemoveIf   [T] = TSL.RemoveIf   .TSelector[T]
  type Cotransform[T] = TSL.Cotransform.TSelector[T]
  type Transform  [T] = TSL.Transform  .TSelector[T]
}

// ===========================================================================

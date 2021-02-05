package gallia.selection.typed

import gallia.WTT
import gallia.KPath
import gallia.RPathz
import gallia.target._

// ===========================================================================
private[typed] trait TsBase {
    type From
    val  from: From

    // ===========================================================================
    // TODO: rename
    def resolve [T: WTT](tsl: TSelector[T]): TypedTargetQuery[KPath ] = new TypedTargetQuery[KPath ](tqkpath (tsl), node[T], HT.instantiator[T], wrapper(tsl).ignoreContainer)
    def resolves[T: WTT](tsl: TSelector[T]): TypedTargetQuery[RPathz] = new TypedTargetQuery[RPathz](tqqpathz(tsl), node[T], HT.instantiator[T], wrapper(tsl).ignoreContainer)

    // ---------------------------------------------------------------------------
    private def wrapper[T: WTT](tsl: TSelector[T]): TsWrapper[T] = tsl.apply(from)

    // ---------------------------------------------------------------------------
    def tqkpath (tsl: TSelector[_]): TqKPath  = wrapper(tsl).forceTqKPath  // only for generate?
    def tqqpathz(tsl: TSelector[_]): TqRPathz = wrapper(tsl).forceTqRPathz // for uu

    // ===========================================================================
    type TSelector[T] = From => TsWrapper[T]

    // ---------------------------------------------------------------------------
    private type It[T] = TSelector[T]

      def resolve2[T1: WTT, T2: WTT                           ](f1: It[T1], f2: It[T2]                                    ) = new TtqKPath2(resolve(f1), resolve(f2))
      def resolve3[T1: WTT, T2: WTT, T3: WTT                  ](f1: It[T1], f2: It[T2], f3: It[T3]                        ) = new TtqKPath3(resolve(f1), resolve(f2), resolve(f3))
      def resolve4[T1: WTT, T2: WTT, T3: WTT, T4: WTT         ](f1: It[T1], f2: It[T2], f3: It[T3], f4: It[T4]            ) = new TtqKPath4(resolve(f1), resolve(f2), resolve(f3), resolve(f4))
      def resolve5[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT](f1: It[T1], f2: It[T2], f3: It[T3], f4: It[T4], f5: It[T5]) = new TtqKPath5(resolve(f1), resolve(f2), resolve(f3), resolve(f4), resolve(f5))
  }

  // ===========================================================================
  object TsBase {
    object RemoveIf    extends TsBase { type From = TsOps.RemoveIf   ; val from = new From {} }
    object Squash      extends TsBase { type From = TsOps.Squash     ; val from = new From {} } // also grab and pivot (see 200924162200)
    object AssertData  extends TsBase { type From = TsOps.AssertData ; val from = new From {} }
    object Sorting     extends TsBase { type From = TsOps.Sorting    ; val from = new From {} }
    object FuseFission extends TsBase { type From = TsOps.FuseFission; val from = new From {} }
    object Generate1   extends TsBase { type From = TsOps.Generate1  ; val from = new From {} }
    object Generate2   extends TsBase { type From = TsOps.Generate2  ; val from = new From {} }
    object Cotransform extends TsBase { type From = TsOps.Cotransform; val from = new From {} }
    object Transform   extends TsBase { type From = TsOps.Transform  ; val from = new From {} }
    object FilterBy1   extends TsBase { type From = TsOps.FilterBy1  ; val from = new From {} }
    object FilterByT   extends TsBase { type From = TsOps.FilterByT  ; val from = new From {} }
  }

// ===========================================================================

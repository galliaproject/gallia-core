package gallia
package selection
package typed

import target._

// ===========================================================================
private[typed] trait TsBase {
    type From
    val  from: From

    // ===========================================================================
    // TODO: rename
    def resolve [T: WTT](tsl: TSelector[T]): TypedTargetQuery[KPath ] = new TypedTargetQuery[KPath ](tqkpath (tsl), TypeDuo.build[T], tsl.apply(from).ignoreContainer)
    def resolves[T: WTT](tsl: TSelector[T]): TypedTargetQuery[RPathz] = new TypedTargetQuery[RPathz](tqrpathz(tsl), TypeDuo.build[T], tsl.apply(from).ignoreContainer)
    def resolve2[T: WTT](tsl: TSelector[T]): TypedTargetQuery[Key]    = new TypedTargetQuery[Key]   (tqkey   (tsl), TypeDuo.build[T], tsl.apply(from).ignoreContainer)

    // ---------------------------------------------------------------------------
    def tqkey   [T: WTT](tsl: TSelector[T]): TqKey    = tsl.apply(from).forceTqKey
    def tqkpath [T: WTT](tsl: TSelector[T]): TqKPath  = tsl.apply(from).forceTqKPath  // only for generate?
    def tqrpathz[T: WTT](tsl: TSelector[T]): TqRPathz = tsl.apply(from).forceTqRPathz // for uu

    // ===========================================================================
    type TSelector[T] = From => TsWrapper[T]

    // ---------------------------------------------------------------------------
    private type It[T] = TSelector[T]

      def resolve2[T1: WTT, T2: WTT](f1: It[T1], f2: It[T2]) = new TtqKPath2(resolve(f1), resolve(f2))

        def resolve3 [T1: WTT, T2: WTT, T3: WTT]                                                                (f1: It[T1], f2: It[T2], f3: It[T3])                                                                                       = new TtqKPath3 (resolve(f1), resolve(f2), resolve(f3))
        def resolve4 [T1: WTT, T2: WTT, T3: WTT, T4: WTT]                                                       (f1: It[T1], f2: It[T2], f3: It[T3], f4: It[T4])                                                                           = new TtqKPath4 (resolve(f1), resolve(f2), resolve(f3), resolve(f4))
        def resolve5 [T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT]                                              (f1: It[T1], f2: It[T2], f3: It[T3], f4: It[T4], f5: It[T5])                                                               = new TtqKPath5 (resolve(f1), resolve(f2), resolve(f3), resolve(f4), resolve(f5))
        def resolve6 [T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT]                                     (f1: It[T1], f2: It[T2], f3: It[T3], f4: It[T4], f5: It[T5], f6: It[T6])                                                   = new TtqKPath6 (resolve(f1), resolve(f2), resolve(f3), resolve(f4), resolve(f5), resolve(f6))
        def resolve7 [T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT, T7: WTT]                            (f1: It[T1], f2: It[T2], f3: It[T3], f4: It[T4], f5: It[T5], f6: It[T6], f7: It[T7])                                       = new TtqKPath7 (resolve(f1), resolve(f2), resolve(f3), resolve(f4), resolve(f5), resolve(f6), resolve(f7))
        def resolve8 [T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT, T7: WTT, T8: WTT]                   (f1: It[T1], f2: It[T2], f3: It[T3], f4: It[T4], f5: It[T5], f6: It[T6], f7: It[T7], f8: It[T8])                           = new TtqKPath8 (resolve(f1), resolve(f2), resolve(f3), resolve(f4), resolve(f5), resolve(f6), resolve(f7), resolve(f8))
        def resolve9 [T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT, T7: WTT, T8: WTT, T9: WTT]          (f1: It[T1], f2: It[T2], f3: It[T3], f4: It[T4], f5: It[T5], f6: It[T6], f7: It[T7], f8: It[T8], f9: It[T9])               = new TtqKPath9 (resolve(f1), resolve(f2), resolve(f3), resolve(f4), resolve(f5), resolve(f6), resolve(f7), resolve(f8), resolve(f9))
        def resolve10[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT, T7: WTT, T8: WTT, T9: WTT, T10: WTT](f1: It[T1], f2: It[T2], f3: It[T3], f4: It[T4], f5: It[T5], f6: It[T6], f7: It[T7], f8: It[T8], f9: It[T9], f10: It[T10]) = new TtqKPath10(resolve(f1), resolve(f2), resolve(f3), resolve(f4), resolve(f5), resolve(f6), resolve(f7), resolve(f8), resolve(f9), resolve(f10))
  }

  // ===========================================================================
  object TsBase {
    object RemoveIf    extends TsBase { type From = TsOps.RemoveIf   ; val from = new From {} }
    object IfValueFor  extends TsBase { type From = TsOps.IfValueFor ; val from = new From {} }
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

    object TransformSomeObjects extends TsBase { type From = TsOps.TransformSomeObjects; val from = new From {} }
  }

// ===========================================================================

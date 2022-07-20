package gallia
package target
package utils

import target._
import selection.typed._

// ===========================================================================
object TypedTargetQueryUtils {
  private def _tqkpath(value: KPath)  = new TqKPath (_ => Nil, _ => value) // TODO: ok to keep?

  // ===========================================================================
  // ttqrpathz

  def ttqrpathz1[T: WTT](value: TqRPathz): TtqRPathz = new TtqRPathz(value, typeNode[T].normalizeSome, HT.instantiator[T], ignoreContainer = false)

  // ===========================================================================
  // ttqkpath

  def ttqkpath1(tq: TqKPath, ht: HasType) = new TtqKPath(tq, ht.node, ht.instantiator, ignoreContainer = false) // for cotransform + override at least

  def ttqkpath1[T1: WTT](f1: KPathW) = new TtqKPath(_tqkpath(f1.kpath), typeNode[T1], HT.instantiator[T1], ignoreContainer = false)

    def ttqkpath2[T1: WTT, T2: WTT](k1: KPathW, k2: KPathW) = new TtqKPath2(ttqkpath1[T1](k1), ttqkpath1[T2](k2))

      // ---------------------------------------------------------------------------
      def ttqkpath3[T1: WTT, T2: WTT, T3: WTT]                                                                (k1: KPathW, k2: KPathW, k3: KPathW)                                                                                      = new TtqKPath3 (ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3))
      def ttqkpath4[T1: WTT, T2: WTT, T3: WTT, T4: WTT]                                                       (k1: KPathW, k2: KPathW, k3: KPathW, k4: KPathW)                                                                          = new TtqKPath4 (ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3), ttqkpath1[T4](k4))
      def ttqkpath5[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT]                                              (k1: KPathW, k2: KPathW, k3: KPathW, k4: KPathW, k5: KPathW)                                                              = new TtqKPath5 (ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3), ttqkpath1[T4](k4), ttqkpath1[T5](k5))
      def ttqkpath6[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT]                                     (k1: KPathW, k2: KPathW, k3: KPathW, k4: KPathW, k5: KPathW, k6: KPathW)                                                  = new TtqKPath6 (ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3), ttqkpath1[T4](k4), ttqkpath1[T5](k5), ttqkpath1[T6](k6))
      def ttqkpath7[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT, T7: WTT]                            (k1: KPathW, k2: KPathW, k3: KPathW, k4: KPathW, k5: KPathW, k6: KPathW, k7: KPathW)                                      = new TtqKPath7 (ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3), ttqkpath1[T4](k4), ttqkpath1[T5](k5), ttqkpath1[T6](k6), ttqkpath1[T7](k7))
      def ttqkpath8[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT, T7: WTT, T8: WTT]                   (k1: KPathW, k2: KPathW, k3: KPathW, k4: KPathW, k5: KPathW, k6: KPathW, k7: KPathW, k8: KPathW)                          = new TtqKPath8 (ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3), ttqkpath1[T4](k4), ttqkpath1[T5](k5), ttqkpath1[T6](k6), ttqkpath1[T7](k7), ttqkpath1[T8](k8))
      def ttqkpath9[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT, T7: WTT, T8: WTT, T9: WTT]          (k1: KPathW, k2: KPathW, k3: KPathW, k4: KPathW, k5: KPathW, k6: KPathW, k7: KPathW, k8: KPathW, k9: KPathW)              = new TtqKPath9 (ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3), ttqkpath1[T4](k4), ttqkpath1[T5](k5), ttqkpath1[T6](k6), ttqkpath1[T7](k7), ttqkpath1[T8](k8), ttqkpath1[T9](k9))
      def ttqkpath9[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT, T6: WTT, T7: WTT, T8: WTT, T9: WTT, T10: WTT](k1: KPathW, k2: KPathW, k3: KPathW, k4: KPathW, k5: KPathW, k6: KPathW, k7: KPathW, k8: KPathW, k9: KPathW, k10: KPathW) = new TtqKPath10(ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3), ttqkpath1[T4](k4), ttqkpath1[T5](k5), ttqkpath1[T6](k6), ttqkpath1[T7](k7), ttqkpath1[T8](k8), ttqkpath1[T9](k9), ttqkpath1[T10](k10))

    // ---------------------------------------------------------------------------
    // TODO: t200807140927 - tupled for non-cc constructor?
    def ttqkpath2(tuple: (TtqKPath, TtqKPath)): TtqKPath2 = new TtqKPath2(tuple._1, tuple._2)

      // ---------------------------------------------------------------------------
      def ttqkpath3(tuple: (TtqKPath, TtqKPath, TtqKPath))                                                                      : TtqKPath3  = new TtqKPath3 (tuple._1, tuple._2, tuple._3)
      def ttqkpath4(tuple: (TtqKPath, TtqKPath, TtqKPath, TtqKPath))                                                            : TtqKPath4  = new TtqKPath4 (tuple._1, tuple._2, tuple._3, tuple._4)
      def ttqkpath5(tuple: (TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath))                                                  : TtqKPath5  = new TtqKPath5 (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5)
      def ttqkpath6(tuple: (TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath))                                        : TtqKPath6  = new TtqKPath6 (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6)
      def ttqkpath7(tuple: (TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath))                              : TtqKPath7  = new TtqKPath7 (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7)
      def ttqkpath8(tuple: (TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath))                    : TtqKPath8  = new TtqKPath8 (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8)
      def ttqkpath9(tuple: (TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath))          : TtqKPath9  = new TtqKPath9 (tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9)
      def ttqkpathA(tuple: (TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath)): TtqKPath10 = new TtqKPath10(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10)
}

// ===========================================================================

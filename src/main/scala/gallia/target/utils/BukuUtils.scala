package gallia.target.utils

import gallia._
import gallia.target._
import gallia.selection.typed._

// ===========================================================================
object TypedTargetQueryUtils {
  private def _tqkpath(value: KPath)  = new TqKPath (_ => Nil, _ => value) // TODO: ok to keep?

  // ===========================================================================
  // ttqqpathz

  def ttqqpathz1[T: WTT](value: TqRPathz): TtqRPathz = new TtqRPathz(value, node[T].normalizeSome, HT.instantiator[T], ignoreContainer = false)

  // ===========================================================================
  // ttqkpath

  def ttqkpath1(tq: TqKPath, ht: HasType) = new TtqKPath(tq, ht.node, ht.instantiator, ignoreContainer = false) // for cotransform + override at least

  def ttqkpath1[T1: WTT](f1: KPathW) = new TtqKPath (_tqkpath(f1.kpath), node[T1], HT.instantiator[T1], ignoreContainer = false)

    def ttqkpath2[T1: WTT, T2: WTT]                           (k1: KPathW, k2: KPathW)                                     = new TtqKPath2(ttqkpath1[T1](k1), ttqkpath1[T2](k2))
    def ttqkpath3[T1: WTT, T2: WTT, T3: WTT]                  (k1: KPathW, k2: KPathW, k3: KPathW)                         = new TtqKPath3(ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3))
    def ttqkpath4[T1: WTT, T2: WTT, T3: WTT, T4: WTT]         (k1: KPathW, k2: KPathW, k3: KPathW, k4: KPathW)             = new TtqKPath4(ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3), ttqkpath1[T4](k4))
    def ttqkpath5[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT](k1: KPathW, k2: KPathW, k3: KPathW, k4: KPathW, k5: KPathW) = new TtqKPath5(ttqkpath1[T1](k1), ttqkpath1[T2](k2), ttqkpath1[T3](k3), ttqkpath1[T4](k4), ttqkpath1[T5](k5))

  // ---------------------------------------------------------------------------
  // TODO: t200807140927 - tupled for non-cc constructor?
  def ttqkpath2(tuple: (TtqKPath, TtqKPath))                              : TtqKPath2 = new TtqKPath2(tuple._1, tuple._2)
  def ttqkpath3(tuple: (TtqKPath, TtqKPath, TtqKPath))                    : TtqKPath3 = new TtqKPath3(tuple._1, tuple._2, tuple._3)
  def ttqkpath4(tuple: (TtqKPath, TtqKPath, TtqKPath, TtqKPath))          : TtqKPath4 = new TtqKPath4(tuple._1, tuple._2, tuple._3, tuple._4)
  def ttqkpath5(tuple: (TtqKPath, TtqKPath, TtqKPath, TtqKPath, TtqKPath)): TtqKPath5 = new TtqKPath5(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5)
}

// ===========================================================================

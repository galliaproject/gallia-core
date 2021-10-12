package gallia
package heads

import aptus.{One, Opt, Pes, Nes}


import FunctionWrappers._
import actions.ActionsOthers._
import heads.common.{Grab, Squash}
import target.utils.TypedTargetQueryUtils

// ===========================================================================
trait HeadUSquashing { _: HeadU => // TODO: t210122161652 - favor AtomsVO now?
  import TSL.Squash._

  // ---------------------------------------------------------------------------
  // TODO: t210205065320 - rename to .access? or .orphan?
  def grab        (f1: KPathW)  : HeadV[W] = grab(_._explicit(f1))
  def grab[V: WTT](acc: Grab[V]): HeadV[V] = uv(GrabU(resolve(acc)))
  //TODO: VS z's def grab[V: WTT](f1: Grab[V]): HeadV[Seq[V]] = squash(f1).using(x => x)

  // ---------------------------------------------------------------------------
  // TODO: term: t210205065320 - or "combine"?
  def squash[O1: WTT                                    ](f1: Squash[O1])                                                                 = new { def using[D1: WTT](f:  O1                  => D1): HeadV[D1] = uv(SquashU1(resolve (f1                ), node[D1], wrap11(f))) } // = grab
  def squash[O1: WTT, O2: WTT                           ](f1: Squash[O1], f2: Squash[O2])                                                 = new { def using[D1: WTT](f: (O1, O2)             => D1): HeadV[D1] = uv(SquashU2(resolve2(f1, f2            ), node[D1], wrap21(f))) }
  def squash[O1: WTT, O2: WTT, O3: WTT                  ](f1: Squash[O1], f2: Squash[O2], f3: Squash[O3])                                 = new { def using[D1: WTT](f: (O1, O2, O3)         => D1): HeadV[D1] = uv(SquashU3(resolve3(f1, f2, f3        ), node[D1], wrap31(f))) }
  def squash[O1: WTT, O2: WTT, O3: WTT, O4: WTT         ](f1: Squash[O1], f2: Squash[O2], f3: Squash[O3], f4: Squash[O4])                 = new { def using[D1: WTT](f: (O1, O2, O3, O4)     => D1): HeadV[D1] = uv(SquashU4(resolve4(f1, f2, f3, f4    ), node[D1], wrap41(f))) }
  def squash[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](f1: Squash[O1], f2: Squash[O2], f3: Squash[O3], f4: Squash[O4], f5: Squash[O5]) = new { def using[D1: WTT](f: (O1, O2, O3, O4, O5) => D1): HeadV[D1] = uv(SquashU5(resolve5(f1, f2, f3, f4, f5), node[D1], wrap51(f))) }

  def      squashUnsafe[D1: WTT](f: Obj => D1): HeadV[D1] = uv(SquashUUnsafe(node[D1], f))
  def forceSquashUnsafe[D1: WTT](f: Obj => D1):       D1  = squashUnsafe(f).end().runv[D1]().forceData2(_.value)

  // ===========================================================================
  def typed  [T: WTT](key: KPathW): HeadV[One[T]] = uv(GrabU(TypedTargetQueryUtils.ttqkpath1[           T  ](key)))
  def typed_ [T: WTT](key: KPathW): HeadV[Opt[T]] = uv(GrabU(TypedTargetQueryUtils.ttqkpath1[Option[    T ]](key)))
  def typeds [T: WTT](key: KPathW): HeadV[Nes[T]] = uv(GrabU(TypedTargetQueryUtils.ttqkpath1[Seq   [    T ]](key)))
  def typeds_[T: WTT](key: KPathW): HeadV[Pes[T]] = uv(GrabU(TypedTargetQueryUtils.ttqkpath1[Option[Seq[T]]](key)))

  // ---------------------------------------------------------------------------
  def forceTyped  [T: WTT](key: KPathW): One[T] = typed  (key).end().runv[One[T]]().forceData2(_.value)
  def forceTyped_ [T: WTT](key: KPathW): Opt[T] = typed_ (key).end().runv[Opt[T]]().forceData2(_.value)
  def forceTypeds [T: WTT](key: KPathW): Nes[T] = typeds (key).end().runv[Nes[T]]().forceData2(_.value)
  def forceTypeds_[T: WTT](key: KPathW): Pes[T] = typeds_(key).end().runv[Pes[T]]().forceData2(_.value)
}

// ===========================================================================
trait HeadZSquashing { _: HeadZ =>
  import TSL.Squash._

  // ---------------------------------------------------------------------------
  //FIXME: must use different grab selector for z/u

  // TODO: t210205065320 - rename to .access? or .orphan?
  def grab        (f1: KPathW) : HeadV[Seq[W]] = grab(_._explicit(f1))
  def grab[V: WTT](f1: Grab[V]): HeadV[Seq[V]] = squash(f1).using(x => x)

  // ---------------------------------------------------------------------------
  // TODO: term: or "combine"? + name differently than U's counterpart?
  def squash[O1: WTT                                    ](f1: Squash[O1])                                                                 = new { def using[D1: WTT](f: Seq[ O1 ]                 => D1): HeadV[D1] = zv(SquashZ1(resolve (f1                ), node[D1], awrap11(f))) } // = grab
  def squash[O1: WTT, O2: WTT                           ](f1: Squash[O1], f2: Squash[O2])                                                 = new { def using[D1: WTT](f: Seq[(O1, O2)]             => D1): HeadV[D1] = zv(SquashZ2(resolve2(f1, f2            ), node[D1], awrap21(f))) }
  def squash[O1: WTT, O2: WTT, O3: WTT                  ](f1: Squash[O1], f2: Squash[O2], f3: Squash[O3])                                 = new { def using[D1: WTT](f: Seq[(O1, O2, O3)]         => D1): HeadV[D1] = zv(SquashZ3(resolve3(f1, f2, f3        ), node[D1], awrap31(f))) }
  def squash[O1: WTT, O2: WTT, O3: WTT, O4: WTT         ](f1: Squash[O1], f2: Squash[O2], f3: Squash[O3], f4: Squash[O4])                 = new { def using[D1: WTT](f: Seq[(O1, O2, O3, O4)]     => D1): HeadV[D1] = zv(SquashZ4(resolve4(f1, f2, f3, f4    ), node[D1], awrap41(f))) }
  def squash[O1: WTT, O2: WTT, O3: WTT, O4: WTT, O5: WTT](f1: Squash[O1], f2: Squash[O2], f3: Squash[O3], f4: Squash[O4], f5: Squash[O5]) = new { def using[D1: WTT](f: Seq[(O1, O2, O3, O4, O5)] => D1): HeadV[D1] = zv(SquashZ5(resolve5(f1, f2, f3, f4, f5), node[D1], awrap51(f))) }

  def squashUnsafe[D1: WTT](f: Seq[Obj] => D1): HeadV[D1] = zv(SquashZUnsafe(node[D1], f))

  // ===========================================================================
  def typeds [T: WTT](key: KPathW): HeadV[Nes[T]] = zv(GrabZ(TypedTargetQueryUtils.ttqkpath1[       T ](key)))
  def typeds_[T: WTT](key: KPathW): HeadV[Pes[T]] = zv(GrabZ(TypedTargetQueryUtils.ttqkpath1[Option[T]](key))) // try and read this one out loud

  def forceTypeds [T: WTT](key: KPathW): Nes[T] = typeds (key).end().runv[Nes[T]]().forceData2(_.value)
  def forceTypeds_[T: WTT](key: KPathW): Pes[T] = typeds_(key).end().runv[Pes[T]]().forceData2(_.value)
}

// ===========================================================================

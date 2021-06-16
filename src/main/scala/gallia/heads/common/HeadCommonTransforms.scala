package gallia.heads.common

import gallia._
import gallia.target.HT
import gallia.FunctionWrappers._
import gallia.actions.ActionsUUTransforms._

// ===========================================================================
trait HeadCommonTransforms[F <: HeadCommon[F]] { _: HeadCommon[F] =>
  import TSL.Transform._

  // TODO: t201124094119 - how to allow using(identity)?

  // ===========================================================================
  def transform        (f1: Transform[HeadU])                          : _TransformU     = new _TransformU(f1)
  def transform        (f1: Transform[HeadZ])(implicit di: DI)         : _TransformZ     = new _TransformZ(f1)
  def transform[O: WTT](f1: Transform[O])    (implicit di: DI, di2: DI): _TransformVV[O] = new _TransformVV(f1)

  // ---------------------------------------------------------------------------
  def transform (k: Key): _TransformWhatever = new _TransformWhatever(_._explicit(k)) //TODO

//def transform(f1: HasSingleExplicitKPathW => KPathSelection)(implicit di: DI, di2: DI) = new { def using[D: WTT](f: WV => D): Self2 = ??? }
//def transform(f1: Transform[WV])                           (implicit di: DI, di2: DI) = new TransformVV(f1)
//def transformb[O: WTT](f1: Transform[O])                                              = new TransformVV(f1)

    // ===========================================================================
    class _TransformU(f1: Transform[HeadU]) {
      def using         (f: HeadU => HeadU)                            : Self2 = self2 :+ TransformUU    (tqqpathz(f1), f)
      def using         (f: HeadU => HeadZ)    (implicit d: DI)        : Self2 = self2 :+ TransformUZ    (tqqpathz(f1), f)
      def using[V1: WTT](f: HeadU => HeadV[V1])(implicit d: DI, d2: DI): Self2 = self2 :+ TransformUV[V1](tqqpathz(f1), f) }

    // ===========================================================================
    class _TransformZ(f1: Transform[HeadZ]) {
      def using         (f: HeadZ => HeadZ)                            : Self2 = self2 :+ TransformZZ    (tqqpathz(f1), f)
      def using         (f: HeadZ => HeadU)    (implicit d: DI)        : Self2 = self2 :+ TransformZU    (tqqpathz(f1), f)
      def using[D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI, d2: DI): Self2 = self2 :+ TransformZV[D1](tqqpathz(f1), f) }

    // ===========================================================================
    class _TransformWhatever(f1: Transform[WV]) {
      def using        (f: WV => WV1)                                   : Self2 = self2 :+ TransformWW1(resolves(f1).tq, toMultiple = false, __wwrap11a(f))
      def using[D: WTT](f: WV =>     WV2[D] )(implicit di1: DI)         : Self2 = self2 :+ TransformWW2(resolves(f1)     , node[D], false    , __wwrap22a(f))

      def using        (f: WV => String)     (implicit di1: DI, di2: DI): Self2 = self2 :+ TransformWW1(resolves(f1).tq, toMultiple = false, (v: Any) => f(new WV(v)) ) // mostly for s"..."

      // these are probably overkill... TODO: nuke them?
      def using        (f: WV => Seq[WV1])   (implicit di1: DI, di2: DI, di3: DI)         : Self2 = self2 :+ TransformWW1(resolves(f1).tq, toMultiple = true, __wwrap11b(f))
      def using[D: WTT](f: WV => Seq[WV2[D]])(implicit di1: DI, di2: DI, di3: DI, di4: DI): Self2 = self2 :+ TransformWW2(resolves(f1)     , node[D], true    , __wwrap22b(f)) }

    // ===========================================================================
    class _TransformVV[O: WTT](f1: Transform[O]) { val ttq = resolves(f1)
def using2(c: Cls)(f: O => Objs): Self2 = self2 :+ TransformFoo(ttq, c, wrap(f))

      def using[D: WTT](f: O => D): Self2 = self2 :+ { val dest = node[D]
        if (dest.isContainedDataClass)
          if (ttq.ignoreContainer) TransformVVxc(ttq, to = HT.parse[D], wrap(f))
          else                     TransformVVc (ttq, to = HT.parse[D], wrap(f))
        else
          if (ttq.ignoreContainer) TransformVVx(ttq, dest, wrap(f))
          else                     TransformVV(ttq, dest, wrap(f), false) } }

}

// ===========================================================================

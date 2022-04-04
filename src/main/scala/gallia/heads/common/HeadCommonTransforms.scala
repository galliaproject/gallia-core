package gallia
package heads.common

import target.HT
import FunctionWrappers._
import actions.ActionsUUTransforms._

// ===========================================================================
trait HeadCommonTransforms[F <: HeadCommon[F]] { ignored: HeadCommon[F] =>
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
      private def wrap[T, D](f: WV => T)(g: T => D) =          
          (x: Any) => x match {
            case y: Seq[_] => y.map { z => g(f(new WV(z))) }
            case y         =>              g(f(new WV(y))) }

      def using        (f: WV =>  WV)                   : Self2 = self2 :+ TransformWW1a(resolves(f1).tq,       wrap(f)(_.any)  (_))
      def using[D: WTT](f: WV => TWV[D])                : Self2 = self2 :+ TransformWW1b(resolves(f1), node[D], wrap(f)(_.typed)(_))
      def using[D: WTT](f: WV =>     D)(implicit di: DI): Self2 = self2 :+ TransformWW1b(resolves(f1), node[D], wrap(f)(x => x) (_)) }

    // ===========================================================================
    class _TransformVV[O: WTT](f1: Transform[O]) { val ttq = resolves(f1)
      def toObjsUsing(c: Cls)(f: O => Objs): Self2 = self2 :+ TransformToObj(ttq, c, multiple = true , wrap(f)) // TODO: rename
      def toObjUsing (c: Cls)(f: O => Obj ): Self2 = self2 :+ TransformToObj(ttq, c, multiple = false, wrap(f)) // TODO: rename
      
      //TODO: opaque counteparts (see t210110094829)

      // ---------------------------------------------------------------------------
      def using[D: WTT](f: O => D): Self2 = self2 :+ {
        val dest = typeNode[D]

        if (!dest.isContainedDataClass)
          if (!ttq.ignoreContainer) TransformVV (ttq, dest, wrap(f), false)
          else                      TransformVVx(ttq, dest, wrap(f))
        else
          if (!ttq.ignoreContainer) TransformVVc (ttq, to = HT.parse[D], wrap(f))
          else                      TransformVVxc(ttq, to = HT.parse[D], wrap(f)) } }

}

// ===========================================================================

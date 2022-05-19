package gallia
package heads.common

import scala.reflect.runtime.universe.weakTypeTag
import aptus.Index
import target.HT
import FunctionWrappers._
import actions.common.ActionsCommonTransforms._

// ===========================================================================
trait HeadCommonTransforms[F <: HeadCommon[F]] { ignored: HeadCommon[F] => // 220412171653
  import TSL.Transform._

  // TODO: t201124094119 - how to allow using(identity)?

  // ===========================================================================
  def transform        (f1: Transform[HeadU])                          : _TransformU     = new _TransformU(f1)
  def transform        (f1: Transform[HeadZ])(implicit di: DI)         : _TransformZ     = new _TransformZ(f1)
  def transform[O: WTT](f1: Transform[O])    (implicit di: DI, di2: DI): _TransformVV[O] = new _TransformVV(f1)

  // ---------------------------------------------------------------------------
  def transform (k: KPathW): _TransformWhatever = new _TransformWhatever(_._explicit(k)) //TODO

//def transform(f1: HasSingleExplicitKPathW => KPathSelection)(implicit di: DI, di2: DI) = new { def using[D: WTT](f: WV => D): Self2 = ??? }
//def transform(f1: Transform[WV])                           (implicit di: DI, di2: DI) = new TransformVV(f1)
//def transformb[O: WTT](f1: Transform[O])                                              = new TransformVV(f1)

    // ===========================================================================
    trait ___TransformU {
        protected val target: Transform[HeadU]
        protected val disambiguatorOpt: Option[UnionObjectDisambiguator] = None // TODO: t220517123057: support for UZ and UV

        // ---------------------------------------------------------------------------
        def using         (f: HeadU => HeadU)                            : Self2 = self2 :+ TransformUU    (tqqpathz(target), disambiguatorOpt, f)
        def using         (f: HeadU => HeadZ)    (implicit d: DI)        : Self2 = self2 :+ TransformUZ    (tqqpathz(target), f)
        def using[V1: WTT](f: HeadU => HeadV[V1])(implicit d: DI, d2: DI): Self2 = self2 :+ TransformUV[V1](tqqpathz(target), f) }

      // ===========================================================================
      class _TransformU(val target: Transform[HeadU]) extends ___TransformU {
          /** disambiguate in case of union type - see t210125111338 */
          def withPredicate(meta: Cls => Boolean, data: Obj => Boolean): __TransformU = new __TransformU(target, DisambiguateByClassPredicateU(meta, data))

          // ---------------------------------------------------------------------------
          def withTypeName(value: ClsName): __TransformU = withPredicate(_.nameOpt == Some(value), _.string_(_type).exists(_ == value))
          def withFieldHint(field: KPathW): __TransformU = withPredicate(_.contains(field.value), _.contains(field.value)) // convenient if unique to the type + required field
          def withIndex    (value: Index) : __TransformU = new __TransformU(target, DisambiguateByClassIndex(value)) }

        // ---------------------------------------------------------------------------
        class __TransformU(val target: Transform[HeadU], disambiguator: UnionObjectDisambiguator) extends ___TransformU {
          protected override val disambiguatorOpt = Some(disambiguator) }

    // ===========================================================================
    trait ___TransformZ {
        protected val target: Transform[HeadZ]
        protected val disambiguatorOpt: Option[UnionObjectDisambiguator] = None // TODO: t220517123057: support for ZU and ZV

        // ---------------------------------------------------------------------------
        def using         (f: HeadZ => HeadZ)                            : Self2 = self2 :+ TransformZZ    (tqqpathz(target), disambiguatorOpt, f)
        def using         (f: HeadZ => HeadU)    (implicit d: DI)        : Self2 = self2 :+ TransformZU    (tqqpathz(target), f)
        def using[D1: WTT](f: HeadZ => HeadV[D1])(implicit d: DI, d2: DI): Self2 = self2 :+ TransformZV[D1](tqqpathz(target), f) }

      // ===========================================================================
      class _TransformZ(val target: Transform[HeadZ]) extends ___TransformZ {
          /** disambiguate in case of union type - see t210125111338 */
          def withPredicate(meta: Cls => Boolean, data: Seq[Obj] => Boolean): __TransformZ = new __TransformZ(target, DisambiguateByClassPredicateZ(meta, data))

          // ---------------------------------------------------------------------------
          def withTypeName(value: ClsName): __TransformZ = withPredicate(_.nameOpt == Some(value), _.forall(_.string_(_type).exists(_ == value)))
          def withFieldHint(field: KPathW): __TransformZ = withPredicate(_.contains(field.value), _.exists /* one is enough */(_.contains(field.value))) // convenient if unique to the typ
          def withIndex    (value: Index) : __TransformZ = new __TransformZ(target, DisambiguateByClassIndex(value)) }

        // ---------------------------------------------------------------------------
        class __TransformZ(val target: Transform[HeadZ], disambiguator: UnionObjectDisambiguator) extends ___TransformZ {
          protected override val disambiguatorOpt = Some(disambiguator) }

    // ===========================================================================
    class _TransformWhatever(f1: Transform[WV]) {
      private def wrap[T, D](f: WV => T)(g: T => D) =          
          (x: Any) => x match {
            case y: Seq[_] => y.map { z => g(f(new WV(z))) }
            case y         =>              g(f(new WV(y))) }

      def using        (f: WV =>  WV)                   : Self2 = self2 :+ TransformWW1a(resolves(f1).tq,           wrap(f)(_.any)  (_))
      def using[D: WTT](f: WV => TWV[D])                : Self2 = self2 :+ TransformWW1b(resolves(f1), typeNode[D], wrap(f)(_.typed)(_))
      def using[D: WTT](f: WV =>     D)(implicit di: DI): Self2 = self2 :+ TransformWW1b(resolves(f1), typeNode[D], wrap(f)(x => x) (_)) }

    // ===========================================================================
    class _TransformVV[O: WTT](f1: Transform[O]) { val ttq = resolves(f1)
      private val wtto = new WeakTypeTagDecorator(weakTypeTag[O])

      // ---------------------------------------------------------------------------
      def toObjsUsing(c: Cls)(f: O => Objs): Self2 = self2 :+ TransformToObj(ttq, c, multiple = true , wrap(f)) // TODO: rename
      def toObjUsing (c: Cls)(f: O => Obj ): Self2 = self2 :+ TransformToObj(ttq, c, multiple = false, wrap(f)) // TODO: rename
      
      //TODO: opaque counteparts (see t210110094829)

      // ---------------------------------------------------------------------------
      def using[D: WTT](f: O => D): Self2 = self2 :+ {
        val dest = typeNode[D]

        if (!dest.isContainedDataClass)
          if (!ttq.ignoreContainer) TransformVV (ttq, dest, wrap(f), wtto.ifApplicable(f))
          else                      TransformVVx(ttq, dest, wrap(f), wtto.ifApplicable(f))
        else
          if (!ttq.ignoreContainer) TransformVVc (ttq, to = HT.parse[D], wrap(f))
          else                      TransformVVxc(ttq, to = HT.parse[D], wrap(f)) } }

}

// ===========================================================================

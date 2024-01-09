package gallia
package actions
package common

import trgt._
import FunctionWrappers._
import atoms.common.AtomsCommonTransforms._
import meta.ValueType


// ===========================================================================
object ActionsCommonCotransforms { // TODO: t210826102833 - rework co-transforms

  case class Cotransform1to1(from: TtqKPath1, toEither: Either[HasType, TtqKPath1], f: _ff11) extends ActionUUc {
      def  vldt(c: Cls): Errs  =
        from.vldtAsOrigin(c) ++
        to(c).vldtAsCotransformDestination(c, from.kpath_(c))
        //TODO: more (+ throughout)?

      private def to(c: Cls): TtqKPath1 = toEither.fold(from.fromOverride _, identity)

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = to(c).asMultiple.put(c, from.valueType1(c))
      def atomuu(c: Cls): AtomUU = _Transform1to1(from.pathPairT(c), to(c).kpathT(c), f) }

    // ---------------------------------------------------------------------------
  case class Cotransform2to2(from: TtqKPath2, toEither: Either[HasTypes2, TtqKPath2], f: _ff22) extends ActionUUc {
      def  vldt(c: Cls): Errs  =
        from.vldtAsOrigin(c) ++
        to(c).vldtAsCotransformDestination(c, from.__kpathz(c))

      private def to(c: Cls): TtqKPath2 = toEither.fold(from.fromOverride _, identity)

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = to(c).put(c, from.valueTypes(c))
      def atomuu(c: Cls): AtomUU = _Transform2to2(from.pathPairT(c), to(c).kpathT(c), f) }

    // ---------------------------------------------------------------------------
    case class Cotransform1to2(from: TtqKPath1, to: TtqKPath2, f: _ff12) extends ActionUUc {
      def  vldt(c: Cls): Errs  =
        from.vldtAsOrigin(c) ++
        to.vldtAsCotransformDestination(c, from.kpath_(c))

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = to.put(c, from.valueType1(c))
      def atomuu(c: Cls): AtomUU = _Transform1to2(from.pathPairT(c), to.kpathT(c), f) }

    // ---------------------------------------------------------------------------
    case class Cotransform1to3(from: TtqKPath1, to: TtqKPath3, f: _ff13) extends ActionUUc {
      def  vldt(c: Cls): Errs  =
        from.vldtAsOrigin(c) ++
        to.vldtAsCotransformDestination(c, from.kpath_(c))

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = to.put(c, from.valueType1(c))
      def atomuu(c: Cls): AtomUU = _Transform1to3(from.pathPairT(c), to.kpathT(c), f) }

    // ---------------------------------------------------------------------------
    case class Cotransform2to1(from: TtqKPath2, to: TtqKPath, f: _ff21) extends ActionUUc {
      def  vldt(c: Cls): Errs  =
        from.vldtAsOrigin(c) ++
        to.vldtAsCotransformDestination(c, from.__kpathz(c))

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = to.asMultiple.put(c, from.valueTypes(c))
      def atomuu(c: Cls): AtomUU = _Transform2to1(from.pathPairT(c), to.kpathT(c), f) }

    // ---------------------------------------------------------------------------
    case class Cotransform2to3(from: TtqKPath2, to: TtqKPath3, f: _ff23) extends ActionUUc {
      def  vldt(c: Cls): Errs  =
        from.vldtAsOrigin(c) ++
        to.vldtAsCotransformDestination(c, from.__kpathz(c))

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = to.put(c, from.valueTypes(c))
      def atomuu(c: Cls): AtomUU = _Transform2to3(from.pathPairT(c), to.kpathT(c), f) }

  // ===========================================================================
  @deprecated private implicit class HasTypedTargetQuerySeq_(u: gallia.trgt.HasTypedTargetQuerySeq[KPath]) {
      @deprecated def valueTypes(c:Cls): Seq[ValueType] = u.tqs.map(_.kpath_(c)).map(c.field(_).subInfo1.valueType)
    
      // ---------------------------------------------------------------------------
      @deprecated def put(c: Cls, from:  ValueType)                    : Cls = put(c, Seq(from))
      @deprecated def put(c: Cls, froms: Seq[ValueType] /* distinct */): Cls = u.ttqs.map { ttq => ttq.fieldPair(c) }.foldLeft(c)(_ put _)
    }

    // ---------------------------------------------------------------------------
    @deprecated private implicit class TypedTargetQuery_(u: gallia.trgt.TypedTargetQuery[KPath]) {
      @deprecated def asMultiple = new HasTypedTargetQuerySeq[KPath] { override def ttqs: Seq[TypedTargetQuery[KPath]] = Seq(u) }
    }

}

// ===========================================================================

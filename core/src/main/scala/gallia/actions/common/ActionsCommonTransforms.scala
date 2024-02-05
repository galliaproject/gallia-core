package gallia
package actions
package common

import aptus.Anything_

import trgt._
import FunctionWrappers._
import atoms.common.AtomsCommonTransforms._TransformWW

// ===========================================================================
object ActionsCommonTransforms {
  import gallia.actions.utils
  import utils.ActionsUtils._
  import utils.NestedTransform

  // ===========================================================================
  // TODO: check input isn't u or z + destination type is valid
  // FIXME: t210615104657 - if to is Option[T]

  case class TransformVV(from: TtqRPathz, to: TypeNode, f: _ff11, g: _ff11) extends ActionUU1Noswo {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) ++ _vldt.validType(to)
      def _meta  (c: Cls): Cls     = from.rpathz_     (c).intraClss(c) { _.updateType(_, from.typeNode, to) }
.pipe(storeIntraMetas)
      def atomuus(c: Cls): AtomUUs =
        from.rpathz_(c).pipe(_atomsUnion(c)(_TransformVV(_, f), _TransformVV(_, g)))
.pipe(updateAtomMetas) }

    // ---------------------------------------------------------------------------
    case class TransformVVx(from: TtqRPathz, to: TypeNode, f: _ff11, g: _ff11) extends ActionUU1Noswo {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c, SpecialCardiMode.IgnoreAltogether) ++ _vldt.validType(to)
      def _meta  (c: Cls): Cls     = from.rpathz_(c).intraClss(c) { _.updateTypex(_, from.typeNode, to) }
.pipe(storeIntraMetas)
      def atomuus(c: Cls): AtomUUs = from.rpathz_(c).pipe(_atomsUnion(c)(_TransformVV(_, from.wrapx(c, f)), _TransformVV(_, g)))
.pipe(updateAtomMetas) }

    // ===========================================================================
    //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

    case class TransformWW1a(from: TqRPathz, f: _ff11) extends ActionUU1N with IdentityM1 {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) // TODO: t210201164749
      def atomuus(c: Cls): AtomUUs = from.rpathz_(c).pipe(_atoms(c)(_TransformWW(_, f, checkType = true))) }

    // ---------------------------------------------------------------------------
    case class TransformWW1b(from: TtqRPathz, to: TypeNode, f: _ff11) extends ActionUU1N {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) ++ to.pipe(_vldt.validType)
      def _meta  (c: Cls): Cls     = from.rpathz_(c).foldLeft(c)(_.transformSoleValueType(_)(_ => to.forceNonBObjInfo.subInfo1.valueType))
      def atomuus(c: Cls): AtomUUs = from.rpathz_(c).pipe(_atoms(c)(_TransformWW(_, f, checkType = false))) }

  // ===========================================================================
  private[gallia] trait EntityTransformXX  {
        protected final def resolve(c: Cls) = target.rpathz_(c)
        protected       val target: TqRPathz
        protected       def _trnsf(c: Cls): NestedTransform

        // ---------------------------------------------------------------------------
        def  __vldt(c: Cls)(checkInput: Cls => KPathz => Errs): Errs =  /* TODO: t210202155459 - verify input is indeed u or z */
          target.vldtAsOrigin(c) ++
          target.__rpathz(c).pipe {
            _trnsf(c).vldt(c, _) } ++
          target.__rpathz(c).pipe(_.fromz).pipe {
            checkInput(c) } }

      // ===========================================================================
      private[gallia] trait EntityTransformXXX extends EntityTransformXX {
          protected val multiple: Boolean

          // ---------------------------------------------------------------------------
          def _meta  (c: Cls): Cls     = resolve(c).pipe   (_trnsf(c).transformMeta(c, _))
          def atomuus(c: Cls): AtomUUs = resolve(c).flatMap(_trnsf(c).transformData(c, multiple)) /* TODO: can only be one target actualy */ }

    // ===========================================================================
    case class TransformUUx(target: TqRPathz, disambiguatorOpt: UnionObjectDisambiguatorOpt, f: HeadU => HeadU)
          extends EntityTransformXX with ActionUU1N { import aptus.Seq_
        private def multiple(c: Cls): Boolean = resolve(c).values.force.one /* TODO: always here? */.from.pipe(c.isMultiple(_))

        // ---------------------------------------------------------------------------
        final override protected def _trnsf(c: Cls): NestedTransform =
          if (!multiple(c)) utils.NestedTransform.parseUU(disambiguatorOpt)(f)
          else              utils.NestedTransform.parseZZ(disambiguatorOpt)(_.map(f))

        // ---------------------------------------------------------------------------
        def vldt(c: Cls): Errs = __vldt(c) { c => if (!multiple(c)) checkUInput(c) else checkZInput(c) }

          // ---------------------------------------------------------------------------
          def _meta  (c: Cls): Cls     = resolve(c).pipe   (_trnsf(c).transformMeta(c, _))
          def atomuus(c: Cls): AtomUUs = resolve(c).flatMap(_trnsf(c).transformData(c, multiple(c))) /* TODO: can only be one target actualy */ }

    // ===========================================================================
    case class TransformUU(target: TqRPathz, disambiguatorOpt: UnionObjectDisambiguatorOpt, f: HeadU => HeadU)
          extends EntityTransformXXX with ActionUU1N {
        final override protected def _trnsf(c: Cls): NestedTransform = utils.NestedTransform.parseUU(disambiguatorOpt)(f)
        final override protected val multiple = _Single

        def vldt(c: Cls): Errs = __vldt(c)(checkUInput) }

      // ===========================================================================
      case class TransformZZ(target: TqRPathz, disambiguatorOpt: UnionObjectDisambiguatorOpt, f: HeadZ => HeadZ)
          extends EntityTransformXXX with ActionUU1N {
        final override protected def _trnsf(c: Cls): NestedTransform = utils.NestedTransform.parseZZ(disambiguatorOpt)(f)
        final override protected val multiple = _Multiple

        def vldt(c: Cls): Errs = __vldt(c)(checkZInput) }

    // ===========================================================================
    case class TransformUZ(target: TqRPathz, f: HeadU => HeadZ)
          extends EntityTransformXX with ActionUU1N {
        final override protected def _trnsf(c: Cls): NestedTransform = utils.NestedTransform.parseUZ(f)

        // ---------------------------------------------------------------------------
        def vldt(c: Cls): Errs = __vldt(c)(checkUInput)

        // ---------------------------------------------------------------------------
        def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf(c).transformMeta(c, x).toMultiple(x.force1FX) }
        def atomuus(c: Cls): AtomUUs = resolve(c).pipe {      _trnsf(c).atomuusUZ    (c)(_, target.isOptional(c))  } }

      // ===========================================================================
      case class TransformZU(target: TqRPathz, f: HeadZ => HeadU)
          extends EntityTransformXX with ActionUU1N {
        final override protected def _trnsf(c: Cls): NestedTransform = utils.NestedTransform.parseZU(f)

        // ---------------------------------------------------------------------------
        def vldt(c: Cls): Errs = __vldt(c)(checkZInput)

        // ---------------------------------------------------------------------------
        def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf(c).transformMeta(c, x).toSingle(x.force1FX) }
        def atomuus(c: Cls): AtomUUs = resolve(c).pipe {      _trnsf(c).atomuusZU    (c)(_, target.isOptional(c)) } }

    // ===========================================================================
    case class TransformUV[D1: WTT](target: TqRPathz, f: HeadU => HeadV[D1])
          extends EntityTransformXX with ActionUU1N {
        final override protected def _trnsf(c: Cls): NestedTransform = utils.NestedTransform.parseUV(f)

        // ---------------------------------------------------------------------------
        def  vldt(c: Cls): Errs = __vldt(c)(checkUInput) ++
          _vldt.validType(typeNode[D1])

        // ---------------------------------------------------------------------------
        def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf(c).transformMeta(c, x).updateInfo(x.force1FX, Info.forceFrom[D1]) }
        def atomuus(c: Cls): AtomUUs = resolve(c).pipe { x => _trnsf(c).atomuusUV    (c)(x, target.isOptional(c)) } }

      // ===========================================================================
      case class TransformZV[D1: WTT](target: TqRPathz, f: HeadZ => HeadV[D1])
          extends EntityTransformXX with ActionUU1N {
        final override protected def _trnsf(c: Cls): NestedTransform = utils.NestedTransform.parseZV(f)

        // ---------------------------------------------------------------------------
        def  vldt(c: Cls): Errs = __vldt(c)(checkZInput) ++
          _vldt.validType(typeNode[D1])

        // ---------------------------------------------------------------------------
        def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf(c).transformMeta(c, x).updateInfo(x.force1FX, Info.forceFrom[D1]) }
        def atomuus(c: Cls): AtomUUs = resolve(c).pipe { x => _trnsf(c).atomuusZV    (c)(x, target.isOptional(c)) } }

  // ===========================================================================
  case class TransformObjectCustom[D1: WTT](from: TqRPathz, to: TypeNode, f: Obj => D1) extends ActionUU1N {
        def  vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.pipe(_vldt.validType) ++ Nil // TODO: more
          // TODO: t210202155459 - verify input is indeed u
        def _meta  (c: Cls): Cls     = from.rpathz_(c).foldLeft(c) { _.updateInfo(_, to.forceNonBObjInfo) }
        def atomuus(c: Cls): AtomUUs = from.rpathz_(c).pipe(_atoms(c)(_TransformVV(_, wrap(f)))) }

    // ---------------------------------------------------------------------------
    case class TransformObjectsCustom[D1: WTT](from: TqRPathz, to: TypeNode, f: Objs => D1) extends ActionUU1N {
        def  vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.pipe(_vldt.validType) ++ Nil // TODO: more
          // TODO: t210202155459 - verify input is indeed z
        def _meta  (c: Cls): Cls     = from.rpathz_(c).foldLeft(c) { _.updateInfo(_, to.forceNonBObjInfo) }
        def atomuus(c: Cls): AtomUUs = from.rpathz_(c).pipe(_atoms(c)(_TransformVV(_, wrap((x: List[Obj]) => f(Objs.from(x)))))) }

    // ===========================================================================
    case class TransformToObj(from: TtqRPathz, to: Cls, multiple: Boolean, f: _ff11) extends ActionUU1N with TodoV1 { // TODO: split single/multiple
      // TODO: validation, disallow '[]' (use missing field instead)
      def _meta  (c: Cls): Cls     = from.rpathz_(c).pipe { c.transformField(_)(_.transformSoleSubInfo(_ => SubInfo(multiple, to))) }
      def atomuus(c: Cls): AtomUUs = from.rpathz_(c).pipe { _atoms(c)(_TransformVV(_, f)) } }

  // ===========================================================================
  // TODO: move these to validations

  private[actions] def checkUInput(c: Cls)(paths: KPathz): Errs = checkUOrZInput(c, multiple = false, paths)
  private[actions] def checkZInput(c: Cls)(paths: KPathz): Errs = checkUOrZInput(c, multiple = true , paths)

    // ---------------------------------------------------------------------------
    private def checkUOrZInput(c: Cls, multiple: Boolean, paths: KPathz): Errs =
      paths
        .filterNot { path =>
          (!multiple && c.hasSingle  (path) ||
            multiple && c.hasMultiple(path)) &&
          // FIXME: t220517120657 - ensure rightcombination
          c.hasNesting(path) }
        .in.noneIf(_.isEmpty).toSeq
        .flatMap { invalidPaths =>
          if (multiple) errs(s"210110194028:NotObjsOrObjs_:${KPathz(invalidPaths)}")
          else          errs(s"210110194029:NotObjOrObj_:${  KPathz(invalidPaths)}") }

}

// ===========================================================================

package gallia
package actions

import aptus.Anything_

import target._
import FunctionWrappers._
import atoms.AtomsUUTransforms._TransformWW

// ===========================================================================
object ActionsUUTransforms {
  import gallia.actions.utils
  import utils.ActionsUtils._
  import utils.NestedTransform

  // ===========================================================================
  private[actions] trait HasTqRPathzTarget { // TODO
    val target: TqRPathz
    def resolve(c: Cls) = target.qpathz_(c) }

  // ===========================================================================
  case class TransformUU(target: TqRPathz, disambiguatorOpt: UnionObjectDisambiguatorOpt, f: HeadU => HeadU) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf: NestedTransform = utils.NestedTransform.parseUU(disambiguatorOpt)(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        target.__qpathz(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkUInput(c) }
        // TODO: t210202155459 - verify input is indeed u

      // ---------------------------------------------------------------------------
      def _meta  (c: Cls): Cls     = resolve(c).pipe   (_trnsf.transformMeta(c, _))
      def atomuus(c: Cls): AtomUUs = resolve(c).flatMap(_trnsf.transformData(c, _Single)) } //TODO: can only be one target actually

    // ===========================================================================
    case class TransformZZ(target: TqRPathz, disambiguatorOpt: UnionObjectDisambiguatorOpt, f: HeadZ => HeadZ) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf: NestedTransform = utils.NestedTransform.parseZZ(disambiguatorOpt)(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        resolve(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkZInput(c) }

      // ---------------------------------------------------------------------------
      def _meta  (c: Cls): Cls     = resolve(c).pipe   (_trnsf.transformMeta(c, _))
      def atomuus(c: Cls): AtomUUs = resolve(c).flatMap(_trnsf.transformData(c, _Multiple)) //TODO: can only be one target actualy
    }

  // ===========================================================================
  case class TransformObjectCustom[D1: WTT](from: TqRPathz, to: TypeNode, f: Obj => D1) extends ActionUUb {
        def  vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.pipe(_vldt.validType) ++ Nil // TODO: more       
          // TODO: t210202155459 - verify input is indeed u
        def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c) { _.updateInfo(_, to.forceNonBObjInfo) }
        def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformVV(_, wrap(f)))) }
    
    // ---------------------------------------------------------------------------
    case class TransformObjectsCustom[D1: WTT](from: TqRPathz, to: TypeNode, f: Objs => D1) extends ActionUUb {
        def  vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.pipe(_vldt.validType) ++ Nil // TODO: more        
          // TODO: t210202155459 - verify input is indeed z
        def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c) { _.updateInfo(_, to.forceNonBObjInfo) }
        def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformVV(_, wrap((x: Seq[Obj]) => f(Objs.from(x)))))) }    

    // ===========================================================================
    case class TransformToObj(from: TtqRPathz, to: Cls, multiple: Boolean, f: _ff11) extends ActionUUb with TodoV1 { // TODO: split single/multiple
      // TODO: validation, disallow '[]' (use missing field instead)
      def _meta  (c: Cls): Cls     = from.qpathz_(c).pipe { c.transformField(_)(_.transformSoleSubInfo(_ => SubInfo(multiple, to))) }
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe { _atoms(c)(_TransformVV(_, f)) } }
    
  // ===========================================================================
  case class TransformUZ(target: TqRPathz, f: HeadU => HeadZ) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf: NestedTransform = utils.NestedTransform.parseUZ(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        resolve(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkUInput(c) }
        // TODO: t210202155459 - verify input is indeed z

      def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf.transformMeta(c, x).toMultiple(x.force1FX) }
      def atomuus(c: Cls): AtomUUs = resolve(c).pipe {      _trnsf.atomuusUZ    (c)(_, target.isOptional(c)) }
    }

    // ===========================================================================
    case class TransformZU(target: TqRPathz, f: HeadZ => HeadU) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf: NestedTransform = utils.NestedTransform.parseZU(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        resolve(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkZInput(c) }

      def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf.transformMeta(c, x).toNonMultiple(x.force1FX) }
      def atomuus(c: Cls): AtomUUs = resolve(c).pipe {      _trnsf.atomuusZU    (c)(_, target.isOptional(c)) }
    }

  // ===========================================================================
  case class TransformUV[D1: WTT](target: TqRPathz, f: HeadU => HeadV[D1]) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf: NestedTransform = utils.NestedTransform.parseUV(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        target.__qpathz(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkUInput(c) } ++
        _vldt.validType(typeNode[D1])
        //TODO: t210202155459 - verify input is indeed z

      // ---------------------------------------------------------------------------
      def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf.transformMeta(c, x).updateInfo(x.force1FX, Info.forceFrom[D1]) }
      def atomuus(c: Cls): AtomUUs = resolve(c).pipe {      _trnsf.atomuusUV    (c)(_, target.isOptional(c)) }
    }

    // ===========================================================================
    case class TransformZV[D1: WTT](target: TqRPathz, f: HeadZ => HeadV[D1]) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf: NestedTransform = utils.NestedTransform.parseZV(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        target.__qpathz(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkZInput(c) } ++
        _vldt.validType(typeNode[D1])

      // ---------------------------------------------------------------------------
      def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf.transformMeta(c, x).updateInfo(x.force1FX, Info.forceFrom[D1]) }
      def atomuus(c: Cls): AtomUUs = resolve(c).pipe { _trnsf.atomuusZV(c)(_, target.isOptional(c)) }
    }

  // ===========================================================================
  // TODO: check input isn't u or z + destination type is valid
  // FIXME: t210615104657 - if to is Option[T]
  case class TransformVV(from: TtqRPathz, to: TypeNode, f: _ff11, g: _ff11) extends ActionUUb {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) ++ _vldt.validType(to)
      def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c) { _.updateType(_, from.node, to) }
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms2(c)(_TransformVV(_, f), _TransformVV(_, g))) }

    // ---------------------------------------------------------------------------
    case class TransformVVx(from: TtqRPathz, to: TypeNode, f: _ff11, g: _ff11) extends ActionUUb {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c, SpecialCardiMode.IgnoreAltogether) ++ _vldt.validType(to)
      def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c) { _.updateTypex(_, from.node, to) }
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms2(c)(_TransformVV(_, from.wrapx(c, f)), _TransformVV(_, g))) }

    // ---------------------------------------------------------------------------
    case class TransformVVc(from: TtqRPathz, to: HT, f: _ff11) extends ActionUUb with TodoV1 {
      def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c) { _.updateInfo(_, to.node.forceNonBObjInfo) }
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformVV(_, from.wrapc(to, f) ))) }

    // ---------------------------------------------------------------------------
    case class TransformVVxc(from: TtqRPathz, to: HT, f: _ff11) extends ActionUUb with TodoV1 {
      def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c)(_.transformSoleValueType(_)(_ => to.node.forceNonBObjInfo.subInfo1.valueType))
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformVV(_, from.wrapxc(c, to, f) ))) }

    // ===========================================================================
    //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

    case class TransformWW1a(from: TqRPathz, f: _ff11) extends ActionUUb with IdentityM1 {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) // TODO: t210201164749
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformWW(_, f, checkType = true))) }

    // ---------------------------------------------------------------------------
    case class TransformWW1b(from: TtqRPathz, to: TypeNode, f: _ff11) extends ActionUUb {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) ++ to.pipe(_vldt.validType)
      def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c)(_.transformSoleValueType(_)(_ => to.forceNonBObjInfo.subInfo1.valueType))
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformWW(_, f, checkType = false))) }

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

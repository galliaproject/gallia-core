package gallia.actions

import scala.util.chaining._
import aptus.Anything_

import gallia._
import gallia.target._
import gallia.FunctionWrappers._
import gallia.atoms.AtomsUUTransforms._TransformWW

// ===========================================================================
object ActionsUUTransforms {
  import gallia.actions.utils
  import utils.ActionsUtils._

  // ===========================================================================
  private[actions] trait HasTqRPathzTarget { // TODO
    val target: TqRPathz
    def resolve(c: Cls) = target.qpathz_(c) }

  // ===========================================================================
  case class TransformUU(target: TqRPathz, f: HeadU => HeadU) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf = utils.NestedTransform.parseUU(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        target.__qpathz(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkUInput(c) }
        // TODO: t210202155459 - verify input is indeed z

      // ---------------------------------------------------------------------------
      def _meta  (c: Cls): Cls     = resolve(c).pipe   (_trnsf.transformMeta(c, _))
      def atomuus(c: Cls): AtomUUs = resolve(c).flatMap(_trnsf.transformData(c, multiple = false)) } //TODO: can only be one target actually

    // ===========================================================================
    case class TransformZZ(target: TqRPathz, f: HeadZ => HeadZ) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf = utils.NestedTransform.parseZZ(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        resolve(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkZInput(c) }

      // ---------------------------------------------------------------------------
      def _meta  (c: Cls): Cls     = resolve(c).pipe   (_trnsf.transformMeta(c, _))
      def atomuus(c: Cls): AtomUUs = resolve(c).flatMap(_trnsf.transformData(c, multiple = true)) //TODO: can only be one target actualy               
    }

  // ===========================================================================
  case class TransformUZ(target: TqRPathz, f: HeadU => HeadZ) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf = utils.NestedTransform.parseUZ(f)

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

      private val _trnsf = utils.NestedTransform.parseZU(f)

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
      private val _trnsf = utils.NestedTransform.parseUV(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        target.__qpathz(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkUInput(c) }
        //TODO: t210202155459 - verify input is indeed z
        //TODO: t210202155202 - check dest type is valid

      // ---------------------------------------------------------------------------
      def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf.transformMeta(c, x).transformInfo(x.force1FX)(_ => Info.forceFrom[D1]) }
      def atomuus(c: Cls): AtomUUs = resolve(c).pipe {      _trnsf.atomuusUV    (c)(_, target.isOptional(c)) }
    }

    // ===========================================================================
    case class TransformZV[D1: WTT](target: TqRPathz, f: HeadZ => HeadV[D1]) extends ActionUUb with HasTqRPathzTarget {
      private val _trnsf = utils.NestedTransform.parseZV(f)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        target.__qpathz(c).pipe {
            _trnsf.vldt(c, _) } ++
        target.__qpathz(c).pipe(_.fromz).pipe {
              checkZInput(c) }
        //TODO: t210202155202 - check dest type is valid

      // ---------------------------------------------------------------------------
      def _meta  (c: Cls): Cls     = resolve(c).pipe { x => _trnsf.transformMeta(c, x).transformInfo(x.force1FX)(_ => Info.forceFrom[D1]) }
      def atomuus(c: Cls): AtomUUs = resolve(c).pipe { _trnsf.atomuusZV(c)(_, target.isOptional(c)) }
    }

  // ===========================================================================
  // TODO: check input isn't u or z + destination type is valid

  case class TransformVV(from: TtqRPathz, to: TypeNode, f: _ff11, b: Boolean) extends ActionUUb {
      def  vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ Nil //TODO: t210202155202 - check dest type is valid
      def _meta(c: Cls): Cls  =
        if (b) from.qpathz_(c).foldLeft(c)(_.updateContainee(_, to))
        else   from.qpathz_(c).foldLeft(c)(_.updateType(_, to))
      //FIXME: t210615104657 - if to is Option[T]        
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformVV(_, f))) }

    // ---------------------------------------------------------------------------
    case class TransformVVx(from: TtqRPathz, to: TypeNode, f: _ff11) extends ActionUUb {
      def  vldt(c: Cls): Errs = from.vldtAsOrigin(c, SpecialCardiMode.IgnoreAltogether) ++ Nil //TODO: t210202155202 - check dest type is valid
      def _meta(c: Cls): Cls  = from.qpathz_(c).foldLeft(c)(_.transformInfo(_)(_.updateContainee(to.forceNonBObjInfo.containee)))
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformVV(_, from.wrapx(c, f)))) }

    // ---------------------------------------------------------------------------
    case class TransformVVc(from: TtqRPathz, to: HT, f: _ff11) extends ActionUUb with TodoV1 {
      def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c)(_.updateType(_, to.node))
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformVV(_, from.wrapc(to, f) ))) }

    // ---------------------------------------------------------------------------
case class TransformFoo(from: TtqRPathz, to: Cls, f: _ff11) extends ActionUUb with TodoV1 {  
      def _meta  (c: Cls): Cls     = from.qpathz_(c).pipe(c.transformInfo(_)(_.updateContainee(to)))
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformVV(_, f))) }
    
    // ---------------------------------------------------------------------------
    case class TransformVVxc(from: TtqRPathz, to: HT, f: _ff11) extends ActionUUb with TodoV1 {
      def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c)(_.updateContainee(_, to.node))
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformVV(_, from.wrapxc(c, to, f) ))) }

    // ===========================================================================
    //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

    case class TransformWW1a(from: TqRPathz, f: _ff11) extends ActionUUb with IdentityM1 {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) // TODO: t210201164749
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformWW(_, f, checkType = true))) }

    // ---------------------------------------------------------------------------
    case class TransformWW1b(from: TtqRPathz, to: TypeNode, f: _ff11) extends ActionUUb {
      def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) ++ to.thn(_vldt.validType)
      def _meta  (c: Cls): Cls     = from.qpathz_(c).foldLeft(c) { (curr, path) => curr.updateContainee(path, to) }
      def atomuus(c: Cls): AtomUUs = from.qpathz_(c).pipe(_atoms(c)(_TransformWW(_, f, checkType = false))) }

  // ===========================================================================
  // TODO: move these to validations

  private[actions] def checkUInput(c: Cls)(paths: KPathz): Errs = checkUOrZInput(c, multiple = false, paths)
  private[actions] def checkZInput(c: Cls)(paths: KPathz): Errs = checkUOrZInput(c, multiple = true , paths)

    // ---------------------------------------------------------------------------
    private def checkUOrZInput(c: Cls, multiple: Boolean, paths: KPathz): Errs =
      paths
        .filterNot(path =>
          (!multiple && c.isScalar  (path) ||
            multiple && c.isMultiple(path)) &&
          c.isNesting(path))
        .as.noneIf(_.isEmpty).toSeq
        .flatMap { invalidPaths =>
          if (multiple) errs(s"210110194028:NotObjsOrObjs_:${KPathz(invalidPaths)}")
          else          errs(s"210110194029:NotObjOrObj_:${  KPathz(invalidPaths)}") }

  // ---------------------------------------------------------------------------
  import gallia.meta.InfoUtils

  private[actions] def checkNode(node: TypeNode): Errs =
    node.validContainerOpt match {
      case None =>
        if (node.isContainedWhatever2) Nil
        else                           errs(s"TODO:210112142932:${node}")

      case Some(leaf) =>
        InfoUtils.containeeOpt(leaf) match {
          case None     => errs(s"TODO:210112142933:${node}")
          case Some(yy) => Nil } }
      //bobj('v -> 1  ).transform ('v).using(x => Seq(x, x +  1 )).fail("210112142933") // limitation; .test(bobj('v -> Seq(1  ,   2 ) ))
      //bobj('v -> "1").transform ('v).using(x => Seq(x, x + "1")).fail("210112142933") // limitation; .test(bobj('v -> Seq("1", "11") ))
      //Default01.transform('f).using(_.size.toString).test(bobj('f -> "3", 'g -> 1))

}

// ===========================================================================

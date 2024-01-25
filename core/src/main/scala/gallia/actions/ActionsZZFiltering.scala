package gallia
package actions

import aptus.Anything_
import trgt._
import FunctionWrappers.{_pp11, _pp21, _pp31}
import atoms.AtomsZZFiltering._
import Whatever._

// ===========================================================================
object ActionsZZFiltering {
  import gallia.actions.utils.NestedTransform._

  //TODO: t210111095156 separate all the Whatever

  // ---------------------------------------------------------------------------
  protected trait HasAsFind { // bit of a hack... (see t201021120752)
    val asFind: Boolean

    final def max: Option[Int] = if (asFind) Some(1) else None
  }

  // ===========================================================================
  case class FilterUnsafe(pred: Obj => Boolean, asFind: Boolean)
      extends ActionZZ01 with IdentityVM1 with HasAsFind {
    def  atomzz: AtomZZ = _FilterUnsafe(pred, max) }

  // ===========================================================================
  case class FilterByV[$Ignored](target: TtqKPath, pred: $Ignored => Boolean, asFind: Boolean)
      extends ActionZZ11 with IdentityM1 with UsesSimpleTypedTargetQuery1Target[KPath] with HasAsFind {
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy1(_, pred, max)) }

      // ===========================================================================
      case class FilterByV2[T1, T2](target: TtqKPath2, pred: (T1, T2) => Boolean, asFind: Boolean)
          extends ActionZZ11 with IdentityM1 with UsesSimpleTypedTargetQuery2Target[KPath] with HasAsFind {
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy2(_, pred, max)) }

      // ---------------------------------------------------------------------------
      case class FilterByV3[T1, T2, T3](target: TtqKPath3, pred: (T1, T2, T3) => Boolean, asFind: Boolean)
          extends ActionZZ11 with IdentityM1 with UsesSimpleTypedTargetQuery3Target[KPath] with HasAsFind {
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy3(_, pred, max)) }

    // ===========================================================================
    @Distributivity case class TakeWhile(pred: HeadU => HeadV[Boolean]) extends ActionZZ11 with IdentityM1 {
        private val _trsf = parseUV(pred)

        // ---------------------------------------------------------------------------
        def vldt  (c: Cls): Errs   = _trsf._vldt(c)
        def atomzz(c: Cls): AtomZZ = _TakeWhile(_trsf.dataU2B(c)) }

      // ---------------------------------------------------------------------------
      @Distributivity case class DropWhile(pred: HeadU => HeadV[Boolean]) extends ActionZZ11 with IdentityM1 {
        private val _trsf = parseUV(pred)

        // ---------------------------------------------------------------------------
        def vldt  (c: Cls): Errs   = _trsf._vldt(c)
        def atomzz(c: Cls): AtomZZ = _DropWhile(_trsf.dataU2B(c)) }

  // ===========================================================================
    case class FilterByU(target: TtqKPath, pred: HeadU => HeadV[Boolean], asFind: Boolean)
          extends ActionZZ11 with IdentityM1 with HasAsFind {
        private val _trsf = parseUV(pred)

        // ---------------------------------------------------------------------------
        def vldt(c: Cls): Errs   =
          target.tq.vldtAsOrigin(c)
            .orIfEmpty { target.resolve(c).pipe(checkUInput(c)) }
            .orIfEmpty { target.resolve(c).pipe(_trsf.vldt(c, _)) }

        // ---------------------------------------------------------------------------
        def  atomzz(c: Cls): AtomZZ =
          target
            .pathPairT(c)
            .pipe { pathPair =>
              _trsf
                .dataU2B(c.forceNestedClass(pathPair.path))
                .pipe(_FilterBy1(pathPair, _, max)) } }

    // ---------------------------------------------------------------------------
    case class FilterByZ(target: TtqKPath, pred: HeadZ => HeadV[Boolean], asFind: Boolean)
          extends ActionZZ11 with IdentityM1 with HasAsFind {
        private val _trsf = parseZV(pred)

        // ---------------------------------------------------------------------------
        def vldt(c: Cls): Errs   =
          target.tq.vldtAsOrigin(c)
            .orIfEmpty { target.resolve(c).pipe(checkZInput(c)) }
            .orIfEmpty { target.resolve(c).pipe(_trsf.vldt(c, _)) }

        // ---------------------------------------------------------------------------
        def  atomzz(c: Cls): AtomZZ =
          target
            .pathPairT(c)
            .pipe { pathPair =>
              _trsf
                .dataZ2B(c.forceNestedClass(pathPair.path))
                .pipe(wrap)
                .pipe(_FilterBy1(pathPair, _, max)) }

        // ---------------------------------------------------------------------------
        private def wrap(f: Objs => Boolean): List[Obj] => Boolean = (x: List[Obj]) => f(Objs.from(x))
    }

    // ===========================================================================
    private[actions] def checkUInput(c: Cls)(path: KPath): Errs = checkUOrZInput(c, multiple = false, KPathz(Seq(path)))
    private[actions] def checkZInput(c: Cls)(path: KPath): Errs = checkUOrZInput(c, multiple = true , KPathz(Seq(path)))

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

    // ===========================================================================
    case class FilterByPresence(target: TqKPath, negate: Boolean, asFind: Boolean)
      extends ActionZZ11 with IdentityM1 with HasAsFind {
        def vldt(c: Cls): Errs   =
          target.vldtAsOrigin(c)
            .orIfEmpty(_vldt.checkNonRequired(c, target.resolve(c).pipe(KPathz.from)))

        // ---------------------------------------------------------------------------
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy1(_,
            if (negate) (x: Any) => x match { case None => true ; case Some(_) => false }
            else        (x: Any) => x match { case None => false; case Some(_) => true },
          max)) }

    // ===========================================================================
    case class FilterByWV(target: TqKPath, pred: WV => Boolean, asFind: Boolean)
      extends ActionZZ11 with IdentityM1 with HasAsFind {
        def vldt  (c: Cls): Errs   = target.vldtAsOrigin(c)
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy1(_, wrap, max))

        // ---------------------------------------------------------------------------
        private def wrap: _pp11 = (x: Any) => whateverOpt(x).map(pred).getOrElse(false) }

      // ===========================================================================
      case class FilterByWV2(target: TqKPath2, pred: (WV, WV) => Boolean, asFind: Boolean)
        extends ActionZZ11 with IdentityM1 with HasAsFind {
          def vldt  (c: Cls): Errs   = target.vldtAsOrigin(c)
          def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy2(_, wrap, max))
  
          // ---------------------------------------------------------------------------
          private def wrap: _pp21 = (x: Any, y: Any) => whatever2Opt(x, y).map(x => pred(x._1, x._2)).getOrElse(false) }

      // ===========================================================================
      case class FilterByWV3(target: TqKPath3, pred: (WV, WV, WV) => Boolean, asFind: Boolean)
        extends ActionZZ11 with IdentityM1 with HasAsFind {
          def vldt  (c: Cls): Errs   = target.vldtAsOrigin(c)
          def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy3(_, wrap, max))
  
          // ---------------------------------------------------------------------------
          private def wrap: _pp31 = (x: Any, y: Any, z: Any) => whatever3Opt(x, y, z).map(x => pred(x._1, x._2, x._3)).getOrElse(false) }    

}

// ===========================================================================

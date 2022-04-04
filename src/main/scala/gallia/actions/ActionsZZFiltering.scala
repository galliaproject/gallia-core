package gallia
package actions

import target._
import FunctionWrappers.{_pp11, _pp21, _pp31}
import atoms.AtomsZZFiltering._
import gallia.FunctionWrappers._pp21
import gallia.Whatever._

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
      extends ActionZZd with IdentityVM1 with HasAsFind {
    def  atomzz: AtomZZ = _FilterUnsafe(pred, max) }

  // ===========================================================================
  case class FilterByV[$Ignored](target: TtqKPath, pred: $Ignored => Boolean, asFind: Boolean)
      extends ActionZZc with IdentityM1 with UsesSimpleTypedTargetQuery1Target[KPath] with HasAsFind {
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy1(_, pred, max)) }

      // ===========================================================================
      case class FilterByV2[T1, T2](target: TtqKPath2, pred: (T1, T2) => Boolean, asFind: Boolean)
          extends ActionZZc with IdentityM1 with UsesSimpleTypedTargetQuery2Target[KPath] with HasAsFind {
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy2(_, pred, max)) }

      // ---------------------------------------------------------------------------
      case class FilterByV3[T1, T2, T3](target: TtqKPath3, pred: (T1, T2, T3) => Boolean, asFind: Boolean)
          extends ActionZZc with IdentityM1 with UsesSimpleTypedTargetQuery3Target[KPath] with HasAsFind {
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy3(_, pred, max)) }

    // ===========================================================================
    case class FilterByU[T](target: TtqKPath, pred: HeadU => HeadV[Boolean], asFind: Boolean)
          extends ActionZZc with IdentityM1 with HasAsFind {
        def  vldt  (c: Cls): Errs   = Nil//TODO parseUV(pred)._vldt(nestedC)
        def  atomzz(c: Cls): AtomZZ =
          target
            .pathPairT(c)
            .pipe { pathPair =>
              parseUV(pred)
                .dataU2B(c.forceNestedClass(pathPair.path))
                .pipe(_FilterBy1(pathPair, _, max)) } }

    // ---------------------------------------------------------------------------
    case class FilterByZ[T](target: TtqKPath, pred: HeadZ => HeadV[Boolean], asFind: Boolean)
          extends ActionZZc with IdentityM1 with HasAsFind {
        def  vldt  (c: Cls): Errs   = Nil//TODO parseUV(pred)._vldt(nestedC)
        def  atomzz(c: Cls): AtomZZ =
          target
            .pathPairT(c)
            .pipe { pathPair =>
              parseZV(pred)
                .dataZ2B(c.forceNestedClass(pathPair.path))
                .pipe(wrap)
                .pipe(_FilterBy1(pathPair, _, max)) }

        // ---------------------------------------------------------------------------
        private def wrap(f: Objs => Boolean): Seq[Obj] => Boolean = (x: Seq[Obj]) => f(Objs.from(x))
    }

    // ===========================================================================
    case class FilterByWV(target: TqKPath, pred: WV => Boolean, asFind: Boolean)
      extends ActionZZc with IdentityM1 with HasAsFind {
        def vldt  (c: Cls): Errs   = target.vldtAsOrigin(c)
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy1(_, wrap, max))

        // ---------------------------------------------------------------------------
        private def wrap: _pp11 = (x: Any) => whateverOpt(x).map(pred).getOrElse(false) }

      // ===========================================================================
      case class FilterByWV2(target: TqKPath2, pred: (WV, WV) => Boolean, asFind: Boolean)
        extends ActionZZc with IdentityM1 with HasAsFind {
          def vldt  (c: Cls): Errs   = target.vldtAsOrigin(c)
          def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy2(_, wrap, max))
  
          // ---------------------------------------------------------------------------
          private def wrap: _pp21 = (x: Any, y: Any) => whatever2Opt(x, y).map(x => pred(x._1, x._2)).getOrElse(false) }

      // ===========================================================================
      case class FilterByWV3(target: TqKPath3, pred: (WV, WV, WV) => Boolean, asFind: Boolean)
        extends ActionZZc with IdentityM1 with HasAsFind {
          def vldt  (c: Cls): Errs   = target.vldtAsOrigin(c)
          def atomzz(c: Cls): AtomZZ = target.pathPairT(c).pipe(_FilterBy3(_, wrap, max))
  
          // ---------------------------------------------------------------------------
          private def wrap: _pp31 = (x: Any, y: Any, z: Any) => whatever3Opt(x, y, z).map(x => pred(x._1, x._2, x._3)).getOrElse(false) }    

}

// ===========================================================================


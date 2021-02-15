package gallia.actions

import aptus.Anything_

import gallia._
import gallia.target._
import gallia.FunctionWrappers._pp11
import gallia.atoms.AtomsZZFiltering._

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
  case class FilterUnsafe(pred: Obj => Boolean, asFind: Boolean = false)
      extends ActionZZd with IdentityVM1 with HasAsFind {
    def  atomzz: AtomZZ = _FilterUnsafe(pred, max) }

  // ===========================================================================
  case class FilterByV[$Ignored](target: TtqKPath, pred: $Ignored => Boolean, asFind: Boolean = false)
      extends ActionZZc with IdentityM1 with UsesSimpleTypedTargetQuery1Target[KPath] with HasAsFind {
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).thn(_FilterBy1(_, pred, max)) }

    // ===========================================================================
    case class FilterByWV[T](target: TqKPath, pred: WV => WV2[Boolean], asFind: Boolean = false)
      extends ActionZZc with IdentityM1 with HasAsFind {
        def vldt  (c: Cls): Errs   = target.vldtAsOrigin(c)
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).thn(_FilterBy1(_, wrap, max))

        // ---------------------------------------------------------------------------
        private def wrap: _pp11 = (x: Any) => pred(new WV(x)).forceOne }

      // ===========================================================================
      case class FilterByV2[T1, T2](target: TtqKPath2, pred: (T1, T2) => Boolean, asFind: Boolean = false)
          extends ActionZZc with IdentityM1 with UsesSimpleTypedTargetQuery2Target[KPath] with HasAsFind {
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).thn(_FilterBy2(_, pred, max)) }

      // ---------------------------------------------------------------------------
      case class FilterByV3[T1, T2, T3](target: TtqKPath3, pred: (T1, T2, T3) => Boolean, asFind: Boolean = false)
          extends ActionZZc with IdentityM1 with UsesSimpleTypedTargetQuery3Target[KPath] with HasAsFind {
        def atomzz(c: Cls): AtomZZ = target.pathPairT(c).thn(_FilterBy3(_, pred, max)) }

    // ===========================================================================
    case class FilterByU[T](target: TtqKPath, pred: HeadU => HeadV[Boolean], asFind: Boolean = false)
          extends ActionZZc with IdentityM1 with HasAsFind {
        def  vldt  (c: Cls): Errs   = Nil//TODO parseUV(pred)._vldt(nestedC)
        def  atomzz(c: Cls): AtomZZ =
          target
            .pathPairT(c)
            .thn { pathPair =>
              parseUV(pred)
                .dataU2B(c.forceNestedClass(pathPair.path))
                .thn(_FilterBy1(pathPair, _, max)) } }

    // ---------------------------------------------------------------------------
    case class FilterByZ[T](target: TtqKPath, pred: HeadZ => HeadV[Boolean], asFind: Boolean = false)
          extends ActionZZc with IdentityM1 with HasAsFind {
        def  vldt  (c: Cls): Errs   = Nil//TODO parseUV(pred)._vldt(nestedC)
        def  atomzz(c: Cls): AtomZZ =
          target
            .pathPairT(c)
            .thn { pathPair =>
              parseZV(pred)
                .dataZ2B(c.forceNestedClass(pathPair.path))
                .thn(wrap)
                .thn(_FilterBy1(pathPair, _, max)) }

        // ---------------------------------------------------------------------------
        private def wrap(f: Objs => Boolean): Seq[Obj] => Boolean = (x: Seq[Obj]) => f(Objs.from(x))
    }

}

// ===========================================================================


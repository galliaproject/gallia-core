package gallia
package actions
package common

import target._
import domain.PathPair

// ===========================================================================
object ActionsUUTransformObjectIf {
  import gallia.actions.utils.NestedTransform._
  import gallia.atoms.AtomsCustom

  // ===========================================================================
  abstract class _TransformObjectIf(pred: AnyValue => Boolean, f: HeadO => HeadO) extends ActionUUc with IdentityM1 {
    def pathPair(c: Cls): PathPair

    // ===========================================================================
    private val _trsf = parseUU(f)

    // ---------------------------------------------------------------------------
    def __vldt(c: Cls): Errs =
      _trsf._vldt(c)
        .orIfEmpty {
      _trsf._meta(c).pipe { c2 => _Error.SchemaMustNotChange(c, c2).errsIf(c2 != c) } }

    // ---------------------------------------------------------------------------
    def  atomuu(c: Cls): AtomUU = pathPair(c).pipe { pair => AtomsCustom._CustomConditionalOO(o => pred(pair.lookup(o)), _trsf.dataU2U(c)) }
  }

  // ===========================================================================
  case class TransformObjectIf(target: TtqKPath, pred: AnyValue => Boolean, f: HeadO => HeadO)
      extends _TransformObjectIf(pred, f) {
        def pathPair(c: Cls) = target.pathPairT(c)

        // ---------------------------------------------------------------------------
        def vldt(c: Cls): Errs = target.vldtAsOrigin(c).orIfEmpty(__vldt(c)) }

    // ===========================================================================
    case class TransformObjectIfW(target: KPath, pred: AnyValue => Boolean, f: HeadO => HeadO)
      extends _TransformObjectIf(pred, f) {
        def pathPair(c: Cls) = PathPair(target, c.isOptional(target))

        // ---------------------------------------------------------------------------
        def vldt(c: Cls): Errs = _vldt.fieldPresence(c, target).toSeq.orIfEmpty(__vldt(c)) }

}

// ===========================================================================

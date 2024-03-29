package gallia
package actions
package common

import trgt._
import domain._
import FunctionWrappers._
import atoms.common.AtomsCommonTransforms._
import actions.utils.NestedTransform

// ===========================================================================
object ActionsCommonGenerationOther { //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...
  import ActionsCommonTransforms.{checkUInput, checkZInput}
  
  // ---------------------------------------------------------------------------
  private val ValuePlaceholder: AnyValue = null // when only need meta

  // ---------------------------------------------------------------------------
  type GenerateVtoV = ActionsCommonGenerusion.Generate1VtoV
  val  GenerateVtoV = ActionsCommonGenerusion.Generate1VtoV

  // ===========================================================================
  abstract class _Generate(target: TqKPath, newPath: KPath) extends ActionUU11 {
      protected val _trnsf: NestedTransform

      // ---------------------------------------------------------------------------
      protected val metaF: Optional => Cls  => Info
      protected val dataF: (Cls, PathPair) => _ff11

      // ---------------------------------------------------------------------------
      def protoValidate(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        _vldt.fieldAbsence(c, newPath) ++
        target.kpath_(c).pipe {
          _trnsf.vldt(c, _) }

      // ---------------------------------------------------------------------------
      def _meta (c: Cls): Cls    = target.pathPairT(c).pipe { pair => _trnsf.generateMeta(c, pair.path).pipe(metaF(pair.optional)).pipe(c.add(newPath, _)) }
      def atomuu(c: Cls): AtomUU = target.pathPairT(c).pipe { pair => _Transform1to1(pair, newPath, dataF(c, pair)) }
    }

    // ===========================================================================
    case class GenerateUU(target: TqKPath, newPath: KPath, f: HeadU => HeadU) extends _Generate(target, newPath) {
        protected val _trnsf = NestedTransform.parseUU(f)

        protected val metaF = optional  => nc => Info(optional, SubInfo(multiple = false, nc))
        protected val dataF = (nc, pair) => _trnsf.uu(nc.forceNestedClass(pair.path), pair.optional)

        // ---------------------------------------------------------------------------
        def  vldt(c: Cls): Errs = protoValidate(c) ++ target.checkErrors(c)(checkUInput) }

      // ===========================================================================
      case class GenerateZZ(target: TqKPath, newPath: KPath, f: HeadZ => HeadZ) extends _Generate(target, newPath) {
        protected val _trnsf = NestedTransform.parseZZ(f)

        protected val metaF = optional  => nc => Info(optional, SubInfo(multiple = true, nc))
        protected val dataF = (c, pair) => _trnsf.zz(c.forceNestedClass(pair.path), pair.optional)

        // ---------------------------------------------------------------------------
        def  vldt(c: Cls): Errs = protoValidate(c) ++ target.checkErrors(c)(checkZInput) }

}

// ===========================================================================

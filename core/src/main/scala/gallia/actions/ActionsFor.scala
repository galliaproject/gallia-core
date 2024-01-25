package gallia
package actions

import trgt._
import atoms.AtomsCustom._
import plans.AtomPlan
import actions.utils.NestedTransform

// ===========================================================================
object ActionsFor {
  import actions.utils.NestedTransform.{parseUU, parseZZ, parseUZ, parseZU, parseUV, parseZV}

  // ===========================================================================
  case class ForPathUU(target: TqKPath, f: (HeadU, KPath) => HeadU) extends ActionUU11 {
      protected def resolve(c: Cls): HeadU => HeadU = target.resolve(c).pipe(p => u => f(u, p))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).pipe(parseUU)._vldt(c) //TODO: validate target too
      def _meta  (c: Cls): Cls    = resolve(c).pipe(parseUU)._meta(c)
      def  atomuu(c: Cls): AtomUU = resolve(c).pipe(parseUU).dataU2U(c).pipe(_CustomOO)
    }

    // ---------------------------------------------------------------------------
    case class ForPathZZ(target: TqKPath, f: (HeadZ, KPath) => HeadZ) extends ActionZZ11 {
      private def resolve(c: Cls): HeadZ => HeadZ = target.resolve(c).pipe { p => z => f(z, p) }

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).pipe(parseZZ)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).pipe(parseZZ)._meta(c)
      def  atomzz(c: Cls): AtomZZ = resolve(c).pipe(parseZZ).dataZ2Z(c).pipe(_CustomZZ)
    }

    // ===========================================================================
    case class ForPathUZ(target: TqKPath, f: (HeadU, KPath) => HeadZ) extends ActionUZ11 {
      protected def resolve(c: Cls): HeadU => HeadZ = target.resolve(c).pipe(p => u => f(u, p))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).pipe(parseUZ)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).pipe(parseUZ)._meta(c)
      def  atomuz(c: Cls): AtomUZ = resolve(c).pipe(parseUZ).dataU2Z(c).pipe(_CustomOZ)
    }

    // ---------------------------------------------------------------------------
    case class ForPathZU(target: TqKPath, f: (HeadZ, KPath) => HeadU) extends ActionZU11 {
      protected def resolve(c: Cls): HeadZ => HeadU = target.resolve(c).pipe(p => z => f(z, p))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).pipe(parseZU)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).pipe(parseZU)._meta(c)
      def  atomzu(c: Cls): AtomZU = resolve(c).pipe(parseZU).dataZ2U(c).pipe(_CustomZO)
    }

    // ===========================================================================
    case class ForPathUV[V: WTT](target: TqKPath, f: (HeadU, KPath) => HeadV[V]) extends ActionUV11b {
      private def resolve(c: Cls): HeadU => HeadV[V] = target.resolve(c).pipe(k => u => f(u, k))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).pipe(parseUV)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).pipe(parseUV)._meta(c)
      def  atomuv(c: Cls): AtomUV = resolve(c).pipe(parseUV).dataU2V(c).pipe(_CustomOV)
    }

    // ---------------------------------------------------------------------------
    case class ForPathZV[V: WTT](target: TqKPath, f: (HeadZ, KPath) => HeadV[V]) extends ActionZV11b {
      private def resolve(c: Cls): HeadZ => HeadV[V] = target.resolve(c).pipe(k => x => f(x, k))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).pipe(parseZV)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).pipe(parseZV)._meta(c)
      def  atomzv(c: Cls): AtomZV = resolve(c).pipe(parseZV).dataZ2V(c).pipe(_CustomZV)
    }

  // ===========================================================================
  case class ForPathsUU(target: TqKPathz, f: (HeadU, KPathW) => HeadU) extends ActionUU1N {
      private def resolveSequence(c: Cls): Seq[HeadU => HeadU] = target.resolve(c).map { p => (u: HeadU) => f(u, p) }

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs     = Nil//TODO
      def _meta  (c: Cls): Cls      = resolveSequence(c).foldLeft(c)((curr, g) => parseUU(g)._meta(curr) )
      def  atomuus(c: Cls): AtomUUs = {
        val nestedTransforms: Seq[NestedTransform] = resolveSequence(c).pipe(_nestedTransforms(c, parseUU))
        val atomPlans       : Seq[AtomPlan]        = nestedTransforms.map(_.metaToAtomPlan(c))
        val optimizable     : Boolean              = atomPlans.nonEmpty && atomPlans.forall(_.dag.isChain)
        
        if (optimizable) atomPlans.pipe(AtomPlan.stitchAll).chainU   
        else             nestedTransforms.map(_.dataU2U(c)).map(_CustomOO)        
      }
    }

  // ---------------------------------------------------------------------------
  case class ForPathsZZ(target: TqKPathz, f: (HeadZ, KPathW) => HeadZ) extends ActionZZ1N {
      private def resolveSequence(c: Cls): Seq[HeadZ => HeadZ] = target.resolve(c).map { p => (z: HeadZ) => f(z, p) }

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs     = Nil//TODO
      def _meta  (c: Cls): Cls      = resolveSequence(c).foldLeft(c)((curr, g) => parseZZ(g)._meta(curr) )
      def  atomzzs(c: Cls): AtomZZs = {
        val nestedTransforms: Seq[NestedTransform] = resolveSequence(c).pipe(_nestedTransforms(c, parseZZ))
        val atomPlans       : Seq[AtomPlan]        = nestedTransforms.map(_.metaToAtomPlan(c))        
        val optimizable     : Boolean              = atomPlans.nonEmpty && atomPlans.forall(_.dag.isChain)
        
        if (optimizable) atomPlans.pipe(AtomPlan.stitchAll).chainZ
        else             nestedTransforms.map(_.dataZ2Z(c)).map(_CustomZZ)
      }
    }

    // ===========================================================================
    private def _nestedTransforms[$Head](c: Cls, f: Function[$Head, $Head] => NestedTransform)(sequence: Seq[$Head => $Head]): Seq[NestedTransform] = {
      var currCls: Cls = c

      sequence
        .toList // ensure stricteness
        .map(f)
        .map { nestedTransform => // TODO: better functional equivalent?
          currCls = nestedTransform._meta(currCls)
          nestedTransform
        }
    }

}

// ===========================================================================

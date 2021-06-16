package gallia.actions

import scala.util.chaining._
import aptus.Anything_

import gallia._
import gallia.target._
import gallia.atoms.AtomsCustom._
import gallia.plans.AtomPlan
import gallia.actions.utils.NestedTransform

// ===========================================================================
object ActionsFor {
  import gallia.actions.utils.NestedTransform.{parseUU, parseZZ, parseUZ, parseZU, parseUV, parseZV}

  // ===========================================================================
  case class ForPathUU(target: TqKPath, f: (HeadU, KPath) => HeadU) extends ActionUUc {
      protected def resolve(c: Cls): HeadU => HeadU = target.resolve(c).thn(p => u => f(u, p))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).thn(parseUU)._vldt(c) //TODO: validate target too
      def _meta  (c: Cls): Cls    = resolve(c).thn(parseUU)._meta(c)
      def  atomuu(c: Cls): AtomUU = resolve(c).thn(parseUU).dataU2U(c).thn(_CustomOO)
    }

    // ---------------------------------------------------------------------------
    case class ForPathZZ(target: TqKPath, f: (HeadZ, KPath) => HeadZ) extends ActionZZc {
      private def resolve(c: Cls): HeadZ => HeadZ = target.resolve(c).thn { p => z => f(z, p) }

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).thn(parseZZ)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).thn(parseZZ)._meta(c)
      def  atomzz(c: Cls): AtomZZ = resolve(c).thn(parseZZ).dataZ2Z(c).thn(_CustomZZ)
    }

    // ===========================================================================
    case class ForPathUZ(target: TqKPath, f: (HeadU, KPath) => HeadZ) extends ActionUZc {
      protected def resolve(c: Cls): HeadU => HeadZ = target.resolve(c).thn(p => u => f(u, p))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).thn(parseUZ)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).thn(parseUZ)._meta(c)
      def  atomuz(c: Cls): AtomUZ = resolve(c).thn(parseUZ).dataU2Z(c).thn(_CustomOZ)
    }

    // ---------------------------------------------------------------------------
    case class ForPathZU(target: TqKPath, f: (HeadZ, KPath) => HeadU) extends ActionZUc {
      protected def resolve(c: Cls): HeadZ => HeadU = target.resolve(c).thn(p => z => f(z, p))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).thn(parseZU)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).thn(parseZU)._meta(c)
      def  atomzu(c: Cls): AtomZU = resolve(c).thn(parseZU).dataZ2U(c).thn(_CustomZO)
    }

    // ===========================================================================
    case class ForPathUV[V: WTT](target: TqKPath, f: (HeadU, KPath) => HeadV[V]) extends ActionUVc2 {
      private def resolve(c: Cls): HeadU => HeadV[V] = target.resolve(c).thn(k => u => f(u, k))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).thn(parseUV)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).thn(parseUV)._meta(c)
      def  atomuv(c: Cls): AtomUV = resolve(c).thn(parseUV).dataU2V(c).thn(_CustomOV)
    }

    // ---------------------------------------------------------------------------
    case class ForPathZV[V: WTT](target: TqKPath, f: (HeadZ, KPath) => HeadV[V]) extends ActionZVc2 {
      private def resolve(c: Cls): HeadZ => HeadV[V] = target.resolve(c).thn(k => x => f(x, k))

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs   = resolve(c).thn(parseZV)._vldt(c)
      def _meta  (c: Cls): Cls    = resolve(c).thn(parseZV)._meta(c)
      def  atomzv(c: Cls): AtomZV = resolve(c).thn(parseZV).dataZ2V(c).thn(_CustomZV)
    }

  // ===========================================================================
  case class ForPathsUU(target: TqKPathz, f: (HeadU, KPathW) => HeadU) extends ActionUUb {
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
  case class ForPathsZZ(target: TqKPathz, f: (HeadZ, KPathW) => HeadZ) extends ActionZZb {
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

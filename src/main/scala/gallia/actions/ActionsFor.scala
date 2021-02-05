package gallia.actions

import aptus.Anything_

import gallia._
import gallia.target._
import gallia.atoms.AtomsCustom._

// ===========================================================================
object ActionsFor {
  import utils.NestedTransform.{parseUU, parseZZ, parseUZ, parseZU, parseUV, parseZV}

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
      private def resolve(c: Cls): Seq[HeadU => HeadU] = target.resolve(c).map { p => (u: HeadU) => f(u, p) }

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs     = Nil//TODO
      def _meta  (c: Cls): Cls      = resolve(c).foldLeft(c)((curr, g) => parseUU(g)._meta(curr) )
      def  atomuus(c: Cls): AtomUUs = resolve(c).thn(_dataOOs(c)).map(_CustomOO)
    }

  // ---------------------------------------------------------------------------
  case class ForPathsZZ(target: TqKPathz, f: (HeadZ, KPathW) => HeadZ) extends ActionZZb {
      private def resolve(c: Cls): Seq[HeadZ => HeadZ] = target.resolve(c).map { p => (z: HeadZ) => f(z, p) }

      // ---------------------------------------------------------------------------
      def  vldt  (c: Cls): Errs     = Nil//TODO
      def _meta  (c: Cls): Cls      = resolve(c).foldLeft(c)((curr, g) => parseZZ(g)._meta(curr) )
      def  atomzzs(c: Cls): AtomZZs = resolve(c).thn(_dataZZs(c)).map(_CustomZZ)
    }

    // ===========================================================================
    private def _dataOOs(c: Cls)(x: Seq[HeadU => HeadU]): Seq[Obj => Obj] = {
        var curr: Cls = c

        x
          // TODO: better functional equivalent?
          .toList // ensure stricteness
          .map { g: Function[HeadU, HeadU] =>
            val tmp = parseUU(g)

            val next: Cls        = tmp._meta  (curr)
            val h   : Obj => Obj = tmp.dataU2U(curr)

            curr = next
            h
          }
      }

      // ---------------------------------------------------------------------------
      private def _dataZZs(c: Cls)(x: Seq[HeadZ => HeadZ]): Seq[Objs => Objs] = {
        var curr: Cls = c

        x
          // TODO: better functional equivalent?
          .toList // ensure stricteness
          .map { g: Function[HeadZ, HeadZ] =>
            val tmp = parseZZ(g)

            val next: Cls          = tmp._meta  (curr)
            val h   : Objs => Objs = tmp.dataZ2Z(curr)

            curr = next
            h
          }
      }

}

// ===========================================================================

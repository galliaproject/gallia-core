package gallia
package actions

import atoms.AtomsCustom._

// ===========================================================================
object ActionsThns {
  import utils.NestedTransform.{parseUU, parseZZ, parseUZ, parseZU, parseUV, parseZV}

  // ---------------------------------------------------------------------------
  //TODO: if optional?
  case class ThnUU(f: HeadU => HeadU) extends ActionUUc {
      def  vldt  (c: Cls): Errs   = parseUU(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseUU(f)._meta(c)
      def  atomuu(c: Cls): AtomUU = parseUU(f).dataU2U(c).pipe(_CustomOO) }

    // ---------------------------------------------------------------------------
    case class ThnZZ(f: HeadZ => HeadZ) extends ActionZZc {
      def  vldt  (c: Cls): Errs   = parseZZ(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseZZ(f)._meta(c)
      def  atomzz(c: Cls): AtomZZ = parseZZ(f).dataZ2Z(c).pipe(_CustomZZ) }

  // ===========================================================================
  case class ThnUZ(f: HeadU => HeadZ) extends ActionUZc {
      def  vldt  (c: Cls): Errs   = parseUZ(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseUZ(f)._meta(c)
      def  atomuz(c: Cls): AtomUZ = parseUZ(f).dataU2Z(c).pipe(_CustomOZ) }

    // ---------------------------------------------------------------------------
    case class ThnZU(f: HeadZ => HeadU) extends ActionZUc {
      def  vldt  (c: Cls): Errs   = parseZU(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseZU(f)._meta(c)
      def  atomzu(c: Cls): AtomZU = parseZU(f).dataZ2U(c).pipe(_CustomZO) }

  // ===========================================================================
  case class ThnUV[V: WTT](f: HeadU => HeadV[V]) extends ActionUVc2 {
      def  vldt  (c: Cls): Errs   = parseUV(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseUV(f)._meta(c)
      def  atomuv(c: Cls): AtomUV = parseUV(f).dataU2V(c).pipe(_CustomOV) }

    // ---------------------------------------------------------------------------
    case class ThnZV[V: WTT](f: HeadZ => HeadV[V]) extends ActionZVc2 {
      def  vldt  (c: Cls): Errs   = parseZV(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseZV(f)._meta(c)
      def  atomzv(c: Cls): AtomZV = parseZV(f).dataZ2V(c).pipe(_CustomZV) }

}

// ===========================================================================

package gallia
package actions

import atoms._UWrappers
import atoms.AtomsZZ._

// ===========================================================================
object ActionsZZ {

  case class LogProgress(nOpt: Option[Int], debug: Obj => String) extends ActionZZ01 with IdentityVM1 { // TODO: version with AObj debug
    def  atomzz: AtomZZ = _LogProgress(nOpt, debug) }

  // ===========================================================================
  case class Distinct() extends ActionZZ11 with IdentityVM1 {
    def atomzz(c: Cls) = _Distinct(c) }

  // ===========================================================================
  @Distributivity case class Take(n: Int) extends ActionZZ01 with IdentityVM1 { // TODO: validate n
    def atomzz = _Take(n) }

  // ---------------------------------------------------------------------------
  @Distributivity case class Drop(n: Int) extends ActionZZ01 with IdentityVM1 { // TODO: validate n
    def atomzz = _Drop(n) }

  // ---------------------------------------------------------------------------
  @Distributivity case class AddIndex(key: Key, oneBased: Boolean) extends ActionZZ01 with IdentityV1 {
    // TODO: validtte not already present
    def _meta (c: Cls): Cls = c.add(key.int)  
    def atomzz              = _AddIndex(key, oneBased) } 
  
  // ===========================================================================
  import utils.NestedTransform.{parseUU, parseUZ, parseUV}

  // ---------------------------------------------------------------------------
  case class MapU2V[V: WTT](to: TypeNode, f: HeadU => HeadV[V]) extends ActionZV11 with ActionVM1 {
        def  vldt (c: Cls): Errs   = Nil//TODO: make sure V not a List already
        def _meta (c: Cls): Cls    = c//FIXME
        def atomzv(c: Cls): AtomZV = parseUV(f).dataU2V(c).pipe(_MapU2V) }

    // ---------------------------------------------------------------------------
    case class MapU2U(f: HeadU => HeadU) extends ActionZZ11 {
      def  vldt  (c: Cls): Errs   = parseUU(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseUU(f)._meta(c)
      def  atomzz(c: Cls): AtomZZ = {
        val plan = parseUU(f).metaToAtomPlan(c)

        if (plan.dag.isChain) _UWrappers.fromMapU2U(plan)      
        else                  _MapU2U(o2o = plan.V1.naiveRunUU _) } }

    // ---------------------------------------------------------------------------
    case class FlatMap(f: HeadU => HeadZ) extends ActionZZ11 {
      def  vldt  (c: Cls): Errs   = parseUZ(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseUZ(f)._meta(c)
      def  atomzz(c: Cls): AtomZZ = parseUZ(f).dataU2Z(c).pipe(_FlatMap) }

}

// ===========================================================================


package gallia.actions

import aptus.Anything_

import gallia._
import gallia.atoms.AtomsZZ._
import gallia.atoms.AtomsCustom.{_CustomZZ, _CustomZV}
import gallia.atoms._UWrappers

// ===========================================================================
object ActionsZZ {

  case class LogProgress(nOpt: Option[Int], debug: Obj => String) extends ActionZZd with IdentityVM1 { // TODO: version with AObj debug
    def  atomzz: AtomZZ = _LogProgress(nOpt, debug) }

  // ---------------------------------------------------------------------------
  case object Distinct extends ActionZZd with IdentityVM1 {
    def  atomzz = _Distinct }

  // ---------------------------------------------------------------------------
  case class EnsureUniqueness(keysOpt: Option[Keyz] /* None = all */) extends ActionZZd with IdentityVM1 {
    //TODO: validate keys
    def atomzz = keysOpt match {
      case None       => _EnsureUniqueness
      case Some(keys) => _EnsureUniquenessBy(keys) } }

  // ===========================================================================
  @gallia.Distributivity case class Take(n: Int) extends ActionZZd with IdentityVM1 { // TODO: validate n
    def atomzz = _Take(n) }

  // ---------------------------------------------------------------------------
  @gallia.Distributivity case class AddIndex(key: Key, oneBased: Boolean) extends ActionZZd with IdentityV1 {
    // TODO: validtte not already present
    def _meta (c: Cls): Cls = c.add(key.int)  
    def atomzz              = _AddIndex(key, oneBased) } 
  
  // ===========================================================================
  import utils.NestedTransform.{parseUU, parseUZ, parseUV}

  // ---------------------------------------------------------------------------
  case class MapU2V[V: WTT](to: TypeNode, f: HeadU => HeadV[V]) extends ActionZVc with ActionVM1 {
        def  vldt (c: Cls): Errs   = Nil//TODO: make sure V not a List already
        def _meta (c: Cls): Cls    = c//FIXME
        def atomzv(c: Cls): AtomZV = parseUV(f).dataU2V(c).thn { o2v => _CustomZV(_.mapToStreamer(o2v).toList) } }

    // ---------------------------------------------------------------------------
    case class MapU2U(f: HeadU => HeadU) extends ActionZZc {
      def  vldt  (c: Cls): Errs   = parseUU(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseUU(f)._meta(c)
      def  atomzz(c: Cls): AtomZZ = {
        val plan = parseUU(f).metaToAtomPlan(c)

        if (plan.dag.isChain) _UWrappers.fromMapU2U(plan)      
        else                  _CustomZZ(_.map(plan.V1.naiveRunUU)) } }

    // ---------------------------------------------------------------------------
    case class FlatMap(f: HeadU => HeadZ) extends ActionZZc {
      def  vldt  (c: Cls): Errs   = parseUZ(f)._vldt(c)
      def _meta  (c: Cls): Cls    = parseUZ(f)._meta(c)
      def  atomzz(c: Cls): AtomZZ = parseUZ(f).dataU2Z(c).thn { g => _CustomZZ(_.flatMap(g(_).toListAndTrash)) } }

}

// ===========================================================================


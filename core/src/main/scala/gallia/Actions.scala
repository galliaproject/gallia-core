package gallia

import aptus.Seq_

import actions._
import plans.Clss

// ===========================================================================
trait Action
    extends ActionVN
    with    ActionMN
    with    ActionAN

  // ===========================================================================
  trait ActionVN {
    def  vldt(in: Clss): Errs
    final val callSite : CallSite = CallSite.generate()

    // ---------------------------------------------------------------------------
    var resultCls: Cls = null // t201214105653 - hack (also see t220615121216)
  }

  // ===========================================================================
  trait ActionMN { def _meta(in: Clss): Cls }

  // ===========================================================================
  trait ActionAN { def atoms(ctx: NodeMetaContext): Atoms }

    // ---------------------------------------------------------------------------
    case class NodeMetaContext    (afferents: Clss, efferent: Cls, origin: CallSite) { import aptus.String_
      def formatDebugAfferents: String = afferents.values.map(_.formatShort0.sectionAllOff).joinln.newline
      def formatDebugEfferent : String = efferent              .formatShort0.sectionAllOff        .newline }

// ===========================================================================
trait ActionAN1 extends ActionAN {
  final override def atoms(ctx: NodeMetaContext): Atoms = Seq(atom)
                 def atom: Atom }

// ===========================================================================
trait ActionVM0   extends ActionV0 with ActionM0
  trait ActionVM1 extends ActionV1 with ActionM1
  trait ActionVM2 extends ActionV2 with ActionM2

// ===========================================================================
trait IdentityV0 extends ActionV0 { def  vldt: Errs  = Nil }
    @deprecated // pas bien
    trait TodoV0 extends ActionV0 { def  vldt: Errs  = Nil } // TODO: t201210104557 - all missing validations to be added

trait IdentityV1 extends ActionV1 { def  vldt  (c: Cls): Errs  = Nil }
    @deprecated // pas bien
    trait TodoV1 extends ActionV1 { def  vldt  (c: Cls): Errs  = Nil } // TODO: t201210104557 - all missing validations to be added

trait IdentityM1 extends ActionM1 { def _meta  (c: Cls): Cls   = c }

trait IdentityVM1 extends IdentityV1 with IdentityM1

// ===========================================================================
trait ActionV0 extends ActionVN { // = input
    final override def vldt(in: Clss): Errs = { assert(in.isEmpty, in -> this); vldt }
                   def vldt: Errs }

  // ---------------------------------------------------------------------------
  trait ActionV1 extends ActionVN {
    final override def vldt(in: Clss): Errs = vldt(in.forceOne)
                   def vldt(in: Cls ): Errs }

  // ---------------------------------------------------------------------------
  trait ActionV2 extends ActionVN {
    final override def vldt(in: Clss): Errs = { val (in1, in2) = in.forcePair; vldt(in1, in2) }
                   def vldt(in1: Cls, in2: Cls): Errs }

// ===========================================================================
trait ActionM0 extends ActionMN {
    final override def _meta(in: Clss): Cls = { assert(in.isEmpty, in); _meta }
                   def _meta: Cls }

  // ---------------------------------------------------------------------------
  trait ActionM1 extends ActionMN {
    final override def _meta(in: Clss): Cls = _meta(in.forceOne)
                   def _meta(in: Cls ): Cls }

  // ---------------------------------------------------------------------------
  trait ActionM2 extends ActionMN { // more like 2 to 1
    final override def _meta(in: Clss      ): Cls = { val (in1, in2) = in.forcePair; _meta(in1, in2) }
                   def _meta(in1: Cls, in2: Cls): Cls }

// ===========================================================================
import actions.boilerplate.ActionAtomsBoilerplate._

// ---------------------------------------------------------------------------
trait ActionUU extends /*Action with*/ ActionVM1 with ActionAtomsUU
trait ActionZZ extends Action with ActionVM1 with ActionAtomsZZ

  // ---------------------------------------------------------------------------
  trait ActionZU extends Action with ActionV1 with ActionM1
  trait ActionUZ extends Action with ActionV1 with ActionM1

  // ---------------------------------------------------------------------------
  trait ActionUV extends Action// with ActionV1 with ActionM1
  trait ActionZV extends Action// with ActionV1 with ActionM1
  trait ActionVV extends Action// with ActionV1 with ActionM1

  // ---------------------------------------------------------------------------
  trait ActionIU extends Action with ActionVM0 with ActionAtomsIU
  trait ActionIZ extends Action with ActionVM0 with ActionAtomsIZ
  trait ActionIV extends Action with ActionVM0 with ActionAtomsIV

  // ---------------------------------------------------------------------------
  trait ActionUO extends Action with ActionV1 with IdentityM1 with ActionAtomsUO
  trait ActionZO extends Action with ActionV1 with IdentityM1 with ActionAtomsZO
  trait ActionVO extends Action with ActionV1 with IdentityM1 with ActionAtomsVO

  // ---------------------------------------------------------------------------
  trait ActionVU extends Action with ActionV1 with ActionM1 with ActionAtomsVU
  trait ActionVZ extends Action with ActionV1 with ActionM1 with ActionAtomsVZ

  // ---------------------------------------------------------------------------
  trait ActionZzToZ extends Action with ActionV2 with ActionM2 with ActionAtomsZzToZ { def vldt(c1: Cls , c2: Cls ): Errs = Nil } // TODO
  trait ActionZvToZ extends Action with ActionV2 with ActionM2 with ActionAtomsZvToZ { def vldt(c1: Cls , c2: Cls ): Errs = Nil } // TODO
  trait ActionVvToV extends Action with ActionV2 with ActionM2 with ActionAtomsVvToV { def vldt(c1: Cls , c2: Cls ): Errs = Nil } // TODO

// ===========================================================================

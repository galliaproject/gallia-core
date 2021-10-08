package gallia

import aptus.{Anything_, Seq_}

import gallia.actions._

// ===========================================================================
trait Action
    extends ActionVN
    with    ActionMN
    with    ActionAN

  // ===========================================================================
  trait ActionVN  {
    def  vldt(in: Seq[Cls]): Errs
    final val callSite : CallSite = CallSite.generate()

    // ---------------------------------------------------------------------------
    var resultCls: Cls = null // t201214105653 - hack
  }

  // ===========================================================================
  trait ActionMN  { def _meta(in: Seq[Cls]): Cls }

  // ===========================================================================
  trait ActionAN { def atoms(ctx: NodeMetaContext): Atoms }

    // ---------------------------------------------------------------------------
    case class NodeMetaContext(afferents: Seq[Cls], efferent: Cls, origin: CallSite) { def forceSingleAfferent: Cls = afferents.force.one }

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

trait IdentityUUa extends ActionUU with AtomsUUa { def atomuus: AtomUUs = Nil }
trait IdentityZZa extends ActionZZ with AtomsZZa { def atomzzs: AtomZZs = Nil }
trait IdentityVVa extends ActionVV with AtomsVVa { def atomvvs: AtomVVs = Nil }

// ===========================================================================
trait ActionV0 extends ActionVN { // = input
    final def vldt(in: Seq[Cls]): Errs = { assert(in.isEmpty, in -> this); vldt }
          def vldt: Errs
  }

  // ---------------------------------------------------------------------------
  trait ActionV1 extends ActionVN {
    final def vldt(in: Seq[Cls]): Errs = vldt(in.force.one)
          def vldt(in: Cls ): Errs
  }

  // ---------------------------------------------------------------------------
  trait ActionV2 extends ActionVN {
    final def vldt(in: Seq[Cls]): Errs = { val (in1, in2) = in.force.tuple2; vldt(in1, in2) }
          def vldt(in1: Cls, in2: Cls): Errs
  }

// ===========================================================================
trait ActionM0 extends ActionMN {
    final def _meta(in: Seq[Cls]): Cls = { assert(in.isEmpty, in); _meta }
          def _meta: Cls
  }

  // ---------------------------------------------------------------------------
  trait ActionM1 extends ActionMN {
    final def _meta(in: Seq[Cls]): Cls = _meta(in.force.one)
          def _meta(in: Cls ): Cls
  }

  // ---------------------------------------------------------------------------
  trait ActionM2 extends ActionMN { // more like 2 to 1
    final def _meta(in: Seq[Cls]      ): Cls = { val (in1, in2) = in.force.tuple2; _meta(in1, in2) }
          def _meta(in1: Cls, in2: Cls): Cls
  }

// ===========================================================================
trait ActionUU extends Action with ActionV1 with ActionM1 with AtomsUU
trait ActionZZ extends Action with ActionV1 with ActionM1 with AtomsZZ

  // ---------------------------------------------------------------------------
  trait ActionZU extends Action with ActionV1 with ActionM1
  trait ActionUZ extends Action with ActionV1 with ActionM1

  // ---------------------------------------------------------------------------
  trait ActionUV extends Action// with ActionV1 with ActionM1
  trait ActionZV extends Action// with ActionV1 with ActionM1
  trait ActionVV extends Action// with ActionV1 with ActionM1

  // ---------------------------------------------------------------------------
  trait ActionIU extends Action with ActionVM0 with AtomsIU
  trait ActionIZ extends Action with ActionVM0 with AtomsIZ
  trait ActionIV extends Action with ActionVM0 with AtomsIV

  // ---------------------------------------------------------------------------
  trait ActionUO extends Action with ActionV1 with IdentityM1 with AtomsUO
  trait ActionZO extends Action with ActionV1 with IdentityM1 with AtomsZO

  // ---------------------------------------------------------------------------
  trait ActionZzToZ extends Action with ActionV2 with ActionM2 with AtomsZzToZ {
    def vldt(c1: Cls , c2: Cls ): Errs = Nil } // TODO

// ===========================================================================
trait ActionUUa extends ActionUU with AtomsUUa
trait ActionUUd extends ActionUU with AtomsUUd
trait ActionUUc extends ActionUU with AtomsUUc
trait ActionUUb extends ActionUU with AtomsUUb

trait ActionUUbb extends ActionUU with AtomsUUbb // provides origin, TODO: t210616122449 - generalize

  // ---------------------------------------------------------------------------
  trait ActionZZd extends ActionZZ with AtomsZZd
  trait ActionZZa extends ActionZZ with AtomsZZa
  trait ActionZZc extends ActionZZ with AtomsZZc
  trait ActionZZb extends ActionZZ with AtomsZZb

  // ---------------------------------------------------------------------------
  trait ActionUZc extends ActionUZ with AtomsUZc
  trait ActionUZd extends ActionUZ with AtomsUZd

  // ---------------------------------------------------------------------------
  trait ActionZUc extends ActionZU with AtomsZUc
  trait ActionZUd extends ActionZU with AtomsZUd

  // ---------------------------------------------------------------------------
  trait ActionIUd extends ActionIU with AtomsIUd
  trait ActionIUa extends ActionIU with AtomsIUa
  trait ActionIUc extends ActionIU with AtomsIUc
  trait ActionIUb extends ActionIU with AtomsIUb

  // ---------------------------------------------------------------------------
  trait ActionIZd extends ActionIZ with AtomsIZd
  trait ActionIZa extends ActionIZ with AtomsIZa
  trait ActionIZc extends ActionIZ with AtomsIZc
  trait ActionIZb extends ActionIZ with AtomsIZb

  // ---------------------------------------------------------------------------
  trait ActionIVd extends ActionIV with AtomsIVd
  trait ActionIVa extends ActionIV with AtomsIVa
  trait ActionIVc extends ActionIV with AtomsIVc
  trait ActionIVb extends ActionIV with AtomsIVb

  // ===========================================================================
  trait ActionUOd extends ActionUO with AtomsUOd
  trait ActionUOa extends ActionUO with AtomsUOa
  trait ActionUOc extends ActionUO with AtomsUOc
  trait ActionUOb extends ActionUO with AtomsUOb

  // ---------------------------------------------------------------------------
  trait ActionZOd extends ActionZO with AtomsZOd
  trait ActionZOa extends ActionZO with AtomsZOa
  trait ActionZOc extends ActionZO with AtomsZOc
  trait ActionZOb extends ActionZO with AtomsZOb

  // ---------------------------------------------------------------------------
//  trait ActionVOd extends ActionVO with AtomsVOd
//  trait ActionVOa extends ActionVO with AtomsVOa
//  trait ActionVOc extends ActionVO with AtomsVOc
//  trait ActionVOb extends ActionVO with AtomsVOb

  // ===========================================================================
              trait ActionUVc  extends ActionUV with AtomsUVc
  @deprecated trait ActionUVc2 extends ActionUVc with ActionV1 with ActionM1

              trait ActionZVc  extends ActionZV with AtomsZVc
  @deprecated trait ActionZVc2 extends ActionZV with AtomsZVc with ActionV1 with ActionM1

// ===========================================================================
import gallia.target.{TypedTargetQuery, TypedTargetQuery2, TypedTargetQuery3}

// TODO: generalize + TargetQuery counterpart
trait UsesSimpleTypedTargetQuery1Target[$Target] { // TODO: mixin action?
    val target: TypedTargetQuery[$Target]
    final def vldt(c: Cls): Errs = target.vldtAsOrigin(c) }

  trait UsesSimpleTypedTargetQuery2Target[$Target] {
    val target: TypedTargetQuery2[$Target]
    final def vldt(c: Cls): Errs = target.vldtAsOrigin(c) }

  trait UsesSimpleTypedTargetQuery3Target[$Target] {
    val target: TypedTargetQuery3[$Target]
    final def vldt(c: Cls): Errs = target.vldtAsOrigin(c) }


// ===========================================================================
trait SquashXN extends Action with TodoV1 {
    val to: gallia.reflect.TypeNode
    def atom(c: Cls): Atom

    // ---------------------------------------------------------------------------
    final def _meta(ignored: Seq[Cls])   : Cls = Cls.Dummy //TODO?
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.forceSingleAfferent.pipe(atom).in.seq
  }

// ===========================================================================

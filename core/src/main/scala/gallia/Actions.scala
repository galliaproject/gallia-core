package gallia

import aptus.{Anything_, Seq_}

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

trait IdentityUUa extends ActionUU with AtomsUUa { def atomuus: AtomUUs = Nil }
trait IdentityZZa extends ActionZZ with AtomsZZa { def atomzzs: AtomZZs = Nil }
trait IdentityVVa extends ActionVV with AtomsVVa { def atomvvs: AtomVVs = Nil }

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
trait ActionUU extends /*Action with*/ ActionVM1 with AtomsUU
trait ActionZZ extends Action with ActionVM1 with AtomsZZ

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
  trait ActionVO extends Action with ActionV1 with IdentityM1 with AtomsVO

  // ---------------------------------------------------------------------------
  trait ActionVU extends Action with ActionV1 with ActionM1 with AtomsVU
  trait ActionVZ extends Action with ActionV1 with ActionM1 with AtomsVZ

  // ---------------------------------------------------------------------------
  trait ActionZzToZ extends Action with ActionV2 with ActionM2 with AtomsZzToZ { def vldt(c1: Cls , c2: Cls ): Errs = Nil } // TODO
  trait ActionZvToZ extends Action with ActionV2 with ActionM2 with AtomsZvToZ { def vldt(c1: Cls , c2: Cls ): Errs = Nil } // TODO
  trait ActionVvToV extends Action with ActionV2 with ActionM2 with AtomsVvToV { def vldt(c1: Cls , c2: Cls ): Errs = Nil } // TODO

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
  trait ActionVOd extends ActionVO with AtomsVOd
  trait ActionVOa extends ActionVO with AtomsVOa
  trait ActionVOc extends ActionVO with AtomsVOc
  trait ActionVOb extends ActionVO with AtomsVOb

  // ---------------------------------------------------------------------------
  trait ActionVUd extends ActionVU with AtomsVUd
  trait ActionVUa extends ActionVU with AtomsVUa
  trait ActionVUc extends ActionVU with AtomsVUc
  trait ActionVUb extends ActionVU with AtomsVUb

  // ---------------------------------------------------------------------------
  trait ActionVZd extends ActionVZ with AtomsVZd
  trait ActionVZa extends ActionVZ with AtomsVZa
  trait ActionVZc extends ActionVZ with AtomsVZc
  trait ActionVZb extends ActionVZ with AtomsVZb

  // ===========================================================================
              trait ActionUVc  extends ActionUV with AtomsUVc
  @deprecated trait ActionUVc2 extends ActionUVc with ActionV1 with ActionM1

              trait ActionZVc  extends ActionZV with AtomsZVc
  @deprecated trait ActionZVc2 extends ActionZV with AtomsZVc with ActionV1 with ActionM1

// ===========================================================================
// mostly boilerplate from this point on:

trait AtomsUU extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomuus(ctx)
            def atomuus(ctx: NodeMetaContext): AtomUUs }

    // ---------------------------------------------------------------------------
    trait AtomsZZ extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomzzs(ctx)
            def atomzzs(ctx: NodeMetaContext): AtomZZs }

  // ===========================================================================
  trait AtomsIU extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomius(ctx)
            def atomius(ctx: NodeMetaContext): AtomIUs }

    // ---------------------------------------------------------------------------
    trait AtomsIZ extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomizs(ctx)
            def atomizs(ctx: NodeMetaContext): AtomIZs }

    // ---------------------------------------------------------------------------
    trait AtomsIV extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomivs(ctx)
            def atomivs(ctx: NodeMetaContext): AtomIVs }

  // ===========================================================================
  trait AtomsUO extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomuos(ctx)
              def atomuos(ctx: NodeMetaContext): AtomUOs }

    // ---------------------------------------------------------------------------
    trait AtomsZO extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomzos(ctx)
              def atomzos(ctx: NodeMetaContext): AtomZOs }

    // ---------------------------------------------------------------------------
    trait AtomsVO extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomvos(ctx)
              def atomvos(ctx: NodeMetaContext): AtomVOs }

  // ===========================================================================
  trait AtomsVU extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomvus(ctx)
              def atomvus(ctx: NodeMetaContext): AtomVUs }

    // ---------------------------------------------------------------------------
    trait AtomsVZ extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomvzs(ctx)
              def atomvzs(ctx: NodeMetaContext): AtomVZs }

  // ===========================================================================
  trait AtomsUZ extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomuzs(ctx)
            def atomuzs(ctx: NodeMetaContext): AtomUZs }

    // ---------------------------------------------------------------------------
    trait AtomsZU extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomzus(ctx)
            def atomzus(ctx: NodeMetaContext): AtomZUs }

  // ===========================================================================
  trait AtomsUV extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomuvs(ctx)
            def atomuvs(ctx: NodeMetaContext): AtomUVs }

    // ---------------------------------------------------------------------------
    trait AtomsZV extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomzvs(ctx)
            def atomzvs(ctx: NodeMetaContext): AtomZVs }

    // ---------------------------------------------------------------------------
    trait AtomsVV extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomvvs(ctx)
            def atomvvs(ctx: NodeMetaContext): AtomVVs }

  // ===========================================================================
  trait AtomsZzToZ extends ActionAN {
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
          def dataz2(c1: Cls , c2: Cls)  : Atoms }

  // ---------------------------------------------------------------------------
  trait AtomsZvToZ extends ActionAN {
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
          def dataz2(c1: Cls , c2: Cls)  : Atoms }

  // ---------------------------------------------------------------------------
  trait AtomsVvToV extends ActionAN {
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
          def dataz2(c1: Cls , c2: Cls)  : Atoms }

// ===========================================================================
trait AtomsUUd extends AtomsUU {
    final def atomuus(ignored: NodeMetaContext): AtomUUs = atomuu.in.seq
          def atomuu                           : AtomUU }

  // ---------------------------------------------------------------------------
  trait AtomsUUa extends AtomsUU {
    final def atomuus(ignored: NodeMetaContext): AtomUUs = atomuus
          def atomuus                          : AtomUUs }

  // ---------------------------------------------------------------------------
  trait AtomsUUc extends AtomsUU {
    final def atomuus(ctx: NodeMetaContext): AtomUUs = atomuu(ctx.afferents.forceOne).in.seq
          def atomuu (afferent: Cls       ): AtomUU }

  // ---------------------------------------------------------------------------
  trait AtomsUUb extends AtomsUU {
    final def atomuus(ctx: NodeMetaContext): AtomUUs = atomuus(ctx.afferents.forceOne)
          def atomuus(afferent: Cls       ): AtomUUs }

  // ===========================================================================
  trait AtomsUUbb extends AtomsUU { // TODO: see t210616122449
    final def atomuus(ctx: NodeMetaContext)           : AtomUUs = atomuus(ctx.origin)(ctx.afferents.forceOne)
          def atomuus(origin: CallSite)(afferent: Cls): AtomUUs }

// ---------------------------------------------------------------------------
trait AtomsZZd extends AtomsZZ {
    final def atomzzs(ignored: NodeMetaContext): AtomZZs = atomzz.in.seq
          def atomzz                           : AtomZZ }

  // ---------------------------------------------------------------------------
  trait AtomsZZa extends AtomsZZ {
    final def atomzzs(ignored: NodeMetaContext): AtomZZs = atomzzs
          def atomzzs                          : AtomZZs }

  // ---------------------------------------------------------------------------
  trait AtomsZZc extends AtomsZZ {
    final def atomzzs(ctx: NodeMetaContext): AtomZZs = atomzz(ctx.afferents.forceOne).in.seq
          def atomzz (afferent: Cls       ): AtomZZ }

  // ---------------------------------------------------------------------------
  trait AtomsZZb extends AtomsZZ {
    final def atomzzs(ctx: NodeMetaContext): AtomZZs = atomzzs(ctx.afferents.forceOne)
          def atomzzs(afferent: Cls       ): AtomZZs }

// ---------------------------------------------------------------------------
trait AtomsVVa extends AtomsVV {
    final def atomvvs(ignored: NodeMetaContext): AtomVVs = atomvvs
          def atomvvs                          : AtomVVs }

// ===========================================================================
trait AtomsIUd extends AtomsIU {
      final def atomius(ignored: NodeMetaContext): AtomIUs = atomiu.in.seq
            def atomiu                           : AtomIU }

    // ---------------------------------------------------------------------------
    trait AtomsIUa extends AtomsIU {
      final def atomius(ignored: NodeMetaContext): AtomIUs = atomius
            def atomius                          : AtomIUs }

    // ---------------------------------------------------------------------------
    trait AtomsIUc extends AtomsIU {
      final def atomius(ctx: NodeMetaContext): AtomIUs = atomiu(ctx.afferents.forceOne).in.seq
            def atomiu (afferent: Cls       ): AtomIU }

    // ---------------------------------------------------------------------------
    trait AtomsIUb extends AtomsIU {
      final def atomius(ctx: NodeMetaContext): AtomIUs = atomius(ctx.afferents.forceOne)
            def atomius(afferent: Cls       ): AtomIUs }


  // ===========================================================================
  trait AtomsIZd extends AtomsIZ {
      final def atomizs(ignored: NodeMetaContext): AtomIZs = atomiz.in.seq
            def atomiz                           : AtomIZ }

    // ---------------------------------------------------------------------------
    trait AtomsIZa extends AtomsIZ {
      final def atomizs(ignored: NodeMetaContext): AtomIZs = atomizs
            def atomizs                          : AtomIZs }

    // ---------------------------------------------------------------------------
    trait AtomsIZc extends AtomsIZ {
      final def atomizs(ctx: NodeMetaContext): AtomIZs = atomiz(ctx.afferents.forceOne).in.seq
            def atomiz (afferent: Cls       ): AtomIZ }

    // ---------------------------------------------------------------------------
    trait AtomsIZb extends AtomsIZ {
      final def atomizs(ctx: NodeMetaContext): AtomIZs = atomizs(ctx.afferents.forceOne)
            def atomizs(afferent: Cls       ): AtomIZs }

  // ===========================================================================
  trait AtomsIVd extends AtomsIV {
      final def atomivs(ignored: NodeMetaContext): AtomIVs = atomiv.in.seq
            def atomiv                           : AtomIV }

    // ---------------------------------------------------------------------------
    trait AtomsIVa extends AtomsIV {
      final def atomivs(ignored: NodeMetaContext): AtomIVs = atomivs
            def atomivs                          : AtomIVs }

    // ---------------------------------------------------------------------------
    trait AtomsIVc extends AtomsIV {
      final def atomivs(ctx: NodeMetaContext): AtomIVs = atomiv(ctx.afferents.forceOne).in.seq
            def atomiv (afferent: Cls       ): AtomIV }

    // ---------------------------------------------------------------------------
    trait AtomsIVb extends AtomsIV {
      final def atomivs(ctx: NodeMetaContext): AtomIVs = atomivs(ctx.afferents.forceOne)
            def atomivs(afferent: Cls       ): AtomIVs }

// ===========================================================================
trait AtomsUOd extends AtomsUO {
      final def atomuos(ignored: NodeMetaContext): AtomUOs = atomuo.in.seq
            def atomuo                           : AtomUO }

    // ---------------------------------------------------------------------------
    trait AtomsUOa extends AtomsUO {
      final def atomuos(ignored: NodeMetaContext): AtomUOs = atomuos
            def atomuos                          : AtomUOs }

    // ---------------------------------------------------------------------------
    trait AtomsUOc extends AtomsUO {
      final def atomuos(ctx: NodeMetaContext): AtomUOs = atomuo(ctx.afferents.forceOne).in.seq
            def atomuo (afferent: Cls       ): AtomUO }

    // ---------------------------------------------------------------------------
    trait AtomsUOb extends AtomsUO {
      final def atomuos(ctx: NodeMetaContext): AtomUOs = atomuos(ctx.afferents.forceOne)
            def atomuos(afferent: Cls       ): AtomUOs }

  // ===========================================================================
  trait AtomsZOd extends AtomsZO {
      final def atomzos(ignored: NodeMetaContext): AtomZOs = atomzo.in.seq
            def atomzo                           : AtomZO }

    // ---------------------------------------------------------------------------
    trait AtomsZOa extends AtomsZO {
      final def atomzos(ignored: NodeMetaContext): AtomZOs = atomzos
            def atomzos                          : AtomZOs }

    // ---------------------------------------------------------------------------
    trait AtomsZOc extends AtomsZO {
      final def atomzos(ctx: NodeMetaContext): AtomZOs = atomzo(ctx.afferents.forceOne).in.seq
            def atomzo (afferent: Cls       ): AtomZO }

    // ---------------------------------------------------------------------------
    trait AtomsZOb extends AtomsZO {
      final def atomzos(ctx: NodeMetaContext): AtomZOs = atomzos(ctx.afferents.forceOne)
            def atomzos(afferent: Cls       ): AtomZOs }

  // ===========================================================================
  trait AtomsVOd extends AtomsVO {
      final def atomvos(ignored: NodeMetaContext): AtomVOs = atomvo.in.seq
            def atomvo                           : AtomVO }

    // ---------------------------------------------------------------------------
    trait AtomsVOa extends AtomsVO {
      final def atomvos(ignored: NodeMetaContext): AtomVOs = atomvos
            def atomvos                          : AtomVOs }

    // ---------------------------------------------------------------------------
    trait AtomsVOc extends AtomsVO {
      final def atomvos(ctx: NodeMetaContext): AtomVOs = atomvo(ctx.afferents.forceOne).in.seq
            def atomvo (afferent: Cls       ): AtomVO }

    // ---------------------------------------------------------------------------
    trait AtomsVOb extends AtomsVO {
      final def atomvos(ctx: NodeMetaContext): AtomVOs = atomvos(ctx.afferents.forceOne)
            def atomvos(afferent: Cls       ): AtomVOs }

  // ===========================================================================
  trait AtomsVUd extends AtomsVU {
      final def atomvus(ignored: NodeMetaContext): AtomVUs = atomvu.in.seq
            def atomvu                           : AtomVU }

    // ---------------------------------------------------------------------------
    trait AtomsVUa extends AtomsVU {
      final def atomvus(ignored: NodeMetaContext): AtomVUs = atomvus
            def atomvus                          : AtomVUs }

    // ---------------------------------------------------------------------------
    trait AtomsVUc extends AtomsVU {
      final def atomvus(ctx: NodeMetaContext): AtomVUs = atomvu(ctx.afferents.forceOne).in.seq
            def atomvu (afferent: Cls       ): AtomVU }

    // ---------------------------------------------------------------------------
    trait AtomsVUb extends AtomsVU {
      final def atomvus(ctx: NodeMetaContext): AtomVUs = atomvus(ctx.afferents.forceOne)
            def atomvus(afferent: Cls       ): AtomVUs }

  // ===========================================================================
  trait AtomsVZd extends AtomsVZ {
      final def atomvzs(ignored: NodeMetaContext): AtomVZs = atomvz.in.seq
            def atomvz                           : AtomVZ }

    // ---------------------------------------------------------------------------
    trait AtomsVZa extends AtomsVZ {
      final def atomvzs(ignored: NodeMetaContext): AtomVZs = atomvzs
            def atomvzs                          : AtomVZs }

    // ---------------------------------------------------------------------------
    trait AtomsVZc extends AtomsVZ {
      final def atomvzs(ctx: NodeMetaContext): AtomVZs = atomvz(ctx.afferents.forceOne).in.seq
            def atomvz (afferent: Cls       ): AtomVZ }

    // ---------------------------------------------------------------------------
    trait AtomsVZb extends AtomsVZ {
      final def atomvzs(ctx: NodeMetaContext): AtomVZs = atomvzs(ctx.afferents.forceOne)
            def atomvzs(afferent: Cls       ): AtomVZs }

// ===========================================================================
trait AtomsUZd extends AtomsUZ {
    final def atomuzs(ignored: NodeMetaContext): AtomUZs = atomuz.in.seq
          def atomuz                           : AtomUZ }

  // ---------------------------------------------------------------------------
  trait AtomsUZc extends AtomsUZ {
    final def atomuzs(ctx: NodeMetaContext): AtomUZs = atomuz(ctx.afferents.forceOne).in.seq
          def atomuz (afferent: Cls       ): AtomUZ }

  // ===========================================================================
  trait AtomsZUd extends AtomsZU {
    final def atomzus(ignored: NodeMetaContext): AtomZUs = atomzu.in.seq
          def atomzu                           : AtomZU }

  // ---------------------------------------------------------------------------
  trait AtomsZUc extends AtomsZU {
    final def atomzus(ctx: NodeMetaContext): AtomZUs = atomzu(ctx.afferents.forceOne).in.seq
          def atomzu (afferent: Cls       ): AtomZU }

  // ===========================================================================
  trait AtomsUVc extends AtomsUV {
    final def atomuvs(ctx: NodeMetaContext): AtomUVs = atomuv(ctx.afferents.forceOne).in.seq
          def atomuv (afferent: Cls       ): AtomUV }

  // ---------------------------------------------------------------------------
  trait AtomsZVc extends AtomsZV {
    final def atomzvs(ctx: NodeMetaContext): AtomZVs = atomzv(ctx.afferents.forceOne).in.seq
          def atomzv (afferent: Cls       ): AtomZV }

// ===========================================================================
import trgt.{TypedTargetQuery, TypedTargetQuery2, TypedTargetQuery3}

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
    val to: reflect.TypeNode
    def atom(c: Cls): Atom

    // ---------------------------------------------------------------------------
    final def _meta(ignored: Clss): Cls = Cls.Dummy //TODO?
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forceOne.pipe(atom).in.seq }

// ===========================================================================

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

trait IdentityUUa extends ActionUU with ActionAtomsUUa { def atomuus: AtomUUs = Nil }
trait IdentityZZa extends ActionZZ with ActionAtomsZZa { def atomzzs: AtomZZs = Nil }
trait IdentityVVa extends ActionVV with ActionAtomsVVa { def atomvvs: AtomVVs = Nil }

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
trait ActionUUa extends ActionUU with ActionAtomsUUa
trait ActionUUd extends ActionUU with ActionAtomsUUd
trait ActionUUc extends ActionUU with ActionAtomsUUc
trait ActionUUb extends ActionUU with ActionAtomsUUb

trait ActionUUbb extends ActionUU with ActionAtomsUUbb // provides origin, TODO: t210616122449 - generalize

  // ---------------------------------------------------------------------------
  trait ActionZZd extends ActionZZ with ActionAtomsZZd
  trait ActionZZa extends ActionZZ with ActionAtomsZZa
  trait ActionZZc extends ActionZZ with ActionAtomsZZc
  trait ActionZZb extends ActionZZ with ActionAtomsZZb

  // ---------------------------------------------------------------------------
  trait ActionUZc extends ActionUZ with ActionAtomsUZc
  trait ActionUZd extends ActionUZ with ActionAtomsUZd

  // ---------------------------------------------------------------------------
  trait ActionZUc extends ActionZU with ActionAtomsZUc
  trait ActionZUd extends ActionZU with ActionAtomsZUd

  // ---------------------------------------------------------------------------
  trait ActionIUd extends ActionIU with ActionAtomsIUd
  trait ActionIUa extends ActionIU with ActionAtomsIUa
  trait ActionIUc extends ActionIU with ActionAtomsIUc
  trait ActionIUb extends ActionIU with ActionAtomsIUb

  // ---------------------------------------------------------------------------
  trait ActionIZd extends ActionIZ with ActionAtomsIZd
  trait ActionIZa extends ActionIZ with ActionAtomsIZa
  trait ActionIZc extends ActionIZ with ActionAtomsIZc
  trait ActionIZb extends ActionIZ with ActionAtomsIZb

  // ---------------------------------------------------------------------------
  trait ActionIVd extends ActionIV with ActionAtomsIVd
  trait ActionIVa extends ActionIV with ActionAtomsIVa
  trait ActionIVc extends ActionIV with ActionAtomsIVc
  trait ActionIVb extends ActionIV with ActionAtomsIVb

  // ===========================================================================
  trait ActionUOd extends ActionUO with ActionAtomsUOd
  trait ActionUOa extends ActionUO with ActionAtomsUOa
  trait ActionUOc extends ActionUO with ActionAtomsUOc
  trait ActionUOb extends ActionUO with ActionAtomsUOb

  // ---------------------------------------------------------------------------
  trait ActionZOd extends ActionZO with ActionAtomsZOd
  trait ActionZOa extends ActionZO with ActionAtomsZOa
  trait ActionZOc extends ActionZO with ActionAtomsZOc
  trait ActionZOb extends ActionZO with ActionAtomsZOb

  // ---------------------------------------------------------------------------
  trait ActionVOd extends ActionVO with ActionAtomsVOd
  trait ActionVOa extends ActionVO with ActionAtomsVOa
  trait ActionVOc extends ActionVO with ActionAtomsVOc
  trait ActionVOb extends ActionVO with ActionAtomsVOb

  // ---------------------------------------------------------------------------
  trait ActionVUd extends ActionVU with ActionAtomsVUd
  trait ActionVUa extends ActionVU with ActionAtomsVUa
  trait ActionVUc extends ActionVU with ActionAtomsVUc
  trait ActionVUb extends ActionVU with ActionAtomsVUb

  // ---------------------------------------------------------------------------
  trait ActionVZd extends ActionVZ with ActionAtomsVZd
  trait ActionVZa extends ActionVZ with ActionAtomsVZa
  trait ActionVZc extends ActionVZ with ActionAtomsVZc
  trait ActionVZb extends ActionVZ with ActionAtomsVZb

  // ===========================================================================
              trait ActionUVc  extends ActionUV with ActionAtomsUVc
  @deprecated trait ActionUVc2 extends ActionUVc with ActionV1 with ActionM1

              trait ActionZVc  extends ActionZV with ActionAtomsZVc
  @deprecated trait ActionZVc2 extends ActionZV with ActionAtomsZVc with ActionV1 with ActionM1

// ===========================================================================
// mostly boilerplate from this point on:

trait ActionAtomsUU extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomuus(ctx)
            def atomuus(ctx: NodeMetaContext): AtomUUs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZZ extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomzzs(ctx)
            def atomzzs(ctx: NodeMetaContext): AtomZZs }

  // ===========================================================================
  trait ActionAtomsIU extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomius(ctx)
            def atomius(ctx: NodeMetaContext): AtomIUs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIZ extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomizs(ctx)
            def atomizs(ctx: NodeMetaContext): AtomIZs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIV extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomivs(ctx)
            def atomivs(ctx: NodeMetaContext): AtomIVs }

  // ===========================================================================
  trait ActionAtomsUO extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomuos(ctx)
              def atomuos(ctx: NodeMetaContext): AtomUOs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZO extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomzos(ctx)
              def atomzos(ctx: NodeMetaContext): AtomZOs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVO extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomvos(ctx)
              def atomvos(ctx: NodeMetaContext): AtomVOs }

  // ===========================================================================
  trait ActionAtomsVU extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomvus(ctx)
              def atomvus(ctx: NodeMetaContext): AtomVUs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVZ extends ActionAN {
        final def atoms  (ctx: NodeMetaContext): Atoms = atomvzs(ctx)
              def atomvzs(ctx: NodeMetaContext): AtomVZs }

  // ===========================================================================
  trait ActionAtomsUZ extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomuzs(ctx)
            def atomuzs(ctx: NodeMetaContext): AtomUZs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZU extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomzus(ctx)
            def atomzus(ctx: NodeMetaContext): AtomZUs }

  // ===========================================================================
  trait ActionAtomsUV extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomuvs(ctx)
            def atomuvs(ctx: NodeMetaContext): AtomUVs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZV extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomzvs(ctx)
            def atomzvs(ctx: NodeMetaContext): AtomZVs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVV extends ActionAN {
      final def atoms  (ctx: NodeMetaContext): Atoms = atomvvs(ctx)
            def atomvvs(ctx: NodeMetaContext): AtomVVs }

  // ===========================================================================
  trait ActionAtomsZzToZ extends ActionAN {
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
          def dataz2(c1: Cls , c2: Cls)  : Atoms }

  // ---------------------------------------------------------------------------
  trait ActionAtomsZvToZ extends ActionAN {
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
          def dataz2(c1: Cls , c2: Cls)  : Atoms }

  // ---------------------------------------------------------------------------
  trait ActionAtomsVvToV extends ActionAN {
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
          def dataz2(c1: Cls , c2: Cls)  : Atoms }

// ===========================================================================
trait ActionAtomsUUd extends ActionAtomsUU {
    final def atomuus(ignored: NodeMetaContext): AtomUUs = atomuu.in.seq
          def atomuu                           : AtomUU }

  // ---------------------------------------------------------------------------
  trait ActionAtomsUUa extends ActionAtomsUU {
    final def atomuus(ignored: NodeMetaContext): AtomUUs = atomuus
          def atomuus                          : AtomUUs }

  // ---------------------------------------------------------------------------
  trait ActionAtomsUUc extends ActionAtomsUU {
    final def atomuus(ctx: NodeMetaContext): AtomUUs = atomuu(ctx.afferents.forceOne).in.seq
          def atomuu (afferent: Cls       ): AtomUU }

  // ---------------------------------------------------------------------------
  trait ActionAtomsUUb extends ActionAtomsUU {
    final def atomuus(ctx: NodeMetaContext): AtomUUs = atomuus(ctx.afferents.forceOne)
          def atomuus(afferent: Cls       ): AtomUUs }

  // ===========================================================================
  trait ActionAtomsUUbb extends ActionAtomsUU { // TODO: see t210616122449
    final def atomuus(ctx: NodeMetaContext)           : AtomUUs = atomuus(ctx.origin)(ctx.afferents.forceOne)
          def atomuus(origin: CallSite)(afferent: Cls): AtomUUs }

// ---------------------------------------------------------------------------
trait ActionAtomsZZd extends ActionAtomsZZ {
    final def atomzzs(ignored: NodeMetaContext): AtomZZs = atomzz.in.seq
          def atomzz                           : AtomZZ }

  // ---------------------------------------------------------------------------
  trait ActionAtomsZZa extends ActionAtomsZZ {
    final def atomzzs(ignored: NodeMetaContext): AtomZZs = atomzzs
          def atomzzs                          : AtomZZs }

  // ---------------------------------------------------------------------------
  trait ActionAtomsZZc extends ActionAtomsZZ {
    final def atomzzs(ctx: NodeMetaContext): AtomZZs = atomzz(ctx.afferents.forceOne).in.seq
          def atomzz (afferent: Cls       ): AtomZZ }

  // ---------------------------------------------------------------------------
  trait ActionAtomsZZb extends ActionAtomsZZ {
    final def atomzzs(ctx: NodeMetaContext): AtomZZs = atomzzs(ctx.afferents.forceOne)
          def atomzzs(afferent: Cls       ): AtomZZs }

// ---------------------------------------------------------------------------
trait ActionAtomsVVa extends ActionAtomsVV {
    final def atomvvs(ignored: NodeMetaContext): AtomVVs = atomvvs
          def atomvvs                          : AtomVVs }

// ===========================================================================
trait ActionAtomsIUd extends ActionAtomsIU {
      final def atomius(ignored: NodeMetaContext): AtomIUs = atomiu.in.seq
            def atomiu                           : AtomIU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIUa extends ActionAtomsIU {
      final def atomius(ignored: NodeMetaContext): AtomIUs = atomius
            def atomius                          : AtomIUs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIUc extends ActionAtomsIU {
      final def atomius(ctx: NodeMetaContext): AtomIUs = atomiu(ctx.afferents.forceOne).in.seq
            def atomiu (afferent: Cls       ): AtomIU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIUb extends ActionAtomsIU {
      final def atomius(ctx: NodeMetaContext): AtomIUs = atomius(ctx.afferents.forceOne)
            def atomius(afferent: Cls       ): AtomIUs }


  // ===========================================================================
  trait ActionAtomsIZd extends ActionAtomsIZ {
      final def atomizs(ignored: NodeMetaContext): AtomIZs = atomiz.in.seq
            def atomiz                           : AtomIZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIZa extends ActionAtomsIZ {
      final def atomizs(ignored: NodeMetaContext): AtomIZs = atomizs
            def atomizs                          : AtomIZs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIZc extends ActionAtomsIZ {
      final def atomizs(ctx: NodeMetaContext): AtomIZs = atomiz(ctx.afferents.forceOne).in.seq
            def atomiz (afferent: Cls       ): AtomIZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIZb extends ActionAtomsIZ {
      final def atomizs(ctx: NodeMetaContext): AtomIZs = atomizs(ctx.afferents.forceOne)
            def atomizs(afferent: Cls       ): AtomIZs }

  // ===========================================================================
  trait ActionAtomsIVd extends ActionAtomsIV {
      final def atomivs(ignored: NodeMetaContext): AtomIVs = atomiv.in.seq
            def atomiv                           : AtomIV }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIVa extends ActionAtomsIV {
      final def atomivs(ignored: NodeMetaContext): AtomIVs = atomivs
            def atomivs                          : AtomIVs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIVc extends ActionAtomsIV {
      final def atomivs(ctx: NodeMetaContext): AtomIVs = atomiv(ctx.afferents.forceOne).in.seq
            def atomiv (afferent: Cls       ): AtomIV }

    // ---------------------------------------------------------------------------
    trait ActionAtomsIVb extends ActionAtomsIV {
      final def atomivs(ctx: NodeMetaContext): AtomIVs = atomivs(ctx.afferents.forceOne)
            def atomivs(afferent: Cls       ): AtomIVs }

// ===========================================================================
trait ActionAtomsUOd extends ActionAtomsUO {
      final def atomuos(ignored: NodeMetaContext): AtomUOs = atomuo.in.seq
            def atomuo                           : AtomUO }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUOa extends ActionAtomsUO {
      final def atomuos(ignored: NodeMetaContext): AtomUOs = atomuos
            def atomuos                          : AtomUOs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUOc extends ActionAtomsUO {
      final def atomuos(ctx: NodeMetaContext): AtomUOs = atomuo(ctx.afferents.forceOne).in.seq
            def atomuo (afferent: Cls       ): AtomUO }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUOb extends ActionAtomsUO {
      final def atomuos(ctx: NodeMetaContext): AtomUOs = atomuos(ctx.afferents.forceOne)
            def atomuos(afferent: Cls       ): AtomUOs }

  // ===========================================================================
  trait ActionAtomsZOd extends ActionAtomsZO {
      final def atomzos(ignored: NodeMetaContext): AtomZOs = atomzo.in.seq
            def atomzo                           : AtomZO }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZOa extends ActionAtomsZO {
      final def atomzos(ignored: NodeMetaContext): AtomZOs = atomzos
            def atomzos                          : AtomZOs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZOc extends ActionAtomsZO {
      final def atomzos(ctx: NodeMetaContext): AtomZOs = atomzo(ctx.afferents.forceOne).in.seq
            def atomzo (afferent: Cls       ): AtomZO }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZOb extends ActionAtomsZO {
      final def atomzos(ctx: NodeMetaContext): AtomZOs = atomzos(ctx.afferents.forceOne)
            def atomzos(afferent: Cls       ): AtomZOs }

  // ===========================================================================
  trait ActionAtomsVOd extends ActionAtomsVO {
      final def atomvos(ignored: NodeMetaContext): AtomVOs = atomvo.in.seq
            def atomvo                           : AtomVO }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVOa extends ActionAtomsVO {
      final def atomvos(ignored: NodeMetaContext): AtomVOs = atomvos
            def atomvos                          : AtomVOs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVOc extends ActionAtomsVO {
      final def atomvos(ctx: NodeMetaContext): AtomVOs = atomvo(ctx.afferents.forceOne).in.seq
            def atomvo (afferent: Cls       ): AtomVO }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVOb extends ActionAtomsVO {
      final def atomvos(ctx: NodeMetaContext): AtomVOs = atomvos(ctx.afferents.forceOne)
            def atomvos(afferent: Cls       ): AtomVOs }

  // ===========================================================================
  trait ActionAtomsVUd extends ActionAtomsVU {
      final def atomvus(ignored: NodeMetaContext): AtomVUs = atomvu.in.seq
            def atomvu                           : AtomVU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVUa extends ActionAtomsVU {
      final def atomvus(ignored: NodeMetaContext): AtomVUs = atomvus
            def atomvus                          : AtomVUs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVUc extends ActionAtomsVU {
      final def atomvus(ctx: NodeMetaContext): AtomVUs = atomvu(ctx.afferents.forceOne).in.seq
            def atomvu (afferent: Cls       ): AtomVU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVUb extends ActionAtomsVU {
      final def atomvus(ctx: NodeMetaContext): AtomVUs = atomvus(ctx.afferents.forceOne)
            def atomvus(afferent: Cls       ): AtomVUs }

  // ===========================================================================
  trait ActionAtomsVZd extends ActionAtomsVZ {
      final def atomvzs(ignored: NodeMetaContext): AtomVZs = atomvz.in.seq
            def atomvz                           : AtomVZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVZa extends ActionAtomsVZ {
      final def atomvzs(ignored: NodeMetaContext): AtomVZs = atomvzs
            def atomvzs                          : AtomVZs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVZc extends ActionAtomsVZ {
      final def atomvzs(ctx: NodeMetaContext): AtomVZs = atomvz(ctx.afferents.forceOne).in.seq
            def atomvz (afferent: Cls       ): AtomVZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVZb extends ActionAtomsVZ {
      final def atomvzs(ctx: NodeMetaContext): AtomVZs = atomvzs(ctx.afferents.forceOne)
            def atomvzs(afferent: Cls       ): AtomVZs }

// ===========================================================================
trait ActionAtomsUZd extends ActionAtomsUZ {
    final def atomuzs(ignored: NodeMetaContext): AtomUZs = atomuz.in.seq
          def atomuz                           : AtomUZ }

  // ---------------------------------------------------------------------------
  trait ActionAtomsUZc extends ActionAtomsUZ {
    final def atomuzs(ctx: NodeMetaContext): AtomUZs = atomuz(ctx.afferents.forceOne).in.seq
          def atomuz (afferent: Cls       ): AtomUZ }

  // ===========================================================================
  trait ActionAtomsZUd extends ActionAtomsZU {
    final def atomzus(ignored: NodeMetaContext): AtomZUs = atomzu.in.seq
          def atomzu                           : AtomZU }

  // ---------------------------------------------------------------------------
  trait ActionAtomsZUc extends ActionAtomsZU {
    final def atomzus(ctx: NodeMetaContext): AtomZUs = atomzu(ctx.afferents.forceOne).in.seq
          def atomzu (afferent: Cls       ): AtomZU }

  // ===========================================================================
  trait ActionAtomsUVc extends ActionAtomsUV {
    final def atomuvs(ctx: NodeMetaContext): AtomUVs = atomuv(ctx.afferents.forceOne).in.seq
          def atomuv (afferent: Cls       ): AtomUV }

  // ---------------------------------------------------------------------------
  trait ActionAtomsZVc extends ActionAtomsZV {
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

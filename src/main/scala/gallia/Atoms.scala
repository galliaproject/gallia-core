package gallia

import aptus.{Anything_, Seq_}

import gallia.actions._

// ===========================================================================
sealed trait Atom extends ActionAN {
    final def atoms(ignored: NodeMetaContext): Atoms = Seq(this)

    def formatDefault: String = s"ATOM: ${this.getClass.getSimpleName} - ${this.toString}"
  }

  // ===========================================================================
  case object NestingDataPlaceholder extends Atom // data to be provided by runner

  // ===========================================================================
  trait AtomIU extends Atom { def naive: Option[Obj ] }
  trait AtomIZ extends Atom { def naive: Option[Objs] }
  trait AtomIV extends Atom { def naive: Option[Vle ] }

  // ---------------------------------------------------------------------------
  trait AtomUO extends Atom { def naive(o: Obj ): Unit }
  trait AtomZO extends Atom { def naive(z: Objs): Unit }
  trait AtomVO extends Atom { def naive(v: Vle ): Unit } // TODO: used?

  // ---------------------------------------------------------------------------
  trait AtomUU extends Atom { def naive(o: Obj ): Obj  }
  trait AtomZZ extends Atom { def naive(z: Objs): Objs }

  // ---------------------------------------------------------------------------
  trait AtomZU extends Atom { def naive(z: Objs): Obj  }
  trait AtomUZ extends Atom { def naive(o: Obj ): Objs }

  // ---------------------------------------------------------------------------
  trait AtomUV extends Atom { def naive(o: Obj ): Any }
  trait AtomZV extends Atom { def naive(o: Objs): Any }
  trait AtomVV extends Atom { def naive(o: Any ): Any }

  // ---------------------------------------------------------------------------
  trait AtomUUtoU extends Atom { def naive(o1: Obj , o2: Obj ): Obj  }
  trait AtomZZtoZ extends Atom { def naive(z1: Objs, z2: Objs): Objs }

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
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.force.tuple2.thn((dataz2 _).tupled)
          def dataz2(c1: Cls , c2: Cls)  : Atoms }

// ===========================================================================
trait AtomsUUd extends AtomsUU {
    final def atomuus(ignored: NodeMetaContext): AtomUUs = atomuu.as.seq
          def atomuu                           : AtomUU }

  // ---------------------------------------------------------------------------
  trait AtomsUUa extends AtomsUU {
    final def atomuus(ignored: NodeMetaContext): AtomUUs = atomuus
          def atomuus                          : AtomUUs }

  // ---------------------------------------------------------------------------
  trait AtomsUUc extends AtomsUU {
    final def atomuus(ctx: NodeMetaContext): AtomUUs = atomuu(ctx.forceSingleAfferent).as.seq
          def atomuu (afferent: Cls       ): AtomUU }

  // ---------------------------------------------------------------------------
  trait AtomsUUb extends AtomsUU {
    final def atomuus(ctx: NodeMetaContext): AtomUUs = atomuus(ctx.forceSingleAfferent)
          def atomuus(afferent: Cls       ): AtomUUs }

// ---------------------------------------------------------------------------
trait AtomsZZd extends AtomsZZ {
    final def atomzzs(ignored: NodeMetaContext): AtomZZs = atomzz.as.seq
          def atomzz                           : AtomZZ }

  // ---------------------------------------------------------------------------
  trait AtomsZZa extends AtomsZZ {
    final def atomzzs(ignored: NodeMetaContext): AtomZZs = atomzzs
          def atomzzs                          : AtomZZs }

  // ---------------------------------------------------------------------------
  trait AtomsZZc extends AtomsZZ {
    final def atomzzs(ctx: NodeMetaContext): AtomZZs = atomzz(ctx.forceSingleAfferent).as.seq
          def atomzz (afferent: Cls       ): AtomZZ }

  // ---------------------------------------------------------------------------
  trait AtomsZZb extends AtomsZZ {
    final def atomzzs(ctx: NodeMetaContext): AtomZZs = atomzzs(ctx.forceSingleAfferent)
          def atomzzs(afferent: Cls       ): AtomZZs }

// ---------------------------------------------------------------------------
trait AtomsVVa extends AtomsVV {
    final def atomvvs(ignored: NodeMetaContext): AtomVVs = atomvvs
          def atomvvs                          : AtomVVs }

// ===========================================================================
trait AtomsIUd extends AtomsIU {
      final def atomius(ignored: NodeMetaContext): AtomIUs = atomiu.as.seq
            def atomiu                           : AtomIU }

    // ---------------------------------------------------------------------------
    trait AtomsIUa extends AtomsIU {
      final def atomius(ignored: NodeMetaContext): AtomIUs = atomius
            def atomius                          : AtomIUs }

    // ---------------------------------------------------------------------------
    trait AtomsIUc extends AtomsIU {
      final def atomius(ctx: NodeMetaContext): AtomIUs = atomiu(ctx.forceSingleAfferent).as.seq
            def atomiu (afferent: Cls       ): AtomIU }

    // ---------------------------------------------------------------------------
    trait AtomsIUb extends AtomsIU {
      final def atomius(ctx: NodeMetaContext): AtomIUs = atomius(ctx.forceSingleAfferent)
            def atomius(afferent: Cls       ): AtomIUs }


  // ===========================================================================
  trait AtomsIZd extends AtomsIZ {
      final def atomizs(ignored: NodeMetaContext): AtomIZs = atomiz.as.seq
            def atomiz                           : AtomIZ }

    // ---------------------------------------------------------------------------
    trait AtomsIZa extends AtomsIZ {
      final def atomizs(ignored: NodeMetaContext): AtomIZs = atomizs
            def atomizs                          : AtomIZs }

    // ---------------------------------------------------------------------------
    trait AtomsIZc extends AtomsIZ {
      final def atomizs(ctx: NodeMetaContext): AtomIZs = atomiz(ctx.forceSingleAfferent).as.seq
            def atomiz (afferent: Cls       ): AtomIZ }

    // ---------------------------------------------------------------------------
    trait AtomsIZb extends AtomsIZ {
      final def atomizs(ctx: NodeMetaContext): AtomIZs = atomizs(ctx.forceSingleAfferent)
            def atomizs(afferent: Cls       ): AtomIZs }

  // ===========================================================================
  trait AtomsIVd extends AtomsIV {
      final def atomivs(ignored: NodeMetaContext): AtomIVs = atomiv.as.seq
            def atomiv                           : AtomIV }

    // ---------------------------------------------------------------------------
    trait AtomsIVa extends AtomsIV {
      final def atomivs(ignored: NodeMetaContext): AtomIVs = atomivs
            def atomivs                          : AtomIVs }

    // ---------------------------------------------------------------------------
    trait AtomsIVc extends AtomsIV {
      final def atomivs(ctx: NodeMetaContext): AtomIVs = atomiv(ctx.forceSingleAfferent).as.seq
            def atomiv (afferent: Cls       ): AtomIV }

    // ---------------------------------------------------------------------------
    trait AtomsIVb extends AtomsIV {
      final def atomivs(ctx: NodeMetaContext): AtomIVs = atomivs(ctx.forceSingleAfferent)
            def atomivs(afferent: Cls       ): AtomIVs }

// ===========================================================================
trait AtomsUOd extends AtomsUO {
      final def atomuos(ignored: NodeMetaContext): AtomUOs = atomuo.as.seq
            def atomuo                           : AtomUO }

    // ---------------------------------------------------------------------------
    trait AtomsUOa extends AtomsUO {
      final def atomuos(ignored: NodeMetaContext): AtomUOs = atomuos
            def atomuos                          : AtomUOs }

    // ---------------------------------------------------------------------------
    trait AtomsUOc extends AtomsUO {
      final def atomuos(ctx: NodeMetaContext): AtomUOs = atomuo(ctx.forceSingleAfferent).as.seq
            def atomuo (afferent: Cls       ): AtomUO }

    // ---------------------------------------------------------------------------
    trait AtomsUOb extends AtomsUO {
      final def atomuos(ctx: NodeMetaContext): AtomUOs = atomuos(ctx.forceSingleAfferent)
            def atomuos(afferent: Cls       ): AtomUOs }

  // ===========================================================================
  trait AtomsZOd extends AtomsZO {
      final def atomzos(ignored: NodeMetaContext): AtomZOs = atomzo.as.seq
            def atomzo                           : AtomZO }

    // ---------------------------------------------------------------------------
    trait AtomsZOa extends AtomsZO {
      final def atomzos(ignored: NodeMetaContext): AtomZOs = atomzos
            def atomzos                          : AtomZOs }

    // ---------------------------------------------------------------------------
    trait AtomsZOc extends AtomsZO {
      final def atomzos(ctx: NodeMetaContext): AtomZOs = atomzo(ctx.forceSingleAfferent).as.seq
            def atomzo (afferent: Cls       ): AtomZO }

    // ---------------------------------------------------------------------------
    trait AtomsZOb extends AtomsZO {
      final def atomzos(ctx: NodeMetaContext): AtomZOs = atomzos(ctx.forceSingleAfferent)
            def atomzos(afferent: Cls       ): AtomZOs }

  // ===========================================================================
  trait AtomsVOd extends AtomsVO {
      final def atomvos(ignored: NodeMetaContext): AtomVOs = atomvo.as.seq
            def atomvo                           : AtomVO }

    // ---------------------------------------------------------------------------
    trait AtomsVOa extends AtomsVO {
      final def atomvos(ignored: NodeMetaContext): AtomVOs = atomvos
            def atomvos                          : AtomVOs }

    // ---------------------------------------------------------------------------
    trait AtomsVOc extends AtomsVO {
      final def atomvos(ctx: NodeMetaContext): AtomVOs = atomvo(ctx.forceSingleAfferent).as.seq
            def atomvo (afferent: Cls       ): AtomVO }

    // ---------------------------------------------------------------------------
    trait AtomsVOb extends AtomsVO {
      final def atomvos(ctx: NodeMetaContext): AtomVOs = atomvos(ctx.forceSingleAfferent)
            def atomvos(afferent: Cls       ): AtomVOs }

// ===========================================================================
trait AtomsUZd extends AtomsUZ {
    final def atomuzs(ignored: NodeMetaContext): AtomUZs = atomuz.as.seq
          def atomuz                           : AtomUZ }

  // ---------------------------------------------------------------------------
  trait AtomsUZc extends AtomsUZ {
    final def atomuzs(ctx: NodeMetaContext): AtomUZs = atomuz(ctx.forceSingleAfferent).as.seq
          def atomuz (afferent: Cls       ): AtomUZ }

  // ===========================================================================
  trait AtomsZUd extends AtomsZU {
    final def atomzus(ignored: NodeMetaContext): AtomZUs = atomzu.as.seq
          def atomzu                           : AtomZU }

  // ---------------------------------------------------------------------------
  trait AtomsZUc extends AtomsZU {
    final def atomzus(ctx: NodeMetaContext): AtomZUs = atomzu(ctx.forceSingleAfferent).as.seq
          def atomzu (afferent: Cls       ): AtomZU }

  // ===========================================================================
  trait AtomsUVc extends AtomsUV {
    final def atomuvs(ctx: NodeMetaContext): AtomUVs = atomuv(ctx.forceSingleAfferent).as.seq
          def atomuv (afferent: Cls       ): AtomUV }

  // ---------------------------------------------------------------------------
  trait AtomsZVc extends AtomsZV {
    final def atomzvs(ctx: NodeMetaContext): AtomZVs = atomzv(ctx.forceSingleAfferent).as.seq
          def atomzv (afferent: Cls       ): AtomZV }

// ===========================================================================

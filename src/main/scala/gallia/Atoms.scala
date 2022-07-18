package gallia

import aptus.{Anything_, Seq_}

import actions._

// ===========================================================================
sealed trait Atom extends ActionAN {
    final override def atoms(ignored: NodeMetaContext): Atoms = Seq(this)

    def className      : String = getClass.getSimpleName
    def formatDefault  : String = s"ATOM: ${className} - ${this.toString}"
    def formatSuccinct1: String = className
    
    // ===========================================================================
    // commonly used for optimizations

      // ---------------------------------------------------------------------------
      def isUWrapper        : Boolean = this.isInstanceOf[gallia.atoms                                 ._UWrapper]
      def isRename          : Boolean = this.isInstanceOf[gallia.atoms.common.AtomsCommonVeryBasics    ._Rename]
      def isRemoveWhateverIf: Boolean = this.isInstanceOf[gallia.atoms.common.AtomsCommonSomewhatBasics._RemoveWhateverIf]

      // ---------------------------------------------------------------------------
      def asUWrapper         = this.asInstanceOf[gallia.atoms                                 ._UWrapper]
      def asRename           = this.asInstanceOf[gallia.atoms.common.AtomsCommonVeryBasics    ._Rename]
      def asRemoveWhateverIf = this.asInstanceOf[gallia.atoms.common.AtomsCommonSomewhatBasics._RemoveWhateverIf]

      // ===========================================================================
      def isPlaceholder: Boolean = this == NestingDataPlaceholder
      def isIdentityUU : Boolean = this == gallia.atoms.AtomsOthers._IdentityUU    
      def isIdentityZZ : Boolean = this == gallia.atoms.AtomsOthers._IdentityZZ
  }

  // ===========================================================================
  trait AtomCombiner[T <: Atom] extends Atom // eg multiple renaming in sequence bundled as one

  // ---------------------------------------------------------------------------
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
  trait AtomUorZ[T] { def naive(t: T): T }
    trait AtomUU extends Atom with AtomUorZ[Obj]  { def naive(o: Obj ): Obj  }
    trait AtomZZ extends Atom with AtomUorZ[Objs] { def naive(z: Objs): Objs }

  // ---------------------------------------------------------------------------
  trait AtomZU extends Atom { def naive(z: Objs): Obj  }
  trait AtomUZ extends Atom { def naive(o: Obj ): Objs }

  // ---------------------------------------------------------------------------
  trait AtomUV extends Atom { def naive(o: Obj ): Vle }
  trait AtomZV extends Atom { def naive(o: Objs): Vle }
  trait AtomVV extends Atom { def naive(v: Vle ): Vle }

trait AtomVv2V extends Atom { def naive(v1: Vle, v2: Vle): Vle }

  // ---------------------------------------------------------------------------
  trait AtomUUtoU extends Atom { def naive(o1: Obj , o2: Obj ): Obj  }
  trait AtomZZtoZ extends Atom { def naive(z1: Objs, z2: Objs): Objs }
  trait AtomZVtoZ extends Atom { def naive(z : Objs, v : Vle ): Objs }

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
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.force.tuple2.pipe((dataz2 _).tupled)
          def dataz2(c1: Cls , c2: Cls)  : Atoms }

  // ---------------------------------------------------------------------------
  trait AtomsZvToZ extends ActionAN {
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.force.tuple2.pipe((dataz2 _).tupled)
          def dataz2(c1: Cls , c2: Cls)  : Atoms }

  // ---------------------------------------------------------------------------
  trait AtomsVvToV extends ActionAN {
    final def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.force.tuple2.pipe((dataz2 _).tupled)
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
    final def atomuus(ctx: NodeMetaContext): AtomUUs = atomuu(ctx.forceSingleAfferent).in.seq
          def atomuu (afferent: Cls       ): AtomUU }

  // ---------------------------------------------------------------------------
  trait AtomsUUb extends AtomsUU {
    final def atomuus(ctx: NodeMetaContext): AtomUUs = atomuus(ctx.forceSingleAfferent)
          def atomuus(afferent: Cls       ): AtomUUs }

  // ===========================================================================
  trait AtomsUUbb extends AtomsUU { // TODO: see t210616122449
    final def atomuus(ctx: NodeMetaContext)           : AtomUUs = atomuus(ctx.origin)(ctx.forceSingleAfferent)
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
    final def atomzzs(ctx: NodeMetaContext): AtomZZs = atomzz(ctx.forceSingleAfferent).in.seq
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
      final def atomius(ignored: NodeMetaContext): AtomIUs = atomiu.in.seq
            def atomiu                           : AtomIU }

    // ---------------------------------------------------------------------------
    trait AtomsIUa extends AtomsIU {
      final def atomius(ignored: NodeMetaContext): AtomIUs = atomius
            def atomius                          : AtomIUs }

    // ---------------------------------------------------------------------------
    trait AtomsIUc extends AtomsIU {
      final def atomius(ctx: NodeMetaContext): AtomIUs = atomiu(ctx.forceSingleAfferent).in.seq
            def atomiu (afferent: Cls       ): AtomIU }

    // ---------------------------------------------------------------------------
    trait AtomsIUb extends AtomsIU {
      final def atomius(ctx: NodeMetaContext): AtomIUs = atomius(ctx.forceSingleAfferent)
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
      final def atomizs(ctx: NodeMetaContext): AtomIZs = atomiz(ctx.forceSingleAfferent).in.seq
            def atomiz (afferent: Cls       ): AtomIZ }

    // ---------------------------------------------------------------------------
    trait AtomsIZb extends AtomsIZ {
      final def atomizs(ctx: NodeMetaContext): AtomIZs = atomizs(ctx.forceSingleAfferent)
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
      final def atomivs(ctx: NodeMetaContext): AtomIVs = atomiv(ctx.forceSingleAfferent).in.seq
            def atomiv (afferent: Cls       ): AtomIV }

    // ---------------------------------------------------------------------------
    trait AtomsIVb extends AtomsIV {
      final def atomivs(ctx: NodeMetaContext): AtomIVs = atomivs(ctx.forceSingleAfferent)
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
      final def atomuos(ctx: NodeMetaContext): AtomUOs = atomuo(ctx.forceSingleAfferent).in.seq
            def atomuo (afferent: Cls       ): AtomUO }

    // ---------------------------------------------------------------------------
    trait AtomsUOb extends AtomsUO {
      final def atomuos(ctx: NodeMetaContext): AtomUOs = atomuos(ctx.forceSingleAfferent)
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
      final def atomzos(ctx: NodeMetaContext): AtomZOs = atomzo(ctx.forceSingleAfferent).in.seq
            def atomzo (afferent: Cls       ): AtomZO }

    // ---------------------------------------------------------------------------
    trait AtomsZOb extends AtomsZO {
      final def atomzos(ctx: NodeMetaContext): AtomZOs = atomzos(ctx.forceSingleAfferent)
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
      final def atomvos(ctx: NodeMetaContext): AtomVOs = atomvo(ctx.forceSingleAfferent).in.seq
            def atomvo (afferent: Cls       ): AtomVO }

    // ---------------------------------------------------------------------------
    trait AtomsVOb extends AtomsVO {
      final def atomvos(ctx: NodeMetaContext): AtomVOs = atomvos(ctx.forceSingleAfferent)
            def atomvos(afferent: Cls       ): AtomVOs }

// ===========================================================================
trait AtomsUZd extends AtomsUZ {
    final def atomuzs(ignored: NodeMetaContext): AtomUZs = atomuz.in.seq
          def atomuz                           : AtomUZ }

  // ---------------------------------------------------------------------------
  trait AtomsUZc extends AtomsUZ {
    final def atomuzs(ctx: NodeMetaContext): AtomUZs = atomuz(ctx.forceSingleAfferent).in.seq
          def atomuz (afferent: Cls       ): AtomUZ }

  // ===========================================================================
  trait AtomsZUd extends AtomsZU {
    final def atomzus(ignored: NodeMetaContext): AtomZUs = atomzu.in.seq
          def atomzu                           : AtomZU }

  // ---------------------------------------------------------------------------
  trait AtomsZUc extends AtomsZU {
    final def atomzus(ctx: NodeMetaContext): AtomZUs = atomzu(ctx.forceSingleAfferent).in.seq
          def atomzu (afferent: Cls       ): AtomZU }

  // ===========================================================================
  trait AtomsUVc extends AtomsUV {
    final def atomuvs(ctx: NodeMetaContext): AtomUVs = atomuv(ctx.forceSingleAfferent).in.seq
          def atomuv (afferent: Cls       ): AtomUV }

  // ---------------------------------------------------------------------------
  trait AtomsZVc extends AtomsZV {
    final def atomzvs(ctx: NodeMetaContext): AtomZVs = atomzv(ctx.forceSingleAfferent).in.seq
          def atomzv (afferent: Cls       ): AtomZV }

// ===========================================================================

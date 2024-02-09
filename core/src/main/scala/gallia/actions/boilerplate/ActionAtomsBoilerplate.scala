package gallia
package actions
package boilerplate

import aptus.Anything_

// ===========================================================================
object ActionAtomsBoilerplate {

  trait ActionAtomsUU extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Atoms = atomuus(ctx)
                       def atomuus(ctx: NodeMetaContext): Seq[AtomUU] }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZZ extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Atoms = atomzzs(ctx)
                       def atomzzs(ctx: NodeMetaContext): AtomZZs }

    // ===========================================================================
    trait ActionAtomsIU extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Seq[Atom  ] = atomius(ctx)
                       def atomius(ctx: NodeMetaContext): Seq[AtomIU] }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIZ extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Seq[Atom  ] = atomizs(ctx)
                       def atomizs(ctx: NodeMetaContext): Seq[AtomIZ] }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIV extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Atoms = atomivs(ctx)
                       def atomivs(ctx: NodeMetaContext): AtomIVs }

    // ===========================================================================
    trait ActionAtomsUO extends ActionAN {
          final override def atoms  (ctx: NodeMetaContext): Atoms = atomuos(ctx)
                         def atomuos(ctx: NodeMetaContext): AtomUOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZO extends ActionAN {
          final override def atoms  (ctx: NodeMetaContext): Atoms = atomzos(ctx)
                         def atomzos(ctx: NodeMetaContext): AtomZOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVO extends ActionAN {
          final override def atoms  (ctx: NodeMetaContext): Atoms = atomvos(ctx)
                         def atomvos(ctx: NodeMetaContext): AtomVOs }

    // ===========================================================================
    trait ActionAtomsVU extends ActionAN {
          final override def atoms  (ctx: NodeMetaContext): Atoms = atomvus(ctx)
                         def atomvus(ctx: NodeMetaContext): AtomVUs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVZ extends ActionAN {
          final override def atoms  (ctx: NodeMetaContext): Atoms = atomvzs(ctx)
                         def atomvzs(ctx: NodeMetaContext): AtomVZs }

    // ===========================================================================
    trait ActionAtomsUZ extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Atoms = atomuzs(ctx)
                       def atomuzs(ctx: NodeMetaContext): AtomUZs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZU extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Atoms = atomzus(ctx)
                       def atomzus(ctx: NodeMetaContext): AtomZUs }

    // ===========================================================================
    trait ActionAtomsUV extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Atoms = atomuvs(ctx)
                       def atomuvs(ctx: NodeMetaContext): AtomUVs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZV extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Atoms = atomzvs(ctx)
                       def atomzvs(ctx: NodeMetaContext): AtomZVs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVV extends ActionAN {
        final override def atoms  (ctx: NodeMetaContext): Atoms = atomvvs(ctx)
                       def atomvvs(ctx: NodeMetaContext): AtomVVs }

    // ===========================================================================
    trait ActionAtomsZzToZ extends ActionAN {
      final override def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
                     def dataz2(c1: Cls , c2: Cls)  : Atoms }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZvToZ extends ActionAN {
      final override def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
                     def dataz2(c1: Cls , c2: Cls)  : Atoms }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVvToV extends ActionAN {
      final override def atoms(ctx: NodeMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
                     def dataz2(c1: Cls , c2: Cls)  : Atoms }

  // ===========================================================================
  trait ActionAtomsUU01 extends ActionAtomsUU {
      final override def atomuus(ignored: NodeMetaContext): AtomUUs = atomuu.in.seq
                     def atomuu                           : AtomUU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUU0N extends ActionAtomsUU {
      final override def atomuus(ignored: NodeMetaContext): AtomUUs = atomuus
                     def atomuus                          : AtomUUs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUU11 extends ActionAtomsUU {
      final override def atomuus(ctx: NodeMetaContext): AtomUUs = atomuu(ctx.afferents.forceOne).in.seq
                     def atomuu (afferent: Cls       ): AtomUU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUU1N extends ActionAtomsUU {
      final override def atomuus(ctx: NodeMetaContext): Seq[AtomUU] = atomuus(ctx.forceOneAfferent)
                     def atomuus(afferent: Cls)       : Seq[AtomUU] }

  // ===========================================================================
  trait ActionAtomsZZ01 extends ActionAtomsZZ {
      final override def atomzzs(ignored: NodeMetaContext): AtomZZs = atomzz.in.seq
                     def atomzz                           : AtomZZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZZ0N extends ActionAtomsZZ {
      final override def atomzzs(ignored: NodeMetaContext): AtomZZs = atomzzs
                     def atomzzs                          : AtomZZs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZZ11 extends ActionAtomsZZ {
      final override def atomzzs(ctx: NodeMetaContext): AtomZZs = atomzz(ctx.afferents.forceOne).in.seq
                     def atomzz (afferent: Cls       ): AtomZZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZZ1N extends ActionAtomsZZ {
      final override def atomzzs(ctx: NodeMetaContext): AtomZZs = atomzzs(ctx.afferents.forceOne)
                     def atomzzs(afferent: Cls       ): AtomZZs }

  // ===========================================================================
  trait ActionAtomsVV0N extends ActionAtomsVV {
      final override def atomvvs(ignored: NodeMetaContext): AtomVVs = atomvvs
                     def atomvvs                          : AtomVVs }

  // ===========================================================================
  trait ActionAtomsIU01 extends ActionAtomsIU {
        final override def atomius(ignored: NodeMetaContext): Seq[AtomIU] = atomiu.in.seq
                       def atomiu                           :     AtomIU }

  trait ActionAtomsIU01y extends ActionAtomsIU {
        final override def atomius(ctx: NodeMetaContext): Seq[AtomIU] = atomiuy(ctx.efferent).in.seq
                       def atomiuy(efferent: Cls)       :     AtomIU } // !! efferent, not afferent here

      // ---------------------------------------------------------------------------
      trait ActionAtomsIU0N extends ActionAtomsIU {
        final override def atomius(ignored: NodeMetaContext): AtomIUs = atomius
                       def atomius                          : AtomIUs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIU11 extends ActionAtomsIU {
        final override def atomius(ctx: NodeMetaContext): AtomIUs = atomiu(ctx.afferents.forceOne).in.seq
                       def atomiu (afferent: Cls       ): AtomIU }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIU1N extends ActionAtomsIU {
        final override def atomius(ctx: NodeMetaContext): AtomIUs = atomius(ctx.afferents.forceOne)
                       def atomius(afferent: Cls       ): AtomIUs }


    // ===========================================================================
    trait ActionAtomsIZ01 extends ActionAtomsIZ {
        final override def atomizs(ignored: NodeMetaContext): Seq[AtomIZ] = atomiz.in.seq
                       def atomiz                           :     AtomIZ }

    trait ActionAtomsIZ01y extends ActionAtomsIZ {
        final override def atomizs(ctx: NodeMetaContext): Seq[AtomIZ] = atomizy(ctx.efferent).in.seq
                       def atomizy(efferent: Cls)       :     AtomIZ } // !! efferent, not afferent here

      // ---------------------------------------------------------------------------
      trait ActionAtomsIZ0N extends ActionAtomsIZ {
        final override def atomizs(ignored: NodeMetaContext): AtomIZs = atomizs
                       def atomizs                          : AtomIZs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIZ11 extends ActionAtomsIZ {
        final override def atomizs(ctx: NodeMetaContext): AtomIZs = atomiz(ctx.afferents.forceOne).in.seq
                       def atomiz (afferent: Cls       ): AtomIZ }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIZ1N extends ActionAtomsIZ {
        final override def atomizs(ctx: NodeMetaContext): AtomIZs = atomizs(ctx.afferents.forceOne)
                       def atomizs(afferent: Cls       ): AtomIZs }

    // ===========================================================================
    trait ActionAtomsIV01 extends ActionAtomsIV {
        final override def atomivs(ignored: NodeMetaContext): AtomIVs = atomiv.in.seq
                       def atomiv                           : AtomIV }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIV0N extends ActionAtomsIV {
        final override def atomivs(ignored: NodeMetaContext): AtomIVs = atomivs
                       def atomivs                          : AtomIVs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIV11 extends ActionAtomsIV {
        final override def atomivs(ctx: NodeMetaContext): AtomIVs = atomiv(ctx.afferents.forceOne).in.seq
                       def atomiv (afferent: Cls       ): AtomIV }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIV1N extends ActionAtomsIV {
        final override def atomivs(ctx: NodeMetaContext): AtomIVs = atomivs(ctx.afferents.forceOne)
                       def atomivs(afferent: Cls       ): AtomIVs }

  // ===========================================================================
  trait ActionAtomsUO01 extends ActionAtomsUO {
        final override def atomuos(ignored: NodeMetaContext): AtomUOs = atomuo.in.seq
                       def atomuo                           : AtomUO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsUO0N extends ActionAtomsUO {
        final override def atomuos(ignored: NodeMetaContext): AtomUOs = atomuos
                       def atomuos                          : AtomUOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsUO11 extends ActionAtomsUO {
        final override def atomuos(ctx: NodeMetaContext): AtomUOs = atomuo(ctx.afferents.forceOne).in.seq
                       def atomuo (afferent: Cls       ): AtomUO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsUO1N extends ActionAtomsUO {
        final override def atomuos(ctx: NodeMetaContext): AtomUOs = atomuos(ctx.afferents.forceOne)
                       def atomuos(afferent: Cls       ): AtomUOs }

    // ===========================================================================
    trait ActionAtomsZO01 extends ActionAtomsZO {
        final override def atomzos(ignored: NodeMetaContext): AtomZOs = atomzo.in.seq
                       def atomzo                           : AtomZO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZO0N extends ActionAtomsZO {
        final override def atomzos(ignored: NodeMetaContext): AtomZOs = atomzos
                       def atomzos                          : AtomZOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZO11 extends ActionAtomsZO {
        final override def atomzos(ctx: NodeMetaContext): AtomZOs = atomzo(ctx.afferents.forceOne).in.seq
                       def atomzo (afferent: Cls       ): AtomZO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZO1N extends ActionAtomsZO {
        final override def atomzos(ctx: NodeMetaContext): AtomZOs = atomzos(ctx.afferents.forceOne)
                       def atomzos(afferent: Cls       ): AtomZOs }

    // ===========================================================================
    trait ActionAtomsVO01 extends ActionAtomsVO {
        final override def atomvos(ignored: NodeMetaContext): AtomVOs = atomvo.in.seq
                       def atomvo                           : AtomVO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVO0N extends ActionAtomsVO {
        final override def atomvos(ignored: NodeMetaContext): AtomVOs = atomvos
                       def atomvos                          : AtomVOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVO11 extends ActionAtomsVO {
        final override def atomvos(ctx: NodeMetaContext): AtomVOs = atomvo(ctx.afferents.forceOne).in.seq
                       def atomvo (afferent: Cls       ): AtomVO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVO1N extends ActionAtomsVO {
        final override def atomvos(ctx: NodeMetaContext): AtomVOs = atomvos(ctx.afferents.forceOne)
                       def atomvos(afferent: Cls       ): AtomVOs }

    // ===========================================================================
    trait ActionAtomsVU01 extends ActionAtomsVU {
        final override def atomvus(ignored: NodeMetaContext): AtomVUs = atomvu.in.seq
                       def atomvu                           : AtomVU }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVU0N extends ActionAtomsVU {
        final override def atomvus(ignored: NodeMetaContext): AtomVUs = atomvus
                       def atomvus                          : AtomVUs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVU11 extends ActionAtomsVU {
        final override def atomvus(ctx: NodeMetaContext): AtomVUs = atomvu(ctx.afferents.forceOne).in.seq
                       def atomvu (afferent: Cls       ): AtomVU }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVU1N extends ActionAtomsVU {
        final override def atomvus(ctx: NodeMetaContext): AtomVUs = atomvus(ctx.afferents.forceOne)
                       def atomvus(afferent: Cls       ): AtomVUs }

    // ===========================================================================
    trait ActionAtomsVZ01 extends ActionAtomsVZ {
        final override def atomvzs(ignored: NodeMetaContext): AtomVZs = atomvz.in.seq
                       def atomvz                           : AtomVZ }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVZ0N extends ActionAtomsVZ {
        final override def atomvzs(ignored: NodeMetaContext): AtomVZs = atomvzs
                       def atomvzs                          : AtomVZs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVZ11 extends ActionAtomsVZ {
        final override def atomvzs(ctx: NodeMetaContext): AtomVZs = atomvz(ctx.afferents.forceOne).in.seq
                       def atomvz (afferent: Cls       ): AtomVZ }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVZ1N extends ActionAtomsVZ {
        final override def atomvzs(ctx: NodeMetaContext): AtomVZs = atomvzs(ctx.afferents.forceOne)
                       def atomvzs(afferent: Cls       ): AtomVZs }

  // ===========================================================================
  trait ActionAtomsUZ01 extends ActionAtomsUZ {
      final override def atomuzs(ignored: NodeMetaContext): AtomUZs = atomuz.in.seq
                     def atomuz                           : AtomUZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUZ11 extends ActionAtomsUZ {
      final override def atomuzs(ctx: NodeMetaContext): AtomUZs = atomuz(ctx.afferents.forceOne).in.seq
                     def atomuz (afferent: Cls       ): AtomUZ }

    // ===========================================================================
    trait ActionAtomsZU01 extends ActionAtomsZU {
      final override def atomzus(ignored: NodeMetaContext): AtomZUs = atomzu.in.seq
                     def atomzu                           : AtomZU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZU11 extends ActionAtomsZU {
      final override def atomzus(ctx: NodeMetaContext): AtomZUs = atomzu(ctx.afferents.forceOne).in.seq
                     def atomzu (afferent: Cls       ): AtomZU }

    // ===========================================================================
    trait ActionAtomsUV11 extends ActionAtomsUV {
      final override def atomuvs(ctx: NodeMetaContext): AtomUVs = atomuv(ctx.afferents.forceOne).in.seq
                     def atomuv (afferent: Cls       ): AtomUV }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZV11 extends ActionAtomsZV {
      final override def atomzvs(ctx: NodeMetaContext): AtomZVs = atomzv(ctx.afferents.forceOne).in.seq
                     def atomzv (afferent: Cls       ): AtomZV }

}

// ===========================================================================

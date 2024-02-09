package gallia
package actions
package boilerplate

import aptus.Anything_

// ===========================================================================
object ActionAtomsBoilerplate {

  trait ActionAtomsUU extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Atoms = atomuus(ctx)
                       def atomuus(ctx: ActionMetaContext): Seq[AtomUU] }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZZ extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Atoms = atomzzs(ctx)
                       def atomzzs(ctx: ActionMetaContext): AtomZZs }

    // ===========================================================================
    trait ActionAtomsIU extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Seq[Atom  ] = atomius(ctx)
                       def atomius(ctx: ActionMetaContext): Seq[AtomIU] }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIZ extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Seq[Atom  ] = atomizs(ctx)
                       def atomizs(ctx: ActionMetaContext): Seq[AtomIZ] }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIV extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Atoms = atomivs(ctx)
                       def atomivs(ctx: ActionMetaContext): AtomIVs }

    // ===========================================================================
    trait ActionAtomsUO extends ActionAN {
          final override def atoms  (ctx: ActionMetaContext): Atoms = atomuos(ctx)
                         def atomuos(ctx: ActionMetaContext): AtomUOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZO extends ActionAN {
          final override def atoms  (ctx: ActionMetaContext): Atoms = atomzos(ctx)
                         def atomzos(ctx: ActionMetaContext): AtomZOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVO extends ActionAN {
          final override def atoms  (ctx: ActionMetaContext): Atoms = atomvos(ctx)
                         def atomvos(ctx: ActionMetaContext): AtomVOs }

    // ===========================================================================
    trait ActionAtomsVU extends ActionAN {
          final override def atoms  (ctx: ActionMetaContext): Atoms = atomvus(ctx)
                         def atomvus(ctx: ActionMetaContext): AtomVUs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVZ extends ActionAN {
          final override def atoms  (ctx: ActionMetaContext): Atoms = atomvzs(ctx)
                         def atomvzs(ctx: ActionMetaContext): AtomVZs }

    // ===========================================================================
    trait ActionAtomsUZ extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Atoms = atomuzs(ctx)
                       def atomuzs(ctx: ActionMetaContext): AtomUZs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZU extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Atoms = atomzus(ctx)
                       def atomzus(ctx: ActionMetaContext): AtomZUs }

    // ===========================================================================
    trait ActionAtomsUV extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Atoms = atomuvs(ctx)
                       def atomuvs(ctx: ActionMetaContext): AtomUVs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZV extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Atoms = atomzvs(ctx)
                       def atomzvs(ctx: ActionMetaContext): AtomZVs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVV extends ActionAN {
        final override def atoms  (ctx: ActionMetaContext): Atoms = atomvvs(ctx)
                       def atomvvs(ctx: ActionMetaContext): AtomVVs }

    // ===========================================================================
    trait ActionAtomsZzToZ extends ActionAN {
      final override def atoms(ctx: ActionMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
                     def dataz2(c1: Cls , c2: Cls)  : Atoms }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZvToZ extends ActionAN {
      final override def atoms(ctx: ActionMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
                     def dataz2(c1: Cls , c2: Cls)  : Atoms }

    // ---------------------------------------------------------------------------
    trait ActionAtomsVvToV extends ActionAN {
      final override def atoms(ctx: ActionMetaContext): Atoms = ctx.afferents.forcePair.pipe((dataz2 _).tupled)
                     def dataz2(c1: Cls , c2: Cls)  : Atoms }

  // ===========================================================================
  trait ActionAtomsUU01 extends ActionAtomsUU {
      final override def atomuus(ignored: ActionMetaContext): AtomUUs = atomuu.in.seq
                     def atomuu                           : AtomUU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUU0N extends ActionAtomsUU {
      final override def atomuus(ignored: ActionMetaContext): AtomUUs = atomuus
                     def atomuus                          : AtomUUs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUU11 extends ActionAtomsUU {
      final override def atomuus(ctx: ActionMetaContext): AtomUUs = atomuu(ctx.afferents.forceOne).in.seq
                     def atomuu (afferent: Cls       ): AtomUU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUU1N extends ActionAtomsUU {
      final override def atomuus(ctx: ActionMetaContext): Seq[AtomUU] = atomuus(ctx.forceOneAfferent)
                     def atomuus(afferent: Cls)       : Seq[AtomUU] }

  // ===========================================================================
  trait ActionAtomsZZ01 extends ActionAtomsZZ {
      final override def atomzzs(ignored: ActionMetaContext): AtomZZs = atomzz.in.seq
                     def atomzz                           : AtomZZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZZ0N extends ActionAtomsZZ {
      final override def atomzzs(ignored: ActionMetaContext): AtomZZs = atomzzs
                     def atomzzs                          : AtomZZs }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZZ11 extends ActionAtomsZZ {
      final override def atomzzs(ctx: ActionMetaContext): AtomZZs = atomzz(ctx.afferents.forceOne).in.seq
                     def atomzz (afferent: Cls       ): AtomZZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZZ1N extends ActionAtomsZZ {
      final override def atomzzs(ctx: ActionMetaContext): AtomZZs = atomzzs(ctx.afferents.forceOne)
                     def atomzzs(afferent: Cls       ): AtomZZs }

  // ===========================================================================
  trait ActionAtomsVV0N extends ActionAtomsVV {
      final override def atomvvs(ignored: ActionMetaContext): AtomVVs = atomvvs
                     def atomvvs                          : AtomVVs }

  // ===========================================================================
  trait ActionAtomsIU01 extends ActionAtomsIU {
        final override def atomius(ignored: ActionMetaContext): Seq[AtomIU] = atomiu.in.seq
                       def atomiu                           :     AtomIU }

  trait ActionAtomsIU011 extends ActionAtomsIU {
        final override def atomius(ctx: ActionMetaContext): Seq[AtomIU] = atomiuy(ctx.efferent).in.seq
                       def atomiuy(efferent: Cls)       :     AtomIU } // !! efferent, not afferent here

      // ---------------------------------------------------------------------------
      trait ActionAtomsIU0N extends ActionAtomsIU {
        final override def atomius(ignored: ActionMetaContext): AtomIUs = atomius
                       def atomius                          : AtomIUs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIU11 extends ActionAtomsIU {
        final override def atomius(ctx: ActionMetaContext): AtomIUs = atomiu(ctx.afferents.forceOne).in.seq
                       def atomiu (afferent: Cls       ): AtomIU }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIU1N extends ActionAtomsIU {
        final override def atomius(ctx: ActionMetaContext): AtomIUs = atomius(ctx.afferents.forceOne)
                       def atomius(afferent: Cls       ): AtomIUs }


    // ===========================================================================
    trait ActionAtomsIZ01 extends ActionAtomsIZ {
        final override def atomizs(ignored: ActionMetaContext): Seq[AtomIZ] = atomiz.in.seq
                       def atomiz                           :     AtomIZ }

    trait ActionAtomsIZ01y extends ActionAtomsIZ {
        final override def atomizs(ctx: ActionMetaContext): Seq[AtomIZ] = atomizy(ctx.efferent).in.seq
                       def atomizy(efferent: Cls)       :     AtomIZ } // !! efferent, not afferent here

      // ---------------------------------------------------------------------------
      trait ActionAtomsIZ0N extends ActionAtomsIZ {
        final override def atomizs(ignored: ActionMetaContext): AtomIZs = atomizs
                       def atomizs                          : AtomIZs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIZ11 extends ActionAtomsIZ {
        final override def atomizs(ctx: ActionMetaContext): AtomIZs = atomiz(ctx.afferents.forceOne).in.seq
                       def atomiz (afferent: Cls       ): AtomIZ }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIZ1N extends ActionAtomsIZ {
        final override def atomizs(ctx: ActionMetaContext): AtomIZs = atomizs(ctx.afferents.forceOne)
                       def atomizs(afferent: Cls       ): AtomIZs }

    // ===========================================================================
    trait ActionAtomsIV01 extends ActionAtomsIV {
        final override def atomivs(ignored: ActionMetaContext): AtomIVs = atomiv.in.seq
                       def atomiv                           : AtomIV }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIV0N extends ActionAtomsIV {
        final override def atomivs(ignored: ActionMetaContext): AtomIVs = atomivs
                       def atomivs                          : AtomIVs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIV11 extends ActionAtomsIV {
        final override def atomivs(ctx: ActionMetaContext): AtomIVs = atomiv(ctx.afferents.forceOne).in.seq
                       def atomiv (afferent: Cls       ): AtomIV }

      // ---------------------------------------------------------------------------
      trait ActionAtomsIV1N extends ActionAtomsIV {
        final override def atomivs(ctx: ActionMetaContext): AtomIVs = atomivs(ctx.afferents.forceOne)
                       def atomivs(afferent: Cls       ): AtomIVs }

  // ===========================================================================
  trait ActionAtomsUO01 extends ActionAtomsUO {
        final override def atomuos(ignored: ActionMetaContext): AtomUOs = atomuo.in.seq
                       def atomuo                           : AtomUO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsUO0N extends ActionAtomsUO {
        final override def atomuos(ignored: ActionMetaContext): AtomUOs = atomuos
                       def atomuos                          : AtomUOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsUO11 extends ActionAtomsUO {
        final override def atomuos(ctx: ActionMetaContext): AtomUOs = atomuo(ctx.afferents.forceOne).in.seq
                       def atomuo (afferent: Cls       ): AtomUO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsUO1N extends ActionAtomsUO {
        final override def atomuos(ctx: ActionMetaContext): AtomUOs = atomuos(ctx.afferents.forceOne)
                       def atomuos(afferent: Cls       ): AtomUOs }

    // ===========================================================================
    trait ActionAtomsZO01 extends ActionAtomsZO {
        final override def atomzos(ignored: ActionMetaContext): AtomZOs = atomzo.in.seq
                       def atomzo                           : AtomZO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZO0N extends ActionAtomsZO {
        final override def atomzos(ignored: ActionMetaContext): AtomZOs = atomzos
                       def atomzos                          : AtomZOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZO11 extends ActionAtomsZO {
        final override def atomzos(ctx: ActionMetaContext): AtomZOs = atomzo(ctx.afferents.forceOne).in.seq
                       def atomzo (afferent: Cls       ): AtomZO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsZO1N extends ActionAtomsZO {
        final override def atomzos(ctx: ActionMetaContext): AtomZOs = atomzos(ctx.afferents.forceOne)
                       def atomzos(afferent: Cls       ): AtomZOs }

    // ===========================================================================
    trait ActionAtomsVO01 extends ActionAtomsVO {
        final override def atomvos(ignored: ActionMetaContext): AtomVOs = atomvo.in.seq
                       def atomvo                           : AtomVO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVO0N extends ActionAtomsVO {
        final override def atomvos(ignored: ActionMetaContext): AtomVOs = atomvos
                       def atomvos                          : AtomVOs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVO11 extends ActionAtomsVO {
        final override def atomvos(ctx: ActionMetaContext): AtomVOs = atomvo(ctx.afferents.forceOne).in.seq
                       def atomvo (afferent: Cls       ): AtomVO }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVO1N extends ActionAtomsVO {
        final override def atomvos(ctx: ActionMetaContext): AtomVOs = atomvos(ctx.afferents.forceOne)
                       def atomvos(afferent: Cls       ): AtomVOs }

    // ===========================================================================
    trait ActionAtomsVU01 extends ActionAtomsVU {
        final override def atomvus(ignored: ActionMetaContext): AtomVUs = atomvu.in.seq
                       def atomvu                           : AtomVU }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVU0N extends ActionAtomsVU {
        final override def atomvus(ignored: ActionMetaContext): AtomVUs = atomvus
                       def atomvus                          : AtomVUs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVU11 extends ActionAtomsVU {
        final override def atomvus(ctx: ActionMetaContext): AtomVUs = atomvu(ctx.afferents.forceOne).in.seq
                       def atomvu (afferent: Cls       ): AtomVU }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVU1N extends ActionAtomsVU {
        final override def atomvus(ctx: ActionMetaContext): AtomVUs = atomvus(ctx.afferents.forceOne)
                       def atomvus(afferent: Cls       ): AtomVUs }

    // ===========================================================================
    trait ActionAtomsVZ01 extends ActionAtomsVZ {
        final override def atomvzs(ignored: ActionMetaContext): AtomVZs = atomvz.in.seq
                       def atomvz                           : AtomVZ }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVZ0N extends ActionAtomsVZ {
        final override def atomvzs(ignored: ActionMetaContext): AtomVZs = atomvzs
                       def atomvzs                          : AtomVZs }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVZ11 extends ActionAtomsVZ {
        final override def atomvzs(ctx: ActionMetaContext): AtomVZs = atomvz(ctx.afferents.forceOne).in.seq
                       def atomvz (afferent: Cls       ): AtomVZ }

      // ---------------------------------------------------------------------------
      trait ActionAtomsVZ1N extends ActionAtomsVZ {
        final override def atomvzs(ctx: ActionMetaContext): AtomVZs = atomvzs(ctx.afferents.forceOne)
                       def atomvzs(afferent: Cls       ): AtomVZs }

  // ===========================================================================
  trait ActionAtomsUZ01 extends ActionAtomsUZ {
      final override def atomuzs(ignored: ActionMetaContext): AtomUZs = atomuz.in.seq
                     def atomuz                           : AtomUZ }

    // ---------------------------------------------------------------------------
    trait ActionAtomsUZ11 extends ActionAtomsUZ {
      final override def atomuzs(ctx: ActionMetaContext): AtomUZs = atomuz(ctx.afferents.forceOne).in.seq
                     def atomuz (afferent: Cls       ): AtomUZ }

    // ===========================================================================
    trait ActionAtomsZU01 extends ActionAtomsZU {
      final override def atomzus(ignored: ActionMetaContext): AtomZUs = atomzu.in.seq
                     def atomzu                           : AtomZU }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZU11 extends ActionAtomsZU {
      final override def atomzus(ctx: ActionMetaContext): AtomZUs = atomzu(ctx.afferents.forceOne).in.seq
                     def atomzu (afferent: Cls       ): AtomZU }

    // ===========================================================================
    trait ActionAtomsUV11 extends ActionAtomsUV {
      final override def atomuvs(ctx: ActionMetaContext): AtomUVs = atomuv(ctx.afferents.forceOne).in.seq
                     def atomuv (afferent: Cls       ): AtomUV }

    // ---------------------------------------------------------------------------
    trait ActionAtomsZV11 extends ActionAtomsZV {
      final override def atomzvs(ctx: ActionMetaContext): AtomZVs = atomzv(ctx.afferents.forceOne).in.seq
                     def atomzv (afferent: Cls       ): AtomZV }

}

// ===========================================================================

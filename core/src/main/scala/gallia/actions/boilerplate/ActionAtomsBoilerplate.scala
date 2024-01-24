package gallia
package actions
package boilerplate

import aptus.Anything_

// ===========================================================================
object ActionAtomsBoilerplate {

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

}

// ===========================================================================
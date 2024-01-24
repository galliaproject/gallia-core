package gallia
package actions
package boilerplate

// ===========================================================================
object ActionBoilerplate extends ActionBoilerplate

// ---------------------------------------------------------------------------
trait ActionBoilerplate { import ActionAtomsBoilerplate._

  trait IdentityUUa extends ActionUU with ActionAtomsUUa { def atomuus: AtomUUs = Nil }
  trait IdentityZZa extends ActionZZ with ActionAtomsZZa { def atomzzs: AtomZZs = Nil }
  trait IdentityVVa extends ActionVV with ActionAtomsVVa { def atomvvs: AtomVVs = Nil }

  // ---------------------------------------------------------------------------
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
    @deprecated trait ActionZVc2 extends ActionZV with ActionAtomsZVc with ActionV1 with ActionM1 }

// ===========================================================================
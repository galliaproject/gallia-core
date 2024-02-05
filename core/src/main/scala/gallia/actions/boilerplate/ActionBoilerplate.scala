package gallia
package actions
package boilerplate

// ===========================================================================
object ActionBoilerplate extends ActionBoilerplate

// ---------------------------------------------------------------------------
trait ActionBoilerplate { import ActionAtomsBoilerplate._

  trait IdentityUU0N extends ActionUU with ActionAtomsUU0N { def atomuus: AtomUUs = Nil }
  trait IdentityZZ0N extends ActionZZ with ActionAtomsZZ0N { def atomzzs: AtomZZs = Nil }
  trait IdentityVV0N extends ActionVV with ActionAtomsVV0N { def atomvvs: AtomVVs = Nil }

  // ---------------------------------------------------------------------------
  trait ActionUU0N extends ActionUU with ActionAtomsUU0N
  trait ActionUU01 extends ActionUU with ActionAtomsUU01
  trait ActionUU11 extends ActionUU with ActionAtomsUU11
  trait ActionUU1N extends ActionUU with ActionAtomsUU1N

  // ---------------------------------------------------------------------------
  @oswo_ trait ActionUU1Noswo extends ActionUU1N {
    private var _intras: IntraActionClss = null // temporary hack for OSWO prototype

    // ---------------------------------------------------------------------------
    protected def storeIntraMetas(values: Seq[Cls]): Cls = { _intras = IntraActionClss(values); _intras.values.last }
    protected def updateAtomMetas[T <: AtomOswo](atoms: Seq[T]) = { atoms.updateAtomMetas(_intras); atoms } }

    // ---------------------------------------------------------------------------
    trait ActionZZ01 extends ActionZZ with ActionAtomsZZ01
    trait ActionZZ0N extends ActionZZ with ActionAtomsZZ0N
    trait ActionZZ11 extends ActionZZ with ActionAtomsZZ11
    trait ActionZZ1N extends ActionZZ with ActionAtomsZZ1N

    // ---------------------------------------------------------------------------
    trait ActionUZ11 extends ActionUZ with ActionAtomsUZ11
    trait ActionUZ01 extends ActionUZ with ActionAtomsUZ01

    // ---------------------------------------------------------------------------
    trait ActionZU11 extends ActionZU with ActionAtomsZU11
    trait ActionZU01 extends ActionZU with ActionAtomsZU01

    // ---------------------------------------------------------------------------
    trait ActionIU01 extends ActionIU with ActionAtomsIU01
    trait ActionIU0N extends ActionIU with ActionAtomsIU0N
    trait ActionIU11 extends ActionIU with ActionAtomsIU11
    trait ActionIU1N extends ActionIU with ActionAtomsIU1N

      trait ActionIU01y extends ActionIU with ActionAtomsIU01y

    // ---------------------------------------------------------------------------
    trait ActionIZ01 extends ActionIZ with ActionAtomsIZ01
    trait ActionIZ0N extends ActionIZ with ActionAtomsIZ0N
    trait ActionIZ11 extends ActionIZ with ActionAtomsIZ11
    trait ActionIZ1N extends ActionIZ with ActionAtomsIZ1N

      trait ActionIZ01y extends ActionIZ with ActionAtomsIZ01y

    // ---------------------------------------------------------------------------
    trait ActionIV01 extends ActionIV with ActionAtomsIV01
    trait ActionIV0N extends ActionIV with ActionAtomsIV0N
    trait ActionIV11 extends ActionIV with ActionAtomsIV11
    trait ActionIV1N extends ActionIV with ActionAtomsIV1N

    // ===========================================================================
    trait ActionUO01 extends ActionUO with ActionAtomsUO01
    trait ActionUO0N extends ActionUO with ActionAtomsUO0N
    trait ActionUO11 extends ActionUO with ActionAtomsUO11
    trait ActionUO1N extends ActionUO with ActionAtomsUO1N

    // ---------------------------------------------------------------------------
    trait ActionZO01 extends ActionZO with ActionAtomsZO01
    trait ActionZO0N extends ActionZO with ActionAtomsZO0N
    trait ActionZO11 extends ActionZO with ActionAtomsZO11
    trait ActionZO1N extends ActionZO with ActionAtomsZO1N

    // ---------------------------------------------------------------------------
    trait ActionVO01 extends ActionVO with ActionAtomsVO01
    trait ActionVO0N extends ActionVO with ActionAtomsVO0N
    trait ActionVO11 extends ActionVO with ActionAtomsVO11
    trait ActionVO1N extends ActionVO with ActionAtomsVO1N

    // ---------------------------------------------------------------------------
    trait ActionVU01 extends ActionVU with ActionAtomsVU01
    trait ActionVU0N extends ActionVU with ActionAtomsVU0N
    trait ActionVU11 extends ActionVU with ActionAtomsVU11
    trait ActionVU1N extends ActionVU with ActionAtomsVU1N

    // ---------------------------------------------------------------------------
    trait ActionVZ01 extends ActionVZ with ActionAtomsVZ01
    trait ActionVZ0N extends ActionVZ with ActionAtomsVZ0N
    trait ActionVZ11 extends ActionVZ with ActionAtomsVZ11
    trait ActionVZ1N extends ActionVZ with ActionAtomsVZ1N

    // ===========================================================================
                trait ActionUV11  extends ActionUV   with ActionAtomsUV11
    @deprecated trait ActionUV11b extends ActionUV11 with ActionV1 with ActionM1

                trait ActionZV11  extends ActionZV with ActionAtomsZV11
    @deprecated trait ActionZV11b extends ActionZV with ActionAtomsZV11 with ActionV1 with ActionM1 }

// ===========================================================================

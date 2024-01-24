package gallia
package run

// ===========================================================================
private[gallia] class GenericRunnerO(writing: AObj => Unit) {

    def run(head: HeadO): Unit =
        head.runGeneric(GenericRunnerOutputU)

      // ---------------------------------------------------------------------------
      object GenericRunnerOutputU extends actions.boilerplate.ActionBoilerplate.ActionUOc with IdentityVM1 {
          def atomuo(c: Cls) = _GenericRunnerOutputU(c) }

        // ---------------------------------------------------------------------------
        case class _GenericRunnerOutputU(c: Cls) extends AtomUO {
          def naive(o: Obj) = writing(AObj(c, o)) }
  }

  // ===========================================================================
  private[gallia] class GenericRunnerZ(writing: AObjs => Unit) {

    def run(head: HeadS): Unit =
        head.runGeneric(GenericRunnerOutputZ)

      // ---------------------------------------------------------------------------
      object GenericRunnerOutputZ extends actions.boilerplate.ActionBoilerplate.ActionZOc with IdentityVM1 {
          def atomzo(c: Cls) = _GenericRunnerOutputZ(c) }

        // ---------------------------------------------------------------------------
        case class _GenericRunnerOutputZ(c: Cls) extends AtomZO {
          def naive(z: Objs) = writing(AObjs(c, z)) }

  }

// ===========================================================================

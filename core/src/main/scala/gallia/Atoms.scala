package gallia

// ===========================================================================
sealed trait Atom {
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

      def isIdentity   : Boolean = this.isInstanceOf[gallia.atoms.AtomsOthers._Identity]
      def isIdentityUU : Boolean = this == gallia.atoms.AtomsOthers._IdentityUU    
      def isIdentityZZ : Boolean = this == gallia.atoms.AtomsOthers._IdentityZZ }

  // ===========================================================================
  trait AtomCombiner[T <: Atom] extends Atom // eg multiple renaming in sequence bundled as one

  // ---------------------------------------------------------------------------
  case object NestingDataPlaceholder extends ActionAN1 with Atom { def atom = this } // data to be provided by runner

  // ===========================================================================
  trait AtomIU extends Atom { def naive: Option[Obj ] }
  trait AtomIZ extends Atom { def naive: Option[Objs] }
  trait AtomIV extends Atom { def naive: Option[Vle ] }

  // ---------------------------------------------------------------------------
  trait AtomUO extends Atom { def naive(o: Obj ): Unit }
  trait AtomZO extends Atom { def naive(z: Objs): Unit }
  trait AtomVO extends Atom { def naive(v: Vle ): Unit } // TODO: used?

  // ---------------------------------------------------------------------------
  trait AtomVU extends Atom { def naive(v: Vle): Obj  }
  trait AtomVZ extends Atom { def naive(v: Vle): Objs }

  // ---------------------------------------------------------------------------
  @oswo_
  trait AtomUU extends Atom with AtomOswo { def naive(o: Obj ): Obj  }
  trait AtomZZ extends Atom               { def naive(z: Objs): Objs }

  // ---------------------------------------------------------------------------
  trait AtomZU extends Atom { def naive(z: Objs): Obj  }
  trait AtomUZ extends Atom { def naive(o: Obj ): Objs }

  // ---------------------------------------------------------------------------
  trait AtomUV extends Atom { def naive(o: Obj ): Vle }
  trait AtomZV extends Atom { def naive(o: Objs): Vle }
  trait AtomVV extends Atom { def naive(v: Vle ): Vle }

  // ---------------------------------------------------------------------------
  trait AtomVv2V extends Atom { def naive(v1: Vle, v2: Vle): Vle }
  // TODO: t240124135432 - more combinations

  // ---------------------------------------------------------------------------
  trait AtomUUtoU extends Atom { def naive(o1: Obj , o2: Obj ): Obj  }
  trait AtomZZtoZ extends Atom { def naive(z1: Objs, z2: Objs): Objs }
  trait AtomZVtoZ extends Atom { def naive(z : Objs, v : Vle ): Objs }

// ---------------------------------------------------------------------------
@oswo_ trait AtomOswo { var _metaIO: plans.ClsIO = null /* temporary hack for OSWO prototype */ }

// ===========================================================================

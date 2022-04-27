package gallia

// ===========================================================================
package object actions { // TODO: delegate access to all of AtomsUtils._
  def  classTag[T : ClassTag] = scala.reflect.classTag[T]
  type ClassTag[T]            = scala.reflect.ClassTag[T]

  // ---------------------------------------------------------------------------
  type TypeNode  = reflect.TypeNode

  type BasicType = reflect.BasicType
  val  BasicType = reflect.BasicType

  type Container = reflect.Container
  val  Container = reflect.Container

  // ---------------------------------------------------------------------------
  type Info = meta.Info
  val  Info = meta.Info

  type Ofni = meta.Ofni
  val  Ofni = meta.Ofni

  val  Cls  = meta.Cls

  // ---------------------------------------------------------------------------
  type StringSplitter = domain.StringSplitter

  // ---------------------------------------------------------------------------
  @deprecated
  type WV     = heads.WV

  // ---------------------------------------------------------------------------
  val  _vldt  = vldt.MetaValidation

  type SpecialCardiMode = vldt.SpecialCardiMode
  val  SpecialCardiMode = vldt.SpecialCardiMode

  type _Error = vldt._Error
  val  _Error = vldt._Error

  // ---------------------------------------------------------------------------
  type Atoms   = Seq[Atom]

    // ---------------------------------------------------------------------------
    type AtomIUs = Seq[AtomIU]
    type AtomIZs = Seq[AtomIZ]
    type AtomIVs = Seq[AtomIV]

    // ---------------------------------------------------------------------------
    type AtomUUs = Seq[AtomUU]
    type AtomZZs = Seq[AtomZZ]
    type AtomVVs = Seq[AtomVV]

    // ---------------------------------------------------------------------------
    type AtomUVs = Seq[AtomUV]
    type AtomZVs = Seq[AtomZV]

    type AtomUZs = Seq[AtomUZ]
    type AtomZUs = Seq[AtomZU]

    // ---------------------------------------------------------------------------
    type AtomUOs = Seq[AtomUO]
    type AtomZOs = Seq[AtomZO]
    type AtomVOs = Seq[AtomVO]

  // ---------------------------------------------------------------------------
  type _Nested = atoms.AtomsOthers._Nested
  val  _Nested = atoms.AtomsOthers._Nested

  type _Rename = atoms.AtomsUUVeryBasics._Rename
  val  _Rename = atoms.AtomsUUVeryBasics._Rename

  type _TransformVV = atoms.AtomsUUTransforms._TransformVV
  val  _TransformVV = atoms.AtomsUUTransforms._TransformVV
  
  // ===========================================================================
  private[actions] implicit def _atomUUToAtomUUs(value: AtomUU): Seq[AtomUU] = Seq(value)

  // ---------------------------------------------------------------------------
  implicit class Seq__[A](values: Seq[A]) {
    def orIfEmpty(generator: => Seq[A]): Seq[A] = if (values.isEmpty) generator else values // convenient for validations
  }
  
  // ---------------------------------------------------------------------------
  implicit class KPath__(value: KPath) {
    def vldtAsNewDestination(c: Cls) = _vldt.fieldAbsence(c, value)   
  }

  // ---------------------------------------------------------------------------
  implicit class TKPath__(value: TKPath) {
    def vldtAsNewDestination(c: Cls) = value.path.vldtAsNewDestination(c) ++ _vldt.validType(value.tipe)
  }

}

// ===========================================================================


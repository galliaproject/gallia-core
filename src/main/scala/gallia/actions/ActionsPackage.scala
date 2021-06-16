package gallia

// ===========================================================================
package object actions { // TODO: delegate access to all of AtomsUtils._
  def  classTag[T : ClassTag] = scala.reflect.classTag[T]
  type ClassTag[T]            = scala.reflect.ClassTag[T]

  // ---------------------------------------------------------------------------
  type TypeNode  = gallia.reflect.TypeNode

  type BasicType = gallia.reflect.BasicType
  val  BasicType = gallia.reflect.BasicType

  type Container = gallia.reflect.Container
  val  Container = gallia.reflect.Container

  // ---------------------------------------------------------------------------
  type Info = gallia.meta.Info
  val  Info = gallia.meta.Info

  val  Cls  = gallia.meta.Cls

  // ---------------------------------------------------------------------------
  type StringSplitter = gallia.domain.StringSplitter

  // ---------------------------------------------------------------------------
  type WV     = gallia.heads.WV
  type WV1    = gallia.heads.WV
  type WV2[T] = gallia.heads.WV2[T]

  // ---------------------------------------------------------------------------
  val  _vldt  = gallia.vldt.MetaValidation

  type SpecialCardiMode = gallia.vldt.SpecialCardiMode
  val  SpecialCardiMode = gallia.vldt.SpecialCardiMode

  type _Error = gallia.vldt._Error
  val  _Error = gallia.vldt._Error

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
  type _Nested = gallia.atoms.AtomsOthers._Nested
  val  _Nested = gallia.atoms.AtomsOthers._Nested

  type _Rename = gallia.atoms.AtomsUUVeryBasics._Rename
  val  _Rename = gallia.atoms.AtomsUUVeryBasics._Rename

  type _TransformVV = gallia.atoms.AtomsUUTransforms._TransformVV
  val  _TransformVV = gallia.atoms.AtomsUUTransforms._TransformVV
}

// ===========================================================================


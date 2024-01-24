package gallia

// ===========================================================================
package object actions extends boilerplate.ActionBoilerplate { // TODO: delegate access to all of AtomsUtils._
  private[gallia] val dense = atoms.obg9.Obg9Utils.hack // obg9 is codename for memory-optimized Obj counterpart (not in use yet) - good for dense data

  // ---------------------------------------------------------------------------
  type TypeNode  = reflect.TypeNode

  type BasicType = meta.basic.BasicType
  val  BasicType = meta.basic.BasicType

  type Container = reflect.Container
  val  Container = reflect.Container

  // ---------------------------------------------------------------------------
  type SubInfo = meta.SubInfo
  val  SubInfo = meta.SubInfo

  type Info = meta.Info
  val  Info = meta.Info

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
    type AtomVUs = Seq[AtomVU]
    type AtomVZs = Seq[AtomVZ]

  // ---------------------------------------------------------------------------
  type _Nested = atoms.AtomsOthers._Nested
  val  _Nested = atoms.AtomsOthers._Nested

  type _Rename = atoms.common.AtomsCommonVeryBasics._Rename
  val  _Rename = atoms.common.AtomsCommonVeryBasics._Rename

  type _TransformVV = atoms.common.AtomsCommonTransforms._TransformVV
  val  _TransformVV = atoms.common.AtomsCommonTransforms._TransformVV

  // ---------------------------------------------------------------------------
  type PathPair = domain.PathPair
  val  PathPair = domain.PathPair

  // ===========================================================================
  private[actions] implicit def _atomUUToAtomUUs(value: AtomUU): Seq[AtomUU] = Seq(value)

  // ---------------------------------------------------------------------------
  implicit class Seq__[A](values: Seq[A]) {
    def orIfEmpty(generator: => Seq[A]): Seq[A] = if (values.isEmpty) generator else values } // convenient for validations
  
  // ---------------------------------------------------------------------------
  implicit class KPath__(value: KPath) {
    def vldtAsNewDestination(c: Cls) = _vldt.fieldAbsence(c, value) }

  // ---------------------------------------------------------------------------
  implicit class TKPath__(value: TKPath) {
    def vldtAsNewDestination(c: Cls) = value.path.vldtAsNewDestination(c) ++ _vldt.validType(value.typeNode) }

  // ===========================================================================
  import trgt.{TypedTargetQuery, TypedTargetQuery2, TypedTargetQuery3}

  // TODO: generalize + TargetQuery counterpart
  trait UsesSimpleTypedTargetQuery1Target[$Target] { // TODO: mixin action?
      val target: TypedTargetQuery[$Target]
      final def vldt(c: Cls): Errs = target.vldtAsOrigin(c) }

    trait UsesSimpleTypedTargetQuery2Target[$Target] {
      val target: TypedTargetQuery2[$Target]
      final def vldt(c: Cls): Errs = target.vldtAsOrigin(c) }

    trait UsesSimpleTypedTargetQuery3Target[$Target] {
      val target: TypedTargetQuery3[$Target]
      final def vldt(c: Cls): Errs = target.vldtAsOrigin(c) }

  // ===========================================================================
  trait SquashXN extends ActionM0 with TodoV1 {
      val to: reflect.TypeNode
      def atom(c: Cls): Atom

      // ---------------------------------------------------------------------------
      final def _meta: Cls = Cls.Dummy //TODO?
      final def atoms(ctx: NodeMetaContext): Atoms = Seq(ctx.afferents.forceOne.pipe(atom)) } }

// ===========================================================================

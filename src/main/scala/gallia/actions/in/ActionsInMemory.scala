package gallia.actions.in

import gallia._
import gallia.vldt.Location
import gallia.atoms.AtomsIX._
import gallia.vldt.{MetaValidation => _vldt}

// ===========================================================================
case class InMemoryMetaInput(c: Cls) extends ActionVM0 with IdentityV0 {
    def _meta: Cls = c }

  // ---------------------------------------------------------------------------
  case class InMemoryInputUa(value: AObj) extends ActionIUd with IdentityV0 {
    // TODO: t210116191208 - validate consistency of meta/data
    def _meta: Cls = value.c
    def atomiu     = _InMemoryInputUa(value) }

  // ---------------------------------------------------------------------------
  case class InMemoryInputZa(value: AObjs) extends ActionIZd with IdentityV0 {
    //TODO: t210116191208 - validate consistency of meta/data
    def _meta: Cls = value.c
    def atomiz     = _InMemoryInputZa(value) }

// ===========================================================================
case class InMemoryInputUb(value: BObj) extends ActionIUd {
    def vldt: Errs = _vldt.validateBObj(value)
    def _meta: Cls = value.forceCls
    def atomiu     = _InMemoryInputUb(value) }

  // ---------------------------------------------------------------------------
  case class InMemoryInputZb(value: BObjs) extends ActionIZd {
    def vldt: Errs = _vldt.validateBObjs(value)
    def _meta: Cls = value.forceCls
    def atomiz     = _InMemoryInputZb(value) }

// ===========================================================================
case class InMemoryInputV[T: WTT](value: T) extends ActionIVd {
  private val typeNode = node[T]

  // ---------------------------------------------------------------------------
  def vldt: Errs = _vldt.validType(Location.Root -> typeNode)
  def _meta: Cls = Cls.vle(typeNode)
  def atomiv     = _InMemoryInputV(value) }

// ===========================================================================

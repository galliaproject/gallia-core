package gallia
package actions.in

import atoms.AtomsIX._
import vldt.{Location, MetaValidation => _vldt}

// ===========================================================================
case class InMemoryMetaInput(c: Cls) extends ActionVM0 with IdentityV0 {
    def _meta: Cls = c }

  // ---------------------------------------------------------------------------
  case class InMemoryInputUa(value: AObj) extends ActionIUd with IdentityV0 {
    // TODO: t210116191208 - validate consistency of meta/data
    def _meta: Cls = value.c
    def atomiu     = _GenericInputU(value.o) }

  // ---------------------------------------------------------------------------
  case class InMemoryInputZa(value: AObjs) extends ActionIZd with IdentityV0 {
    //TODO: t210116191208 - validate consistency of meta/data
    def _meta: Cls = value.c
    def atomiz     = _GenericInputZ(value.z) }

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
  private val typeNode = gallia.typeNode[T]

  // ---------------------------------------------------------------------------
  def vldt: Errs = _vldt.validType(Location.Root -> typeNode)
  def _meta: Cls = Cls.vle(typeNode)
  def atomiv     = _InMemoryInputV(value) }

// ===========================================================================

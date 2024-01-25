package gallia
package actions
package in

import atoms.AtomsIX._
import vldt.{Location, MetaValidation => _vldt}

// ===========================================================================
case class InMemoryMetaInput(c: Cls) extends ActionVM0 with IdentityV0 {
    def _meta: Cls = c }

  // ---------------------------------------------------------------------------
  case class InMemoryInputUa(value: AObj) extends ActionIU01 with IdentityV0 {
    // TODO: t210116191208 - validate consistency of meta/data
    def _meta: Cls = value.c
    def atomiu     = _GenericInputU(value.o) }

  // ---------------------------------------------------------------------------
  case class InMemoryInputZa(value: AObjs) extends ActionIZ01 with IdentityV0 {
    //TODO: t210116191208 - validate consistency of meta/data
    def _meta: Cls = value.c
    def atomiz     = _GenericInputZ(value.z) }

// ===========================================================================
case class InMemoryInputUb(value: BObj) extends ActionIU01 {
    def vldt: Errs = _vldt.validateBObj(value)
    def _meta: Cls = value.forceCls
    def atomiu     = _InMemoryInputUb(value) }

  // ---------------------------------------------------------------------------
  case class InMemoryInputZb(value: BObjs) extends ActionIZ01 {
    def vldt: Errs = _vldt.validateBObjs(value)
    def _meta: Cls = value.forceCls
    def atomiz     = _InMemoryInputZb(value) }

// ===========================================================================
case class InMemoryInputV[T: WTT](value: T) extends ActionIV01 {
  def vldt: Errs = Nil // not constrained by the One/Opt/Nes/Pes paradigm (eg Seq[Option] is valid) - TODO: add some sanity check nonetheless (t240124111927)
  def _meta: Cls = Cls.Dummy // not actually used
  def atomiv     = _InMemoryInputV(value) }

// ===========================================================================

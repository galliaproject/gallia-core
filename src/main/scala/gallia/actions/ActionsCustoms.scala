package gallia.actions

import gallia._
import gallia.target._
import gallia.atoms.AtomsCustom._

// ===========================================================================
object ActionsCustoms {

  case class CustomMeta(f: Cls => Cls) extends IdentityV1 with IdentityUUa {
        def _meta(c: Cls): Cls = f(c) }

    // ---------------------------------------------------------------------------
    case class CustomField(target: TqRPathz, f: Info => Info) extends IdentityUUa {
          def  vldt(c: Cls): Errs = target.vldtAsOrigin(c)
          def _meta(c: Cls): Cls  = target(c)(c.transformInfo(_)(f))
        }

      // ---------------------------------------------------------------------------
      case class CustomContainer(target: TqRPathz, f: Container => Container) extends IdentityUUa {
          def  vldt(c: Cls): Errs = target.vldtAsOrigin(c)
          def _meta(c: Cls): Cls  = target(c)(c.transformInfo(_)(_.transformContainer(f))) }

      // ---------------------------------------------------------------------------
      case class CustomBasicType(target: TqRPathz, f: BasicType => BasicType) extends IdentityUUa {
          def vldt(c: Cls): Errs = target.vldtAsOrigin(c) // TODO: check not nesting
          def _meta(c: Cls): Cls = target(c)(c.transformInfo(_)(_.transformBasicType(f))) }

  // ===========================================================================
  class CustomUU(meta: Cls => Cls, data: Obj => Obj) extends ActionUUd with IdentityV1 {
        def _meta (c: Cls): Cls  = meta(c)
        def  atomuu       : AtomUU = _CustomOO(data) }

      // ---------------------------------------------------------------------------
      object CustomUU {
        def accessor[T: WTT](f: Obj => T) =
          new CustomUU(
              _ => cls(???),//_accessor.typed[T]), - FIXME: t210115174930
              o => ???/*obj(_accessor -> f(o))*/)
      }

    // ===========================================================================
    class CustomZZ(meta: Cls => Cls, data: Objs => Objs) extends ActionZZd with IdentityV1 {
        def _meta(in: Cls): Cls    = meta(in)
        def atomzz        : AtomZZ = _CustomZZ(data) }

      // ---------------------------------------------------------------------------
      object CustomZZ {
        def from(meta: Cls => Cls, data: Seq[Obj] => Seq[Obj]): CustomZZ =
          new CustomZZ(
              meta,
              _.toListAndTrash.pipe(data).pipe(Objs.from))
      }

}

// ===========================================================================

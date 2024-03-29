package gallia
package actions

import trgt._
import atoms.AtomsCustom._

// ===========================================================================
object ActionsCustoms {

  case class CustomMeta(f: Cls => Cls) extends IdentityV1 with IdentityUU0N {
        def _meta(c: Cls): Cls = f(c) }

    // ---------------------------------------------------------------------------
    case class CustomField(target: TqRPathz, f: Info => Info) extends IdentityUU0N {
          def  vldt(c: Cls): Errs = target.vldtAsOrigin(c)
          def _meta(c: Cls): Cls  = target(c)(c.transformInfo(_, f)) }

      // ---------------------------------------------------------------------------
      case class CustomContainer(target: TqRPathz, f: Container => Container) extends IdentityUU0N { // TODO: t220427091730 - change to optional/multiple
          def  vldt(c: Cls): Errs = target.vldtAsOrigin(c)
          def _meta(c: Cls): Cls  = ??? }//FIXME: t220426160919 - target(c)(c.transformSoleSubInfo(_)(_.transformContainer(f))) }

      // ---------------------------------------------------------------------------
      case class CustomBasicType(target: TqRPathz, f: BasicType => BasicType) extends IdentityUU0N {
          def vldt(c: Cls): Errs = target.vldtAsOrigin(c) // TODO: check not nesting
          def _meta(c: Cls): Cls = target(c)(c.transformSoleSubInfo(_)(_.transformBasicType(f))) }

  // ===========================================================================
  class CustomUU(meta: Cls => Cls, data: Obj => Obj) extends ActionUU01 with IdentityV1 {
        def _meta (c: Cls): Cls  = meta(c)
        def  atomuu       : AtomUU = _CustomOO(data) }

      // ---------------------------------------------------------------------------
      object CustomUU {
        def accessor[T: WTT](f: Obj => T) =
          new CustomUU(
              _ => ???,//_accessor.typed[T]), - FIXME: t210115174930
              o => ???/*obj(_accessor -> f(o))*/)
      }

    // ===========================================================================
    class CustomZZ(meta: Cls => Cls, data: Objs => Objs) extends ActionZZ01 with IdentityV1 {
        def _meta(in: Cls): Cls    = meta(in)
        def atomzz        : AtomZZ = _CustomZZ(data) }

      // ---------------------------------------------------------------------------
      object CustomZZ {
        def from(meta: Cls => Cls, data: List[Obj] => List[Obj]): CustomZZ =
          new CustomZZ(
              meta,
              _.toListAndTrash.pipe(data).pipe(Objs.from))
      }

    // ===========================================================================
    class CustomZU(meta: Cls => Cls, data: Objs => Obj) extends ActionZU01 with IdentityV1 {
        def _meta(in: Cls): Cls    = meta(in)
        def atomzu        : AtomZU = _CustomZO(data) }

      // ---------------------------------------------------------------------------
      object CustomZU {
        def from(meta: Cls => Cls, data: List[Obj] => Obj): CustomZU =
          new CustomZU(
            meta, _.toListAndTrash.pipe(data))
      }

}

// ===========================================================================

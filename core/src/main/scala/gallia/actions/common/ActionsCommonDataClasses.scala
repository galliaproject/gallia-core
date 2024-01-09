package gallia
package actions
package common

import trgt._
import FunctionWrappers._
import atoms.common.AtomsCommonDataClasses._

// ===========================================================================
object ActionsCommonDataClasses {

  case class TransformViaDataClass(target: Key, from: TypeDuo, to: TypeDuo, f: _ff11) extends ActionUUc {
    // TODO: validate those
    lazy val _from: Cls = from.typeNode.forceNonBObjInfo.subInfo1.valueType.nestingOpt.get
    lazy val   _to: Cls =   to.typeNode.forceNonBObjInfo.subInfo1.valueType.nestingOpt.get

    // ---------------------------------------------------------------------------
    def  vldt (c: Cls): Errs   = Nil // TODO: validate from matches origin container
    def _meta (c: Cls): Cls    = c.updateSoleType(target, to.typeNode)
    def atomuu(c: Cls): AtomUU = _TransformVV(c.pathPair(target), from.wrapc(to, f)) }

  // ===========================================================================
  case class CotransformViaDataClass(from: TypeDuo, to: TypeDuo, f: _ff11, eraseOriginIfDifferent: Boolean) extends ActionUUc {
      //TODO: validate Container._One for now
      lazy val _from: Cls = from.typeNode.forceNonBObjInfo.subInfo1.valueType.nestingOpt.get
      lazy val _to  : Cls =   to.typeNode.forceNonBObjInfo.subInfo1.valueType.nestingOpt.get

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs = Nil // TODO

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls =
          if (!eraseOriginIfDifferent) __meta(c)(_.intersect(_to.keys))
          else                         __meta(c)(identity)

        // ---------------------------------------------------------------------------
        private def __meta(c: Cls)(f: Seq[Key] => Seq[Key]): Cls =
          c .add   (_nonEmpty.boolean) // so is never empty
            .remove(_from.keys.pipe(f))
            //FIXME: recurse?
            .merge(_to)
            .remove(_nonEmpty)

      // ---------------------------------------------------------------------------
      def atomuu(c: Cls): AtomUU =
        if (!eraseOriginIfDifferent)
          _CotransformViaDataClassWithoutErasing(from.wrapc(to, f))
        else
          _CotransformViaDataClassWithErasing(
            rest = _from.keyz.pipe(c.complementKeyz),
            f    =  from.wrapc(to, f))
    }

    // ===========================================================================
    case class CotransformViaDataClassAs(from: TypeDuo, to: TypeNode, as: Key, f: _ff11, eraseOriginIfDifferent: Boolean) extends ActionUUc {
      //TODO: validate Container._One for now
      lazy val _from: Cls = from.typeNode.forceNonBObjInfo.subInfo1.valueType.nestingOpt.get
      lazy val _to  : Fld = Fld(as, to.forceNonBObjInfo)

      // ---------------------------------------------------------------------------
      def  vldt(c: Cls): Errs = Nil // TODO

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls =
        if (!eraseOriginIfDifferent)
          c.putField(_to)
        else
          c .add   (_nonEmpty.boolean) // so is never empty
            .remove(_from.keys)
        //FIXME: recurse?
            .addField(_to)
            .remove(_nonEmpty)

      // ---------------------------------------------------------------------------
      def atomuu(c: Cls): AtomUU =
        if (!eraseOriginIfDifferent)
          _CotransformViaDataClassAsWithoutErasing(
            as   = _to.key,
            f    =  from.wrapc(to, f))
        else
          _CotransformViaDataClassAsWithErasing(
            rest = c.complementKeyz(_from.keyz),
            as   = _to.key,
            f    =  from.wrapc(to, f))
    }

}

// ===========================================================================

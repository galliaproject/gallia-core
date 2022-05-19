package gallia
package actions
package common

import aptus.Seq_
import atoms.common.AtomsCommonUnionTypes._

// ===========================================================================
object ActionsCommonUnionTypes {

  // TODO: t220511141020 - generalize to N
  case class FuseToUnion(origin1: Key, origin2: Key, dest: Key) extends ActionUUd {
      def  vldt(c: Cls ): Errs   = Seq(_vldt.fieldsPresence(c, Seq(origin1, origin2)), _vldt.fieldAbsence(c, dest)).flatten
        // TODO: validate would make a valid union
      def _meta(c: Cls ): Cls    = c.fuseToUnion(origin1, origin2)(dest)
      def atomuu        : AtomUU =  _FuseToUnion(origin1, origin2, dest) }

    // ---------------------------------------------------------------------------
    // TODO: t220511141021 - generalize to N
    case class FissionFromUnion(origin: Key, dest1: Key, dest2: Key) extends ActionUUc {
      def  vldt(c: Cls ): Errs =
        Seq(_vldt.fieldPresence(c, origin), _vldt.fieldsAbsence(c, Seq(dest1, dest2))).flatten
          .orIfEmpty(_vldt.checkIsUnionField(c)(origin))
          .orIfEmpty(c.field(origin).pipe(_Error.MoreThanOneNesting).errsIf(
            _.union.filter(_.isNesting).size > 1) /* TODO: t220511152605: a version that allows more (more complex) */)

      // ---------------------------------------------------------------------------
      def _meta(c: Cls ): Cls    = c.fissionFromUnion(origin)(dest1, dest2)
      def atomuu(c: Cls): AtomUU = c.field(origin).union.force.tuple2.pipe { case (subInfo1, subInfo2) =>
        _FissionFromUnion(origin, dest1, dest2, subInfo1.valuePredicate, subInfo2.valuePredicate) } }

}

// ===========================================================================

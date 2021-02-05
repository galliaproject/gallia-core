package gallia.actions

import aptus.Anything_

import gallia._
import gallia.atoms.AtomsZZ._

// ===========================================================================
object ActionsZZGrouping {

  import gallia.heads.grouping._
  import gallia.heads.grouping.GroupData._

  // ===========================================================================
  case class Grouping(data: GroupData) extends ActionZZb with CanForceAs1[Grouping] {
        override def forceAs(key: Key) = copy(data = data.forceAs(key))

      // ---------------------------------------------------------------------------
      def vldt (in: Cls ): Errs = Nil// TODO

      // ---------------------------------------------------------------------------
      def _meta(in: Cls ): Cls  =
        data match {
          case data: GroupData1C => data.pair(in).thn { case (groupee , groupers) => in.group1N(groupee , groupers) }

          case data: GroupData1N => data.pair(in).thn { case (groupee , groupers) => in.group1N(groupee , groupers) }

          case data: GroupDataCN => data.pair(in).thn { case (groupees, groupers) => in.groupNN(groupees, groupers, data.as) }
          case data: GroupDataNC => data.pair(in).thn { case (groupees, groupers) => in.groupNN(groupees, groupers, data.as) }

          case data: GroupDataNN => data.pair(in).thn { case (groupees, groupers) => in.groupNN(groupees, groupers, data.as) } }

      // ===========================================================================
      def atomzzs(in: Cls): AtomZZs =
        Seq(data match {
          case data: GroupData1C => data.pair(in).thn { case (groupee , groupers) => _Group1N(groupee.fromFX, groupers.fromsFX) }

          case data: GroupData1N => data.pair(in).thn { case (groupee , groupers) => _Group1N(groupee.fromFX, groupers.fromsFX) }

          case data: GroupDataCN => data.pair(in).thn { case (groupees, groupers) => _GroupNN(groupees.fromsFX, groupers.fromsFX, data.as) }
          case data: GroupDataNC => data.pair(in).thn { case (groupees, groupers) => _GroupNN(groupees.fromsFX, groupers.fromsFX, data.as) }

          case data: GroupDataNN => data.pair(in).thn { case (groupees, groupers) => _GroupNN(groupees.fromsFX, groupers.fromsFX, data.as) } })
    }

}

// ===========================================================================

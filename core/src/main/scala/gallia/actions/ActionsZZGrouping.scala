package gallia
package actions

import atoms.AtomsZZ._
import heads.grouping._
import heads.grouping.GroupData._

// ===========================================================================
object ActionsZZGrouping {

  case class GroupingAction(data: GroupData) extends ActionZZb with CanForceAs1[GroupingAction] {
        override def forceAs(key: Key) = copy(data = data.forceAs(key))

      // ---------------------------------------------------------------------------
      def vldt (in: Cls ): Errs = Nil// TODO

      // ===========================================================================
      def _meta(in: Cls ): Cls =
        data match {
          case data: GroupData1C => data.pair(in).pipe { case (_, (groupee , groupers)) => in.group1N(groupee , groupers) }
          case data: GroupData1N => data.pair(in).pipe { case (_, (groupee , groupers)) => in.group1N(groupee , groupers) }

          case data: GroupDataCN => data.pair(in).pipe { case (_, (groupees, groupers)) => in.groupNN(groupees, groupers, data.as) }
          case data: GroupDataNC => data.pair(in).pipe { case (_, (groupees, groupers)) => in.groupNN(groupees, groupers, data.as) }

          case data: GroupDataNN => data.pair(in).pipe { case (_, (groupees, groupers)) => in.groupNN(groupees, groupers, data.as) } }

      // ===========================================================================
      def atomzzs(in: Cls): AtomZZs =
        Seq(data match {
          case data: GroupData1C => data.pair(in).pipe { case (bar, (groupee , groupers)) => _Group1N(bar, groupee, groupers) }

          case data: GroupData1N => data.pair(in).pipe { case (bar, (groupee , groupers)) => _Group1N(bar, groupee, groupers) }

          case data: GroupDataCN => data.pair(in).pipe { case (bar, (groupees, groupers)) => _GroupNN(bar, groupees, groupers, data.as) }
          case data: GroupDataNC => data.pair(in).pipe { case (bar, (groupees, groupers)) => _GroupNN(bar, groupees, groupers, data.as) }

          case data: GroupDataNN => data.pair(in).pipe { case (bar, (groupees, groupers)) => _GroupNN(bar, groupees, groupers, data.as) } }) }
}

// ===========================================================================

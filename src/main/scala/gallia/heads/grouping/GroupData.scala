package gallia
package heads
package grouping

import target._
import domain.GroupingPair._

// ===========================================================================
sealed trait GroupData {
    import GroupData._

    // ---------------------------------------------------------------------------
    def forceAs(value: Key): GroupData = this.asInstanceOf[HasAsOpt] match {
      case x: GroupDataCN => x.copy(asOpt = Some(value))
      case x: GroupDataNN => x.copy(asOpt = Some(value))
      case x: GroupDataNC => x.copy(asOpt = Some(value)) }
  }

  // ===========================================================================
  object GroupData {
    import GroupingFluency._

    // ---------------------------------------------------------------------------
    def from(conf: Start => End): GroupData = new Start().pipe(conf).data

    // ===========================================================================
    sealed trait HasAsOpt {
        val asOpt: Option[Key]
        def as   :        Key  = asOpt.getOrElse(_group) }

      // ===========================================================================
      case class GroupData1C(groupee : TqRen                                       ) extends GroupData               { def pair(in: Cls): (GroupingPairN1, (Ren , Renz)) =  groupee .resolve(in).pipe { e => e -> in.complementKeyz(e.from).renz }      .pipe((GroupingPairN1.from(in) _).tupled) }
      case class GroupData1N(groupee : TqRen , groupers: TqRenz                    ) extends GroupData               { def pair(in: Cls): (GroupingPairN1, (Ren , Renz)) = (groupee .resolve(in), groupers.resolve(in))                                 .pipe((GroupingPairN1.from(in) _).tupled) }

      case class GroupDataCN(                  groupers: TqRenz, asOpt: Option[Key]) extends GroupData with HasAsOpt { def pair(in: Cls): (GroupingPairNN, (Renz, Renz)) =  groupers.resolve(in).pipe { r =>      in.complementKeyz(r.froms).renz -> r }.pipe((GroupingPairNN.from(in) _).tupled) }
      case class GroupDataNC(groupees: TqRenz                  , asOpt: Option[Key]) extends GroupData with HasAsOpt { def pair(in: Cls): (GroupingPairNN, (Renz, Renz)) =  groupees.resolve(in).pipe { e => e -> in.complementKeyz(e.froms).renz }     .pipe((GroupingPairNN.from(in) _).tupled) }

      case class GroupDataNN(groupees: TqRenz, groupers: TqRenz, asOpt: Option[Key]) extends GroupData with HasAsOpt { def pair(in: Cls): (GroupingPairNN, (Renz, Renz)) = (groupees.resolve(in), groupers.resolve(in))                                 .pipe((GroupingPairNN.from(in) _).tupled) }
  }

// ===========================================================================

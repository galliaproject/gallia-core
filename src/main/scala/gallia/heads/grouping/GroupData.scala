package gallia.heads.grouping

import aptus.Anything_
import gallia._
import gallia.target._

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
      case class GroupData1C(groupee : TQRen                                      ) extends GroupData               { def pair(in: Cls): (Ren , Renz) = groupee .resolve(in).pipe { e => e -> in.complementKeyz(e.from).renz } }
      case class GroupData1N(groupee : TQRen , groupers: TQRenz                   ) extends GroupData               { def pair(in: Cls): (Ren , Renz) = groupee .resolve(in) -> groupers.resolve(in) }

      case class GroupDataCN(                 groupers: TQRenz, asOpt: Option[Key]) extends GroupData with HasAsOpt { def pair(in: Cls): (Renz, Renz) = groupers.resolve(in).pipe { r =>      in.complementKeyz(r.froms).renz -> r } }
      case class GroupDataNC(groupees: TQRenz                 , asOpt: Option[Key]) extends GroupData with HasAsOpt { def pair(in: Cls): (Renz, Renz) = groupees.resolve(in).pipe { e => e -> in.complementKeyz(e.froms).renz } }

      case class GroupDataNN(groupees: TQRenz, groupers: TQRenz, asOpt: Option[Key]) extends GroupData with HasAsOpt { def pair(in: Cls): (Renz, Renz) = groupees.resolve(in) -> groupers.resolve(in) }
  }

// ===========================================================================

package gallia.heads.merging

import enumeratum.{Enum, EnumEntry}

import aptus.{Anything_, Seq_}

import gallia._
import MergingData._
import MergingFluency._

// ===========================================================================
sealed trait MergingData {
    val joinKeysOpt: Option[JoinKey]

    // ---------------------------------------------------------------------------
    def vldtJoinKeys(c1: Cls, c2: Cls): Option[Any] =
      joinKeysOpt match {
        case Some(_) => None
        case None    => c1.keys.intersect(c2.keys).as.someIf(_.size != 1) }

    // ---------------------------------------------------------------------------
    // TODO: consider case-insentivity? (arbitrarily favor left)
    def joinKeys(c1: Cls, c2: Cls): JoinKey =
      joinKeysOpt match {
        case Some(joinKeys) => joinKeys
        case None           => c1.keys.intersect(c2.keys).force.one.thn(JoinKey.common) } // already validated by here
  }

  // ===========================================================================
  object MergingData {
    //TODO: 210117143536 - also handle union in here?

    def from(conf: Start => End): MergingData = new Start().thn(conf).data

    // ===========================================================================
    case class CoGroupData(
          joinType: JoinType,
            joinKeysOpt: Option[JoinKey],
              as: AsKeys)
        extends MergingData

    // ---------------------------------------------------------------------------
    case class JoinData(
          joinType: JoinType,
            joinKeysOpt: Option[JoinKey])
        extends MergingData

    // ---------------------------------------------------------------------------
    case class BringData(
          targets: TqKeyz,
          joinKeysOpt: Option[JoinKey])
        extends MergingData {

      /** exludes join key */
      def targetKeys(c1: Cls, c2: Cls): Keyz =
        super
          .joinKeys(c1, c2)
          .right
          .thn { rightJoinKey => targets.resolve(c2).filterNot(_ == rightJoinKey) }
          .thn(Keyz.apply)

  }

  // ===========================================================================
  sealed trait JoinType extends EnumEntry
    object JoinType extends Enum[JoinType] {
      val values = findValues

      // ---------------------------------------------------------------------------
      case object  full extends JoinType
      case object  left extends JoinType
      case object right extends JoinType
      case object inner extends JoinType

      // ---------------------------------------------------------------------------
      /*
        match {
          case JoinType.full  => ???
          case JoinType.left  => ???
          case JoinType.right => ???
          case JoinType.inner => ???
        }
      */
    }

    // ===========================================================================
    case class AsKeys(left: Key = _left, right: Key = _right)

    // ---------------------------------------------------------------------------
    case class JoinKey(
          left : Key,
          right: Key) {
        /** arbitrarily choosing left as the "dominant" one */ def key = left
      }

      // ---------------------------------------------------------------------------
      object JoinKey {
        def common(key: Key) = JoinKey(key, key)
      }
  }

// ===========================================================================

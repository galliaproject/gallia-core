package gallia
package heads.merging

import enumeratum.{Enum, EnumEntry}

import aptus.{Anything_, Seq_}

import MergingData._
import MergingFluency._

// ===========================================================================
sealed trait MergingData {
    val joinKeysOpt: Option[JoinKey]

    // ---------------------------------------------------------------------------
    def vldtJoinKeys(c1: Cls, c2: Cls): Option[Keyz] =
      joinKeysOpt match {
        case Some(_) => None
        case None    => c1.keys.intersect(c2.keys).in.someIf(_.size != 1).map(Keyz.apply) }

    // ---------------------------------------------------------------------------
    // TODO: consider case-insentivity? (arbitrarily favor left)
    def joinKeys(c1: Cls, c2: Cls): JoinKey =
      joinKeysOpt match {
        case Some(joinKeys) => joinKeys
        case None           => c1.keys.intersect(c2.keys).force.one.pipe(JoinKey.common) } // already validated by here (see above)
  }

  // ===========================================================================
  object MergingData {
    //TODO: 210117143536 - also handle union in here?

    def from(conf: Start => End): MergingData = new Start().pipe(conf).data

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
          .pipe { rightJoinKey => targets.resolve(c2).filterNot(_ == rightJoinKey) }
          .pipe(Keyz.apply)

  }

  // ===========================================================================
  sealed trait JoinType extends EnumEntry {      
      def isFull : Boolean = this == JoinType.full
      def isLeft : Boolean = this == JoinType.left
      def isRight: Boolean = this == JoinType.right
      def isInner: Boolean = this == JoinType.inner
    }
  
    // ===========================================================================
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

        def keys = Seq(left, right)
      }

      // ---------------------------------------------------------------------------
      object JoinKey {
        def common(key: Key) = JoinKey(key, key)
      }
  }

// ===========================================================================

package gallia
package heads.merging

// ===========================================================================
object MergingFluency {
  import MergingData._
  private implicit def toKey(x: KeyW): Key = x.value

  // ===========================================================================
  class Start {
    def coGroup = new CoGroup
    def join    = new Join
    def bring   = new Bring }

    // ===========================================================================
    class CoGroup {
      def full : CoGroupOn = new CoGroupOn(JoinType.full)
      def left : CoGroupOn = new CoGroupOn(JoinType.left)
      def right: CoGroupOn = new CoGroupOn(JoinType.right)
      def inner: CoGroupOn = new CoGroupOn(JoinType.inner) }

    // ---------------------------------------------------------------------------
    class Join {
      def full : JoinOn = new JoinOn(JoinType.full)
      def left : JoinOn = new JoinOn(JoinType.left)
      def right: JoinOn = new JoinOn(JoinType.right)
      def inner: JoinOn = new JoinOn(JoinType.inner) }

    // ===========================================================================
    class Bring {
        def all: Via = fields(_.allKeys)

        def fields(x: KeyW )                       : Via = fields(_.explicit(x))
        def fields(x: KeyWz)                       : Via = fields(_.explicit(x))
        def fields(x1: KeyW, x2: KeyW, more: KeyW*): Via = fields(_.explicit(x1, x2, more:_*))

        // ---------------------------------------------------------------------------
        def fields(sel: SEL.Merging.Selector): Via = new Via(SEL.Merging.resolve(sel)) }

  // ===========================================================================
  class CoGroupOn(joinType: JoinType) {
      def onCommonKey                      : As = on(None)

      def on(commonKey: KeyW)              : As = on(commonKey, commonKey)
      def on(leftKey: KeyW, rightKey: KeyW): As = on(JoinKey(leftKey, rightKey)) //TODO: vs .onLeftKey('foo).onRightKey('FOO) ?
      def on(joinKey: JoinKey)             : As = on(Some(joinKey))

      def on(joinKeyOpt: Option[JoinKey])  : As = new As(joinType, joinKeyOpt) }

    // ---------------------------------------------------------------------------
    class JoinOn(joinType: JoinType) {
      def onCommonKey                      : End = on(None)

      def on(commonKey: KeyW)              : End = on(commonKey, commonKey)
      def on(leftKey: KeyW, rightKey: KeyW): End = on(JoinKey(leftKey  , rightKey))
      def on(joinKey: JoinKey)             : End = on(Some(joinKey))

      def on(joinKeyOpt: Option[JoinKey])  : End = new End(JoinData(joinType, joinKeyOpt) ) }

    // ---------------------------------------------------------------------------
    class Via(targets: TqKeyz) {
      /** only if one and only one common key */
      def viaCommonKey                      : End = via(None)

      def via(commonKey: KeyW)              : End = via(commonKey, commonKey)
      def via(leftKey: KeyW, rightKey: KeyW): End = via(JoinKey(leftKey, rightKey))
      def via(joinKey: JoinKey)             : End = via(Some(joinKey))

      def via(joinKeyOpt: Option[JoinKey])  : End = new End(BringData(targets, joinKeyOpt)) }

  // ===========================================================================
    class As(joinType: JoinType, joinKeysOpt: Option[JoinKey]) {
      def asDefaults                       : End = as(AsKeys())

      def as(leftKey: KeyW, rightKey: KeyW): End = as(AsKeys(leftKey, rightKey))
      def as(as: AsKeys)                   : End = new End(CoGroupData(joinType, joinKeysOpt, as)) }

  // ===========================================================================
  class End(val data: MergingData)

}

// ===========================================================================

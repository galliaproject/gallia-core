package gallia.env

import gallia._
import gallia.dag.NodeId
import gallia.actions.ActionsOthers
import gallia.heads.HeadEnd
import gallia.heads.grouping.HasAs

// ===========================================================================
class Handler() { // TODO: t210128152949 - add missing cycle detection
  private val _helper = new HandlerHelper()

  // ---------------------------------------------------------------------------
  def startu        (action: ActionIU): HeadU    = new HeadU   (nodeId = _helper.start(action), this)
  def startz        (action: ActionIZ): HeadZ    = new HeadZ   (nodeId = _helper.start(action), this)
  def startv[T: WTT](action: ActionIV): HeadV[T] = new HeadV[T](nodeId = _helper.start(action), this)

  // ---------------------------------------------------------------------------
  def chainuu                  (head: HeadU    )(action: ActionUU): HeadU     = head.rewrap    (_helper.chain(head.nodeId, action))
  def chainzz                  (head: HeadZ    )(action: ActionZZ): HeadZ     = head.rewrap    (_helper.chain(head.nodeId, action))
  def chainvv[T1: WTT, T2: WTT](head: HeadV[T1])(action: ActionVV): HeadV[T2] = head.rewrap[T2](_helper.chain(head.nodeId, action))

  // ---------------------------------------------------------------------------
  // TODO: t210116192032 - generalize mechanism
  def chainzzWithAs(head: HeadZ)(action: ActionZZ): HeadZ with HasAs = head.rewrapWithAs(_helper.chain(head.nodeId, action))
    def updateAs(nodeId: NodeId, key: Key) { _helper.updateAs(nodeId, key) }

  // ---------------------------------------------------------------------------
  def chainzu         (head: HeadZ)(action: ActionZU): HeadU     = head.headU(_helper.chain(head.nodeId, action))
  def chainuz         (head: HeadU)(action: ActionUZ): HeadZ     = head.headZ(_helper.chain(head.nodeId, action))

  // ---------------------------------------------------------------------------
  def chainuv[T : WTT](head: HeadU)(action: ActionUV): HeadV[T] = head.headV[T ](_helper.chain(head.nodeId, action))
  def chainzv[T : WTT](head: HeadZ)(action: ActionZV): HeadV[T] = head.headV[T ](_helper.chain(head.nodeId, action))

  // ---------------------------------------------------------------------------
  def joinuu2u(dis: HeadU, that: HeadU)                     : HeadU = dis.rewrap(_helper.join2(dis.nodeId, that.nodeId)(ActionsOthers.UnionUU))
  def joinZz2z(dis: HeadZ, that: HeadZ)(action: ActionZzToZ): HeadZ = dis.rewrap(_helper.join2(dis.nodeId, that.nodeId)(action))

  // ---------------------------------------------------------------------------
  def chainuo(head: HeadU)(action: ActionUO): HeadU = head.rewrap(_helper.chain(head.nodeId, action))
  def chainzo(head: HeadZ)(action: ActionZO): HeadZ = head.rewrap(_helper.chain(head.nodeId, action))

  // ---------------------------------------------------------------------------
  def end(head: heads.Head[_]): HeadEnd = HeadEnd(_helper.chain(head.nodeId, ActionsOthers.Output))
}

// ===========================================================================

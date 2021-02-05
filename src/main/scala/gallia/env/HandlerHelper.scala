package gallia.env

import gallia._
import gallia.dag._

// ===========================================================================
class HandlerHelper(env: Env) {

  private[gallia] def start(input: ActionVM0): RootId = {
    val dagId  = env.nextDagId()
    val rootId = env.nextNodeId()
    env.associateNode(rootId -> dagId)

    val dag: ActionDag = DAG.trivial[gallia.env.NodePair](_._1)(rootId -> input)
    env.associateDag(dagId -> dag)

    rootId
  }

  // ---------------------------------------------------------------------------
  private[gallia] def chain(nodeId: NodeId, action: Node): NodeId = {
    val (dagId, originalDag) = env.retrieveDagPair(nodeId)

    val newNodeId = env.nextNodeId()
    val updatedDag = originalDag.appendNode(nodeId -> (newNodeId, action))

    env.associateNode(newNodeId -> dagId)
    env.associateDag(dagId -> updatedDag)

    newNodeId
  }

  // ---------------------------------------------------------------------------
  private[gallia] def join2(thisNodeId: NodeId, thatNodeId: NodeId)(action: Node): LeafId = {
    val newDagId  = env.nextDagId()
    val newNodeId = env.nextNodeId()

    val (originalDagId1, originalDag1) = env.retrieveDagPair(thisNodeId)
    val (originalDagId2, originalDag2) = env.retrieveDagPair(thatNodeId)

    val newDag =
      originalDag1.appendNode(thisNodeId -> (newNodeId, action))
        .mergeBlindly(
      originalDag2.appendNode(thatNodeId -> (newNodeId, action)) )

    env.associateDag(newDagId -> newDag)
    env.dissociateDag(originalDagId1)
    env.dissociateDag(originalDagId2)

    newDag
      .nodeIds
      .foreach { nodeId =>
        // TODO: differentiate reassociate?
        env.associateNode(nodeId -> newDagId) }

    newNodeId
  }

  // ===========================================================================
  import gallia.actions.CanForceAs1

  // ---------------------------------------------------------------------------
  def updateAs(nodeId: NodeId, key: Key) { // TODO: t210116192032 - generalize
      val (dagId, dag) = env.retrieveDagPair(nodeId)

      val updatedDag: ActionDag =
        dag.transformNode[Node](nodeId) { // by design
          _ .asInstanceOf[CanForceAs1[_]]
            .forceAs(key)
            .asInstanceOf[Node] }

      env.associateDag(dagId -> updatedDag)
    }

}

// ===========================================================================

package gallia
package env

import dag._
import env.ActionNodePair

// ===========================================================================
class HandlerHelper() {

  private[gallia] def start(input: ActionVM0): RootId = {
    val dagId  = Env.nextDagId()
    val rootId = Env.nextNodeId()
    Env.associateNode(rootId -> dagId)

    val dag: ActionDag = DAG.trivial[ActionNodePair](_.id)(ActionNodePair(rootId, input))
    Env.associateDag(dagId -> dag)

    rootId
  }

  // ---------------------------------------------------------------------------
  private[gallia] def chain(nodeId: NodeId, action: ActionVN with ActionMN): NodeId = {
    val (dagId, originalDag) = Env.retrieveDagPair(nodeId)

    val newNodeId = Env.nextNodeId()
    val updatedDag = originalDag.appendNode(nodeId -> ActionNodePair(newNodeId, action))

    Env.associateNode(newNodeId -> dagId)
    Env.associateDag(dagId -> updatedDag)

    newNodeId
  }

  // ---------------------------------------------------------------------------
  private[gallia] def join2(thisNodeId: NodeId, thatNodeId: NodeId)(action: ActionVN with ActionMN): LeafId = {
    val newDagId  = Env.nextDagId()
    val newNodeId = Env.nextNodeId()

    val (originalDagId1, originalDag1) = Env.retrieveDagPair(thisNodeId)
    val (originalDagId2, originalDag2) = Env.retrieveDagPair(thatNodeId)

    val newDag =
      originalDag1.appendNode(thisNodeId -> ActionNodePair(newNodeId, action))
        .mergeBlindly(
      originalDag2.appendNode(thatNodeId -> ActionNodePair(newNodeId, action)) )

    Env.associateDag(newDagId -> newDag)
    Env.dissociateDag(originalDagId1)
    Env.dissociateDag(originalDagId2)

    newDag
      .nodeIds
      .foreach { nodeId =>
        // TODO: differentiate reassociate?
        Env.associateNode(nodeId -> newDagId) }

    newNodeId
  }

  // ===========================================================================
  def updateAs(nodeId: NodeId, key: Key) = { // TODO: t210116192032 - generalize
      val (dagId, dag) = Env.retrieveDagPair(nodeId)

      val updatedDag: ActionDag =
        dag.transformNodeTypeConditionally(_ == nodeId) {
          _.tranform {
            _ .asInstanceOf[actions.CanForceAs1[_]] /* by design */
              .forceAs(key)
              .asInstanceOf[ActionVN with ActionMN]  /* by design */ } }

      Env.associateDag(dagId -> updatedDag)
      
      ()
    }

}

// ===========================================================================

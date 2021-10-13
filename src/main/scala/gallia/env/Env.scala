package gallia
package env

import aptus.String_
import dag._

// ===========================================================================
private[gallia] object Env extends Env

  // ---------------------------------------------------------------------------
  class Env private () {
    private var  dagId: Int = -1
    private var nodeId: Int = -1
  
    // ---------------------------------------------------------------------------
    private val _dags  = collection.mutable.Map[DagId , ActionDag ]()
    private val _nodes = collection.mutable.Map[NodeId, DagId]()
  
    // ---------------------------------------------------------------------------
    private[gallia] def nextDagId (): DagId  = synchronized { dagId   += 1; dagId  .toString.padLeft(6, '0').prepend("G") }
    private[gallia] def nextNodeId(): NodeId = synchronized { nodeId  += 1; nodeId .toString.padLeft(9, '0').prepend("N") }
  
    // ---------------------------------------------------------------------------
    def retrieveDagId(nodeId: NodeId): DagId      = _nodes(nodeId)
    def retrieveDag  (dagId : NodeId): ActionDag  = _dags(dagId)
  
    // ---------------------------------------------------------------------------
    def associateNode(pair: (NodeId, DagId))     = { _nodes += pair }
    def associateDag (pair: (DagId , ActionDag)) = { _dags  += pair }
    def dissociateDag(dagId: DagId)              = { _dags  -= dagId }
  
    // ---------------------------------------------------------------------------
    def retrieveDagFromNode(nodeId: NodeId):         ActionDag  =  retrieveDag(retrieveDagId(nodeId))
    def retrieveDagPair    (nodeId: NodeId): (DagId, ActionDag) = (retrieveDagId(nodeId), retrieveDagFromNode(nodeId))
  }

// ===========================================================================

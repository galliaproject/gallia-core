package gallia
package env

import aptus.String_

import plans.ActionMetaDag
import dag._

// ===========================================================================
private[gallia] object Env extends Env

  // ---------------------------------------------------------------------------
  class Env private () {
    private var  dagId: Int = -1
    private var nodeId: Int = -1
  
    // ---------------------------------------------------------------------------
    private val _dags  = collection.mutable.Map[DagId , ActionMetaDag ]()
    private val _nodes = collection.mutable.Map[NodeId, DagId]()
  
    // ---------------------------------------------------------------------------
    private[gallia] def nextDagId (): DagId  = synchronized { dagId   += 1; dagId  .toString.padLeft(6, '0').prepend("G") }
    private[gallia] def nextNodeId(): NodeId = synchronized { nodeId  += 1; nodeId .toString.padLeft(9, '0').prepend("N") }
  
    // ---------------------------------------------------------------------------
    def retrieveDagId(nodeId: NodeId): DagId      = synchronized { _nodes(nodeId) }
    def retrieveDag  (dagId : NodeId): ActionMetaDag  = synchronized { _dags (dagId)  }
  
    // ---------------------------------------------------------------------------
    def associateNode(pair: (NodeId, DagId))     = synchronized { _nodes += pair }
    def associateDag (pair: (DagId , ActionMetaDag)) = synchronized { _dags  += pair }
    def dissociateDag(dagId: DagId)              = synchronized { _dags  -= dagId }
  
    // ---------------------------------------------------------------------------
    def retrieveDagFromNode(nodeId: NodeId):         ActionMetaDag  =  retrieveDag(retrieveDagId(nodeId))
    def retrieveDagPair    (nodeId: NodeId): (DagId, ActionMetaDag) = (retrieveDagId(nodeId), retrieveDagFromNode(nodeId)) }

// ===========================================================================

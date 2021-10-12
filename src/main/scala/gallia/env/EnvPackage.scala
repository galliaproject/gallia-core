package gallia

import dag._

// ===========================================================================
package object env {
  type Node      = ActionVMN
  type NodePair  = (NodeId, Node)
  type ActionDag = DAG[NodePair]
}

// ===========================================================================
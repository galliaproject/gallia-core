package gallia

import dag._

// ===========================================================================
package object env {
  type Node      = ActionVN with ActionMN
  type NodePair  = (NodeId, Node)
  type ActionDag = DAG[NodePair] }

// ===========================================================================
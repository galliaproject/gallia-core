package gallia

import aptus.Seq_

// ===========================================================================
package object plans {
  case class Clss(values: Seq[Cls]) { // eg join
    def isEmpty  :  Boolean   = values.isEmpty
    def forceOne :  Cls       = values.force.one
    def forcePair: (Cls, Cls) = values.force.tuple2 }

  // ---------------------------------------------------------------------------
  case class ClsS(values: Seq[Cls]) // eg intermediate steps

  // ---------------------------------------------------------------------------
  type DAG[T] = dag.DAG[T]
  type NodeId = dag.NodeId }

// ===========================================================================

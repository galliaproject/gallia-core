package gallia
package plans

import dag._

// ===========================================================================
object NaiveGraphDataRun {
  import DataInput._
  
  // ---------------------------------------------------------------------------
  def apply(missingInputs: Map[RootId, NDT])(dag: DAG[AtomNode]): NDT = {
    var latest: (NodeId, NDT) = null
    val forkJoinData = collection.mutable.Map[NodeId, NDT]() // TODO: t210611140539 - value as queue based on # of consumers

    // ---------------------------------------------------------------------------      
    dag
      .chainTraverseNodes // TODO: change to a chain-first kahn-like traversal
      .foreach { node =>
        val afferentIds: Seq[NodeId] = dag.afferentIds(node.id)

        val fork: Boolean = afferentIds.exists { afferentId => dag.efferentCount(afferentId) > 1 }

        val input: DataInput =
          afferentIds match {
            case Nil             => NoInput
            case Seq(_) if !fork => SingleInput    (latest._2)          // if next on same chain
            case Seq(sole)       => SingleForkInput(forkJoinData(sole)) // if previous node is a fork
            case multiple        => MultipleInputs(multiple.map(forkJoinData.apply)) }

        latest = node.id -> AtomProcessor(
            input, missingInputs.apply)(
            node.id, node.atom, node.debug)

        dag.efferentIds(node.id) match {
          case Nil       => ()
          case Seq(sole) =>
            if (dag.afferentCount(sole) == 1) ()
            else    forkJoinData += latest   // has co-parent                          
          case _ => forkJoinData += latest } // has multiple children
        
        latest
      }

    // ---------------------------------------------------------------------------
    latest._2
   }
  
}

// ===========================================================================

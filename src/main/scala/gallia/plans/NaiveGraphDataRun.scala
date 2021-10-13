package gallia
package plans

import dag._

// ===========================================================================
object NaiveGraphDataRun {

  def apply(missingInputs: Map[RootId, NDT])(dag: DAG[AtomNode]) = {
    var latest: NDT = null
    val forkJoinData = collection.mutable.Map[NodeId, NDT]() // TODO: t210611140539 - value as queue based on # of consumers

    // ---------------------------------------------------------------------------      
    dag
      .chainTraverseNodes // TODO: change to a chain-first kahn-like traversal
      .foreach { node =>  
        val afferentIds: Seq[NodeId] = dag.afferentIds(node.id)
        val efferentIds: Seq[NodeId] = dag.efferentIds(node.id)

        val input: Either[Seq[NDT], NDT] =
          if (afferentIds.size > 1) Left(afferentIds.map(forkJoinData.apply))
          else                      Right(latest)

        latest = AtomProcessor(
            input, missingInputs.apply)(
            node.id, node.atom, node.debug)

        dag.efferentIds(node.id) match {
          case Nil       => ()
          case Seq(sole) =>
            if (dag.afferentCount(sole) == 1) ()
            else    forkJoinData += node.id -> latest   // has co-parent                          
          case _ => forkJoinData += node.id -> latest } // has multiple children          
      }

    // ---------------------------------------------------------------------------
    latest
   }
  
}

// ===========================================================================

package gallia
package plans

import dag._

// ===========================================================================
object ChainDataRun {

  def apply(missingInputs: Map[RootId, NDT])(nodes: AtomNodes): NDT = {  
    var latest: NDT = null // using foldLeft is tricky because of missingInputs

    nodes
      .values
      .foreach { node =>  
        latest = AtomProcessor(
            DataInput.SingleInput(latest), missingInputs.apply)(
            node.id, node.atom, node.debug) }

    latest
  }
  
}

// ===========================================================================

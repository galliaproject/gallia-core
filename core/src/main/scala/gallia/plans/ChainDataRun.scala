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
        latest =
          node.process(
            input = DataInput.SingleInput(latest))(
            missingInputs.apply) }

    latest } }

// ===========================================================================

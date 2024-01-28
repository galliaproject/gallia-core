package gallia
package plans

import dag._

// ===========================================================================
object ChainDataRun {

  def apply(missingInputs: Map[RootId, NDT])(atomNodes: AtomNodes): NDT = {
    var latest: NDT = null // using foldLeft is tricky because of missingInputs

    atomNodes
      .values
      .foreach { atomNode =>
        latest =
          atomNode.process(
            input = DataInput.SingleInput(latest))(
            missingInputs.apply) }

    latest } }

// ===========================================================================

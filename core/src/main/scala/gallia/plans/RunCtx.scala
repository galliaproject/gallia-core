package gallia
package plans

import aptus.{Seq_, String_}

// ===========================================================================
private case class RunCtx[$Data](
    nodeId   : NodeId,
    nodeAtom : Atom,    
    debug    : AtomNodeDebugging,
    throwable: Throwable,
    inputData: InputData) {

  // ===========================================================================
  override def toString: String = formatDefault

    // ---------------------------------------------------------------------------
    def formatDefault: String = // quick and dirty for now, to help debug...
      Seq(
        // have those twice as the rest can be big
        "message"    .padRight(12, ' ') -> throwable.getMessage,

        // ---------------------------------------------------------------------------
        "node.parent".padRight(12, ' ') -> debug.parentId,
        "node.id"    .padRight(12, ' ') -> nodeId,
        "node.atom"  .padRight(12, ' ') -> nodeAtom,
          "afferent schema(s):" -> debug.ctx.formatDebugAfferents,
            "example input data:" -> inputData.formatDebug.sectionAllOff.newline, // TODO: may not show the relevant object if Objs... (see t210114111539) - hence "example" for now
          "efferent schema:"    -> debug.ctx.formatDebugEfferent,

        // ---------------------------------------------------------------------------
        // have those twice as the rest can be big
        "message".padRight(12, ' ') -> throwable.getMessage,
        "origin" .padRight(12, ' ') -> debug.origin.formatDefault)
      .map { case (key, value) => s"${key}\t${value}" }
      .joinln
      .sectionAllOff(2)
      .prepend("\n")
}

// ===========================================================================

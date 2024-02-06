package gallia
package oswo

import aptus._

// ===========================================================================
/** reflective compilation for scala 2 */
object RuntimeCompilationScala2 extends RuntimeCompilation {
  private lazy val Toolbox =
    scala.reflect.runtime.currentMirror
      .pipe(scala.tools.reflect.ToolBox)
      .mkToolBox()

  // ---------------------------------------------------------------------------
  def warmUp(): Unit = { println("..."); Toolbox.p } // FIXME

  // ===========================================================================
  /** source shouldn't contain package statements (TODO: why?) */
  def objectInstance[A](objectDefinition: SourceString, objectName: Name /* TODO: parse source to extract name rather? */): A = {
    val code = objectDefinition.newline(objectName) /* because must return an instance */

    // look into https://stackoverflow.com/questions/39137175/dynamically-compiling-scala-class-files-at-runtime-in-scala-2-11/39139732
    		// and https://stackoverflow.com/questions/21972132/creating-serializable-objects-from-scala-source-code-at-runtime?rq=1

    val tree        : Toolbox.u.Tree = Toolbox.parse /* to tree */(code)
    val anyProducer : () => Any      = Toolbox.compile(tree)

    // TODO: how to write class file?

    anyProducer.apply.asInstanceOf[A]
  }

}

// ===========================================================================

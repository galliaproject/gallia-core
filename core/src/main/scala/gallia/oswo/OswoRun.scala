package gallia
package oswo

import aptus._
import source._

// ===========================================================================
object OswoRun
    extends RuntimeCompilationProvider /* scala version specific */ {
  private val FinalObjectName = "All"

  // ---------------------------------------------------------------------------
  def apply(head: heads.Head[_]) = {
    // populates gallia.finalCode
    head.end().runToNdt()

    // ---------------------------------------------------------------------------
    val code = formatCode(FinalObjectName)(gallia.finalCode)
      .tap { _.format.writeFileContent("/tmp/code.scala") }

    // ---------------------------------------------------------------------------
    runtimeCompilation
      .objectInstance[gallia.oswo.OptimRunner](code.format, FinalObjectName)
      .run()
      .tap { _ =>
        val parentDir = "/home/tony/scl/gallia/gallia-core/core/src/main/scala/gallia/oswo/"

        if (parentDir.path.isDir()) {
          writeOutputFile(code)( // just for reference
            output = s"$parentDir/OswoGenerated.scala") } } }

  // ---------------------------------------------------------------------------
  private def writeOutputFile(code: SourceCode)(output: aptus.FilePath) =
    code
      .format
      .prepend(
        Seq(
            "package gallia",
            "package oswo",
            "package generated")
          .joinln.newline.newline)
    .writeFileContent(output)

  // ===========================================================================
  import source.SourceFluentBuilders._

  // ---------------------------------------------------------------------------
  private def formatCode(name: Name)(body: String): SourceCode =
    _Object(_Name(name), fullName(classOf[gallia.oswo.OptimRunner]).in.list,
      // TODO: use macro to extract method name rather: mymacro[gallia.oswo.MyApp](body: Source)
      ().methodDefinition("run").returnAny.body(body))

}

// ===========================================================================

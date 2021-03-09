package gallia

import aptus.{Anything_, Seq_, String_}
import aptus.Unit_
import aptus.FileName

// ===========================================================================
case class CallSite(
      sub      : Option[SubCallSite],
      fullTrace: List[StackTraceElement]) {

    override def toString: String = formatDefault
      def formatDefault: String = formatLines.joinln
        def formatLines: Seq[String] = sub.flatMap(_.formatPathOpt).toSeq ++ Seq(sub.toString, fullTrace.section)

    def formatSuccinct: String = sub.flatMap(_.formatPathOpt).getOrElse(fullTrace.joinln.sectionAllOff("?"))
  }

  // ===========================================================================
  case class SubCallSite(
        fileName     : FileName,      // eg         "MyOrigin.scala"
        fullClassName: String,        // eg "foo.bar.MyOrigin$"
        line         : Int,           // eg 15
        filePathOpt  : Option[String] // eg "/path/to/foo/bar/MyOrigin$.class(MyOrigin.scala:15)"; meant to be clickable in IDE
      ) {
    def formatPathOpt: Option[String] =
      filePathOpt
        .map(filePath =>
          s"${filePath}(${fileName}:${line})\n")
  }

  // ===========================================================================
  object CallSite {

      private lazy val SystemClassLoader = java.lang.ClassLoader.getSystemClassLoader

      // ---------------------------------------------------------------------------
      private val Prefices = Seq(
          "gallia.",
          "aptus.",
          "scala.",
          "java.",
          "sun.reflect.")

      // ---------------------------------------------------------------------------
      //FIXME: pretty hacky...
      //TODO: t210308145846 - look into https://github.com/com-lihaoyi/sourcecode
      def generate(): CallSite = {
        val elems = ().reflect.stackTrace()

        elems
          .dropWhile(elem =>
            Prefices.exists(elem.getClassName.startsWith) ||
            elem.getLineNumber < 0)
          .headOption
          .map { elem =>
            SubCallSite(
              elem.getFileName,
              elem.getClassName,
              elem.getLineNumber,
              filePathOpt(elem)) }
            .thn(CallSite(_, elems))
      }

      // ---------------------------------------------------------------------------
      private def filePathOpt(elem: StackTraceElement): Option[String] = // TODO: t210115152435 - try from jar
        elem
          .getClassName
          .replace(".", "/")
          .append(".class")
          .thn(SystemClassLoader.getResource)
          .thn(Option.apply)
          .map(_.toExternalForm.replaceAll("^file:", ""))

  }

// ===========================================================================

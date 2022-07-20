package gallia
package data
package multiple

import aptus.{Seq_, String_}

// ===========================================================================
trait ObjsOut { self: Objs =>

  @Scalability
  def formatDefault: String = {
    (values match {
      case x: streamer.IteratorStreamer[Obj] =>
        x.formatEither match {
          case Left (consumedOrExited) => consumedOrExited
          case Right(r) => r  .pipe { list => list.map(_.formatDefault).section(s"#${list.size}") } }
      case x => toListAndTrash.pipe { list => list.map(_.formatDefault).section(s"#${list.size}") } }) }

  // ---------------------------------------------------------------------------
  @Scalability // TODO: use Writer
  def formatPrettyJson : String = toListAndTrash.map(_.formatPrettyJson .sectionAllOff).mkString("[\n", ",", "\n]")
  def formatCompactJson: String = toListAndTrash.map(_.formatCompactJson)              .mkString("["  , ",",   "]")

  // TODO: print, ...

  // ===========================================================================
  def naiveFormatArrayLines: Iterator[String] = {
    val itr = consumeSelfClosing.map(_.formatCompactJson)

    if (itr.isEmpty) Iterator("[]")
    else {
      val first = itr.next

      Iterator("[") ++
        Iterator(first.prepend("  ")) ++
        itr.map(     _.prepend(", ")) ++
      Iterator("]")
    }
  }
}

// ===========================================================================

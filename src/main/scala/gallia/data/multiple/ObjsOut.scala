package gallia.data.multiple

import aptus.{Anything_, String_, Seq_}

// ===========================================================================
trait ObjsOut { self: Objs =>

  @gallia.Scalability
  def formatDefault: String = toListAndTrash.thn { list => list.map(_.formatDefault).section(s"#${list.size}") }

  // ---------------------------------------------------------------------------
  @gallia.Scalability // TODO: use Writer
  def formatPrettyJson : String = toListAndTrash.map(_.formatPrettyJson .sectionAllOff).mkString("[\n", ",", "\n]")
  def formatCompactJson: String = toListAndTrash.map(_.formatCompactJson)              .mkString("["  , ",",   "]")

  // TODO: print, ...

  // ===========================================================================
  def naiveFormatArrayLines: Iterator[String] = {
    val itr = consume.map(_.formatCompactJson)

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

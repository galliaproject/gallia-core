package gallia
package vldt

import aptus.{Anything_, String_, Seq_}

// ===========================================================================
case class Parent(values: Seq[String]) { // TODO: use location with 0-index?
    def append(x: String) = Parent(values :+ x)

    override def toString: String = formatDefault
      def formatDefault: String = if (values.isEmpty) "<root>" else values.map(_.escapeQuotes.quote).mkString("->")
  }

  // ---------------------------------------------------------------------------
  object Parent {
    val Root = Parent(Seq())
  }

// ===========================================================================
case class Location private (values: Seq[LocationEntry] = Nil) {
      def addKey  (key  : SKey): Location = Location(values :+ LocationEntry(key.symbol, None))
      def addKey  (key  :  Key): Location = Location(values :+ LocationEntry(key       , None))
      def addIndex(index: Int): Location = Location(values.init :+ values.last.addIndex(index))

      // ---------------------------------------------------------------------------
      override def toString: String = formatDefault
        def formatDefault: String =
          values.map(_.formatDefault).join(" |> ")
    }

    // ---------------------------------------------------------------------------
    object Location { def Root = Location(Nil) }

  // ===========================================================================
  case class LocationEntry(key: Key, index: Option[Int] = None) {
    override def toString: String = formatDefault

      def formatDefault: String =
        key
          .name /* TODO: truncate? */
          .escapeQuotes
          .quote
          .append(
            index
              .map { i => s"[${index}]" }
              .getOrElse("") )

    // ---------------------------------------------------------------------------
    def addIndex(value: Int): LocationEntry =
      this
        .assert(_.index.isEmpty)
        .copy(index = Some(value))
  }

// ===========================================================================

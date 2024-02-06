package gallia
package oswo
package source

import aptus._

// ===========================================================================
// TODO: consolidate with Domain
object Domain0 {
  type Tipe2 = String
  import Utils._

  // ---------------------------------------------------------------------------
  sealed trait Tipe { def formatSource: String }

    // ===========================================================================
    case class __Basic(fullName: String) extends Tipe {
      def formatSource: String = fullName.replace("scala.", "") }

    // ---------------------------------------------------------------------------
    case class __Class(name: String, fields: Seq[__Field]) extends Tipe {
      require(name  .nonEmpty)
      require(fields.nonEmpty)

      // ---------------------------------------------------------------------------
      def formatSource =
        s"case class ${sanitize(name)}(${fields.map(_.format(max).append(",")).joinln.sectionAllOff}\n)"

      // ---------------------------------------------------------------------------
      private def max: Int = fields.map(_.name.pipe(sanitize).size).max }

  // ===========================================================================
  case class __Field(name: String, tipe: Tipe2) {
    require(name.nonEmpty)

    def format(x: Int): String = s"${sanitize(name).padRight(x, ' ')}: ${tipe/*.format*/}" }

  // ===========================================================================
  object Utils {

    def sanitize(value: String): String = {
      import aptus.aptutils.CharUtils._

      val simple =
        (AlphaSet.contains(value.head) || value.head == '_') &&
        value.tail.forall(AlphaNumericalWithUnderscoreSet.contains)

      if (simple) value else s"`${value}`"
    }
  }
}

// ===========================================================================

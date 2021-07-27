package gallia.io.in

import aptus.Name
import aptus.QueryString

// ===========================================================================
sealed trait ReadQuerying { def query: QueryString } // TODO: t210114202848 - validate

  // ---------------------------------------------------------------------------
  object ReadQuerying {
    case class All  (target: Name  ) extends ReadQuerying { val query = s"SELECT * from ${target}" /* TODO: t210114145431 - safe quoting + injection */ }
    case class Query(value : String) extends ReadQuerying { val query = value }
  }

// ===========================================================================

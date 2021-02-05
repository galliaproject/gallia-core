package gallia.io.in

import aptus.Name

// ===========================================================================
sealed trait ReadQuerying

  // ---------------------------------------------------------------------------
  object ReadQuerying {
    case class All  (target: Name  ) extends ReadQuerying
    case class Query(value : String) extends ReadQuerying
  }

// ===========================================================================

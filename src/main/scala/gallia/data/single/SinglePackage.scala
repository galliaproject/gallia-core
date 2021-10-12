package gallia
package data

// ===========================================================================
package object single
    extends       Aliases
    with    DomainAliases {

  case class RetainMapping(data: Map[Key, Option[gallia.KPathz]]) // TODO: t201215121718 - need more than one level for efficiency
}

// ===========================================================================
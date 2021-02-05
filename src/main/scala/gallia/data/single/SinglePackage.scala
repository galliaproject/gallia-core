package gallia.data

// ===========================================================================
package object single
    extends gallia.Aliases
    with    gallia.DomainAliases {

  case class RetainMapping(data: Map[Key, Option[gallia.KPathz]]) // TODO: t201215121718 - need more than one level for efficiency
}

// ===========================================================================
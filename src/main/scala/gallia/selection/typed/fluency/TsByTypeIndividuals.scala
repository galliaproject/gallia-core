package gallia
package selection
package typed.fluency

// ===========================================================================
@TypeMatching // very boilerplatey; TODO: t210124092716 - codegen (very boilerplaty)
object TsByTypeIndividuals {
  import untyped.fluency.UtsIndividuals.{HasIfType, HasIfTypeRecursively}

  // ---------------------------------------------------------------------------
  // by type; intentionally leaving out Nes/Pes equivalent, and many of the lesser used types

  trait HasAllOneStringKeys  { def allStringKeys  = new One_[String ](new HasIfType {}.ifType[String ]) }
  trait HasAllOneIntKeys     { def allIntKeys     = new One_[Int    ](new HasIfType {}.ifType[Int    ]) }
  trait HasAllOneDoubleKeys  { def allDoubleKeys  = new One_[Double ](new HasIfType {}.ifType[Double ]) }
  trait HasAllOneBooleanKeys { def allBooleanKeys = new One_[Boolean](new HasIfType {}.ifType[Boolean]) }

  // ---------------------------------------------------------------------------
  trait HasAllOptStringKeys  { def allOptionalStringKeys  = new Opt_[String  ](new HasIfType {}.ifType[Option[String ]]) }
  trait HasAllOptIntKeys     { def allOptionalIntKeys     = new Opt_[Int     ](new HasIfType {}.ifType[Option[Int    ]]) }
  trait HasAllOptDoubleKeys  { def allOptionalDoubleKeys  = new Opt_[Double  ](new HasIfType {}.ifType[Option[Double ]]) }
  trait HasAllOptBooleanKeys { def allOptionalBooleanKeys = new Opt_[Boolean ](new HasIfType {}.ifType[Option[Boolean]]) }

  // ===========================================================================
  // by type recursively

  trait HasAllOneStringPaths  { def allStringPaths  = new One_[String ](new HasIfTypeRecursively {}.ifTypeRecursively[String ]) }
  trait HasAllOneIntPaths     { def allIntPaths     = new One_[Int    ](new HasIfTypeRecursively {}.ifTypeRecursively[Int    ]) }
  trait HasAllOneDoublePaths  { def allDoublePaths  = new One_[Double ](new HasIfTypeRecursively {}.ifTypeRecursively[Double ]) }
  trait HasAllOneBooleanPaths { def allBooleanPaths = new One_[Boolean](new HasIfTypeRecursively {}.ifTypeRecursively[Boolean]) }

  // ---------------------------------------------------------------------------
  trait HasAllOptStringPaths  { def allOptionalStringPaths  = new Opt_[String  ](new HasIfTypeRecursively {}.ifTypeRecursively[Option[String ]]) }
  trait HasAllOptIntPaths     { def allOptionalIntPaths     = new Opt_[Int     ](new HasIfTypeRecursively {}.ifTypeRecursively[Option[Int    ]]) }
  trait HasAllOptDoublePaths  { def allOptionalDoublePaths  = new Opt_[Double  ](new HasIfTypeRecursively {}.ifTypeRecursively[Option[Double ]]) }
  trait HasAllOptBooleanPaths { def allOptionalBooleanPaths = new Opt_[Boolean ](new HasIfTypeRecursively {}.ifTypeRecursively[Option[Boolean]]) }

}

// ===========================================================================

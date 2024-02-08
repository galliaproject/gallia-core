package gallia
package reflect

// ===========================================================================
object TypeNodeBuiltIns { import FullNameBuiltIns._

  val JavaString  = TypeNode.trivial(_JavaString) // same for scala.Predef.String
  val ScalaString = JavaString // just an alias, unlias eg scala.Int
  val String      = JavaString // for convenience

  // ---------------------------------------------------------------------------
  val ScalaAny       = TypeNode.trivial(_ScalaAny)

  // ---------------------------------------------------------------------------
  val ScalaBoolean   = TypeNode.trivial(_ScalaBoolean)
  val ScalaInt       = TypeNode.trivial(_ScalaInt)
  val ScalaDouble    = TypeNode.trivial(_ScalaDouble)

  // ---------------------------------------------------------------------------
  val ScalaByte    = TypeNode.trivial(_ScalaByte)
  val ScalaShort   = TypeNode.trivial(_ScalaShort)
  val ScalaLong    = TypeNode.trivial(_ScalaLong)

  val ScalaFloat   = TypeNode.trivial(_ScalaFloat)

  // ---------------------------------------------------------------------------
  val ScalaMathBigInt     = TypeNode.trivial(_ScalaMathBigInt)
  val ScalaMathBigDecimal = TypeNode.trivial(_ScalaMathBigDecimal)

  // ---------------------------------------------------------------------------
  val JavaTimeLocalDate      = TypeNode.trivial(_JavaTimeLocalDate)
  val JavaTimeLocalTime      = TypeNode.trivial(_JavaTimeLocalTime)
  val JavaTimeLocalDateTime  = TypeNode.trivial(_JavaTimeLocalDateTime)
  val JavaTimeOffsetDateTime = TypeNode.trivial(_JavaTimeOffsetDateTime)
  val JavaTimeZonedDateTime  = TypeNode.trivial(_JavaTimeZonedDateTime)
  val JavaTimeInstant        = TypeNode.trivial(_JavaTimeInstant)

  // ---------------------------------------------------------------------------
  val JavaNioByteByffer      = TypeNode.trivial(_JavaNioByteByffer).bytes(value = true)

  // ---------------------------------------------------------------------------
  val GalliaEnumValue        = TypeNode.trivial(_GalliaEnumValue).galliaEnumValue(value = true)
  val GalliaAObj             = TypeNode.trivial(_GalliaAObj)
  val GalliaBObj             = TypeNode.trivial(_GalliaBObj)
  val GalliaWhatever         = TypeNode.trivial(_GalliaWhatever)

  // ---------------------------------------------------------------------------
  def scalaOption(typeArg: TypeNode) = TypeNode(TypeLeaf.ScalaOption, List(typeArg))
  def scalaSeq   (typeArg: TypeNode) = TypeNode(TypeLeaf.ScalaSeq,    List(typeArg)) }

// ===========================================================================
package gallia.io
package in

import gallia._

// ===========================================================================
class InputUStringDrivenFluency(val conf: InputUStringDrivenConf) // TODO: still needed?
      extends FluencyBase[InputUStringDrivenFluency, InputUStringDrivenConf](new InputUStringDrivenFluency(_))

      with    HasCharsetFluency    [InputUStringDrivenFluency]
      with    HasCompressionFluency[InputUStringDrivenFluency]

      with    CanSetSchemaFluency[InputUStringDrivenFluency]

      with    CanProjectFluency[InputUStringDrivenFluency]
      with    CanQueryFluency  [InputUStringDrivenFluency]

      with    EndReadUFluency {
    val self = this

    // ===========================================================================
    // boilerplate:
    override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
    override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))

    override def explicitSchema(c: Cls) = conf.copy(schemaProvider = ExplicitSchema(c))
    override def inferSchema            = conf.copy(schemaProvider = InferSchema)

    override private[io] def project(projectionOpt: Option[ReadProjection]) = conf.copy(projectionOpt = projectionOpt)

    override protected[io] def querying(value: ReadQuerying) = conf.copy(queryingOpt = Some(value))
  }

  // ===========================================================================
  class InputZStringDrivenFluency(val conf: InputZStringDrivenConf) // TODO: still needed?
      extends FluencyBase[InputZStringDrivenFluency, InputZStringDrivenConf](new InputZStringDrivenFluency(_))

      with    HasCharsetFluency    [InputZStringDrivenFluency]
      with    HasCompressionFluency[InputZStringDrivenFluency]

      with    CanSetMemoryModeFluency[InputZStringDrivenFluency]

      with    CanSetSchemaFluency[InputZStringDrivenFluency]

      with    CanProjectFluency[InputZStringDrivenFluency]
      with    CanQueryFluency  [InputZStringDrivenFluency]

      with    EndReadZFluency {
    val self = this

    // ===========================================================================
    // boilerplate:
    override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
    override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))

    // ---------------------------------------------------------------------------
    override def iteratorMode = conf.copy(inMemoryMode  = false)

    // ---------------------------------------------------------------------------
    override def explicitSchema(c: Cls) = conf.copy(schemaProvider = ExplicitSchema(c))
    override def inferSchema            = conf.copy(schemaProvider = InferSchema)

    // ---------------------------------------------------------------------------
    override private[io] def project(projectionOpt: Option[ReadProjection]) = conf.copy(projectionOpt = projectionOpt)

    override protected[io] def querying(value: ReadQuerying) = conf.copy(queryingOpt = Some(value))
  }

// ===========================================================================

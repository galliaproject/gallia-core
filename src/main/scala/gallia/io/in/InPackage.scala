package gallia
package io

// ===========================================================================
package object in extends gallia._io with gallia.__io {

  /** can be either content or uri, eg """{"foo": 1}""" or "http://foo", ... */
  type InputString = String

  // ===========================================================================
  def startU(value: String) = new StartReadUFluency(value)
  def startZ(value: String) = new StartReadZFluency(value)

  // ===========================================================================
  trait EndReadUFluency { protected[io] val conf: InputConfU }
  trait EndReadZFluency { protected[io] val conf: InputConfZ }

  // ===========================================================================
  val ExplicitSchema = OtherSchemaProvider.ExplicitSchema
  val InferSchema    = OtherSchemaProvider.InferSchema

  // ===========================================================================
  import gallia.data.multiple.streamer.Streamer
  type LinesPreprocessing = Streamer[String] => Streamer[String] // see t210202112203
}

// ===========================================================================

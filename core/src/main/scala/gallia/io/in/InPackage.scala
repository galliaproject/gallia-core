package gallia
package io

import aptus.Line
import streamer.Streamer

// ===========================================================================
package object in extends _io with __io {

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
  type LinesPreprocessing = Streamer[Line] => Streamer[Line] // see t210202112203

  // ===========================================================================
  val DefaultStreamMode = true /* = in memory */
}

// ===========================================================================

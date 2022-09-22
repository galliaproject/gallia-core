package gallia

import aptus.Line
import streamer._
import io.in.InputUrlLike

// ===========================================================================
private[gallia] final class SparkRddHack(
   val streamRddLines: InputUrlLike      => /* Rdd */ Streamer[Line],
   val toRddStreamer : ViewStreamer[Obj] => /* Rdd */ Streamer[Obj])

// ===========================================================================
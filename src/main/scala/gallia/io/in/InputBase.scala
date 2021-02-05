package gallia.io
package in

import aptus.{Anything_, String_}
import gallia._
import gallia.heads.Head
import gallia.data.json.JsonParsing

// ===========================================================================
private[io] trait InputBase { val inputString: InputString }

  // ===========================================================================
  // TODO: t210113204047: compression and so on (reproduce config like for heads)
  trait ReadObj extends InputBase {
    def readObj(): Obj =
      InputStringType
        .parse(inputString)
         match {
          case InputStringType.JsonObject => JsonParsing.parseObject(inputString)
          case _                          => inputString.readFileContent.thn(JsonParsing.parseObject) } }

  // ---------------------------------------------------------------------------
  trait StreamObjs extends InputBase {
    def streamObjs(): Objs =
      InputStringType
        .parse(inputString)
         match {
          case InputStringType.JsonObject  => inputString.splitBy("\n").filterNot(_.trim.isEmpty).map(JsonParsing.parseObject).thn(Objs.from)
          case InputStringType.JsonArray   => JsonParsing.parseArray(inputString).thn(Objs.from)
          case InputStringType.Indirection =>
            val content = inputString.readFileContent

           (if (InputStringType.parse(content).isJsonObject) JsonParsing.parseObject(content).thn(Objs.splat(_))
            else                                             JsonParsing.parseArray (content).thn(Objs.from))
          }
  }

  // ===========================================================================
  trait ReadHead extends InputBase {
    def read()                                       : HeadU = read(_.inputDriven)

    def read[T: WTT]                                 : HeadU = read(_.schema[T]) // TODO: nicer error due to invoking .read.
    def read(field1: Fld, more: Fld*)                : HeadU = read(_.schema(field1, more:_*))
    def read(schemaFilePath: String)                 : HeadU = read(_.schema(schemaFilePath))

    def read(f: StartReadUFluency => EndReadUFluency): HeadU =
      in.startU(inputString)
        .thn(f)
        .conf
        .actionU
        .thn(Head.inputU)
  }

  // ===========================================================================
  trait StreamHead extends InputBase {
    def stream()                                       : HeadZ = stream(_.inputDriven) // TODO: nicer error due to invoking .read.
    def streamContainer(container: String)             : HeadZ = stream(_.allFrom(container))

    def stream[T: WTT]                                 : HeadZ = stream(_.schema[T])
    def stream(field1: Fld, more: Fld*)                : HeadZ = stream(_.schema(field1, more:_*))
    def stream(schemaFilePath: String)                 : HeadZ = stream(_.schema(schemaFilePath))

    def stream(f: StartReadZFluency => EndReadZFluency): HeadZ =
        inputString
          .thn(in.startZ)
          .thn(f)
          .conf
          .actionZ
          .thn(Head.inputZ)
  }

// ===========================================================================


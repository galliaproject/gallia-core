package gallia
package io
package in

import aptus.String_
import aptus.{TableName, QueryString}

import heads.Head
import data.json.JsonParsing

// ===========================================================================
private[io] trait InputStringBase { val inputString: InputString }

  // ===========================================================================
  // TODO: t210113204047: compression and so on (reproduce config like for heads)
  trait ReadObjFromString extends InputStringBase {
    def readObj(): Obj =
      InputStringType
        .parse(inputString)
         match {
          case InputStringType.JsonObject => JsonParsing.parseObject(inputString)
          case _                          => inputString.readFileContent.pipe(JsonParsing.parseObject) } }

  // ---------------------------------------------------------------------------
  trait StreamObjsFromString extends InputStringBase {
    def streamObjs(): Objs =
      InputStringType
        .parse(inputString)
         match {
          case InputStringType.JsonObject  => inputString.splitBy("\n").filterNot(_.trim.isEmpty).map(JsonParsing.parseObject).pipe(Objs.from)
          case InputStringType.JsonArray   => JsonParsing.parseArray(inputString).pipe(Objs.from)
          case InputStringType.Indirection =>
            val content = inputString.readFileContent

           (if (InputStringType.parse(content).isJsonObject) JsonParsing.parseObject(content).pipe(Objs.splat(_))
            else                                             JsonParsing.parseArray (content).pipe(Objs.from))
          }
  }

  // ===========================================================================
  trait StreamObjsFromIterable[T] {
    val values: Iterable[T]
    
    // ---------------------------------------------------------------------------
    def toHead(f: T => BObj): HeadZ = values.toSeq.map(f).pipe(BObjs(_))              
  }
  
  // ===========================================================================
  trait ReadHeadFromString extends InputStringBase {    
    def readContent(): HeadU = bobj(_content -> inputString)

    // ---------------------------------------------------------------------------
    def read()                                       : HeadU = read(_.inputDriven)

    def read[T: WTT]                                 : HeadU = read(_.schema[T]) // TODO: nicer error due to invoking .read.
    def read(field1: Fld, more: Fld*)                : HeadU = read(_.schema(field1, more:_*))
    def read(schemaFilePath: String)                 : HeadU = read(_.schema(schemaFilePath))

    def read(f: StartReadUFluency => EndReadUFluency): HeadU =
      in.startU(inputString)
        .pipe(f)
        .conf
        .actionU
        .pipe(Head.inputU)
  }

  // ===========================================================================
  trait StreamHeadFromString extends InputStringBase {
    def streamLines(): HeadZ = inputString.splitBy("\n").pipe(lines => gallia.domain.BObjs(lines.map(line => bobj(_line -> line))))

    // ---------------------------------------------------------------------------
    def stream()                                       : HeadZ = stream(_.inputDriven) // TODO: nicer error due to invoking .read.
    def streamContainer(container: String)             : HeadZ = stream(_.allFrom(container))

    def stream[T: WTT]                                 : HeadZ = stream(_.schema[T])
    def stream(field1: Fld, more: Fld*)                : HeadZ = stream(_.schema(field1, more:_*))
    def stream(schemaFilePath: String)                 : HeadZ = stream(_.schema(schemaFilePath))

    def stream(f: StartReadZFluency => EndReadZFluency): HeadZ =
        inputString
          .pipe(in.startZ)
          .pipe(f)
          .conf
          .actionZ
          .pipe(Head.inputZ)
  }

  // ===========================================================================
  trait StreamConnection {
    val connection: java.sql.Connection

    // ---------------------------------------------------------------------------
    def streamTable(table: TableName)  : HeadZ = gallia.actions.in.JdbcInputZ2(connection, ReadQuerying.All(table))  .pipe(heads.Head.inputZ) 
    def streamQuery(query: QueryString): HeadZ = gallia.actions.in.JdbcInputZ2(connection, ReadQuerying.Query(query)).pipe(heads.Head.inputZ)
  }
  
// ===========================================================================


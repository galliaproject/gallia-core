package gallia
package atoms
package utils

import aptus._
import aptus.aptjson.GsonFormatter

import domain.GroupingPair._
import meta.PNF
import data.json.{GalliaToGsonData, GsonToGalliaData, GsonParsing}
import data.multiple.streamer.{Streamer, IteratorStreamer}

// ===========================================================================
trait GalliaSpillingSer { type Row; def _serialize  (pair: Row) : Line }
trait GalliaSpillingDes { type Row; def _deserialize(line: Line): Row  }

// ===========================================================================
trait GalliaSpillingSerDes extends GalliaSpillingSer with GalliaSpillingDes {
    type Row

    // ---------------------------------------------------------------------------
    def apply
          (lines: Streamer[Row])
          (gnuSort: CloseabledIterator[Line] => CloseabledIterator[Line])
        : IteratorStreamer[Row] =
      lines
        .asInstanceOfIteratorStreamer
        ._map  (_serialize)
        ._alter(gnuSort)
        ._map  (_deserialize)
  }

  // ===========================================================================
  trait GalliaSpillingPairSerDes[$Ctx <: domain.GroupingPair, K, V /* typically Obj or Vle */] extends GalliaSpillingSerDes {
      type Row = (Option[K], Option[V])
      val ctx: $Ctx

      // ---------------------------------------------------------------------------
      final override def   _serialize(row : Row) : Line = serialize  (row)
      final override def _deserialize(line: Line): Row  = deserialize(line)

      // ---------------------------------------------------------------------------
      protected def   serialize(pair: (Option[K], Option[V])): Line
      protected def deserialize(line: Line)                  : (Option[K], Option[V])
    }

// ===========================================================================
class SpillingSortSerDes(pnfs: Seq[PNF], c: Cls) extends GalliaSpillingSerDes { import SpillingSortSerDes.formatGnuSortKey
      type Row = Obj

      // ---------------------------------------------------------------------------
      override def _serialize(o: Obj): Line =
        // t220624170410 - quite wasteful as it is, we would need a space efficient serialization that remains textual in nature (at least so gnusort can process it)
        (pnfs.map(formatGnuSortKey(o)) ++
            Some(GalliaToGsonData.convertRecursively(c)(o).pipe(GsonFormatter.compact)))
          .mkString("\t")

      // ---------------------------------------------------------------------------
      override def _deserialize(line: Line): Obj = // same for all Ns since we discard sort key(s) anyway
        line
          .splitBy("\t")
          .last // only keep last (value)
          .pipe(GsonParsing.parseObject)
          .pipe(GsonToGalliaData.convertRecursively(c) /* 220623171056 */)
    }

    // ===========================================================================
    object SpillingSortSerDes {

      private def formatGnuSortKey(o: Obj)(pnf: PNF): String =
        if (!pnf.optional) o.forcePath  (pnf.path).pipe(GalliaToGsonData.formatGsonJsonElement(debug = pnf.formatDefault)(pnf.info))
        else               o.attemptPath(pnf.path).map (GalliaToGsonData.formatGsonJsonElement(debug = pnf.formatDefault)(pnf.info)).getOrElse("")

    }

  // ===========================================================================
  class SpillingSortAllSerDes(c: Cls) extends GalliaSpillingSerDes {
    type Row = Obj

    // ---------------------------------------------------------------------------
    override def _serialize(o: Obj): Line =
      o .pipe(GalliaToGsonData.convertRecursively(c))
        .pipe(GsonFormatter.compact)

    // ---------------------------------------------------------------------------
    override def _deserialize(line: Line): Obj =
      line
        .pipe(GsonParsing.parseObject)
        .pipe(GsonToGalliaData.convertRecursively(c) /* 220623171056 */)
  }

  // ===========================================================================
  class SpillingGroupBy11SerDes(override val ctx: GroupingPair11) extends GalliaSpillingPairSerDes[GroupingPair11, Vle, Vle] {

    override def serialize(pair: (OVle, OVle)): aptus.Line = {
      val (keyOpt, valOpt) = pair

      val keyString  : String = keyOpt.map(GalliaToGsonData.formatGsonJsonElement(ctx.grouper)).getOrElse("")
      val valueString: String = valOpt.map(GalliaToGsonData.formatGsonJsonElement(ctx.groupee)).getOrElse("")

      s"${keyString}\t${valueString}" }

    // ---------------------------------------------------------------------------
    override def deserialize(line: Line): (OVle, OVle) =
      line
        .span(_ != '\t')
        .pipe { case (keyString, valueString) =>
          val key  : OVle = keyString                      .inNoneIf(_.isEmpty).map(GsonToGalliaData.parseGsonJsonElement(ctx.grouper))
          val value: OVle = valueString.tail /* drop tab */.inNoneIf(_.isEmpty).map(GsonToGalliaData.parseGsonJsonElement(ctx.groupee))

          key -> value }
  }

  // ===========================================================================
  class SpillingGroupBy1NSerDes(override val ctx: GroupingPairN1) extends GalliaSpillingPairSerDes[GroupingPairN1, Obj, Vle] {

    override def serialize(pair: (OObj, OVle)): aptus.Line = {
      val (keyOpt, valOpt) = pair

      val keyString  : String = keyOpt.map(GalliaToGsonData.formatRecursively(c = ctx.groupers, _)).getOrElse("")
      val valueString: String = valOpt.map(GalliaToGsonData.formatGsonJsonElement(ctx.groupee)).getOrElse("")

      s"${keyString}\t${valueString}" }

    // ---------------------------------------------------------------------------
    override def deserialize(line: Line): (OObj, OVle) =
      line
        .span(_ != '\t')
        .pipe { case (keyString, valueString) =>
          val key  : OObj = keyString                      .inNoneIf(_.isEmpty).map(GsonToGalliaData.parseRecursively    (ctx.groupers, _))
          val value: OVle = valueString.tail /* drop tab */.inNoneIf(_.isEmpty).map(GsonToGalliaData.parseGsonJsonElement(ctx.groupee))

          key -> value }
  }


  // ===========================================================================
  class SpillingGroupByN1SerDes(override val ctx: GroupingPair1N) extends GalliaSpillingPairSerDes[GroupingPair1N, Vle, Obj] {

    override def serialize(pair: (OVle, OObj)): aptus.Line = {
      val (keyOpt, valOpt) = pair

      val keyString: String = keyOpt.map(GalliaToGsonData.formatGsonJsonElement(ctx.grouper))    .getOrElse("")
      val valString: String = valOpt.map(GalliaToGsonData.formatRecursively    (ctx.groupees, _)).getOrElse("")

      s"${keyString}\t${valString}" }

    // ---------------------------------------------------------------------------
    override def deserialize(line: Line): (OVle, OObj) =
      line
        .span(_ != '\t')
        .pipe { case (keyString, valString) =>

          val key  : OVle = keyString                    .inNoneIf(_.isEmpty).map(GsonToGalliaData.parseGsonJsonElement(ctx.grouper))
          val value: OObj = valString.tail /* drop tab */.inNoneIf(_.isEmpty).map(GsonToGalliaData.parseRecursively    (ctx.groupees, _))

          key -> value }
  }

  // ===========================================================================
  class SpillingGroupByNNSerDes(override val ctx: GroupingPairNN) extends GalliaSpillingPairSerDes[GroupingPairNN, Obj, Obj] {

    override def serialize(pair: (OObj, OObj)): aptus.Line = {
      val (keyOpt, valOpt) = pair

      val keyString: String = keyOpt.map(GalliaToGsonData.formatRecursively(ctx.groupers, _)).getOrElse("")
      val valString: String = valOpt.map(GalliaToGsonData.formatRecursively(ctx.groupees, _)).getOrElse("")

      s"${keyString}\t${valString}" }

    // ---------------------------------------------------------------------------
    override def deserialize(line: Line): (OObj, OObj) =
      line
        .span(_ != '\t')
        .pipe { case (keyString, valString) =>

          val key  : OObj = keyString                    .inNoneIf(_.isEmpty).map(GsonToGalliaData.parseRecursively(ctx.groupers, _))
          val value: OObj = valString.tail /* drop tab */.inNoneIf(_.isEmpty).map(GsonToGalliaData.parseRecursively(ctx.groupees, _))

          key -> value }

  }

  // ===========================================================================
  class SpillingJoinSerializer(groupee: Fld, c: Cls) extends GalliaSpillingSer { import SpillingJoinSerializer.SecondarySeparator
      type Row = (OVle, List[OObj])

      // ---------------------------------------------------------------------------
      override def _serialize(pair: Row): aptus.Line = {
        val (keyOpt, values) = pair

        val keyString: String = keyOpt      .map(GalliaToGsonData.formatGsonJsonElement(groupee)).getOrElse("")
        val valString: String = values.map(_.map(GalliaToGsonData.formatRecursively(c, _))       .getOrElse("")).mkString(SecondarySeparator)

        s"${keyString}\t${valString}"
      }
    }

    // ---------------------------------------------------------------------------
    object SpillingJoinSerializer {
      val SecondarySeparator = "\u0000" // TODO: disallow in data
    }

  // ===========================================================================
  class SpillingJoinDeserializer(leftGroupees: Cls, rightGroupees: Cls, rightGrouper: Key) extends GalliaSpillingDes { import SpillingJoinSerializer.SecondarySeparator
    type Row = (List[Obj], List[Obj])

    // ---------------------------------------------------------------------------
    override def _deserialize(line: Line): Row = {
      val (left, right) = line.span(_ != '\t')

      left .splitBy(SecondarySeparator).map(GsonToGalliaData.parseRecursively(leftGroupees,  _)).toList ->
      right.splitBy(SecondarySeparator).map(GsonToGalliaData.parseRecursively(rightGroupees, _)).toList.map(_.remove(rightGrouper)) }

  }

// ===========================================================================

package gallia
package inferring.table

import scala.collection.mutable

import aptus.{Seq_, String_}

import reflect.BasicType
import meta.Info

// ===========================================================================
class MutableValuesSubset(keys: Seq[Key], max: Int) {

  private val subsets = mutable.Map[Key, mutable.Set[String]]()
  init()

  // ---------------------------------------------------------------------------
  private def init() = {
    keys
      .foreach { key =>
        subsets += key -> mutable.Set[String]() } }

  // ===========================================================================
  def addValues(key: Key, values: Set[String]) = {
    val _values = subsets(key)

    if (_values.size <= max) {
      subsets += key -> (_values ++= values) }
  }

  // ---------------------------------------------------------------------------
  private def getValues(key: Key): Seq[String] = subsets(key).toList.sorted

  // ---------------------------------------------------------------------------
  def potentiallyUpdateInfo(key: Key, subInfo: Info): Info =
    if (subInfo.subInfo1.isMultiple) subInfo
    else {
      val values = subsets(key)

      if (values.size <= 2 &&
          BooleanDetector.isLikelyBoolean(values.toSet))
         subInfo.updateValueType(BasicType._Boolean)
       else
         subInfo
    }

  // ===========================================================================
  override def toString: String = formatDefault

    def formatDefault: String =
      keys
        .map { key =>
          s"${key.name.quote}: ${getValues(key)}" }
        .joinln
}

// ===========================================================================

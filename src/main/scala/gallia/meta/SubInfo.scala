package gallia
package meta

import aptus.String_

// ===========================================================================
case class SubInfo(
          multiple : Multiple,
          valueType: ValueType)
        extends HasSingleValueType {

    override def toString = formatDefault

      // ---------------------------------------------------------------------------
      def formatDefault: String =
        valueType match {
          case basic: BasicType =>
            if (multiple) s"${formatMultiple(multiple)}:${basic}"
            else          s"${basic}"
          case nesting: Cls => s"${formatMultiple(multiple)}\t${nesting.formatDefault.sectionAllOff}" }

    // ===========================================================================
    def transformValueType  (f: ValueType => ValueType): SubInfo = copy(valueType = f(this.valueType))
    def transformBasicType  (f: BasicType => BasicType): SubInfo = copy(valueType = f(this.valueType.leafOpt   .get))
    def transformNestedClass(f: Cls       => Cls      ): SubInfo = copy(valueType = f(this.valueType.nestingOpt.get))

    // ---------------------------------------------------------------------------
    def updateValueType(newValue: BasicType): SubInfo = transformValueType(_ => newValue)
    def updateValueType(newValue: Cls)      : SubInfo = transformValueType(_ => newValue)
    def updateValueType(newValue: ValueType): SubInfo = transformValueType(_ => newValue)

    // ---------------------------------------------------------------------------
    def updateMultiple(newValue: Multiple): SubInfo = copy(multiple = newValue)

    // ===========================================================================
    def isMultiple: Boolean =  multiple // for consistency
    def isSingle  : Boolean = !multiple

    // ---------------------------------------------------------------------------
    def toMultiple: SubInfo = copy(multiple = true)
    def toSingle  : SubInfo = copy(multiple = false)

    // ---------------------------------------------------------------------------
    def isEnmMatching(multiple: Multiple): Boolean = isEnm && this.multiple == multiple

    // ===========================================================================
    @PartialTypeMatching
      def toBoolean: SubInfo = copy(valueType = BasicType._Boolean)
      def toInt    : SubInfo = copy(valueType = BasicType._Int)
      def toDouble : SubInfo = copy(valueType = BasicType._Double)

    // ---------------------------------------------------------------------------
    def info1(optional: Optional): Info1 = Info1(optional, multiple, valueType)

    // ===========================================================================
    def valuePredicate(value: Any): Boolean = valueType.valuePredicate(if (multiple) value.asInstanceOf[Seq[_]].head else value)
  }

  // ===========================================================================
  object SubInfo {
    def single  (basic: BasicType.type => BasicType): SubInfo = SubInfo(_Single,   basic(BasicType))
    def multiple(basic: BasicType.type => BasicType): SubInfo = SubInfo(_Multiple, basic(BasicType))

    // ---------------------------------------------------------------------------
    def single  (c: Cls): SubInfo = SubInfo(_Single,   c)
    def multiple(c: Cls): SubInfo = SubInfo(_Multiple, c)

    // ---------------------------------------------------------------------------
    def string  : SubInfo = single  (_._String)
    def int     : SubInfo = single  (_._Int)
    def double  : SubInfo = single  (_._Double)
    def boolean : SubInfo = single  (_._Boolean)

    def strings : SubInfo = multiple(_._String)
    def ints    : SubInfo = multiple(_._Int)
    def doubles : SubInfo = multiple(_._Double)
    def booleans: SubInfo = multiple(_._Boolean)
  }

// ===========================================================================

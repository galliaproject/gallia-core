package gallia
package meta

import aptus.String_

// ===========================================================================
case class Info(
          multiple : Multiple,
          containee: Containee)
        extends HasSingleContainee {

    override def toString = formatDefault

      // ---------------------------------------------------------------------------
      def formatDefault: String =
        containee match {
          case basic: BasicType =>
            if (multiple) s"${formatMultiple(multiple)}:${basic}"
            else          s"${basic}"
          case nesting: Cls => s"${formatMultiple(multiple)}\t${nesting.formatDefault.sectionAllOff}" }

    // ===========================================================================
    def transformContainee  (f: Containee => Containee): Info = copy(containee = f(this.containee))
    def transformBasicType  (f: BasicType => BasicType): Info = copy(containee = f(this.containee.leafOpt   .get))
    def transformNestedClass(f: Cls       => Cls      ): Info = copy(containee = f(this.containee.nestingOpt.get))

    // ---------------------------------------------------------------------------
    def updateContainee(newValue: BasicType): Info = transformContainee(_ => newValue)
    def updateContainee(newValue: Cls)      : Info = transformContainee(_ => newValue)
    def updateContainee(newValue: Containee): Info = transformContainee(_ => newValue)

    // ---------------------------------------------------------------------------
    def updateMultiple(newValue: Multiple): Info = copy(multiple = newValue)

    // ===========================================================================
    def isMultiple: Boolean =  multiple // for consistency
    def isSingle  : Boolean = !multiple

    // ---------------------------------------------------------------------------
    def toMultiple: Info = copy(multiple = true)
    def toSingle  : Info = copy(multiple = false)

    // ---------------------------------------------------------------------------
    def isEnmMatching(multiple: Multiple): Boolean = isEnm && this.multiple == multiple

    // ===========================================================================
    @PartialTypeMatching
      def toBoolean: Info = copy(containee = BasicType._Boolean)
      def toInt    : Info = copy(containee = BasicType._Int)
      def toDouble : Info = copy(containee = BasicType._Double)

    // ---------------------------------------------------------------------------
    def ofnu(optional: Optional): Ofnu = Ofnu(optional, multiple, containee)
  }

  // ===========================================================================
  object Info {
    def single  (basic: BasicType.type => BasicType): Info = Info(_Single,   basic(BasicType))
    def multiple(basic: BasicType.type => BasicType): Info = Info(_Multiple, basic(BasicType))

    // ---------------------------------------------------------------------------
    def single  (c: Cls): Info = Info(_Single,   c)
    def multiple(c: Cls): Info = Info(_Multiple, c)

    // ---------------------------------------------------------------------------
    def string  : Info = single  (_._String)
    def int     : Info = single  (_._Int)
    def double  : Info = single  (_._Double)
    def boolean : Info = single  (_._Boolean)

    def strings : Info = multiple(_._String)
    def ints    : Info = multiple(_._Int)
    def doubles : Info = multiple(_._Double)
    def booleans: Info = multiple(_._Boolean)
  }

// ===========================================================================

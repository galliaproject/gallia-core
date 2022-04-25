package gallia

// ===========================================================================
trait Reserved {
  val _id    = Symbol("_id")

  val _group  = Symbol("_group")

  val _count_all              = Symbol("_count_all")
  val _count_present          = Symbol("_count_present")
  val _count_missing          = Symbol("_count_missing")
  val _count_distinct         = Symbol("_count_distinct")
  val _count_distinct_present = Symbol("_count_distinct_present")

  val _values = Symbol("_values")
  val _sum    = Symbol("_sum")
  val _mean   = Symbol("_mean")
  val _median = Symbol("_median")
  val _stdev  = Symbol("_stdev")
  val _stats  = Symbol("_stats")

    val _min   = Symbol("_min")
    val _max   = Symbol("_max")
    val _range = Symbol("_range")
    val _IQR   = Symbol("_IQR")

  val _left  = Symbol("_left")
  val _right = Symbol("_right")

  val _tmp  = Symbol("_tmp")
  val _tmp1 = Symbol("_tmp1")
  val _tmp2 = Symbol("_tmp2")
  val _tmp3 = Symbol("_tmp3")

  val _type = Symbol("_type")

  // ---------------------------------------------------------------------------
  val _vle    = Symbol("_vle")
  val _sole   = Symbol("_sole")

    //TODO: or always use _sole?
    val _array   = Symbol("_array") // typically JSON array;
    val _content = Symbol("_content")
    val _line    = Symbol("_line")
    val _row     = Symbol("_row")

  val _index = Symbol("_index")
  val _rank  = Symbol("_rank")

  val  _1 = Symbol("_1")
  val  _2 = Symbol("_2")
  val  _3 = Symbol("_3")
  val  _4 = Symbol("_4")
  val  _5 = Symbol("_5")
  val  _6 = Symbol("_6")
  val  _7 = Symbol("_7")
  val  _8 = Symbol("_8")
  val  _9 = Symbol("_9")
  val _10 = Symbol("_10")
}

// ===========================================================================

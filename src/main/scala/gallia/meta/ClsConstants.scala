package gallia
package meta

// ===========================================================================
private object ClsConstants { // TODO: revamp, see t210118084355

  val FullPercentiles = cls(
      Symbol("p01")        .double,
      Symbol("p05")        .double,
      Symbol("p25")        .double,

      Symbol("p50")        .double,

      Symbol("p75")        .double,
      Symbol("p95")        .double,
      Symbol("p99")        .double)    

  // ===========================================================================
  val FullDescriptiveStats = cls(
      Symbol("size")       .long,
      Symbol("uniq")       .long,

      // ---------------------------------------------------------------------------
      Symbol("mean")       .double,
      Symbol("sd")         .double,

      // ---------------------------------------------------------------------------
      Symbol("skewness")   .double_,
      Symbol("kurtosis")   .double_,

      // ---------------------------------------------------------------------------
      Symbol("min")        .double,
      Symbol("max")        .double,

      // ---------------------------------------------------------------------------
      Symbol("percentiles").cls(FullPercentiles))

}

// ===========================================================================

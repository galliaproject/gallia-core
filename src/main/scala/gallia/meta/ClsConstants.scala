package gallia.meta

import gallia._

// ===========================================================================
private object ClsConstants { // TODO: revamp, see t210118084355

  val FullPercentiles = cls(
      'p01        .double,
      'p05        .double,
      'p25        .double,

      'p50        .double,

      'p75        .double,
      'p95        .double,
      'p99        .double)    

  // ===========================================================================
  val FullDescriptiveStats = cls(
      'size       .long,
      'uniq       .long,

      // ---------------------------------------------------------------------------
      'mean       .double,
      'sd         .double,

      // ---------------------------------------------------------------------------
      'skewness   .double_,
      'kurtosis   .double_,

      // ---------------------------------------------------------------------------
      'min        .double,
      'max        .double,

      // ---------------------------------------------------------------------------
      'percentiles.cls(FullPercentiles))

}

// ===========================================================================

package gallia
package actions
package utils

// ===========================================================================
object JdbcMetaUtils {
  import java.sql.Types
  import meta.basic.BasicType
  import meta.basic.BasicType._

  // ===========================================================================
  def columnsToCls(columns: aptus.aptmisc.Rdbms.Columns): Cls =
    columns
      .values
      .map(columnToFld)
      .pipe(Cls.apply)

  // ---------------------------------------------------------------------------
  private def columnToFld(column: aptus.aptmisc.Rdbms.Column): Fld =
    Fld(
      key  = Symbol(column.name),
      info = meta.Info(
        optional = column.nullable,
        union    = Seq(meta.SubInfo(
          multiple  = _Single,
          valueType = basicType(debug = column.name)(column.typeCode, column.originalTypeName) ))) )

  // ---------------------------------------------------------------------------
  private implicit def _toOption(value: BasicType): Option[BasicType] = Some(value)

    // ---------------------------------------------------------------------------
    // TODO: t220513142231 - we might need to actually see the value for some? may also need to do a pre-mapping (eg for TIME_WITH_TIMEZONE)
    private def basicType(debug: String)(typeCode: Int, originalTypeName: String): BasicType = _basicType(debug)(typeCode, originalTypeName).get

    // ---------------------------------------------------------------------------
    private def _basicType(debug: String)(typeCode: Int, originalTypeName: String): Option[BasicType] = // probably also need the exact RDBMS (eg mysql vs postgres, ...)
      typeCode match { // see java.sql.Types
        case Types.BIT                      /*   -7 */ => _Boolean

        case Types.TINYINT                  /*   -6 */ => _Int
        case Types.SMALLINT                 /*    5 */ => _Int
        case Types.INTEGER                  /*    4 */ => _Int

        case Types.BIGINT                   /*   -5 */ => _Long // not to be confused with java's BigInt

        case Types.FLOAT                    /*    6 */ => _Float // t220615101855 - to test (JDBC seems to map MySql FLOAT to Types.REAL instead of Types.FLOAT)
        case Types.REAL                     /*    7 */ => originalTypeName match {
                                                            case "FLOAT"           => _Float
                                                            case "DOUBLE" | "REAL" => _Double
                                                            case x                 => aptus.illegalState(x, debug) }
        case Types.DOUBLE                   /*    8 */ => _Double

        case Types.NUMERIC                  /*    2 */ => _BigDec
        case Types.DECIMAL                  /*    3 */ => _BigDec

        case Types.CHAR                     /*    1 */ => _String
        case Types.VARCHAR                  /*   12 */ => _String
        case Types.LONGVARCHAR              /*   -1 */ => _String

        case Types.DATE                     /*   91 */ => _LocalDate     // see 220615092031 for data counterpart
        case Types.TIME                     /*   92 */ => _LocalTime     // see 220615092032 for data counterpart
        case Types.TIMESTAMP                /*   93 */ => _LocalDateTime // see 220615092033 for data counterpart

        case Types.BINARY                   /*   -2 */ => _Binary
        case Types.VARBINARY                /*   -3 */ => _Binary
        case Types.LONGVARBINARY            /*   -4 */ => _Binary

        case Types.NULL                     /*    0 */ => None
        case Types.OTHER                    /* 1111 */ => None
        case Types.JAVA_OBJECT              /* 2000 */ => None
        case Types.DISTINCT                 /* 2001 */ => None
        case Types.STRUCT                   /* 2002 */ => None
        case Types.ARRAY                    /* 2003 */ => None

        case Types.BLOB                     /* 2004 */ => _Binary
        case Types.CLOB                     /* 2005 */ => _String // TODO: to test

        case Types.REF                      /* 2006 */ => None
        case Types.DATALINK                 /*   70 */ => None

        case Types.BOOLEAN                  /*   16 */ => _Boolean

        case Types.ROWID                    /*   -8 */ => None

        case Types.NCHAR                    /*  -15 */ => _String
        case Types.NVARCHAR                 /*   -9 */ => _String
        case Types.LONGNVARCHAR             /*  -16 */ => _String // TODO: to test
        case Types.NCLOB                    /* 2011 */ => _String // TODO: to test

        case Types.SQLXML                   /* 2009 */ => None
        case Types.REF_CURSOR               /* 2012 */ => None

        case Types.TIME_WITH_TIMEZONE       /* 2013 */ => _String  // TODO: to test - no java support (TODO: t220513140234)
        case Types.TIMESTAMP_WITH_TIMEZONE  /* 2014 */ => _Instant // TODO: to test
    }
}

// ===========================================================================
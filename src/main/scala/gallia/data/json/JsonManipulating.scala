package gallia.data.json

// ===========================================================================
object JsonManipulating {
  // t210202112439 - abolute minimum to pre-process JSON only aspect (eg duplicate keys, dealing with missing vs null vs "" vs [], ...)
    // echo '{"f":1, "f":2}' | jq -c #> {"f":2}
  // eg to homogenize types (eg if field is int in oen doc and string otherwise)
}

// ===========================================================================

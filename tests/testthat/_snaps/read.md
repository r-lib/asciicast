# errors

    Code
      read_cast(v1)
    Condition
      Error:
      ! Parse error in fixtures/v1.json:1. Only version 2 asciicast files are supported
      Caused by error:
      ! parse error: premature EOF
                                             {
                           (right here) ------^

---

    Code
      read_cast(badver)
    Condition
      Error:
      ! Parse error in fixtures/badver.json:1. Only version 2 asciicast files are supported

---

    Code
      read_cast(bad)
    Condition
      Error:
      ! Parse error in fixtures/bad.json:5.


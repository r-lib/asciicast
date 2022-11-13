# expect_snapshot_r_process

    Code
      r_process()
    Output
      > cat("'4.2.2'")
      '4.2.2'

---

    Code
      r_process()
    Output
      > 1 + ""
      Error in 1 + "" : non-numeric argument to binary operator

---

    Code
      r_process()
    Output
      > cat("\033[31m\033[1mboldred\033[22m\033[39m")
      [31m[1mboldred[22m[39m


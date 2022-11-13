# expect_snapshot_r_process

    Code
      r_process()
    Output
      > getRversion()
      [1] ‘4.2.2’

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
      > cat(cli::col_red(cli::style_bold("boldred")))
      [31m[1mboldred[22m[39m


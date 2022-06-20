
progs <- if (WINDOWS) {
           "rem.exe"
         } else {
           "rem"
         }

dest <- file.path(R_PACKAGE_DIR, paste0("bin", R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
file.copy(progs, dest, overwrite = TRUE)

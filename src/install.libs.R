
progs <- if (WINDOWS) {
           c("rem.exe", "asciicastclient.dll")
         } else {
           c("rem", "asciicastclient.so")
         }

dest <- file.path(R_PACKAGE_DIR, paste0("bin", R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
file.copy(progs, dest, overwrite = TRUE)

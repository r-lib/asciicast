
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

#define R_INTERFACE_PTRS 1
#include <Rembedded.h>
#include <Rinterface.h>

#include <processx/unix-sockets.c>

#include "asciicast.h"

processx_socket_t sock = -1;
FILE *sock_file = NULL;
char *output_buffer = NULL;

void usage(const char *argv0) {
  fprintf(stderr, "Usage: %s [-i] [-v] <unix-socket>\n", argv0);
  exit(5);
}

extern void run_Rmainloop(void);

int main(int argc, char **argv) {

  int interactive = 0;
  int verbose = 0;

  int idx = 1;
  while (1) {
    if (argc <= idx) {
      break;
    } else if (!strcmp(argv[idx], "-i")) {
      interactive = 1;
      idx++;
    } else if (!strcmp(argv[idx], "-v")) {
      verbose = 1;                                       // __NO_COVERAGE__
      idx++;                                             // __NO_COVERAGE__
    } else {                                             // __NO_COVERAGE__
      break;
    }
  }

  if (argc <= idx) {
    usage(argv[0]);
  }

  if (verbose) {
    fprintf(stderr, "starting up\n");                    // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__

  const char *name = argv[idx];
  int ret = processx_socket_connect(name, &sock);
  if (ret == -1) {
    fprintf(                                             // __NO_COVERAGE__
      stderr,                                            // __NO_COVERAGE__
      "Failed to connect to socket at '%s': %s\n",       // __NO_COVERAGE__
      argv[idx],                                         // __NO_COVERAGE__
      processx_socket_error_message()                    // __NO_COVERAGE__
    );                                                   // __NO_COVERAGE__
    exit(6);                                             // __NO_COVERAGE__
  }

  if (verbose) {
    fprintf(stderr, "opening socket\n");                 // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__

  sock_file = fdopen(sock, "r+");
  if (sock_file == NULL) {
    fprintf(                                             // __NO_COVERAGE__
      stderr,                                            // __NO_COVERAGE__
      "Cannot open socket at '%s' as file: %s\n",        // __NO_COVERAGE__
      argv[idx],                                         // __NO_COVERAGE__
      processx_socket_error_message()                    // __NO_COVERAGE__
    );                                                   // __NO_COVERAGE__
    exit(7);                                             // __NO_COVERAGE__
  }
  setbuf(sock_file, NULL);

  if (verbose) {
    fprintf(stderr, "sending header\n");                 // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__

  size_t header_len = strlen(cast_header);
  size_t written = processx_socket_write(&sock, (void*) cast_header,  header_len);
  if (written != header_len) {
    fprintf(                                             // __NO_COVERAGE__
      stderr,                                            // __NO_COVERAGE__
      "Failed to write header to server socket: %s\n",   // __NO_COVERAGE__
      processx_socket_error_message()                    // __NO_COVERAGE__
    );                                                   // __NO_COVERAGE__
    exit(8);                                             // __NO_COVERAGE__
  }

  char *argv2[]= {
    "R",
    "-q",
    "--vanilla",
    "--gui=none",
    "--no-restore",
    "--no-save",
    "--no-readline"
  };

  if (verbose) {
    fprintf(stderr, "initializing embedded R\n");        // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__

  Rf_initEmbeddedR(sizeof(argv2) / sizeof(argv2[0]), argv2);

  if (verbose) {
    fprintf(stderr, "Setting callbacks\n");              // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__

  R_Interactive = interactive;
  R_Outputfile = NULL;
  R_Consolefile = NULL;
  ptr_R_ShowMessage = rem_show_message;
  ptr_R_Busy = rem_busy;
  ptr_R_WriteConsole = NULL;
  ptr_R_WriteConsoleEx = rem_write_console_ex;
  ptr_R_ReadConsole = rem_read_console;
  ptr_R_Suicide = rem_suicide;
  ptr_R_CleanUp = rem_clean_up;

  if (verbose) {
    fprintf(stderr, "initializing REPL\n");              // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__

  R_ReplDLLinit();

  if (verbose) {
    fprintf(stderr, "Running REPL\n");                   // __NO_COVERAGE__
  }                                                      // __NO_COVERAGE__

  run_Rmainloop();

  Rf_endEmbeddedR(0);

  return 0;
}


#include <stdio.h>
#include <string.h>

#include <R_ext/Rdynload.h>
#include <R.h>

#define R_INTERFACE_PTRS 1
#include <Rembedded.h>
#include <Rinterface.h>

#include <processx/unix-sockets.c>
#include "asciicast.h"

processx_socket_t sock = -1;
FILE *sock_file = NULL;
char *output_buffer = NULL;

void R_init_asciicastclient(DllInfo *dll) {
  fprintf(stderr, "Starting up asciicast client\n");

  const char *name = getenv("R_ASCIICAST_SOCKET");
  if (!name) {
    Rf_error("Restart R and set the `R_ASCIICAST_SOCKET` env var");
  }
  unsetenv("R_ASCIICSAT_SOCKET");

  int ret = processx_socket_connect(name, &sock);
  if (ret == -1) {
    fprintf(                                             // __NO_COVERAGE__
      stderr,                                            // __NO_COVERAGE__
      "Failed to connect to socket at '%s': %s\n",       // __NO_COVERAGE__
      name,                                              // __NO_COVERAGE__
      processx_socket_error_message()                    // __NO_COVERAGE__
    );                                                   // __NO_COVERAGE__
    exit(6);                                             // __NO_COVERAGE__
  }

  sock_file = fdopen(sock, "r+");
  if (sock_file == NULL) {
    fprintf(                                             // __NO_COVERAGE__
      stderr,                                            // __NO_COVERAGE__
      "Cannot open socket at '%s' as file: %s\n",        // __NO_COVERAGE__
      name,                                              // __NO_COVERAGE__
      processx_socket_error_message()                    // __NO_COVERAGE__
    );                                                   // __NO_COVERAGE__
    exit(7);                                             // __NO_COVERAGE__
  }
  setbuf(sock_file, NULL);

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

  int interactive = 0;
  const char *ev_interactive = getenv("R_ASCIICAST_INTERACTIVE");
  if (!ev_interactive || !strcmp(ev_interactive, "true")) {
    interactive = 1;
  }

  R_Interactive = interactive;
  R_Outputfile = NULL;
  R_Consolefile = NULL;
  // ptr_R_ShowMessage = rem_show_message;
  ptr_R_Busy = rem_busy;
  ptr_R_WriteConsole = NULL;
  ptr_R_WriteConsoleEx = rem_write_console_ex;
  ptr_R_ReadConsole = rem_read_console;
  ptr_R_Suicide = rem_suicide;
  ptr_R_CleanUp = rem_clean_up;
}

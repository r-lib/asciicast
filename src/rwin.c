
#define Win32
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include <fcntl.h>

#include <Rversion.h>
#define LibExtern __declspec(dllimport) extern
#include <Rembedded.h>
#include <Rinternals.h>
#include <R_ext/RStartup.h>

#include "psignal.h"

#include <processx/unix-sockets.c>

processx_socket_t sock = NULL;
char *output_buffer = NULL;

#define rem_clock_gettime(a,b) clock_gettime(a,b)

double get_time(void) {
  struct timespec t;
  int ret = rem_clock_gettime(CLOCK_MONOTONIC, &t);
  if (ret) {
    fprintf(stderr, "Cannot query monotonic clock: %s", strerror(errno));
    exit(1);
  }
  return (double) t.tv_sec + 1e-9 * (double) t.tv_nsec;
}

const char *escape_len(const char *str, size_t len) {
  if (!output_buffer) {
    output_buffer = malloc(len * 4);
  } else {
    output_buffer = realloc(output_buffer, len * 6 + 1);
  }

  if (output_buffer == NULL) {
    fprintf(stderr, "Cannot allocate output buffer, out of memory\n");
    exit(2);
  }

  const char *end = str + len;
  char *pi = (char*) str;
  char *po = output_buffer;
  int32_t code;
  unsigned nc;

  while (pi < end) {
    uint_fast8_t ch = *pi++;
    // Skip the UTf-8 markers from R windows
    if (ch == '\2' || ch == '\3') {
      uint_fast8_t ch1 = pi[0];
      uint_fast8_t ch2 = pi[1];
      if (ch1 == 255 && ch2 == 254) {
	pi += 2;
	continue;
      }
    }
    if (!(ch & 0x80)) {
      code = ch;
      nc = 0;
    } else if (!(ch & 0x20)) {
      code = ch & 0x1F;
      nc = 1;
    } else if (!(ch & 0x10)) {
      code = ch & 0x0F;
      nc = 2;
    } else {
      code = ch & 0x07;
      nc = 3;
    }

    while (nc-- > 0) {
      ch = *pi++;
      if (ch == 0) {
        fprintf(stderr, "Incomplete UTF-8 character in output");
        exit(3);
      }
      code = (code << 6) + (ch & 0x3F);
    }

    int inc;
    if (code <= 0x7f) {
      if (code == '\n') {
        *po++ = '\\';
        *po++ = 'r';
        *po++ = '\\';
        *po++ = 'n';
      } else if (code == '"') {
        *po++ = '\\';
        *po++ = '"';
      } else if (code == '\\') {
        *po++ = '\\';
        *po++ = '\\';
      } else if (code < 0x20) {
        inc = snprintf(po, 11, "\\u%04x", code);
        po += inc;
      } else {
        *po++ = (char) code;
      }
    } else {
      if(code > 0xffff) {
        // Non-BMP characters must be encoded in surrogate pairs
        code -= 0x10000;
        int p1 = 0xD800 | (code >> 10);
        int p2 = 0xDC00 | (code & 0x3FF);
        inc = snprintf(po, 22, "\\u%04x\\u%04x", p1, p2);
      } else {
        inc = snprintf(po, 11, "\\u%04x", code);
      }
      po += inc;
    }
  }
  *po = '\0';

  return output_buffer;
}

const char *escape(const char *str) {
  return escape_len(str, strlen(str));
}

ssize_t sock_write(HANDLE sock, const char *msg, ...) {
  #define BUFSIZE 4096
  static char buf[BUFSIZE];

  va_list args;
  buf[0] = '\0';
  va_start(args, msg);
  vsnprintf(buf, BUFSIZE, msg, args);
  va_end(args);

  return processx_socket_write(&sock, buf, strlen(buf));
}

ssize_t sock_read_line(HANDLE socket, char *buffer, size_t buflen) {
  char *ptr = buffer;
  char *end = buffer + buflen - 1;
  DWORD bytesread = 0;
  while (1) {
    // At the end of the buffer?
    if (ptr >= end) {
      *ptr = '\0';
      return ptr - buffer;
    }

    // Otherwise read
    BOOL ret = ReadFile(
      socket,
      ptr,
      1,
      &bytesread,
      NULL
    );

    // Error
    if (!ret) return -1;

    // EOF
    if (bytesread == 0) {
      *ptr = '\0';
      return ptr - buffer;
    }

    // Complete line
    if (*ptr == '\n') {
      ptr++;
      *ptr = '\0';
      return ptr - buffer;
    }
    ptr++;
  }

  return 0;
}

void rem_show_message(const char *message) {
  double ts = get_time();
  sock_write(sock, "[%f, \"rlib\", \"type: message\"]\n", ts);
  sock_write(sock, "[%f, \"o\", \"%s\"]\n", ts, escape(message));
}

void rem_busy(int which) {
  double ts = get_time();
  sock_write(sock, "[%f, \"rlib\", \"busy: %d\"]\n", ts, which);
}

void rem_write_console_ex(const char *buf, int buflen, int which) {
  double ts = get_time();
  sock_write(sock, "[%f, \"rlib\", \"type: %s\"]\n", ts, which ? "stderr": "stdout");
  sock_write(sock, "[%f, \"o\", \"%s\"]\n", ts, escape_len(buf, buflen));
}

void rem_write_console(const char *buf, int buflen) {
  rem_write_console_ex(buf, buflen, 0);
}

int rem_read_console(const char *prompt,
#if R_VERSION >= R_Version(4,2,0)
                     unsigned char *buf,
#else
		     char *buf,
#endif
                     int buflen,
                     int hist) {

  // We let the main process know that we want to read (more)
  double ts = get_time();
  sock_write(sock, "[%f, \"rlib\", \"type: read\"]\n", ts);

  errno = 0;
  buf[0] = ' ';
  buf[1] = '\0';
  int nbytes = sock_read_line(sock, (char*) buf, buflen);
  if (nbytes == -1) {
    fprintf(
      stderr,
      "Error %d reading from socket: %s\n",
      errno,
      strerror(errno)
    );
    exit(4);
  } else if (nbytes == 0) {
    return 0;
  }

  // We only do this after we read something, otherwise the timings are
  // off if this process is idle for a long time
  ts = get_time();
  sock_write(sock, "[%f, \"rlib\", \"type: prompt\"]\n", ts);
  sock_write(sock, "[%f, \"o\", \"%s\"]\n", ts, escape(prompt));

  if (strlen((const char*) buf)) {
    const char *escbuf = escape((const char*) buf);
    sock_write(sock, "[%f, \"i\", \"%s\"]\n", ts, escbuf);
    sock_write(sock, "[%f, \"rlib\", \"type: input\"]\n", ts);
    sock_write(sock, "[%f, \"o\", \"%s\"]\n", ts, escbuf);
  }

  return 1;
}

void rem_callback(void) {
  // shall we use this for something?
}

// Do we need this?
static void rem_on_intr(int sig) {
  UserBreak = 1;
}

#if R_VERSION >= R_Version(4,2,0)
void rem_clean_up(SA_TYPE saveact, int status, int run_last) {
  // We never save the data, is this OK? (TODO)
  if (run_last) R_dot_Last();

  R_RunExitFinalizers();
  /* clean up after the editor e.g. CleanEd() */

  R_CleanTempDir();

  /* close all the graphics devices */
  if(saveact != SA_SUICIDE) Rf_KillAllDevices();

  exit(status);
}

void rem_suicide(const char *message) {
  double ts = get_time();
  sock_write(sock, "[%f, \"rlib\", \"type: suicide\"]\n", ts);
  sock_write(sock, "[%f, \"o\", \"%s\"]\n", ts, escape(message));
  rem_clean_up(SA_SUICIDE, 2, 0);
}

void rem_void(void) { }
#endif

void usage(const char *argv0) {
  fprintf(stderr, "Usage: %s [-i] <pipe-name>\n", argv0);
  exit(5);
}

extern void run_Rmainloop(void);

int main(int argc, char **argv) {

  int interactive = 0;

  // TODO: time stamp
  const char *cast_header =
    "{"
    "\"version\":2,"
    "\"command\":\"R -q\","
    "\"env\":{\"TERM\":\"xterm-256color\",\"SHELL\":\"/bin/zsh\"},"
    "\"height\":24,"
    "\"rows\":24,"
    "\"width\":80,"
    "\"cols\":80"
    "}\n";

  if (argc < 2) {
    usage(argv[0]);
  }

  int idx = 1;
  if (!strcmp(argv[1], "-i")) {
    if (argc != 3) {
      usage(argv[0]);
    }
    interactive = 1;
    idx++;
  }

  const char *prefix = "\\\\?\\pipe\\";
  char name[1024] = {0};
  strncpy(name, prefix, sizeof(name) - 1);
  strncat(name, argv[idx], sizeof(name) - 1);

  int ret = processx_socket_connect(name, &sock);
  if (ret == -1) {
    fprintf(
      stderr,
      "Failed to connect to socket: %s\n",
      processx_socket_error_message()
    );
    exit(6);
  }

  size_t written = sock_write(sock, "%s", cast_header);
  if (written == -1) {
    fprintf(
      stderr,
      "Failed to write to socket: %s",
      strerror(errno)
    );
  }

  char *argv2[]= {
    "R",
    "-q",
    "--vanilla",
    "--gui=none",
    "--slave",
    "--no-restore",
    "--no-save",
    "--no-readline"
  };

  structRstart rp;
  Rstart Rp = &rp;
  char *RHome;

  R_setStartTime();
#if R_VERSION >= R_Version(4,2,0)
  R_DefParamsEx(Rp, RSTART_VERSION);
#else
  R_DefParams(Rp);
#endif
  if((RHome = get_R_HOME()) == NULL) {
    fprintf(
      stderr,
      "R_HOME must be set in the environment or Registry\n"
    );
    exit(10);
  }
  Rp->rhome = RHome;
  Rp->home = getRUser();
  Rp->CharacterMode = LinkDLL;
#if R_VERSION >= R_Version(4,0,0)
  Rp->EmitEmbeddedUTF8 = FALSE;
  Rp->R_NoEcho = FALSE;
#endif
  Rp->ReadConsole = rem_read_console;
  Rp->WriteConsole = NULL;
  Rp->WriteConsoleEx = rem_write_console_ex;
  Rp->CallBack = rem_callback;
  Rp->ShowMessage = NULL;
  Rp->YesNoCancel = NULL;
  Rp->Busy = rem_busy;
#if R_VERSION >= R_Version(4,2,0)
  Rp->CleanUp = rem_clean_up;
  Rp->ClearerrConsole = rem_void;
  Rp->FlushConsole = rem_void;
  Rp->ResetConsole = rem_void;
  Rp->Suicide = rem_suicide;
#endif

  Rp->R_Quiet = TRUE;
  Rp->R_Interactive = interactive;
  Rp->R_Verbose = FALSE;
  Rp->LoadSiteFile = TRUE;
  Rp->LoadInitFile = FALSE;
  Rp->DebugInitFile = FALSE;
  Rp->RestoreAction = SA_NORESTORE;
  Rp->SaveAction = SA_NOSAVE;

  R_SetParams(Rp);
  R_set_command_line_arguments(sizeof(argv2[0]), argv2);

  signal(SIGBREAK, rem_on_intr);
  setup_Rmainloop();

  run_Rmainloop();

  Rf_endEmbeddedR(0);

  return 0;
}

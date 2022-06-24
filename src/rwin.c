
#define Win32
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

#include <Rversion.h>
#define LibExtern __declspec(dllimport) extern
#include <Rembedded.h>
#include <R_ext/RStartup.h>

#include "psignal.h"

FILE* input_file = NULL;
FILE* cast_file = NULL;
char *output_buffer = NULL;

#define rem_clock_gettime(a,b) clock_gettime(a,b)

double get_time() {
  struct timespec t;
  int ret = rem_clock_gettime(CLOCK_MONOTONIC, &t);
  if (ret) {
    fprintf(stderr, "Cannot query monotonic clock: %s", strerror(errno));
    exit(3);
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
    exit(4);
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
        exit(5);
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

void rem_show_message(const char *message) {
  double ts = get_time();
  fprintf(cast_file, "[%f, \"rlib\", \"type: message\"]\n", ts);
  fprintf(cast_file, "[%f, \"o\", \"%s\"]\n", ts, escape(message));
}

void rem_busy(int which) {
  double ts = get_time();
  fprintf(cast_file, "[%f, \"rlib\", \"busy: %d\"]\n", ts, which);
}

void rem_write_console_ex(const char *buf, int buflen, int which) {
  double ts = get_time();
  fprintf(cast_file, "[%f, \"rlib\", \"type: %s\"]\n", ts, which ? "stderr": "stdout");
  fprintf(cast_file, "[%f, \"o\", \"%s\"]\n", ts, escape_len(buf, buflen));
}

void rem_write_console(const char *buf, int buflen) {
  rem_write_console_ex(buf, buflen, 0);
}

int rem_read_console(const char *prompt,
                     unsigned char *buf,
                     int buflen,
                     int hist) {

  errno = 0;
  buf[0] = '\0';
  fgets((char*) buf, buflen, input_file);
  if (errno != 0) {
    if (feof(input_file)) {
      errno = 0;
      return 0;
    }
    fprintf(
      stderr,
      "Error %d reading from file: %s\n",
      errno,
      strerror(errno)
    );
    exit(2);
  }

  // We only do this after we read something, otherwise the timings are
  // off if this process is idle for a long time
  double ts = get_time();
  fprintf(cast_file, "[%f, \"rlib\", \"type: prompt\"]\n", ts);
  fprintf(cast_file, "[%f, \"o\", \"%s\"]\n", ts, escape(prompt));

  if (strlen((const char*) buf)) {
    const char *escbuf = escape((const char*) buf);
    fprintf(cast_file, "[%f, \"i\", \"%s\"]\n", ts, escbuf);
    fprintf(cast_file, "[%f, \"rlib\", \"type: input\"]\n", ts);
    fprintf(cast_file, "[%f, \"o\", \"%s\"]\n", ts, escbuf);
  }

  return 1;
}

void rem_callback() {
  // shall we use this for something?
}

// Do we need this?
static void rem_on_intr(int sig) {
  UserBreak = 1;
}

void rem_cleanup(SA_TYPE sa, int x, int y) { }
void rem_void() { }
void rem_suicide(const char *s) {
  fprintf(stderr, "suicide: %s\n", s);
}

extern void run_Rmainloop();

int main(int argc, char **argv) {

  fprintf(stderr, "Starting up\n");

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

  if (argc != 3) {
    fprintf(stderr, "Usage: %s <R-script-file> <cast-file>\n", argv[0]);
    exit(1);
  }

  fprintf(stderr, "Opening input file: '%s'\n", argv[1]);

  input_file = fopen(argv[1], "r");
  if (input_file == NULL) {
    fprintf(
      stderr,
      "Failed to open R script file '%s': %s",
      argv[1],
      strerror(errno)
    );
    exit(1);
  }

  fprintf(stderr, "Opening cast file: '%s'\n", argv[2]);

  cast_file = fopen(argv[2], "w");
  if (cast_file == NULL) {
    fprintf(
      stderr,
      "Failed to open cast output file '%s': %s",
      argv[2],
      strerror(errno)
    );
    exit(1);
  }
  setbuf(cast_file, NULL);

  fprintf(stderr, "Sending header\n");

  size_t header_len = strlen(cast_header);
  size_t written = fwrite(cast_header, 1, header_len, cast_file);
  if (written != header_len) {
    fprintf(
      stderr,
      "Failed to write to cast file '%s': %s",
      argv[2],
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

  fprintf(stderr, "Starting R");

  // Rf_initEmbeddedR(sizeof(argv2) / sizeof(argv2[0]), argv2);

  structRstart rp;
  Rstart Rp = &rp;
  char Rversion[25], *RHome;

  snprintf(Rversion, 25, "%s.%s", R_MAJOR, R_MINOR);
  if(strcmp(getDLLVersion(), Rversion) != 0) {
    fprintf(stderr, "Error: R.DLL version does not match\n");
    exit(1);
  }

  R_setStartTime();
  R_DefParamsEx(Rp, RSTART_VERSION);
  if((RHome = get_R_HOME()) == NULL) {
    fprintf(stderr,
	    "R_HOME must be set in the environment or Registry\n");
    exit(1);
  }
  Rp->rhome = RHome;
  Rp->home = getRUser();
  Rp->CharacterMode = LinkDLL;
  Rp->EmitEmbeddedUTF8 = FALSE;
  Rp->ReadConsole = rem_read_console;
  Rp->WriteConsole = NULL;
  Rp->WriteConsoleEx = rem_write_console_ex;
  Rp->CallBack = rem_callback;
  Rp->ShowMessage = NULL;
  Rp->YesNoCancel = NULL;
  Rp->Busy = rem_busy;
  Rp->CleanUp = rem_cleanup;
  Rp->ClearerrConsole = rem_void;
  Rp->FlushConsole = rem_void;
  Rp->ResetConsole = rem_void;
  Rp->Suicide = rem_suicide;

  Rp->R_Quiet = TRUE;
  Rp->R_NoEcho = FALSE;
  Rp->R_Interactive = TRUE;
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

  fprintf(stderr, "DLL init\n");

  run_Rmainloop();

  fprintf(stderr, "REPL loop\n");

  Rf_endEmbeddedR(0);

  return 0;
}

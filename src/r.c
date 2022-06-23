
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

#define R_INTERFACE_PTRS 1
#include <Rembedded.h>
#include <Rinterface.h>

FILE* input_file = NULL;
FILE* cast_file = NULL;
char *output_buffer = NULL;

/* for older macOS versions */

#if defined(__APPLE__) && defined(__MACH__)
#include <mach/clock.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <sys/time.h>
/* It doesn't really matter what these are defined to, as long as they
   are defined */
#ifndef CLOCK_REALTIME
#define CLOCK_REALTIME 0
#endif
#ifndef CLOCK_MONOTONIC
#define CLOCK_MONOTONIC 1
#endif
static int rem_clock_gettime(int clk_id, struct timespec *t) {
  memset(t, 0, sizeof(*t));
  if (clk_id == CLOCK_REALTIME) {
    struct timeval now;
    int rv = gettimeofday(&now, NULL); // __NO_COVERAGE__
    if (rv) {                          // __NO_COVERAGE__
      return rv;                       // __NO_COVERAGE__
    }                                  // __NO_COVERAGE__
    t->tv_sec = now.tv_sec;            // __NO_COVERAGE__
    t->tv_nsec = now.tv_usec * 1000;   // __NO_COVERAGE__
    return 0;                          // __NO_COVERAGE__

  } else if (clk_id == CLOCK_MONOTONIC) {
    static uint64_t clock_start_time = 0;
    static mach_timebase_info_data_t timebase_ifo = {0, 0};
    uint64_t now = mach_absolute_time();

    if (clock_start_time == 0) {
      kern_return_t mach_status = mach_timebase_info(&timebase_ifo);

      /* appease "unused variable" warning for release builds */
      (void)mach_status;

      clock_start_time = now;
    }

    now = (uint64_t)((double)(now - clock_start_time)
                     * (double)timebase_ifo.numer
                     / (double)timebase_ifo.denom);

    t->tv_sec = now / 1000000000;
    t->tv_nsec = now % 1000000000;
    return 0;
  }
  return EINVAL; /* EINVAL - Clock ID is unknown */ // __NO_COVERAGE__
}
#else
#define rem_clock_gettime(a,b) clock_gettime(a,b)
#endif

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
    output_buffer = realloc(output_buffer, len * 4);
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

int main(int argc, char **argv) {

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
    "--no-restore",
    "--no-save",
    "--no-readline"
  };

  Rf_initEmbeddedR(sizeof(argv2) / sizeof(argv2[0]), argv2);

  R_Interactive = 1;
  R_Outputfile = NULL;
  R_Consolefile = NULL;
  ptr_R_ShowMessage = rem_show_message;
  ptr_R_Busy = rem_busy;
  ptr_R_WriteConsole = NULL;
  ptr_R_WriteConsoleEx = rem_write_console_ex;
  ptr_R_ReadConsole = rem_read_console;

  R_ReplDLLinit();

  while(R_ReplDLLdo1() > 0) {  }

  Rf_endEmbeddedR(0);

  return 0;
}

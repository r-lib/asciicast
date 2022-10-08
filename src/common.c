
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

#define R_INTERFACE_PTRS 1
#include <Rembedded.h>
#include <Rinterface.h>

#include <processx/unix-sockets.h>

extern processx_socket_t sock;
extern FILE *sock_file;
extern char *output_buffer;

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
int rem_clock_gettime(int clk_id, struct timespec *t) {
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
int rem_clock_gettime(int clk_id, struct timespec *t) {
  return clock_gettime(clk_id,t);
}
#endif

double get_time(void) {
  struct timespec t;
  int ret = rem_clock_gettime(CLOCK_MONOTONIC, &t);
  if (ret) {
    fprintf(stderr, "Cannot query monotonic clock: %s", strerror(errno)); // __NO_COVERAGE__
    exit(1);                                                              // __NO_COVERAGE__
  }
  return (double) t.tv_sec + 1e-9 * (double) t.tv_nsec;
}

const char *escape_len(const char *str, size_t len) {
  size_t buffer_size = len == 0 ? 4 : len * 4;
  if (!output_buffer) {
    output_buffer = malloc(buffer_size);
  } else {
    output_buffer = realloc(output_buffer, buffer_size);
  }

  if (output_buffer == NULL) {
    fprintf(stderr, "Cannot allocate output buffer, out of memory\n"); // __NO_COVERAGE__
    exit(2);                                                           // __NO_COVERAGE__
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

// I can't easily trigger this, but include it, just in case

void rem_show_message(const char *message) {                        // __NO_COVERAGE__
  double ts = get_time();                                           // __NO_COVERAGE__
  fprintf(sock_file, "[%f, \"rlib\", \"type: message\"]\n", ts);    // __NO_COVERAGE__
  fprintf(sock_file, "[%f, \"o\", \"%s\"]\n", ts, escape(message)); // __NO_COVERAGE__
}                                                                   // __NO_COVERAGE__

void rem_clean_up(SA_TYPE saveact, int status, int run_last) {
  // We never save the data, is this OK? (TODO)
  if (run_last) R_dot_Last();

    R_RunExitFinalizers();
    /* clean up after the editor e.g. CleanEd() */

    R_CleanTempDir();

    /* close all the graphics devices */
    if(saveact != SA_SUICIDE) Rf_KillAllDevices();
    fpu_setup(FALSE);

    exit(status);
}

void rem_suicide(const char *message) {                             // __NO_COVERAGE__
  double ts = get_time();                                           // __NO_COVERAGE__
  fprintf(sock_file, "[%f, \"rlib\", \"type: suicide\"]\n", ts);    // __NO_COVERAGE__
  fprintf(sock_file, "[%f, \"o\", \"%s\"]\n", ts, escape(message)); // __NO_COVERAGE__
  rem_clean_up(SA_SUICIDE, 2, 0);                                   // __NO_COVERAGE__
}                                                                   // __NO_COVERAGE__

void rem_busy(int which) {
  double ts = get_time();
  fprintf(sock_file, "[%f, \"rlib\", \"busy: %d\"]\n", ts, which);
}

void rem_write_console_ex(const char *buf, int buflen, int which) {
  double ts = get_time();
  fprintf(sock_file, "[%f, \"rlib\", \"type: %s\"]\n", ts, which ? "stderr": "stdout");
  fprintf(sock_file, "[%f, \"o\", \"%s\"]\n", ts, escape_len(buf, buflen));
}

// Not used if Ex version is present

void rem_write_console(const char *buf, int buflen) {    // __NO_COVERAGE__
  rem_write_console_ex(buf, buflen, 0);                  // __NO_COVERAGE__
}                                                        // __NO_COVERAGE__

int rem_read_console(const char *prompt,
                     unsigned char *buf,
                     int buflen,
                     int hist) {

  // We let the main process know that we want to read (more)
  double ts = get_time();
  fprintf(sock_file, "[%f, \"rlib\", \"type: read\"]\n", ts);

  errno = 0;
  buf[0] = '\0';
  char *ret = fgets((char*) buf, buflen, sock_file);
  if (ret == NULL) {
    if (feof(sock_file)) {
      errno = 0;
      return 0;
    }
    fprintf(                                             // __NO_COVERAGE__
      stderr,                                            // __NO_COVERAGE__
      "Error %d reading from file: %s\n",                // __NO_COVERAGE__
      errno,                                             // __NO_COVERAGE__
      strerror(errno)                                    // __NO_COVERAGE__
    );                                                   // __NO_COVERAGE__
    exit(4);                                             // __NO_COVERAGE__
  }

  // We only do this after we read something, otherwise the timings are
  // off if this process is idle for a long time
  ts = get_time();
  fprintf(sock_file, "[%f, \"rlib\", \"type: prompt\"]\n", ts);
  fprintf(sock_file, "[%f, \"o\", \"%s\"]\n", ts, escape(prompt));

  if (strlen((const char*) buf)) {
    const char *escbuf = escape((const char*) buf);
    fprintf(sock_file, "[%f, \"i\", \"%s\"]\n", ts, escbuf);
    fprintf(sock_file, "[%f, \"rlib\", \"type: input\"]\n", ts);
    fprintf(sock_file, "[%f, \"o\", \"%s\"]\n", ts, escbuf);
  }

  return 1;
}

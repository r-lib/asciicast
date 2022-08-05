
#ifndef ASCIICAST_H
#define ASCIICAST_H

#include <time.h>

int rem_clock_gettime(int clk_id, struct timespec *t);
double get_time();
const char *escape_len(const char *str, size_t len);
const char *escape(const char *str);

void rem_show_message(const char *message);
void rem_clean_up(SA_TYPE saveact, int status, int run_last);
void rem_suicide(const char *message);
void rem_busy(int which);
void rem_write_console_ex(const char *buf, int buflen, int which);
void rem_write_console(const char *buf, int buflen);
int rem_read_console(const char *prompt,
                     unsigned char *buf,
                     int buflen,
                     int hist);

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

#endif

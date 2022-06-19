
#include <stdio.h>

#include <Rembedded.h>

int main(int argc, char **argv) {
  char *argv2[]= { "R", "--silent", "--vanilla" };
  Rf_initEmbeddedR(sizeof(argv2) / sizeof(argv2[0]), argv2);

  printf("Starting up\n");
  
  R_ReplDLLinit();
  while(R_ReplDLLdo1() > 0) {
    printf("Done with one\n");
  }
  
  Rf_endEmbeddedR(0);
  
  return 0;
}

#include <sys/mman.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

#define LEN 32
int main(){
  int *p = mmap(NULL, LEN*sizeof(int), PROT_WRITE|PROT_READ, MAP_SHARED|MAP_ANONYMOUS, 0, 0);

  size_t *children = malloc(4*sizeof(size_t));
  size_t child;
  for (size_t i = 0; i < 4; i++){
    if (child=fork()){
      children[i] = child;
    } else {
      for (int j = 0; j < LEN/4; j++){
        p[j*4+i] = j*4;
      }
      return 0;
    }
  }

  for (size_t i = 0; i < 4; i++){
    waitpid(children[i], 0, 0);
  }

  for (size_t i = 0; i < LEN; i++){
    printf("%d ", p[i]);
  }
  printf("\n");
  fflush(stdout);
}

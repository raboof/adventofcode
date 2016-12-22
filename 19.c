#include "stdlib.h"
#include "stdio.h"
#include "string.h"

//#define totalNumberOfElves 7
#define totalNumberOfElves 3004953
//#define DEBUG 1

int main() {
  int* elves = (int*)malloc(sizeof(int) * totalNumberOfElves);
  for (int i = 0; i < totalNumberOfElves; i++) {
    //fprintf(stderr, "i = %d\n", i);
    elves[i] = i;
  }

  int remainingElves = totalNumberOfElves;
  int currentIndex = 0;
  
  while (remainingElves > 1) {
#ifdef DEBUG
    for (int i = 0; i < remainingElves; i++) {
      printf("%d ", elves[i]);
    }
    printf("\n");
#endif
    int indexToBeStolenFrom = 
      (currentIndex + (remainingElves / 2)) % remainingElves;
    remainingElves = remainingElves - 1;
#ifdef DEBUG
    printf("At index %d (elf %d), stealing from index %d (elf %d), moving %d\n",
      currentIndex, elves[currentIndex],
      indexToBeStolenFrom, elves[indexToBeStolenFrom],
      remainingElves - indexToBeStolenFrom);
#endif
    memcpy(
      elves + indexToBeStolenFrom,
      elves + indexToBeStolenFrom + 1,  
      sizeof(int) * (remainingElves - indexToBeStolenFrom));
    if (indexToBeStolenFrom > currentIndex)
      currentIndex = (currentIndex + 1) % remainingElves;
    else if (currentIndex == remainingElves)
      currentIndex = 0;
  }
  printf("Last remaining elf is number %d\n", elves[0] + 1);
}

#include <stdio.h>

int fact(int n) {
  int i = 1, f = 1;
  do
  {
    f = f * i;
    i++;
  } while(i <= n);

  return f;
}

int main(int argc, char **argv) {
  int f = fact(5);
  printf("%d <-- factorial\n", f);
  return 0;
}

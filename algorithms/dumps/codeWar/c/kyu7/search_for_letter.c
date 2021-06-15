#include <ctype.h>
#include <stdlib.h>
#include <string.h>

char *change(const char *str_in) {
  const int sz = 26;
  char *buffer = (char *)malloc(sizeof(char) * sz);
  memset(buffer, '0', sz);
  while (*str_in != '\0') {
    if (*str_in >= 'a' && *str_in <= 'z') {
      buffer[*str_in - 'a'] = '1';
    } else if (*str_in >= 'A' && *str_in <= 'Z') {
      buffer[*str_in - 'A'] = '1';
    }
    str_in++;
  }
  return buffer;
}

char *change1(const char *str_in) {
  char *r = (char *)malloc(27), c;
  memset(r, '0', 26);
  while ((c = *str_in++))
    if (isalpha(c))
      r[(c | 32) - 'a'] = '1';
  return r;
}

#include <stdio.h>
int main(void) {
  const char *a = "a *** bZ";
  printf("%s\n", change1(a));
  return 0;
}

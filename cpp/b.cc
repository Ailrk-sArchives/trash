#include <stdio.h>


// alias analysis.
void foo(char *__restrict arr1, char *__restrict arr2) {
    for (int i = 0; i < 10; ++i) {
        arr1[i] = i;
    }
    for (int i = 0; i < 10; ++i) {
        arr2[i] = arr1[i];
    }

    for (int i = 0; i < 10; ++i) {
        arr1[i]++;
        arr2[i]++;
    }

    for (int i = 0; i < 10; ++i) printf("%d ", i);
}

int main(void) {
    char arr1[10];
    char arr2[10];
    foo(arr1, arr2);
    return 0;
}

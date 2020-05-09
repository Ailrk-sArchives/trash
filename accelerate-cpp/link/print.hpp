#ifndef PRINT_H
#define PRINT_H

#ifdef __cplusplus
void print(int, int);
void print(double, double);
extern "C" {
#endif

void printI(int, int);
void printF(double, double);


#ifdef __cplusplus
} // end extern "C"
#endif

#endif /* ifndef PRINT_H */

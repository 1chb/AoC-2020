#include <stdio.h>

#define size 1000000
unsigned int puzzle[size+1]; /* ix 0 contains start cup. */

void play(unsigned int n, unsigned int c) {
    for (unsigned int k = 0; k<n; k++) {
        unsigned int p0 = puzzle[c];
        unsigned int p1 = puzzle[p0];
        unsigned int p2 = puzzle[p1];
        unsigned int d = c-1;
        while (d==0 || d==p0 || d==p1 || d==p2) {
            d = d==0 ? size : d-1;
        }
        puzzle[c] = puzzle[p2];
        puzzle[p2] = puzzle[d];
        puzzle[d] = p0;
        c = puzzle[c];
    }
}

unsigned int main() {
    unsigned int max = 0, prev = 0;
    for (const char *c = 0 ? "643719258" : "389125467"; *c; c++) {
        unsigned int k = *c - '0';
        if (k>max) max=k;
        puzzle[prev] = k;
        prev = k;
    }
    for (unsigned int k = max+1; k <= size; k++) {
        puzzle[prev] = k;
        prev = k;
    }
    puzzle[size] = puzzle[0]; /* Make circle */
    for (unsigned int k=0; k<15; k++) {
        printf("puzzle[%u] = %u\n", k, puzzle[k]);
    }
    for (unsigned int k=size-5; k<=size; k++) {
        printf("puzzle[%u] = %u\n", k, puzzle[k]);
    }
    play(10000000, puzzle[0]);
    printf("Stars : %u * %u\n", puzzle[1], puzzle[puzzle[1]]);
    return 0;
}

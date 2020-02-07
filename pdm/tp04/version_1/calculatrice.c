#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#define DMAX 512

char expression[DMAX], *p;
int64_t pile[DMAX], a, b, res;
int pilePointe = -1;

int64_t charToInt(const char *);

int64_t _add(int64_t, int64_t);
int64_t _sub(int64_t, int64_t);
int64_t _division(int64_t, int64_t);
int64_t _multiplication(int64_t, int64_t);

int main(){
    fgets(expression, DMAX, stdin);
    expression[strlen(expression) - 1] = 0; // get rid of newline
    p = strtok (expression, " ");
    while (p != NULL){
        if (strcmp(p, "+") == 0){
            // add
            a = pile[pilePointe--];
            b = pile[pilePointe--];
            res = _add (a, b);
            pile[++pilePointe] = res;
        }
        else
            if (strcmp (p, "-") == 0){
                // subtract
                a = pile[pilePointe--];
                b = pile[pilePointe--];
                res = _sub (a, b);
                pile[++pilePointe] = res;
            }
            else
                if (strcmp (p, "*") == 0){
                    // subtract
                    a = pile[pilePointe--];
                    b = pile[pilePointe--];
                    res = _multiplication (a, b);
                    pile[++pilePointe] = res;
                }
                else
                    if (strcmp (p, "/") == 0){
                        // division
                        a = pile[pilePointe--];
                        b = pile[pilePointe--];
                        res = _division (a, b);
                        pile[++pilePointe] = res;
                    }
                    else
                        // it's a number
                        pile[++pilePointe] = charToInt(p);
        p = strtok (NULL, " ");
    }

    printf ("%ld\n", pile[pilePointe]);
    return 0;
}

int64_t charToInt (const char *s){
    int64_t res = 0;
    int i = 0, sign = 1;

    if (s[0] == '-'){
        sign = -1;
        i = 1;
    }
    if (s[0] == '+')
        i = 1;

    while (s[i]){
        res = res * 10 + (s[i] - '0');
        ++i;
    }
    return res * sign;
}
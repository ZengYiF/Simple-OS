#include "ctype.h"

bool islower(char ch)
{
    return ch >= 'a' && ch <= 'z';
}
char toupper(char ch)
{
    return islower(ch) ? (ch - 'a' + 'A') : ch;
}
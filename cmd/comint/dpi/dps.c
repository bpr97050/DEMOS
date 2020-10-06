/* ВЫВОД СТРОКИ СИМВОЛОВ ДО \0 */

#include "tty_codes.h"

dps(               str )
    register char *str;
{
    register char x;
    while( *str ) {
        if( *str == ps )  {++str;  x = *str++;  dpp( x, *str++ );}
        else               dpo( *str++ );
    }
}

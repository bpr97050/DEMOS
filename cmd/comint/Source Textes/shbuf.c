/* ФОРМИРОВАНИЕ СТРОКИ ДЛЯ ВЫЗОВА SHELL'а */

/*
 * $Log:        shbuf.c,v $
 * Revision 1.1  89/08/19  18:11:59  rygoff
 * Initial revision
 * 
 *
 */

#include "comint.h"
#include <setjmp.h>
#include <tty_codes.h>

extern jmp_buf on_break;

#define           lshbuf 512
static char shbuf[lshbuf+1], *pshbuf;
static char metasymb[] = "\\ ;!\"#$%&'()|*?^<>{}:@";


/* ИНИЦИАЛИЗАЦИЯ ЗАПОЛНЕНИЯ БУФЕРА */

ini_shbuf() { pshbuf = shbuf; }


/* ПОМЕЩАЕТ ОДИН СИМВОЛ В БУФЕР */

put_sh(  symb )
    char symb;
{
    if( pshbuf >= &shbuf[lshbuf] ) {
        dpp(0,ysize-1);  dpo(el);
        dps("Слишком длинная строка аргументов");
        dpi();
        longjmp( on_break, 1 );
    }
    *pshbuf++ = symb;
}


/* КОПИРОВАНИЕ СТРОКИ 'p' В БУФЕР */

cpy_shbuf(         p )
    register char *p;
{
    while( *p )  put_sh( *p++ );
}


/* КОПИРОВАНИЕ СТРОКИ 'p' В БУФЕР С ЭКРАНИРОВАНИЕМ МЕТАСИМВОЛОВ */

mcpy_shbuf(        p )
    register char *p;
{
    while( *p ) {
        if( index( metasymb, *p ) != 0 )  put_sh( '\\' );
        put_sh( *p++ );
    }
}


/* ДОБАВЛЕНИЕ ИМЕНИ ИЗ СТАТЬИ pd */

#undef pd
add_name(                 pd, aflag )
    register struct dir2 *pd;
    int                       aflag;
{
    register char c;

    put_sh( ' ' );
    if( (c=pd->d_mark)=='<' or c=='>' )  put_sh( c );
    if( aflag==1 ) {
        mcpy_shbuf(altcat);
        if( altcat[1] and altcat[0] )  put_sh( '/' );
    } else if( aflag==2 ) {
        if( altpath )  {mcpy_shbuf(altpath); put_sh('/');}
    }
    mcpy_shbuf(pd->d_name);
}


/* ВОЗВРАТ СФОРМИРОВАННОЙ СТРОКИ */

char *get_shbuf() {
    put_sh( 0 );
    return( shbuf );
}

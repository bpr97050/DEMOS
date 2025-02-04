/*
 * LSI-11 Linker
 * Запись в файл диагностики
 */
#include <SysIO>
#ifndef NULL
#   define NULL 0
#endif

static int First = 1;
static char mapname[] = "!Maplist  ";

/*
 * Дозапись строки к концу текущей строки файла.
 * Основная задача: открыть "!Maplist" при первом обращении к секции
 * Если параметр == NULL, то закрывает файл.
 * Управляющие символы не дозволены.
 */
void lino( Arg )
    char *Arg;
{
    if( First ){
        if( Arg == NULL ) return;
        First = 0;
        _linop( mapname );    _lino( "\0" );
    }else{
        if( Arg == NULL ){
            _lino( (char*) (-1) );
            First = 1;
            return;
        }
    }
    _zlino( Arg );
}

/*
 * Перевод строки в файле
 */
void linolf()
{
    if( First ){
        First = 0;
        _linop( mapname );
    }
    _lino( "\0" );
}
              
   
 o&  в" ▌+
b 
  " ∙f%,%E
Н
я 
%"╞
`ьgь+│b	+юfQ"0▌%∙@+ fв" ▐р∙" ▌%∙o%  в Э *    ─     .    ъщ                                  HELLO:BYE п4" ─"▐
/*
 * LSI-11 линкер
 * Высокоуровневый интерфейс над ModRead для чтения таблицы символов
 */
#include "LinkerDefs"

static char *readbuff;      /* Указатель в буфер */
static int   readblen;      /* Длина доступного буфера */
static int   readindex;     /* Указатель чтения (в текущем буфере) */

static unsigned  readcount; /* В предыдущих буферах прочтено... байт */
static unsigned  readlimit; /* Всего надо прочесть */

void Next0( sym_off, sym_leng )
    unsigned int sym_off;
    unsigned int sym_leng;
{
    if( sym_off & 1 ) Error(1);
    if( sym_leng == 0 ){
        readbuff = NULL;    readblen = 0;
    }else{
        ModSeek( sym_off );
        ModRead( &readbuff, &readblen );
    }
    readindex = 0;

    readcount = 0;
    readlimit = sym_leng;
}

Symbol *NextSymbol()
{
    static Symbol rval;
    static char symbuff[ SYMSIZEOF ];

    char *getptr;
    int i;

    if( readcount+readindex >= readlimit ){     /* Logical EOF */
        return NULL;
    }
    if( readindex+SYMSIZEOF > readblen ){ /* Будет исчерпание буфера */
        for( i = 0; i < SYMSIZEOF; i++ ){ /* Копируем с проверками */
            if( readindex >= readblen ){  /* Исчерпание */
                readcount += readblen;
                ModRead( &readbuff, &readblen );
                if( readblen <= 0 ) Error(1);
                readindex = 0;
            }
            symbuff[ i ] = readbuff[ readindex++ ];
        }
        getptr = symbuff;
    }else{                                /* Можно читать напрямую */
        getptr = & readbuff[ readindex ];    readindex += SYMSIZEOF;
    }

    for( i = 0; i < SNLENG; i++ ){
        rval.Sname[ i ] = getptr[ i ];
    }
    rval.Stype = (getptr[SYMTYPE+1] << 8) | (getptr[SYMTYPE] & 0xFF);
    rval.Sval  = (getptr[SYMVAL +1] << 8) | (getptr[SYMVAL ] & 0xFF);
    return &rval;
}
         
 {
            bool Msyslib;    
 * Поиск в C.LANGUAGES иначе в т          bool Msyslib;    
 * Поиск в C.LANGUAGES иначе  Пентов */
} Stat} Stat/
} Statsyslib;    
 * Поиск в C.LANGUAGES иначе  Пентов */
} Stat
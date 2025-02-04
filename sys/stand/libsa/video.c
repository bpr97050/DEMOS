/*
 * Имитатор АЦ-терминала для автономных
 * программ на Электронике-85
 *
 * $Log:	video.c,v $
 * Revision 1.3  87/05/17  18:41:58  avg
 * Сделан таймоут a`la dmitry one.
 * 
 * 
 * Revision 1.2  87/03/11  18:09:33  avg
 * Курсор по умолчанию устанавливается в левый нижний угол экрана.
 *
 * Revision 1.1  86/07/16  15:14:43  avg
 * Initial revision
 *
 */

#define R(x) (*((int *)(x)))

/*------------------------ ЭКРАН -------------------------------*/

#define VI_RID  0174400
#define VI_RKS  0174404
#define VI_RDC  0174406
#define VI_RSH  0174414
#define VI_RX   0174416
#define VI_RY   0174420
#define VI_RVL  0174422
#define VI_RBF  0174424
#define VI_RPA  0174426

int     CX = 23, CY = 0;

v_putchar(c)
register c;
{
	register cxx, cyy;
	int      i, OCX, OCY;
	extern short Font1[];

	OCX = CX; OCY = CY;
	switch(c) {

	    case 010:           /*BS*/
		CY--;
		goto CLRC;

	    case 011:           /*TAB*/
		CY = (CY+8) & ~07;
		goto CLRC;

	    case 012:           /*LF*/
		CX++;
		goto CLRC;

	    case 015:           /*CR*/
		CY = 0;

	    CLRC:
		/* Стереть курсор */
		drow( OCX*10+9, OCY*12, 0 );

	    ROL:
		if( CY <  0 ) CY = 0;
		if( CY > 79 ) CY =  0, CX++;
		if( CX > 23 ) {
			CX = 23;
			R(VI_RSH) += 10;
			clrl(0, 23);
		}

		break;

	    default:            /* Обычный символ */
		if( c < 040 )
			break;
		cxx = CX*10; cyy = CY*12;
		drow( cxx++, cyy, 0 );
		if( c & 0200 ) {
			c &= 0177;
			c += 040;
		} else {
			c &= 0177;
			c -= 040;
		}
		c <<= 3;
		drow( cxx++, cyy, (Font1+0)[c] );
		drow( cxx++, cyy, (Font1+1)[c] );
		drow( cxx++, cyy, (Font1+2)[c] );
		drow( cxx++, cyy, (Font1+3)[c] );
		drow( cxx++, cyy, (Font1+4)[c] );
		drow( cxx++, cyy, (Font1+5)[c] );
		drow( cxx++, cyy, (Font1+6)[c] );
		drow( cxx++, cyy, (Font1+7)[c] );
		drow( cxx++, cyy, 0 );  /* Стирает курсор */
		CY++;
		goto ROL;
	}

	/* Нарисовать курсор */
	drow( CX*10+9, CY*12, -1 );
}

/* Рисовать вектор */
drow( x, y, mask )
{
	while( !(R(VI_RKS) & 0100000) );
	R(VI_RY) = x;
	R(VI_RX) = y;
	R(VI_RBF) = mask;
	R(VI_RVL) = 12;
}

/* Очистить строку */
clrl(cy, cx)
int     cy, cx;
{
	register x;
	register cyy = cy*12 , cxx = cx*10 ;
	long     lm;

	for( x = cxx+9 ; x >= cxx ; x-- ) {
		while( !(R(VI_RKS) & 0100000) );
		R(VI_RY) = x;
		R(VI_RX) = cyy;
		R(VI_RBF) = 0;
		R(VI_RVL) = 1024-cyy;
	}
}

/*---------------------- КЛАВИАТУРА ---------------------------*/

#define KEYRS   0173502
#define KEYRD   0173500

v_getchar()
{
	register c;
	extern int boottmout;
	long l;

	if( boottmout ) {
		do {
			l = 2000000;
			while( (R(KEYRS) & 02) == 0  && --l );
			if( l == 0 )
				return( '\n' );
		} while( !(c = v_keyin(R(KEYRD)) ) );
		boottmout = 0;
	} else {
		do {
			while( (R(KEYRS) & 02) == 0 );
		} while( !(c = v_keyin(R(KEYRD)) ) );
	}
	return(c);
}

/*
 * Спецкоды
 */
#define _ROFF   1
#define _RPT    2
#define _CTRL   3
#define _SHF    4

/*
 * Признаки клавиш
 */
#define U       0400
#define X       01000
#define D       01400
#define C       02000

#define CMASK   03400

/*
 *      U - не зависит от обоих регистров
 *      X - спецкод
 *      D - по ВР инвертируется 020
 *      C - аналогично U, но действует CTRL
 */

short Ctab[] = {

/*      0       1       2       3       4       5       6       7 */
/*220*/                 U+'0',      0,  U+'.',  U+012,  U+'1',  U+'2',
/*230*/ U+'3',  U+'4',  U+'5',  U+'6',  U+',',  U+'7',  U+'8',  U+'9',
/*240*/ U+'-',      0,      0,      0,      0,      0,      0,      0,
/*250*/     0,      0,      0,      0,      0,      0,  X+_SHF, X+_CTRL,
/*260*/     0,     0,      0, X+_ROFF,X+_RPT,     0,      0,      0,
/*270*/     0,      0,      0,      0,  U+0177, U+015,  U+011,  D+'+',
/*300*/ D+'!',    'j',    'f',    'q',  U+010,  D+'"',    'c',    'y',
/*310*/   '~',      0,  D+'?',  D+'#',    'u',    'w',    's',      0,
/*320*/ D+'$',    'k',    'a',    'm',  U+' ',      0,  D+'%',    'e',
/*330*/   'p',    'i',      0,  D+'&',    'n',    'r',    't',      0,
/*340*/ D+'\'',   'g',    'o',    'x',      0,  D+'(',    '{',    'l',
/*350*/   'b',      0,  D+')',    '}',    'd',    '`',      0,  U+'0',
/*360*/   'z',  C+'_',    'v',  D+'<',      0,      0,    'h',  D+'>',
/*370*/     0,  D+'=',  D+'*',    '|',      0,      0,     0,      0

};

/*
 * Переменные регистров
 */
int     Ctrl  = 0;
int     Shift = 0;

/*
 * Прием одного символа с клавиатуры
 */
v_keyin(c)
register c;
{
	register c1;
	static Pc;      /* Пред. символ - для повторения */
	static Pcc;     /* Пред. символ 1 */

	c &= 0377;

	if( c < 0222 || (c = Ctab[c-0222]) == 0 )
		return(0);              /* Плохой символ */

	c1 = c & 0177;
	switch( c & CMASK ) {

	    case U:
		break;

	    case D:
		if( Shift )
			c1 ^= 020;
		break;

	    case 0:
		if( Shift )
			c1 ^= 040;
	    case C:
		if( Ctrl )
			c1 &= 037;
		break;

	    case X:
		switch( c1 ) {

		    case _SHF:
			Shift++;
			break;

		    case _CTRL:
			Ctrl++;
			break;

		    case _ROFF:
			if( Ctrl > 0 )
				Ctrl--;
			else if( Shift > 0 )
				Shift--;
			break;

		    case _RPT:
			return( Pc );
			break;
		}
	    default:
		return(0);
	}
	Pc  = c1;
	return( c1 );
}

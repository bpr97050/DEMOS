/*
 * Драйвер видеоконтроллера Электроники-85
 *
 * $Log:	video.c,v $
 * Revision 1.1  90/01/30  10:56:33  rygoff
 * Initial revision
 * 
 */
#include "video.h"
#if NVI > 0
#include "param.h"
#include <sys/conf.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/tty.h>
#include <sys/systm.h>
#include <sys/iopage.h>

#define KB_RIE  (01 | 030)
#define KB_RID  (01 | 070)
#define KB_XIE  (02 | 030)
#define VI_ISET (02 | 030)
#define VI_ID   (02 | 070)
#define VI_IE   (02 | 050)

#define RB(x)   *((char *)(x))

struct kbreg {
	short   keyrd;          /* регистр данных    */
	short   keyrs;          /* регистр состояния */
};

#define KBADDR  ((struct kbreg *)0173500)

struct vireg {
	short   vi_rid;         /* регистр идентификации      */
	short   vi_m1;
	short   vi_rks;         /* регистр команд и состояния */
	short   vi_rdc;         /* рег. управления данными B  */
	short   vi_rcol;        /* рег. - / - G и R           */
	short   vi_rmap;        /* карта цветов               */
	short   vi_rsh;         /* регистр сдвига             */
	short   vi_ry;          /* регистр координаты x       */
	short   vi_rx;          /* регистр координаты y       */
	short   vi_rvl;         /* регистр длины вектора      */
	short   vi_rbf;         /* регистр функции яркости    */
	short   vi_rpa;         /* регистр физического адреса */
};

#define VIADDR ((struct vireg *)0174400)

struct  tty vi11[1];
int     nvi11   = 1;          /* for pstat */

int     vistart();
extern int ttrstrt();
int     vireset;

extern int vilight;     /* надо зажечь/погасить индикатор */
extern int viO;         /* шкала индикаторов */

int    vicolor = 0;     /* флажок - цветной монитор */
int    viIlevel;        /* Флаг - прерывание по вводу */
int    viStart;         /* Флаг - надо запустить start */

/* цвета */
int viR  = 1;        /* 01 - цвет ; 02 - фон */
int viG  = 1;
int viB  = 1;

short viRDC;    /* значение регистра управления данными */
short viRKS;    /* значение РКС */

#define TICKL 0 /* Задержка в тиках для плавного роллирования */

int tickl = TICKL;

/*
 * Переменные регистров
 */
extern int     Rus;
extern int     Ctrl;
extern int     Fix;
extern int     Shift;
extern int     Altkey;
extern int     Stopped;

/* Коды индикаторов */
#define VIL_WAIT 001
#define VIL_COMP 002
#define VIL_FIX  004
#define VIL_STOP 010
#define VIL_RUS  020

extern vibell;  /* Число звонков */


/* ------------ ВИДЕОКОНТРОЛЛЕР -------------- */

/*
 * Регистры
 */
short  Oreg   = 0,      /* 0 - РУС, 1 - ЛАТ   */
       Italic = 1,      /* 0 - курсив         */
       Iflag  = 0,      /* -1 - инверсное     */
       Uflag  = 0,      /* -1 - подчеркивание */
       DoubleSize = 0,  /* 1 - двойная высота */
       AltFont    = 0,  /* 1 - шрифт набор 2  */
       Bold   = 0;      /* 1 - жирный шрифт   */

int viinv = 0;  /* Доп. режим - инверсия */
int viMODE = 2; /* режим преобразования */

static CX = 23, CY = 0; /* позиция курсора на экране */

#define VIWAIT  while( (VIADDR->vi_rks & 0100000) == 0 )

/*
 * Установка преобразования по цвету
 */
RDC( f )
register f;
{
	/* Хитрая таблица соответствия (фон,цвет,режим) -> режим */
	static int fmode[8][4] = {
		/* F    0       1       0       1 */
		/* B    0       0       1       1 */
/* NOP  0 */    {       0,      0,      0,      0,   },
/* XOR  1 */    {       0,      1,      0,      1,   },
/* SET  2 */    {       6,      2,      3,      7,   },
/* SETC 3 */    {       7,      3,      2,      6,   },
/* ADD  4 */    {       0,      4,      0,      4,   },
/* SUB  5 */    {       0,      5,      0,      5,   },
/* 0    6 */    {       6,      6,      6,      6,   },
/* 1    7 */    {       7,      7,      7,      7    }
	};

	f &= 07;
	VIWAIT;
	if( vicolor ) {
		VIADDR->vi_rdc  =   viRDC | fmode[f][viB];
		VIADDR->vi_rcol =   viRDC | fmode[f][viG] |
				  ((viRDC | fmode[f][viR])<<8);
	} else
		VIADDR->vi_rdc = viRDC | f;
}


int cshift ;    /* Сдвиг для soft-роллирования */

/*
 * Координаты углов текущего окна
 */
int viWx0 = 0;
int viWy0 = 0;
int viWx1 = 23;
int viWy1 = 79;

/*
 * Вывод символа без прерываний (для выдачи консольных сообщений)
 */
viputchar(c)
{
	int Savereg = Oreg;
	int s, Srks, Srdc, st;
	register struct VA *v;
	int x0, x1, y0, y1;

	s = spl6();
	x0 = viWx0;
	x1 = viWx1;
	y0 = viWy0;
	y1 = viWy1;
	viWx0 = 0;
	viWy0 = 0;
	viWx1 = 23;
	viWy1 = 79;

	viSave();
	Srks = VIADDR->vi_rks;
	VIADDR->vi_rks = viRKS;
	RDC( 2 );
	viinv = 0;
	st = tickl;
	tickl = -1;
	Uflag = Italic = Iflag = DoubleSize = AltFont = Bold = 0;
	Oreg = 0;       /* Диагностики на английском языке */

Again:
	VIADDR->vi_rsh += cshift;
	cshift = 0;
	vidraw(c, 0);
	if( c == '\n' ) { c = '\r'; goto Again; }
	Oreg = Savereg;
	viRestore();
	VIADDR->vi_rks = Srks;
	VIADDR->vi_rsh += cshift;
	cshift = 0;
	tickl = st;
	viWx0 = x0;
	viWy0 = y0;
	viWx1 = x1;
	viWy1 = y1;
	splx(s);
}

#define draw(x,y,m,l)   VIWAIT; \
			VIADDR->vi_ry = (y);\
			VIADDR->vi_rbf = (m)^viinv;\
			_spl7(); \
			VIADDR->vi_rx  = (x)+cshift;\
			VIADDR->vi_rvl = (l);\
			splx(Ospl)

#define cdraw(x,y,m)    draw(x,y,m,12)

int Grks;       /* 1 если пред. вектор - вертикальный */

static  rollwait;   /* Флажок ожидания роллинга */

static  rollcnt;

/*
 * Инициировать плавное роллирование
 */
softenb(n)
{
	register s = _spl7();

	if( cshift == 0 )
		rollcnt = 0;
	VIADDR->vi_rks |= 0100; /* Enable Frame Intr */
	cshift += n;
	splx(s);
}

/*
 * Плавное роллирование
 */
softroll()
{
	static reenter = 0;
	register rw;
	static awaited = 0;

	if( awaited ) return;

	/* Disable Frame Intr */
	if( cshift == 0 ) {
		if( rollwait )
			goto WakeUp;
		VIADDR->vi_rks &= ~0100;
		return;
	}
	if( rollcnt++ < tickl ) /* Divide roll freq */
		return;
	rollcnt = 0;

	/*
	 * Ждем готовности видеоконтроллера
	 */
	awaited++;
	_spl1();
	VIWAIT;
	_spl5();
	awaited = 0;

	if( cshift > 0 ) {      /* Нужно сдвигать вверх */
		cshift--;
		VIADDR->vi_rsh++;
	} else {
		cshift++;
		VIADDR->vi_rsh--;
	}
WakeUp:
	if( rollwait && !reenter &&
	    (cshift == 0  ||
	    (!DoubleSize && cshift < 5 && cshift > -5 && rollwait < 3) ) ) {
		if( cshift == 0 )
			VIADDR->vi_rks &= ~0100;
		reenter++;
		rw = rollwait;
		rollwait = 0;
		_spl1();
		vidraw(0, rw);
		_spl5();
		(&vi11[0])->t_state &= ~BUSY;
		vistart( &vi11[0] );
		reenter = 0;
	}
}

int GrafText = 0;

int viHSR = 0;  /* верхняя граница зоны роллирования */
int viLSR = 23; /* нижняя граница зоны роллирования */

/*
 * Подготовка массива векторов для рисования символа
 */
vidraw(c, sf)
register c;
{
	register cxx, cyy;
	static   OCX, OCY;
	int i, j;
	extern Gx, Gy, Gfill, Gbrf, ArcR;
	extern short *_Font1,   /* KLUDGE, SEE font1fix.c */
		       Font2[];
	extern FillStyle, Gfx, Gfy;
	static int dpflag = 0, pxc, pxc1;
	static int Eflag = 0, Gfunc, Gtx, Gty, OGtx, OGty;
	static long Lc;
	static SoftRol = 1;
	static VCOL = 0;
	short *fp, *fp1, csrf;
	int   Ospl;
	short arr[10];
	static char DS[8] = { 0, 03, 014, 017, 060, 063, 074, 077 };
	static struct {
		char vc_cx;
		char vc_cy;
	} vcoor[10];
	static char charvect[10];

	Ospl = _spl7();
	splx(Ospl);
	switch( sf ) {
	    case 1: goto SF1;
	    case 2: goto SF2;
	    case 3: goto SF3;
	    case 4: goto SF4;
	}

	if( vireset ) {
		vireset = 0;
		dpflag = 0;
		Eflag = 0;
	}

	c &= 0177;
	switch( c ) {
	    case 016:           /* RUS (SI) */
		Oreg = 1;
		goto null_ret;

	    case 017:           /* LAT (SO) */
		Oreg = 0;
		goto null_ret;
	}

	switch( VCOL ) {
	    case 1:     /* FG */
		viR &= ~1; viG &= ~1; viB &= ~1;
		viR |= (c&4)? 1:0;
		viG |= (c&2)? 1:0;
		viB |= (c&1)? 1:0;
		VCOL = 0;
		RDC( viMODE );
		goto null_ret;

	    case 2:     /* BG */
		viR &= ~2; viG &= ~2; viB &= ~2;
		viR |= (c&4)? 2:0;
		viG |= (c&2)? 2:0;
		viB |= (c&1)? 2:0;
		VCOL = 0;
		RDC( viMODE );
		goto null_ret;
	}
	OCX = CX; OCY = CY;
	csrf = 0;
	if( Grks ) {
		Grks = 0;
		VIWAIT;
		VIADDR->vi_rks &= ~0400;
	}

	if( Eflag ) {
		Eflag = 0;
		switch( c ) {
			/* дополнительный набор */
		    case 'i':   /* p ^= ~a */
			c = 'b'; viinv = -1; goto modes;
		    case 'j':   /* p |= ~a */
			c = 'e'; viinv = -1; goto modes;
		    case 'k':   /* p &=  a */
			c = 'f'; viinv = -1; goto modes;

		    case 'a':   /* p       */
		    case 'b':   /* p ^=  a */
		    case 'c':   /* p  =  a */
		    case 'd':   /* p  = ~a */
		    case 'e':   /* p |=  a */
		    case 'f':   /* p &= ~a */
		    case 'g':   /* p  =  0 */
		    case 'h':   /* p  =  1 */
			viinv = 0;
		    modes:
			viMODE = (c-1) & 07;
			RDC( viMODE );
			break;

		    case 'A':   Italic = 0; break;
		    case 'B':   Italic = 1; break;
		    case 'C':   Uflag = -1; break;
		    case 'D':   Uflag = 0;  break;
		    case 'E':   Iflag = -1; break;
		    case 'F':   Iflag = 0;  break;
		    case 'G':   Altkey = 1; viO |=  VIL_COMP; goto LB;
		    case 'H':   Altkey = 0; viO &= ~VIL_COMP;
			  LB:   vilight++; vikeyout(); break;
		    case 'S':   viSave();    break;
		    case 'R':   viRestore(); break;
		    case '0':   Gbrf   = 0; break;
		    case '1':   Gbrf   = 1; break;
		    case '2':   FillStyle = 0; break;
		    case '3':   FillStyle = 1; break;
		    case 'X':   i = 0;   goto GM;
		    case 'Y':   i = 010; goto GM;
		    case 'Z':   i = 020; goto GM;
		    case '@':   i = 030;
		    GM:         viRDC &= ~030;
				viRDC |= i;
				RDC( viMODE );
				break;
		    case 'I':   SoftRol = 1; break;
		    case 'J':   SoftRol = 0; break;
		    case 'x':   VCOL = 1; break;
		    case 'y':   VCOL = 2; break;
		    case 'K':   DoubleSize = 1; break;
		    case 'L':   DoubleSize = 0; break;
		    case 'M':   AltFont = 1; break;
		    case 'N':   AltFont = 0; break;
		    case 'O': /* get color map*/
				dpflag = 32; break;
		    case 'P': /* set color map*/
				dpflag = 33; break;
		    case 'Q': /* set default color map*/
				viMapReset(); break;
		    case 'T':   /* Insert line */
			if( cshift ) {
				rollwait = 3;
				goto true_ret;
			}
		    SF3:
			vi_clrcur();
			vi_il(CX);
			goto DCUR;
		    case 'U':   /* Delete line */
			if( cshift ) {
				rollwait = 4;
				goto true_ret;
			}
		    SF4:
			vi_clrcur();
			vi_dl(CX);
			goto DCUR;

		    case 'V':   /* save coordinates */
			dpflag = 5;
			break;
		    case 'W':   /* restore coordinates */
			dpflag = 6;
			break;
		    case 'w':   /* set window */
			dpflag = 7;
			break;
		    case 'v':   /* set full screen */
			viHSR = viWx0 = viWy0 = 0;
			viLSR = viWx1 = 23;
			viWy1 = 79;
			break;
		    case '4':   /* enter bold font */
			Bold = 1; break;
		    case '5':   /* exit bold font */
			Bold = 0; break;
		    case 'z':   /* load download font letter */
			dpflag = 31; break;
		}
		goto null_ret;
	}
	switch( dpflag ) {
	    case 1:
	    case 3:
		pxc = c-040;
		dpflag++;
		goto null_ret;

	    case 2:
		pxc += viWx0;
		if( pxc < viWx0 )
			pxc = viWx0;
		else if( pxc > viWx1 )
			pxc = viWx1;
		CX = pxc;
		pxc = c-040 + viWy0;
		if( pxc < viWy0 )
			pxc = viWy0;
		else if( pxc > viWy1 )
			pxc = viWy1;
		CY = pxc;
		dpflag = 0;
		csrf++;
		goto CLRC;

	    case 4:
		viHSR = pxc + viWx0;
		viLSR = c - 040 + viWx0;
		if( viHSR < viWx0 ) viHSR = viWx0;
		if( viHSR > viWx1 ) viHSR = viWx1;
		if( viLSR < viWx0 ) viLSR = viWx0;
		if( viLSR > viWx1 ) viLSR = viWx1;
		if( viHSR > viLSR ) {
			pxc = viHSR; viHSR = viLSR; viLSR = pxc;
		}
		dpflag = 0;
		goto null_ret;

	    case 5:     /* save position */
		dpflag = 0;
		if( c < '0' || c > '9' )
			break;
		vcoor[c-'0'].vc_cx = CX;
		vcoor[c-'0'].vc_cy = CY;
		goto null_ret;

	    case 6:     /* restore position */
		dpflag = 0;
		if( c < '0' || c > '9' )
			break;
		CX = vcoor[c-'0'].vc_cx;
		CY = vcoor[c-'0'].vc_cy;
		csrf++;
		goto CLRC;

	    case 7:     /* set window, x0 */
		pxc = c-040;
		if(      pxc < 0  ) pxc = 0;
		else if( pxc > 23 ) pxc = 23;
		viWx0 = pxc;
		viHSR = viWx0;
		dpflag++;
		goto null_ret;

	    case 8:     /* set window, x1 */
		pxc = c-040;
		if( pxc < viWx0 )  pxc = viWx0;
		else if( pxc > 23 ) pxc = 23;
		viWx1 = pxc;
		dpflag++;
		viLSR = viWx1;
		goto null_ret;

	    case 9:     /* set window, y0 */
		pxc = c-040;
		if(      pxc < 0  ) pxc = 0;
		else if( pxc > 79 ) pxc = 79;
		viWy0 = pxc;
		dpflag++;
		goto null_ret;

	    case 10:    /* set window, y1 */
		pxc = c-040;
		if( pxc < viWy0 )  pxc = viWy0;
		else if( pxc > 79 ) pxc = 79;
		viWy1 = pxc;
		dpflag = 0;
		CX = viWx0;
		CY = viWy0;
		goto CLRC;

		/* load download char */
	    case 11: case 12: case 13: case 14: case 15:
	    case 16: case 17: case 18: case 19: case 20:
	    case 21: case 22: case 23: case 24: case 25:
	    case 26: case 27: case 28: case 29:
		c = (c-0100)&077;
		if( ++dpflag & 1 )
			c <<= 6;
		Font2[pxc + (dpflag-12)/2] |= c;
		goto null_ret;

	    case 30:
		c = ((c-0100)&077)<<6;
		Font2[pxc + 9] |= c;
		dpflag = 0;
		goto null_ret;

	    case 31:
		dpflag = 11;
		if( (c & 0100) && Oreg )
			pxc = c | 0200;
		else
			pxc = c;
		if( pxc < 040 )         pxc = 0;
		else if( pxc < 0200 )   pxc -= 040;
		else if( pxc < 0300 )   pxc = 0;
		else                    pxc -= 0140;
		pxc *= 10;
		for( cxx = 0 ; cxx < 10 ; cxx++ )
			Font2[pxc+cxx] = 0;
		goto null_ret;

	    case 32:    /* get color map */
		viMapGet(c&07);
		dpflag = 0;
		goto null_ret;

	    case 33:    /* set color map, got n */
		pxc = c & 07;
		dpflag++;
		goto null_ret;

	    case 34:    /* set color map, got R */
		pxc1 = (c&06)<<6;
		dpflag++;
		goto null_ret;

	    case 35:    /* set color map, got G */
		pxc1 |= (c&07)<<3;
		dpflag++;
		goto null_ret;

	    case 36:    /* set color map, got B */
		pxc1 |= c & 07;
		{ extern char vicm[8];
		  vicm[pxc] = pxc1;
		  VIADDR->vi_rmap = (pxc<<8) | pxc1;
		}
		dpflag = 0;
		goto null_ret;

	    case -1:
		Lc = ((long)(c-0100)) << 12;
		dpflag--;
		goto null_ret;

	    case -2:
		Lc |= ((long)(c-0100)) << 6;
		dpflag--;
		goto null_ret;

	    case -3:
		Lc |= ((long)(c-0100));
		dpflag = 0;

		cxx = (int)((Lc >> 10) & 0377l);
		cyy = (int)(Lc & 01777l);
		switch( Gfunc ) {
		    case  2:    ArcR = (int)Lc; break;     /* SET ARC R */
		    case  1:    draw(cxx,cyy,-1,Gbrf?1:2);         /* POINT  */
		    case  0:    Gx = cxx; Gy = cyy;     break;
		    case -1:    Gline( cxx, cyy );      break;     /* LINE   */
		    case -2:    Garc( cxx, cyy, 0 );    break;     /* ARC+   */
		    case -3:    Garc( cxx, cyy, 1 );    break;     /* ARC-   */
		    case -4:    Gcirc( cxx, cyy );      break;     /* CIRCLE */
		    case -5:    Gtx=OGtx=cxx; Gty=OGty=cyy; break; /* TEXT   */
		    case -6:    Gfill = 1; goto Fzoo;    /* FILL from X */
		    case -7:    Gfill = 2; goto Fzoo;    /* FILL from Y */
		    case -8:    Gfill = 3; Fzoo:         /* FILL from (X,Y) */
				Gfx = cxx; Gfy = cyy;
		}
		goto null_ret;
	    default:
		dpflag = 0;
	}

	i = DoubleSize? 2: 1;
	switch(c) {
	    case 001:           /* CLEAR END OF SCREEN */
		cxx = 10*CX;
		cyy = 12*viWy0;
		if( GrafText ) { cxx = Gtx; cyy = OGty; }
		cxx += 10;
		if( viWy1 == 79 )
			pxc = 1024;
		else
			pxc = (viWy1+1)*12;
		pxc -= viWy0*12;
		while( cxx < 10*(viWx1+1) ) {
			draw( cxx++, cyy, 0, pxc );
		}

	    case 025:           /* CLEAR END OF LINE */
	    CLREL:
		cyy = CY*12;
		cxx = (10*CX) & 0377;
		if( GrafText ) {
			cxx = Gtx;
			cyy = Gty;
		}
		if( viWy1 == 79 )
			pxc = 1024;
		else
			pxc = (viWy1+1)*12;
		pxc -= CY*12;
		i *= 10;
		if( DoubleSize ) cxx -= 10;
		while( i-- ) {
			draw( cxx++, cyy, 0, pxc );
		}
		break;

	    case 003:           /* GRAF TEXT MODE */
		GrafText = 1;
		dpflag = -1;
		Gfunc  = -5;
		goto null_ret;

	    case 005:           /* END GRAF TEXT MODE */
		GrafText = 0;
		goto null_ret;

	    case 007:           /* BELL */
		vibell++;
		vikeyout();
		goto null_ret;

	    case 014:           /*ERASE*/
		cyy = viWy0*12;
		if( viWy1 == 79 )
			pxc = 1024;
		else
			pxc = (viWy1+1)*12;
		pxc -= viWy0*12;
		for( cxx = viWx0*10 ; cxx < (viWx1+1)*10 ; cxx++ ) {
			draw( cxx, cyy, 0, pxc );
		}
		CX = viWx0; CY = viWy0;
		Gtx = Gty = 0;
		Iflag = Uflag = Bold = GrafText = DoubleSize = AltFont = 0;
		Italic = 1;
		csrf++;
		break;

	    case 022:           /*UP*/
		if( GrafText ) { Gtx -= 10*i; goto null_ret; }
		CX -= i;
		goto CLRC;

	    case 020:           /*RIGHT*/
		if( GrafText ) { Gty += 12*i; goto null_ret; }
		CY += i;
		goto CLRC;

	    case 010:           /*BS*/
		if( GrafText ) { Gty -= 12*i; goto null_ret; }
		CY -= i;
		goto CLRC;

	    case 011:           /*TAB*/
		if( GrafText ) {
			Gty = ((Gty-OGty)/12*i + 8) & ~07;
			Gty = OGty + Gty*12*i;
			goto null_ret;
		}
		CY = (((CY-viWy0)*i+8) & ~07) + viWy0;
		if( CY > viWy1 ) CY = viWy1;
		goto CLRC;

	    case 012:           /*LF*/
		if( GrafText ) { Gtx += 10*i; goto null_ret; };
		CX += i;
		goto CLRC;

	    case 024:           /* HOME */
		if( GrafText ) { Gtx = OGtx; Gty = OGty; goto null_ret; }
		CX = viWx0;
		CY = viWy0;
		if( DoubleSize ) CX++;
		csrf++;
		goto CLRC;

	    case 026:           /* DIRECT POSITIONING */
		dpflag = 1;
		goto null_ret;

	    case 030:           /* CHANGE SCROLL REGION */
		dpflag = 3;
		goto null_ret;

	    case 015:           /*CR*/
		if( GrafText ) { Gty = OGty; goto null_ret; }
		CY = 0;

	    CLRC:
		/* Стереть курсор */
		i = viinv;
		viinv = 0;
		RDC(1);
		cdraw( OCX*10+9, OCY*12, -1 );
		viinv = i;
		RDC(viMODE);

	    ROL:
		i = DoubleSize?2:1;
		if( CY < viWy0 ) CY = viWy0;
		if( CY > viWy1 ) CY = viWy1;
		if( !csrf ) {
			if( CX > viLSR && OCX == viLSR &&
			    (viHSR!=0 || viLSR!=23 || viWy0!=0 || viWy1!=79) ) {
				vi_RU(DoubleSize);
				CX = viLSR;
				break;
			} else
			if( CX < viHSR && OCX == viHSR &&
			    (viHSR!=0 || viLSR!=23 || viWy0!=0 || viWy1!=79) ) {
				vi_RD(DoubleSize);
				CX = viHSR;
				break;
			}
		}
		/*
		 * Roll all of screen - can not happen if
		 * current window is non-full width or non-full heigt of scr
		 *
		 * SOFT ROLLING FOR WINDOWS AREN'T ALLOWED
		 */
		if( CX > 23 ) {
			_spl7();
			if( cshift > 0 ) {
				if( DoubleSize || cshift > 5 ) {
					rollwait = 1;
					splx(Ospl);
					goto true_ret;
				}
			} else if( cshift < 0 ) {
				VIADDR->vi_rsh += cshift;
				cshift = 0;
			}
		    SF1:
			i = CX-23;
			CX = 23;
			for( cxx = 10*i ; cxx > 0 ; cxx-- ) {
				draw( cxx+239, 0, 0, 1024 );
			}
			VIWAIT;
			if( !SoftRol || tickl < 0 ) {
				VIADDR->vi_rsh += 10*i;
			} else
				softenb(10*i);
		} else if ( CX < 0 ) {
			_spl7();
			if( cshift < 0 ) {
				if( DoubleSize || cshift < -5 ) {
					rollwait = 2;
					splx(Ospl);
					goto true_ret;
				}
			} else if( cshift > 0 ) {
				VIADDR->vi_rsh += cshift;
				cshift = 0;
			}
			splx(Ospl);
		    SF2:
			i = -CX;
			CX = 0;
			for( cxx = 10*i ; cxx > 0 ; cxx-- ) {
				draw( -cxx, 0, 0, 1024 );
			}
			VIWAIT;
			if( !SoftRol || tickl < 0 ) {
				VIADDR->vi_rsh -= 10*i;
			} else
				softenb(-10*i);
		}
		break;

	    case 033:           /* ESCAPE */
		Eflag++;
		goto null_ret;

	    case 006:           /* FILL FROM POINT */
		Gfunc = -8;
		dpflag = -1;
		goto null_ret;

	    case 013:           /* FILL FROM X (HORIZONTAL LINE) */
		Gfunc  = -6;
		dpflag = -1;
		goto null_ret;

	    case 023:           /* FILL FROM Y (VERTICAL LINE) */
		Gfunc  = -7;
		dpflag = -1;
		goto null_ret;

	    case 021:           /* NO FILLING */
		Gfill = 0;
		goto null_ret;

	    case 002:           /* DRAW CIRCLE (prev is the center) */
		dpflag = -1;
		Gfunc  = -4;
		goto null_ret;

	    case 027:           /* DRAW ARC по час. стрелке */
		dpflag = -1;
		Gfunc  = -3;
		goto null_ret;

	    case 032:           /* DRAW ARC против час. стрелки */
		dpflag = -1;
		Gfunc  = -2;
		goto null_ret;

	    case 034:           /* SET ARC RADIUS; <0 - по большой окр. */
		dpflag = -1;
		Gfunc = 2;
		goto null_ret;

	    case 035:           /* PLOT LINE */
		dpflag = -1;
		Gfunc = -1;
		goto null_ret;

	    case 036:           /* SET GRAPHIC POSITION */
		dpflag = -1;
		Gfunc = 0;
		goto null_ret;

	    case 037:           /* PLOT ONE POINT */
		dpflag = -1;
		Gfunc = 1;
		goto null_ret;

	    default:            /* Обычный символ */
		if( c < 040 )
			break;
		if( Oreg && c > 077 ) c += 040;
		else                  c -= 040;
		fp = AltFont? (&Font2[c*10]) : (&(_Font1[c<<3]));
		fp1 = arr;
		cxx = 10;
		if( !AltFont ) { arr[0] = arr[9] = 0; cxx = 8; fp1++; }
		while( cxx-- )
			*fp1++ = *fp++;
		if( Italic == 0 ) {  /* IF REALLY ITALIC */
			fp = arr;
			*fp++ <<= 1; *fp++ <<= 1; *fp <<= 1;
			fp += 4;
			*fp++ >>= 1; *fp++ >>= 1; *fp >>= 1;
		}
		arr[9] ^= Uflag;
		if( Bold ) {
			fp = arr;
			cxx = 5;
			while( cxx-- ) {
				*fp |= *fp << 1; fp++;
				*fp |= *fp << 1; fp++;
			}
		}
		cxx = CX*10; cyy = CY*12;
		if( GrafText ) { cxx = Gtx; cyy = Gty; }
		fp = arr;
		if( !DoubleSize ) {
			for( i = 0; i < 10 ; i++ ) {
				cdraw(cxx++,cyy,*fp++ ^ Iflag);
			}
		} else {
			if( cxx >= 10 ) cxx -= 10;
			pxc = 0;
			for( i = 0; i < 20 ; i++ ) {
				if( !(i & 01) )
					j = *fp++ ^ Iflag;
				Lc = ((long)DS[ j    &07]    ) |
				     ((long)DS[(j>>3)&07]<<6 ) |
				     ((long)DS[(j>>6)&07]<<12) |
				     ((long)DS[(j>>9)&07]<<18);
				if( !Italic && ((1<<i) & 070707) ) Lc <<= 1;
				cdraw(cxx, cyy, (short)(Lc));
				cdraw(cxx++, cyy+12, (short)(Lc>>12));
			}
		}
		if( GrafText ) { Gty += DoubleSize?24:12; goto null_ret; }
		CY++;
		if( DoubleSize ) CY++;
		if( CY > viWy1 ) CY = viWy1;
	}

DCUR:
	/* Нарисовать курсор */
	i = viinv;
	viinv = 0;
	RDC(1);
	cdraw(CX*10+9, CY*12, -1);
	viinv = i;
	RDC(viMODE);

null_ret: return 0;
true_ret: return 1;
}

vi_clrcur()
{
	register i, Ospl = _spl7();

	/* Стереть курсор */
	i = viinv;
	viinv = 0;
	RDC(1);
	cdraw(CX*10+9, CY*12, -1);
	viinv = i;
	RDC(viMODE);
}

vi_CLRL(l)
{
	register xx, cnt, Ospl = _spl7();
	int beg, len;

	beg = 12*viWy0;
	if( viWy1 == 79 )
		len = 1024;
	else
		len = (viWy1+1)*12;
	len -= viWy0*12;
	if( len == 960 )
		len = 1024;
	for( xx = l*10, cnt = 11 ; --cnt ; ) {
		draw(xx++, beg, 0, len);
	}
}
#endif NVI

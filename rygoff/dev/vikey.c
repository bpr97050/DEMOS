/*
 * Драйвер клавиатуры Электроники-85
 *
 * $Log:	vikey.c,v $
 * Revision 1.2  90/01/08  18:37:18  rygoff
 * Изменена программная кодировка клавиатуры:
 * цифры теперь нажимаются без shift'a, знаки !"#$%&''() и некоторые другие
 * - с shift'ом. Так запрограммировано гораздо больше клавиатур в мире.
 * В том числе на PC и DEC - машинах.
 * 
 * Revision 1.1  90/01/05  03:16:05  rygoff
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

#define KB_RIE  (01 | 030)
#define KB_RID  (01 | 070)
#define KB_XIE  (02 | 030)
#define VI_ISET (02 | 030)
#define VI_ID   (02 | 070)
#define VI_IE   (02 | 050)

#define RB(x)   *((char *)(x))

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

struct kbreg {
	short   keyrd;          /* регистр данных    */
	short   keyrs;          /* регистр состояния */
};

#define KBADDR  ((struct kbreg *)0173500)

struct  tty vi11[1];

int    vilight = 0;     /* надо зажечь/погасить индикатор */
int    viO = 0;         /* шкала индикаторов */

int tickl;
int     vistart();
static viinit = 1;
int    vicolor;     /* флажок - цветной монитор */
short viRDC;    /* значение регистра управления данными */
short viRKS;    /* значение РКС */
extern ttrstrt();

char vicm[8];   /* software copy of VI color map */

/*ARGSUSED*/
viopen(dev, flag)
{
	register struct tty *tp;

	tp = &vi11[0];
	tp->t_oproc = vistart;
	if ((tp->t_state & ISOPEN) == 0) {
		tp->t_state = ISOPEN | CARR_ON;
		tp->t_flags = ANYP | ECHO | CRMOD | CEL15I;
		tp->t_line = DFLT_LDISC;
		ttychars(tp);
	}
	ttyopen(dev, tp);
	if( viinit ) {
		RB(I85_CR1) = KB_XIE;
		RB(I85_CR1) = KB_RIE;
		RB(I85_CR2) = VI_ISET;
		RB(&(VIADDR->vi_rpa)) = 0160;   /* Phys addr 016000000    */
		if( (VIADDR->vi_rks & 020000) == 0 )
			vicolor = 1;
		viRKS = 0;      /* кадр ?? */
		if( vicolor ) {
			viMapReset();        /* Set up color map */
			viRKS |= 02000;      /* ENABLE COLOR MAP */
		}
		viRDC = 040;
		RDC(2);           /* set mode + enable phys */
		VIADDR->vi_rks = viRKS;
		viinit = KBADDR->keyrd;
		vilight++;
		vikeyout();
		viinit = 0;
	}
}

/*ARGSUSED*/
viclose(dev, flag)
{
	ttyclose(&vi11[0]);
}

/*ARGSUSED*/
viread(dev)
{
	register struct tty *tp;

	tp = &vi11[0];
	(*linesw[tp->t_line].l_read)(tp);
}

/*AGRSUSED*/
viwrite(dev)
{
	register struct tty *tp;

	tp = &vi11[0];
	(*linesw[tp->t_line].l_write)(tp);
}

/*ARGSUSED*/
viioctl(dev, cmd, addr, flag)
caddr_t addr;
dev_t   dev;
{
	switch (ttioctl(&vi11[0], cmd, addr, flag)) {
		case TIOCSETN:
		case TIOCSETP:
		case TIOCSETA:
		case TIOCSETB:
		case 0:
			break;
		case PR_SCREEN:
			viprscreen(addr);
			break;
		case PR_SDONE:
			viprsdone();
			break;
		default:
			u.u_error = ENOTTY;
	}
}

/*
 * Запустить рисование символа на экране
 */
vistart(tp)
register struct tty *tp;
{
	register c;
	int      Os;
	int      r;
	extern viIlevel, viStart;

	Os = spl5();
	if(tp->t_state&(BUSY|TTSTOP|TIMEOUT)) {
		splx( Os ) ;
		return ;
	}
	if( viIlevel ) {
		viStart = 1;
		splx( Os );
		return;
	}
	tp->t_state |= BUSY;
Loop:
	if( tp->t_state & TTSTOP ) {
		tp->t_state &= ~BUSY ;
		splx( Os );
		return;
	}
	if ((c=getc(&tp->t_outq)) >= 0) {
		if ( !(tp->t_flags & RAW) && c>0177 ) {
			timeout(ttrstrt, (caddr_t)tp, (c & 0177) + 1);
			tp->t_state |= TIMEOUT;
			goto RET;
		}
	} else {
RET:
		if (tp->t_state & ASLEEP && tp->t_outq.c_cc <= TTLOWAT(tp)) {
			tp->t_state &= ~ASLEEP;
				wakeup((caddr_t) &tp->t_outq);
		}
		tp->t_state &= ~BUSY;
		splx(Os);
		return;
	}

	/* Рисовать символ c */
	_spl1();
	if( vidraw(c, 0) ) { splx(Os); return; }
	_spl5();
	goto Loop;
}

viMapReset()
{
	register i;
	static char defmap[8] = { 0, 07, 070, 077, 0300, 0307, 0370, 0377 };

	for( i = 0; i < 8 ; i++ )
		VIADDR->vi_rmap = ((vicm[i] = defmap[i]) & 0377) | (i<<8);
}

viMapGet(n)
{
	register int (*input)();
	register struct tty *tp = &vi11[0];
	int mval = vicm[n];

	input = linesw[tp->t_line].l_input;
	(*input)('0'+((mval>>5)&06), tp);
	(*input)('0'+((mval>>3)&07), tp);
	(*input)('0'+( mval    &07), tp);
}

/* -------------- КЛАВИАТУРА ---------------- */

/*
 * Спецкоды
 */
#define _FIX    0
#define _ROFF   1
#define _RPT    2
#define _CTRL   3
#define _RUS    4
#define _SHF    5
#define _SROLL  6       /* soft rolling ch speed circulary rolling */
#define _HOLD   7       /* trigged stop/start */
#define _PRSCR  8       /* print screen */

/*
 * Признаки клавиш
 */
#define U       0400    /* не зависит от обоих регистров */
#define R       01000   /* требует регистра РУС */
#define E       01400   /* ЕSC-последовательность */
#define X       02000   /* спецкод */
#define D       02400   /* по ВР инвертируется 020, не зависит от РУС/ЛАТ */
#define C       03000   /* аналогично U, но действует CTRL */
#define A       03400   /* переключается по altkey */

#define CXMASK   03400

/*
 * Таблица scan-кодов
 */

static short Ctab[] = {

/*      0       1       2       3       4       5       6       7 */
/*120*/                                               X+_HOLD,X+_PRSCR,
/*130*/ E+'F',  E+'G',  E+'H',      0,      0,      0,      0,      0,
/*140*/     0,      0,      0,      0,  E+'I',  E+'J',  E+'K',  E+'L',
/*150*/ E+'M',      0,      0,      0,      0,      0,      0,      0,
/*160*/     0,  E+'N',  E+'O',  E+'P',  E+'Q',      0,      0,      0,
/*170*/     0,      0,      0,      0,  E+'R',  E+'S',      0,      0,
/*200*/ E+'T',  E+'U',  E+'V',  E+'W',      0,      0,      0,      0,
/*210*/     0,      0,  E+'X',  E+'Y',  E+'Z',  E+'@',  E+'0',  E+'1',
/*220*/     0,      0,  A+'0',      0,  A+'.',  U+012,  A+'1',  A+'2',
/*230*/ A+'3',  A+'4',  A+'5',  A+'6',  A+',',  A+'7',  A+'8',  A+'9',
/*240*/ A+'-',  E+'2',  E+'3',  E+'4',  E+'5',      0,      0,  E+'6',
/*250*/ E+'7',  E+'8',  E+'9',      0,      0,      0,  X+_SHF, X+_CTRL,
/*260*/ X+_FIX,X+_SROLL,X+_RUS, X+_ROFF,X+_RPT,     0,      0,      0,
/*270*/     0,      0,      0,      0,  U+0177, U+015,  U+011,  D+'+',
/*300*/ D+'!',    'j',    'f',    'q',  U+010,  D+'"',    'c',    'y',
/*310*/   '~',  E+'B',  D+'?',  D+'#',    'u',    'w',    's',      0,
/*320*/ D+'$',    'k',    'a',    'm',  U+' ',      0,  D+'%',    'e',
/*330*/   'p',    'i',      0,  D+'&',    'n',    'r',    't',      0,
/*340*/ D+'\'',   'g',    'o',    'x',      0,  D+'(',    '{',    'l',
/*350*/   'b',      0,  D+')',    '}',    'd',    '`',      0,  U+'0',
/*360*/   'z',  C+'_',    'v',  D+'<',      0,  E+'A',    'h',  D+'>',
/*370*/     0,  D+'=',  D+'*',    '|',  E+'C',  R+0137,     0,      0

};

/*
 * Переменные регистров
 */
int     Rus   = 0;
int     Ctrl  = 0;
int     Fix   = 0;
int     Shift = 0;
int     Altkey= 0;

/* Коды индикаторов */
#define VIL_WAIT 001
#define VIL_COMP 002
#define VIL_FIX  004
#define VIL_STOP 010
#define VIL_RUS  020

int    viIlevel = 0;    /* Флаг - прерывание по вводу */
int    viStart  = 0;    /* Флаг - надо запустить start */
int    vibell = 0; /* Число звонков */

static viPRSwaited = 0; /* Признак ожидания печати экрана */
static viPRSgone   = 0; /* "идет печать кадра" */
static viPRSmode;       /* режим печати */
static viPRSisstop;     /* вывод уже был остановлен */

/*
 * Прерывание от клавиатуры
 */
/*ARGSUSED*/
virint(dev)
dev_t   dev;
{
	register int c;
	register struct tty *tp;
	register c1;
	static Pc;      /* Пред. символ - для повторения */
	static Pcc;     /* Пред. символ 1 */

	viIlevel++;
	tp = &vi11[0];
Loop:
	c = KBADDR->keyrd & 0377;

	/*
	 * Преобразуем из скэн-кода в нормальный код
	 */
	if( c < 0126 || (c = Ctab[c-0126]) == 0 )
		goto RET;                 /* Плохой символ */

	c1 = c & 0177;
	switch( c & CXMASK ) {

	    case R:
		if( !Rus ) {
			Pc = Pcc = 0;
			goto RET;
		}

	    case U:
		break;

	    case A:
		if( !Altkey )
			break;
		(*linesw[tp->t_line].l_input)(Pcc = '\035', tp);
		goto NCPCC;

	    case E:
		(*linesw[tp->t_line].l_input)(Pcc = '\033', tp);
		goto NCPCC;

	    case D:
		if( !Shift )
			c1 ^= 020;
		break;

	    case 0:
		if( Shift )
			c1 ^= 040;
		if( Fix )
			c1 ^= 040;
	    case C:
		if( Ctrl )
			c1 &= 037;
		break;

	    case X:
		switch( c1 ) {

		    case _SHF:
			if( Shift++ > 1 )
				Shift = 2;
			goto RET;

		    case _CTRL:
			Shift = 0;
			Ctrl = 1;
			goto RET;

		    case _ROFF:
			if( Ctrl )
				Ctrl = 0;
			else if( Shift > 0 )
				Shift--;
			goto RET;

		    case _FIX:
			Fix = (Fix==0);
			if( Fix )
				viO |= VIL_FIX;
			else
				viO &= ~VIL_FIX;
			vilight++;
			vikeyout();
			goto RET;

		    case _RUS:
			Rus = (Rus==0);
			if( Rus ) {
				c1 = '\016';
				viO |= VIL_RUS;
			} else {
				c1 = '\017';
				viO &= ~VIL_RUS;
			}
			Pcc = 0;
			Pc = 0;
			vilight++;
			vikeyout();
			(*linesw[tp->t_line].l_input)(c1, tp);
			goto RET;

		    case _RPT:
			if( tp->t_rawq.c_cc + tp->t_canq.c_cc > TTYHOG/2 )
				goto RET;
			if( Pcc )
				(*linesw[tp->t_line].l_input)(Pcc, tp);
			if( !Pc )
				goto RET;
			c1 = Pc & 0377;
			goto NCPCC;

		    case _SROLL:
			if( Shift || Ctrl ) {
				if( --tickl < -1 ) tickl = -1;
				if( tp->t_state & TTSTOP ) {
					c1 = tun.t_startc;
					c1 &= 0377;
					break;
				}
			} else {
				if( ++tickl > 3 ) {
					tickl = 3;
					if( !(tp->t_state & TTSTOP) ) {
						c1 = tun.t_stopc;
						c1 &= 0377;
						break;
					}
				}
			}
			goto RET;

		    case _HOLD:
			if(Ctrl&&Shift) {
				viRESET();
				vibell++;
				vikeyout();
				goto RET;
			} else
			if( tp->t_state & TTSTOP )
				c1 = tun.t_startc;
			else
				c1 = tun.t_stopc;
			c1 &= 0377;
			if( c1 == 0 || c1 == 0377 )
				goto RET;
			break;

		    case _PRSCR:
			if( viPRSwaited ) {
				/*
				 * Приостановить вывод на экран
				 */
				viPRSisstop = vi11[0].t_state & TTSTOP;
				vi11[0].t_state |= TTSTOP;

				/*
				 * Запустить печать
				 */
				viPRSgone = 1;
				viPRSmode = (Ctrl||Shift)? PRS_POSITIVE : 0;
				wakeup(&viPRSgone);

				/*
				 * Зажечь индикатор ОЖИД
				 */
				viO |= VIL_WAIT;
				vilight++;
				vikeyout();
			}
			goto RET;
		}
		break;

	    default:
		goto RET;
	}
	Pcc = 0;
	if( c1 == (tun.t_stopc&0377) && c1 != 0 && c1 != 0377 &&
	    !(tp->t_flags&RAW) && !(tp->t_state&TTSTOP) ) {
		viO |= VIL_STOP;
		vilight++;
		vikeyout();
	}
NCPCC:
	Pc  = c1;
	if( !(tp->t_flags&RAW) && (tp->t_state&TTSTOP) &&
	    (!(tp->t_local & LDECCTQ) || c1 == (tun.t_startc&0377)) ) {
		viO &= ~(VIL_STOP|VIL_WAIT);
		vilight++;
		vikeyout();
	}

	/* а дальше как обычно */
	(*linesw[tp->t_line].l_input)(c1, tp);
RET:
	/* посмотрим, пришел ли еще один символ */
	if( KBADDR->keyrs & 02 )
		goto Loop;
	viIlevel--;
	if( viStart )
		vistart( tp );
}

/*
 * Прерывание от клавиатуры на вывод
 */
vixint(dev)
dev_t   dev;
{
	if( vilight || vibell )
		vikeyout();
}

/*
 * Вывод символа на клавиатуру
 */
vikeyout()
{
	static OVL = 020;         /* стар. шкала индикаторов */
	static Vicont = 0;
	int    c;
	register i;

	if( (KBADDR->keyrs & 01) == 0 )
		return;
	if( Vicont ) {
		KBADDR->keyrd = Vicont|0200;
		Vicont = 0;
		return;
	}
Loop1:
	if( vilight ) {
		for( i = 1 ; i < 040 ; i <<= 1 ) {
			if( (viO & i) != (OVL & i) )  {
				if( viO & i ) {
					c   = 023;
					OVL |=  i;
				} else {
					c   = 021;
					OVL &= ~i;
				}
				Vicont = i;
				goto VIOK;
			}
		}
		vilight = 0;
		goto Loop1;
	    VIOK: ;
	} else if( vibell > 0 ) {
		vibell--;
		c = 0247;
	} else
		return;
	KBADDR->keyrd = c;
}

/*
 * ждать нажатия клавиши "печать кадра"
 */
viprscreen(addr)
caddr_t addr;
{
	if( suword(addr, 0) != 0 ) {
		u.u_error = EINVAL;
		return;
	}
	if(viPRSgone) {
		u.u_error = EBUSY;
		return;
	}
	viPRSwaited = 1;
	sleep(&viPRSgone, TTIPRI);
	viPRSwaited = 0;
	if( !viPRSgone )
		return;
	suword(addr, viPRSmode);
}

/*
 * Печать кадра окончена
 */
viprsdone()
{
	viPRSgone = 0;
	if( !viPRSisstop ) {
		vi11[0].t_state &= ~TTSTOP;
		ttstart(&vi11[0]);
	}
	viO &= ~VIL_WAIT;
	vilight++;
	vikeyout();
}

#endif NVI

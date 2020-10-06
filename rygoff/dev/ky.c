/*
 * Драйвер коммуникационного интерфейса для Электроники-85
 *
 * $Log:        ky.c,v $
 * Revision 22.3  89/04/27  13:47:40  korotaev
 * Изменения связанные с небольшим перемещением каталогов и файлов
 *
 * Revision 22.2  89/04/12  14:28:18  korotaev
 * "param.h" ==> "param.">
 *
 * Revision 22.1  89/04/06  16:48:11  avg
 * Добавлен Log для rcs.
 *
 */
#include "ky.h"
#if NKY > 0
#include "param.h"
#include <sys/conf.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/tty.h>
#include <sys/systm.h>
#include <sys/iopage.h>

/*
 * Определения данных коммуникационного порта
 */
struct  kydevice {
	short   ky_dbuf; /* буфер данных */
	short   ky_csrA; /* команды/состояние A */
	short   ky_res;  /* резервирован */
	short   ky_csrB; /* команды/состояния B */
	short   ky_mcr0; /* регистр 0 управления модемом */
	short   ky_mcr1; /* регистр 1 управления модемом */
	short   ky_brate; /* скорость обмена */
};

/* регистр команд/состояния A */

/* WR0 */
#define KYAWR0_RR      0007    /* биты номера регистра */
#define KYAWR0_SA      0010    /* послать abort */
#define KYAWR0_RESI    0020    /* сброс внешнего/изм.сост. прерывания */
#define KYAWR0_CR      0030    /* сброс канала */
#define KYAWR0_EINRC   0040    /* разрешить прерывание по приему символа */
#define KYAWR0_RTIP    0050    /* сбросить запомненное прер. передатчика */
#define KYAWR0_ER      0060    /* сброс ошибки */
#define KYAWR0_EI      0070    /* конец прерывания */

/* WR1 */
#define KYAWR1_EIE     0001    /* разрешение внешнего прерывания */
#define KYAWR1_TIE     0002    /* разрешение прерывания от передатчика */
#define KYAWR1_RIE0    0010    /* разрешение прерывания 0 от приемника */
#define KYAWR1_RIE1    0020    /* разрешение прерывания 1 от приемника */

/* WR2 - не используется */

/* WR3 */
#define KYAWR3_RXEN    0001    /* приемник готов */
#define KYAWR3_SCLH    0002    /* запретить прием сихронизующего символа */
#define KYAWR3_ASM     0004    /* режим поиска адреса */
#define KYAWR3_RCE     0010    /* разрешен КЦК приемника */
#define KYAWR3_EHP     0020    /* ввод управляющего такта */
#define KYAWR3_R7BIT   0100    /* длина символа на приеме 7 бит */
#define KYAWR3_R8BIT   0300    /* длина символа на приеме 8 бит */

/* WR4 */
#define KYAWR4_PEN     0001    /* включена проверка четности */
#define KYAWR4_EP      0002    /* четная четность */
#define KYAWR4_1SB     0004    /* 1 стоповый бит */
#define KYAWR4_15SB    0010    /* 1.5 стоповых бита */
#define KYAWR4_2SB     0014    /* 2 стоповых бита */
#define KYAWR4_CM16    0100    /* 16-кратная частота таймера */

/* WR5 */
#define KYAWR5_TXEN    0010    /* Передача разрешена */
#define KYAWR5_T7BIT   0040    /* длина символа на передаче 7 бит */
#define KYAWR5_T8BIT   0140    /* длина символа на передаче 8 бит */

/* WR6 и WR7 - только для синхронного режима */

/* RR0 */
#define KYARR0_RXCA    0001    /* принят символ */
#define KYARR0_INTR    0002    /* прерывание не обслужено */
#define KYARR0_TBMT    0004    /* буфер передачи пуст */
#define KYARR0_TUEM    0100    /* антипереполнение передатчика */
#define KYARR0_B       0200    /* разрыв линии */

/* RR1 */
#define KYARR1_AS      0001    /* все сообщение передано */
#define KYARR1_RXPE    0020    /* ошибка четности приемника */
#define KYARR1_RXOE    0040    /* переполнение приемника */
#define KYARR1_FE      0100    /* ошибка формата */

/* регистр команд/состояния B */
/* WR0 */
#define KYBWR0_RP      0007    /* биты номера регистра  */

/* WR1 */
#define KYBWR1_ON      0004    /* должен быть загружен */

#ifndef NEW
/* RR2 - причина возникновения прерывания */
#define KYBRR2_V         0034    /* маска */
#define KY_TBMT          0020    /* буфер передатчика пуст -
				  * (внимание! ошибка в "Pro350 Handbook") */
#define KY_ESC           0024    /* изменение состояния/внешнее событие */
#define KY_RXCA          0030    /* принят символ */
#define KY_SRC           0034    /* ошибка на приеме */

/* Регистр 0 управления модемом */
#define KYMCR0_105      04      /* запрос передачи                      */
#define KYMCR0_111      010     /* переключатель скорости               */
#define KYMCR0_108      020     /* оконечное оборудование данных готово */

#define RB(x) (*(char *)x)
#define RES_IE  073
#define SET_IE  053
#define CLR_INT 033

/*
 * Биты регистра скорости обмена
 */

short kyspeed[] = {
	0,
	0,      /*    50 бод */
	01,     /*    75 бод */
	02,     /*   110 бод */
	03,     /*   134 бод */
	04,     /*   150 бод */
	04,     /*   200 бод */
	05,     /*   300 бод */
	06,     /*   600 бод */
	07,     /*  1200 бод */
	010,    /*  1800 бод */
	012,    /*  2400 бод */
	014,    /*  4800 бод */
	016,    /*  9600 бод */
	017,    /* 19200 бод */
	017     /*   EXT B    */
};

extern struct kydevice *KYADDR;

struct  tty ky11;
int     nky11 = 1;      /* для pstat */
int     kystart();
int     ttrstrt();
extern  char    partab[];
static kyinited = 0;

#define KYDELAY                04

static kyinit()
{
	register struct kydevice *addr = KYADDR;
	int timf;

	addr->ky_csrA = KYAWR0_CR;       /* сброс */

	/* Задержка 2+ сек. */
	for( timf = 3; timf > 0 ; timf-- )
		sleep(&lbolt, TTIPRI);

	addr->ky_csrA = 04;      /* WR4 */
	addr->ky_csrA = KYAWR4_CM16|KYAWR4_1SB;

	addr->ky_csrA = 03;      /* WR3 */
	addr->ky_csrA = KYAWR3_RXEN|KYAWR3_R7BIT;

	addr->ky_csrA = 05;      /* WR5 */
	addr->ky_csrA = KYAWR5_TXEN|KYAWR5_T7BIT;

	addr->ky_brate = kyspeed[B9600] | (kyspeed[B9600]<<4);

	addr->ky_csrB = 01;      /* WR1 B */
	addr->ky_csrB = KYBWR1_ON;

	addr->ky_csrB = 02;      /* WR2 B */
	addr->ky_csrB = 0;

	addr->ky_mcr0 = KYMCR0_105 | KYMCR0_108;        /* у-во активно */

	RB(I85_CR1) = CLR_INT;
}


/*ARGSUSED*/
kyopen(dev, flag)
dev_t   dev;
{
	register struct kydevice *addr;
	register struct tty *tp;

	if( !kyinited ) { kyinit(); kyinited++; }
	tp = &ky11;
	addr = tp->t_addr = KYADDR;
	tp->t_oproc = kystart;
	if ((tp->t_state & ISOPEN) == 0) {
		tp->t_state = ISOPEN | CARR_ON;
		tp->t_flags = ANYP | LCASE | CRMOD;
		tp->t_line = DFLT_LDISC;
		ttychars(tp);
	} else if (tp->t_state & XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return;
	}
	addr->ky_csrA = 01;      /* WR1 A */
	addr->ky_csrA = KYAWR1_TIE|KYAWR1_RIE1;
	RB(I85_CR1) = SET_IE; /* разрешить прерывания */
	ttyopen(dev, tp);
}

/*ARGSUSED*/
kyclose(dev, flag)
dev_t   dev;
int     flag;
{
	ttyclose(&ky11);
}

kyread(dev)
dev_t   dev;
{
	(*linesw[ky11.t_line].l_read)(&ky11);
}

kywrite(dev)
dev_t   dev;
{
	(*linesw[ky11.t_line].l_write)(&ky11);
}

static kyblock, kybf;

kyintr(dev)
dev_t   dev;
{
	register int              c;
	register struct kydevice *addr;
	register struct tty      *tp;
	static struct clist que;
	static nsd;
	int s;

	RB(I85_CR1) = RES_IE; /* запретить прерывания */
	tp = &ky11;
	addr = (struct kydevice *)tp->t_addr;
	addr->ky_csrB = 02;      /* RR2 */

	switch (addr->ky_csrB & KYBRR2_V) {     /* причина возникновения прерывания */
	case KY_TBMT:                           /* буфер передатчика пуст */
		tp->t_state &= ~BUSY;
		if( tp->t_state & (TTSTOP|TIMEOUT) )
			addr->ky_csrA = KYAWR0_RTIP;
		else
			kystart(tp);
		if (tp->t_state & ASLEEP && tp->t_outq.c_cc <= TTLOWAT(tp)) {
				wakeup((caddr_t) &tp->t_outq);
		}
		/* проверим заодно и прием */

	case KY_RXCA:                            /* принят символ */
		while (addr->ky_csrA & KYARR0_RXCA) {
			addr->ky_csrA = 01;      /* RR1 */
			if (addr->ky_csrA & (KYARR1_RXPE|KYARR1_RXOE|KYARR1_FE))
				addr->ky_csrA = KYAWR0_ER;

			c = addr->ky_dbuf;
			c &= 0377;
			if ((tp->t_flags&RAW) == 0) {
				c &= 0177;
				if( c == tun.t_stopc ) {
					tp->t_state |= TTSTOP;
					continue;
				}
			} else if( tp->t_line == MPYLDISC &&
				   c == tun.t_stopc ) {
				tp->t_state |= TTSTOP;
				continue;
			}
			if( nsd ) {
				if( (tp->t_flags & TANDEM) &&
				    que.c_cc > TTYHOG/2 && !kybf ) {
					kyblock++;
					ttstart(tp);
				}
				putc(c, &que);
				continue;
			}
			nsd++;
			(void) _spl5();
			RB(I85_CR1)   = SET_IE;
			addr->ky_csrA = KYAWR0_EI;
			for(;;) {
				(void) _spl1();
				(*linesw[tp->t_line].l_input)(c, tp);
				if( kybf && que.c_cc < TTYHOG/6 ) {
					if( tp->t_state & TBLOCK )
						kybf = 0;
					else
					if( putc(tun.t_startc, &tp->t_outq) == 0) {
						kybf = 0;
						ttstart(tp);
					}
				}
				(void) _spl5();
				if( que.c_cc > 0 ) {
					c = getc( &que );
					continue;
				}
				nsd = 0;
				return;
			}
		}
out:
		addr->ky_csrA = KYAWR0_EI;
		break;

	case KY_SRC:                            /* ошибка на приеме */
		addr->ky_csrA = KYAWR0_ER;      /* сброс ошибки */
		goto out;

	case KY_ESC:                             /* внешнее событие */
		if (addr->ky_csrA&KYARR0_B) {    /* разрыв связи */
			if (tp->t_flags&(RAW|CBREAK))
				(*linesw[tp->t_line].l_input)(0,tp);
		}
		addr->ky_csrA = KYAWR0_RESI;
		goto out;
	}
	(void) _spl5();
	RB(I85_CR1) = SET_IE; /* разрешить прерывания */
}

kyioctl(dev, cmd, addr, flag)
caddr_t addr;
dev_t   dev;
{
	switch (ttioctl(&ky11, cmd, addr, flag)) {
		case TIOCSETN:
		case TIOCSETP:
		case TIOCSETA:
		case TIOCSETB:
			kyparam();
			break;
		case 0:
			break;
		default:
			u.u_error = ENOTTY;
	}
}

kystart(tp)
register struct tty *tp;
{
	register c;
	register struct kydevice *addr;

	addr = (struct kydevice *) tp->t_addr;
	/* addr->ky_csrA = 01;    /* RR1 */
	if ((addr->ky_csrA & KYARR0_TBMT) == 0 )
		return;
	if ( kyblock ) {
		kyblock = 0;
		kybf++;
		tp->t_state |= BUSY;
		addr->ky_dbuf = tun.t_stopc;
		return;
	}
	if ((c=getc(&tp->t_outq)) >= 0) {
		if ((tp->t_lstate & L8BITS) || c<0200 || c>=0300) {
			tp->t_state |= BUSY;
			addr->ky_dbuf = c;
			return;
		} else {
			timeout(ttrstrt, (caddr_t)tp, (c & 0177) + KYDELAY);
			tp->t_state |= TIMEOUT;
		}
	}
	addr->ky_csrA = KYAWR0_RTIP;
}

kyparam()
{
	register struct  kydevice *addr = KYADDR;
	register struct  tty      *tp   = &ky11;
	register                   wr4;
	int                        wr3, wr5;
	int                        s;

	RB(I85_CR1) = RES_IE;          /* запретить прерывания */
	switch (tp->t_flags & (EVENP|ODDP)) {
		case EVENP:
			wr4 = KYAWR4_PEN|KYAWR4_EP|KYAWR4_CM16;
			break;
		case ODDP:
			wr4 = KYAWR4_PEN|KYAWR4_CM16;
			break;
		default:
			wr4 = KYAWR4_CM16;
	}

	if (tp->t_ospeed == B110)
		wr4 |= KYAWR4_2SB;
	else
		wr4 |= KYAWR4_1SB;
	wr5 = KYAWR5_T8BIT|KYAWR5_TXEN;
	wr3 = KYAWR3_R8BIT|KYAWR3_RXEN;
	addr->ky_csrA = 04;      /* WR4 */
	addr->ky_csrA = wr4;
	addr->ky_csrA = 03;      /* WR3 */
	addr->ky_csrA = wr3;
	addr->ky_csrA = 05;      /* WR5 */
	addr->ky_csrA = wr5;
	addr->ky_brate = kyspeed[tp->t_ispeed] | (kyspeed[tp->t_ospeed]<<4);
	RB(I85_CR1) = SET_IE; /* разрешить прерывания */
}
#endif

#endif NKY

/*
 * Драйвер флоппи-дисков Электроники-85
 *
 * $Log:	fd.c,v $
 * Revision 1.2  90/02/21  16:12:09  rygoff
 * Исключена проверка записи (чтение и сравнение)
 * Уменьшено число попыток при ошибках.
 * 
 * Revision 1.1  90/02/06  12:19:27  rygoff
 * Initial revision
 * 
 */

#include "fd.h"
#if     NFD > 0
#include "param.h"
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/conf.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/seg.h>
#include <sys/iopage.h>

#ifndef FDSLOT
#define FDSLOT 1
#endif

#define SET_IE  (FDSLOT | 030)
#define RES_IE  (FDSLOT | 070)
#define CLR_IN  (FDSLOT | 0170)

#define RB(x)   *((char *)(x))

struct fddevice {
	short   fd_rid;         /* ч - регистр идентификации         */
	short   fd_unused1;
	short   fd_state;       /* ч - регистр состояния контроллера */
#define fd_com  fd_state        /* з - регистр команд                */
	short   fd_err;         /* ч - регистр ошибок                */
#define fd_trk  fd_err          /* з - номер дорожки                 */
	short   fd_rs1;         /* ч - регистр состояния 1           */
#define fd_sect fd_rs1          /* з - номер сектора                 */
	short   fd_rs2;         /* ч - регистр состояния 2           */
#define fd_rpm  fd_rs2          /* з - парамеры микропрограммы       */
	short   fd_rs3;         /* ч - регистр состояния 3           */
	short   fd_excom;       /* з - расширенная команда           */
	short   fd_buf;         /* ч/з - буфер данных                */
	short   fd_zero;        /* с - начальная установка           */
	short   fd_start;       /* с - старт команды                 */
	short   fd_wbuf;        /* з - запись буфера данных          */
};

#define NFDBLK  (FDSEC*FDTRK)
/* #define FDWCHK 1 */
#define FDTRYS 3

struct  fddevice *FDADDR;

struct  buf     fdtab;
#ifdef  UCB_DBUFS
struct  buf     rfdbuf[NFD];
#else
struct  buf     rfdbuf;
#endif

static short fdwchk = 0;

fdattach(addr, unit)
struct fddevice *addr;
{
	if (unit != 0)
		return(0);
	FDADDR = addr;
	return(1);
}

fdstrategy(bp)
register struct buf *bp;
{
	register s;

	if (FDADDR == (struct fddevice *) NULL) {
		bp->b_error = ENXIO;
		goto errexit;
	}
	if (bp->b_blkno >= ((minor(bp->b_dev)&04)? (2*NFDBLK): NFDBLK) ||
	    minor(bp->b_dev) >= (2*8) ) {
		bp->b_error = EINVAL;
errexit:
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	bp->av_forw = (struct buf *)NULL;
	s = spl5();
	if(fdtab.b_actf == NULL)
		fdtab.b_actf = bp;
	else
		fdtab.b_actl->av_forw = bp;
	fdtab.b_actl = bp;
	if(fdtab.b_active == NULL)
		fdstart(0);
	splx(s);
}

/*
 * Программные регистры для псевдо-DMA
 */
static unsigned fd_addr, fdbytec, fdxmem, fdisread, fddev;

/*
 * Запустить операцию на флоппи
 *
 *      0 - с блоком из bp->b_blkno (начало обмена)
 *      1 - с тем же номером блока  (повтор обмена)
 *      2 - со след. номером блока  (прод. обмена)
 */
fdstart(arg)
{
	register struct fddevice *fdaddr = FDADDR;
	register struct buf *bp;
	static daddr_t bn;
	int tn, sn, side;

	if ((bp = fdtab.b_actf) == NULL)
		return;
	fdtab.b_active++;

	if( arg == 0 ) {
		bn = bp->b_blkno;
		fdbytec  = bp->b_bcount;
		fd_addr  = bp->b_un.b_addr;
		fdxmem   = bp->b_xmem;
		fdisread = ((bp->b_flags & B_READ) != 0);
		fddev    = minor(bp->b_dev) & 077;
	} else if( arg == 2 )
		bn++;

	switch( fddev & 07 ) {
	    case 0: /* Single side; 3/10 interleaving + 1 trk rotate */
		sn = (bn*3) % FDSEC;
		tn =  bn    / FDSEC;
		if( ++tn == FDTRK )  tn = 0;
		side = 0;
		break;
	    case 1: /* Single side; no interleaving */
		sn = bn % FDSEC;
		tn = bn / FDSEC;
		side = 0;
		break;
	    case 6:
	    case 2: /* Single side; RT-11 interleaving */
		sn = bn % FDSEC;
		tn = bn / FDSEC;
		sn += sn;
		if( sn >= FDSEC ) sn -= (FDSEC-1);
		sn = (sn + tn*2) % FDSEC;
		if( ++tn == FDTRK ) tn = 0;
		side = ( fddev&07 ) == 6 ;
		break;
	    case 4: /* Double side; 3/10 interleaving + 1 trk rotate,
		       Compatible with Single Side 3/10 */
		sn = (bn*3) % FDSEC;
		tn =  bn    / FDSEC;
		side = (tn >= FDTRK);
		tn %= FDTRK;
		if( ++tn == FDTRK )  tn = 0;
		break;
	    case 7: /* Double side; 3/20 interleaving + 1 trk rotate */
		sn = (bn*3) % FDSEC;
		tn =  bn    / (FDSEC*2);
		if( ++tn == FDTRK )  tn = 0;
		side = ((bn*3) % (FDSEC*2)) >= FDSEC;
		break;
	    case 5: /* Double side with no interleaving */
		sn = bn % FDSEC;
		tn = bn / (FDSEC*2);
		side = (bn % (FDSEC*2)) >= FDSEC;
		break;
	    /****** BAD INTERLEAVINGS *******/
	    case 3: /* Single side reserved interleaving */
		sn = tn = 1;
		side = 0;
	}
	fdaddr->fd_trk  = tn;
	fdaddr->fd_sect = sn + 1;

	RB(I85_CR2) = SET_IE;
	RB(I85_CR2) = CLR_IN;
	if( fdisread || fdwchk )
		tn = 0110;
	else {
		tn = 0170;
		fdpdma();
	}
	fdaddr->fd_com = tn | ((fddev&070)>>2) | side;
	fdaddr->fd_start = 0;

#ifdef  FD_DKN
	dk_busy |= 1 << FD_DKN;
	dk_numb[FD_DKN]++;
	dk_wds[FD_DKN] += bp->b_bcount >> 6;
#endif  FD_DKN
}

#define PROTO5  ((15<<8) | RW)

/*
 * Имитация прямого доступа к памяти
 */
fdpdma()
{
	register struct fddevice *fdaddr = FDADDR;
	register       i;
	register char *a;
	segm           save5;

	saveseg5(save5);
	a = (fd_addr & 077) | SEG5;
	mapseg5( (fdxmem<<10) | ((fd_addr>>6) & 01777), PROTO5 );
	i = fdbytec;
	if( i > 512 )
		i = 512;
	fdaddr->fd_zero = 0;
	if( fdisread ) {
		do {
			*a++ = fdaddr->fd_buf;
		} while( --i != 0 );
	} else {
		do {
			fdaddr->fd_buf = *a++;
		} while( --i != 0 );
	}

	restorseg5(save5);
}

/*
 * Запрос обмена "A"
 *   'операция завершена'
 */
fdintr()
{
	register struct fddevice *fdaddr = FDADDR;
	register struct buf *bp;
	int     cnt;
	static  fdreset = 0;
	char   *p;

	if (fdtab.b_active == NULL)
		return;
	if( fdreset ) {
		fdreset = 0;
		fdstart(1);
		return;
	}
	RB(I85_CR2) = RES_IE;
#ifdef  FD_DKN
	dk_busy &= ~(1 << FD_DKN);
#endif  FD_DKN
	bp = fdtab.b_actf;
	fdtab.b_active = NULL;

	if (fdaddr->fd_state & 0200) {

		fdwchk = 0;
#ifdef UCB_DEVERR
		switch( fdaddr->fd_err ) {
		    case 0260: p = "write locked\n";    goto De;
		    case 0120: p = "drive not ready\n"; goto De;
		    case 030:
		    case 0220: p = "drive not exist\n"; goto De;
		    case 054:  p = "can't write in 40-tracks mode\n";
		    De:
			harderr(bp, "fd");
			printf( p );
			bp->b_flags |= B_ERROR;
			fdtab.b_errcnt = 0;
			goto NOTRY;
		}
		if( fdtab.b_errcnt++ == FDTRYS ) {
		       harderr(bp, "fd");
		       printf("er=%o ds=%o\n",fdaddr->fd_err,fdaddr->fd_state);
		}
#else
		if( fdtab.b_errcnt++ == FDTRYS )
			deverror(bp, fdaddr->fd_err, fdaddr->fd_state);
#endif
		if( !(fdtab.b_errcnt & 07) ) {
			fdreset++;
			fdtab.b_active++;
			/* Reset drive */
			RB(I85_CR2) = SET_IE;
			fdaddr->fd_com = 040 | ((fddev&070)>>2);
			fdaddr->fd_start = 0;
			return;
		}
		if( fdtab.b_errcnt <= FDTRYS) {
			fdstart(1);
			return;
		}
		bp->b_flags |= B_ERROR;
	}
	if( fdisread && !fdwchk )
		fdpdma();
	if( fdbytec > 512 ) {
		fdbytec -= 512;
		if( (fd_addr+512) < fd_addr )
			fdxmem++;
		fd_addr += 512;
		fdstart(2);
		return;
	}
#ifdef FDWCHK
	if( !(bp->b_flags & (B_READ|B_ERROR)) && !fdwchk ) {
		fdwchk++;
		fdstart(0);
		return;
	}
#endif
NOTRY:
	fdtab.b_errcnt = 0;
	fdwchk = 0;
	fdtab.b_actf = bp->av_forw;
	bp->b_resid = 0;
	iodone(bp);
	fdstart(0);
}

fdread(dev)
dev_t   dev;
{
	physio(fdstrategy, &rfdbuf, dev, B_READ);
}

fdwrite(dev)
dev_t   dev;
{
	physio(fdstrategy, &rfdbuf, dev, B_WRITE);
}
#endif  NFD

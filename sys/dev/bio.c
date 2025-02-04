/*
 * $Log:	bio.c,v $
 * Revision 22.3  90/11/12  19:04:17  root
 * Новые вещи для СМ1425 и перемещение include.
 * 
 * Revision 22.2  89/05/19  17:51:18  avg
 * Добавлена ф-я расчета числа еще несброшенных буферов.
 * 
 * Revision 22.1  89/04/12  14:15:47  korotaev
 * "param.h" ==> <sys/param.h>
 * 
 * Revision 22.0  89/03/25  12:20:49  korotaev
 * Отсюда начинается версия 2.2
 * 
 * Revision 1.7  89/01/22  18:00:06  korotaev
 * Егошинские правки для обмена устройств с DMA с областью сдвинутой
 * вызовом phys.
 * 
 * Revision 1.6  88/11/16  17:12:52  dvolodin
 * *** empty log message ***
 * 
 * Revision 1.4  87/01/29  17:14:45  alex
 * Исправлен макс. размер обмена в swap на 32 к-байта
 * для работы "UNIBUS-MAP" + 200 буферов.
 *
 * Revision 1.3  87/01/06  20:32:17  alex
 * Внесена правка: после конца raw-обмена (physio), если
 * планировщик висит на ожидании места в памяти (runin),
 * он толкается дальше. В результате при работе МЛ система
 * не зависает.
 *
 * Revision 1.2  86/11/23  22:33:21  alex
 * исправлена ошибка при выключенном хешировании буферов.
 * Руднев А.П.
 *
 * Revision 1.1  86/04/19  17:53:43  avg
 * Initial revision
 *
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/dir.h>
#include <sys/user.h>
#include "../include/buf.h"
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/seg.h>
#ifdef UCB_METER
#include <sys/vm.h>
#endif
#ifdef  UNIBUS_MAP
#include "../include/uba.h"
#endif

#ifdef  DISKMON
struct  ioinfo  io_info;
#endif

/*
 * swap IO headers.
 * they are filled in to point
 * at the desired IO operation.
 */
struct  buf     swbuf1;
struct  buf     swbuf2;

/*
 * The following several routines allocate and free
 * buffers with various side effects.  In general the
 * arguments to an allocate routine are a device and
 * a block number, and the value is a pointer to
 * to the buffer header; the buffer is marked "busy"
 * so that no one else can touch it.  If the block was
 * already in core, no I/O need be done; if it is
 * already busy, the process waits until it becomes free.
 * The following routines allocate a buffer:
 *      getblk
 *      bread
 *      breada
 * Eventually the buffer must be released, possibly with the
 * side effect of writing it out, by using one of
 *      bwrite
 *      bdwrite
 *      bawrite
 *      brelse
 */

#ifdef  UCB_BHASH
#ifdef  SMALL
#define BUFHSZ  8       /* must be power of 2 */
#else
#define BUFHSZ  64      /* must be power of 2 */
#endif  SMALL
#define BUFHASH(blkno)  (blkno & (BUFHSZ-1))

struct  buf     *bhash[BUFHSZ];
#endif

/*
 * Read in (if necessary) the block and return a buffer pointer.
 */
struct buf *
bread(dev, blkno)
register dev_t dev;
daddr_t blkno;
{
	register struct buf *bp;

	bp = getblk(dev, blkno);
	if (bp->b_flags&B_DONE) {
#ifdef  DISKMON
		io_info.ncache++;
#endif
		return(bp);
	}
	bp->b_flags |= B_READ;
	bp->b_bcount = BSIZE;
	(void) (*bdevsw[major(dev)].d_strategy)(bp);
#ifdef  DISKMON
	io_info.nread++;
#endif
	iowait(bp);
	return(bp);
}

/*
 * Read in the block, like bread, but also start I/O on the
 * read-ahead block (which is not allocated to the caller)
 */
struct buf *
breada(dev, blkno, rablkno)
register dev_t dev;
daddr_t blkno, rablkno;
{
	register struct buf *bp, *rabp;

	bp = NULL;
	if (!incore(dev, blkno)) {
		bp = getblk(dev, blkno);
		if ((bp->b_flags&B_DONE) == 0) {
			bp->b_flags |= B_READ;
			bp->b_bcount = BSIZE;
			(void) (*bdevsw[major(dev)].d_strategy)(bp);
#ifdef  DISKMON
			io_info.nread++;
#endif
		}
	}
	if (rablkno && !incore(dev, rablkno)) {
		rabp = getblk(dev, rablkno);
		if (rabp->b_flags & B_DONE)
			brelse(rabp);
		else {
			rabp->b_flags |= B_READ|B_ASYNC;
			rabp->b_bcount = BSIZE;
			(void) (*bdevsw[major(dev)].d_strategy)(rabp);
#ifdef  DISKMON
			io_info.nreada++;
#endif
		}
	}
	if(bp == NULL)
		return(bread(dev, blkno));
	iowait(bp);
	return(bp);
}

/*
 * Write the buffer, waiting for completion.
 * Then release the buffer.
 */
bwrite(bp)
register struct buf *bp;
{
	register flag;

	flag = bp->b_flags;
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR | B_DELWRI);
	bp->b_bcount = BSIZE;
#ifdef  DISKMON
	io_info.nwrite++;
#endif
	(void) (*bdevsw[major(bp->b_dev)].d_strategy)(bp);
	if ((flag&B_ASYNC) == 0) {
		iowait(bp);
		brelse(bp);
	} else if (flag & B_DELWRI)
		bp->b_flags |= B_AGE;
}

/*
 * Release the buffer, marking it so that if it is grabbed
 * for another purpose it will be written out before being
 * given up (e.g. when writing a partial block where it is
 * assumed that another write for the same block will soon follow).
 * This can't be done for magtape, since writes must be done
 * in the same order as requested.
 */
bdwrite(bp)
register struct buf *bp;
{
	register struct buf *dp;

	dp = bdevsw[major(bp->b_dev)].d_tab;
	if(dp->b_flags & B_TAPE)
		bawrite(bp);
	else {
		bp->b_flags |= B_DELWRI | B_DONE;
		brelse(bp);
	}
}

/*
 * Release the buffer, start I/O on it, but don't wait for completion.
 */
bawrite(bp)
register struct buf *bp;
{

	bp->b_flags |= B_ASYNC;
	bwrite(bp);
}

/*
 * release the buffer, with no I/O implied.
 */
brelse(bp)
register struct buf *bp;
{
	register struct buf **backp;
	register s;

	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	if (bfreelist.b_flags&B_WANTED) {
		bfreelist.b_flags &= ~B_WANTED;
		wakeup((caddr_t)&bfreelist);
	}
	if (bp->b_flags&B_ERROR) {
#ifdef UCB_BHASH
		bunhash(bp);
#endif
		bp->b_dev = NODEV;   /* no assoc. on error */
	}
	s = spl6();
	if(bp->b_flags & B_AGE) {
		backp = &bfreelist.av_forw;
		(*backp)->av_back = bp;
		bp->av_forw = *backp;
		*backp = bp;
		bp->av_back = &bfreelist;
	} else {
		backp = &bfreelist.av_back;
		(*backp)->av_forw = bp;
		bp->av_back = *backp;
		*backp = bp;
		bp->av_forw = &bfreelist;
	}
	bp->b_flags &= ~(B_WANTED|B_BUSY|B_ASYNC|B_AGE);
	splx(s);
}

/*
 * See if the block is associated with some buffer
 * (mainly to avoid getting hung up on a wait in breada)
 */
incore(dev, blkno)
register dev_t dev;
daddr_t blkno;
{
	register struct buf *bp;
#ifndef UCB_BHASH
	register struct buf *dp;
#endif

#ifdef  UCB_BHASH
	bp = bhash[BUFHASH(blkno)];
	blkno = fsbtodb(blkno);
	for(; bp != NULL; bp = bp->b_link)
#else
	dp = bdevsw[major(dev)].d_tab;
	blkno = fsbtodb(blkno);
	for (bp = dp->b_forw; bp != dp; bp = bp->b_forw)
#endif
		if (bp->b_blkno == blkno && bp->b_dev == dev)
			return(1);
	return(0);
}

/*
 * Assign a buffer for the given block.  If the appropriate
 * block is already associated, return it; otherwise search
 * for the oldest non-busy buffer and reassign it.
 */
struct buf *
getblk(dev, blkno)
dev_t dev;
daddr_t blkno;
{
	register struct buf *bp;
	register struct buf *dp;
#ifdef UCB_BHASH
	register int j;
#endif
	daddr_t dblkno;

	if(major(dev) >= nblkdev)
		panic("blkdev");

	dp = bdevsw[major(dev)].d_tab;
	if(dp == NULL)
		panic("devtab");
    loop:
	(void) _spl0();
#ifdef  UCB_BHASH
	j = BUFHASH(blkno);
	bp = bhash[j];
	dblkno  = fsbtodb(blkno);
	for(; bp != NULL; bp = bp->b_link)
#else
	dblkno  = fsbtodb(blkno);
	for (bp=dp->b_forw; bp != dp; bp = bp->b_forw)
#endif
	{
		if (bp->b_blkno != dblkno || bp->b_dev != dev)
			continue;
		(void) _spl6();
		if (bp->b_flags&B_BUSY) {
			bp->b_flags |= B_WANTED;
			sleep((caddr_t)bp, PRIBIO+1);
			goto loop;
		}
		(void) _spl0();
		notavail(bp);
		return(bp);
	}
	(void) _spl6();
	if (bfreelist.av_forw == &bfreelist) {
		bfreelist.b_flags |= B_WANTED;
		sleep((caddr_t)&bfreelist, PRIBIO+1);
		goto loop;
	}
	(void) _spl0();
	notavail(bp = bfreelist.av_forw);
	if (bp->b_flags & B_DELWRI) {
		bawrite(bp);
		goto loop;
	}
#ifdef UCB_BHASH
	bunhash(bp);
#endif
	bp->b_flags = B_BUSY;
	bp->b_back->b_forw = bp->b_forw;
	bp->b_forw->b_back = bp->b_back;
	bp->b_forw = dp->b_forw;
	bp->b_back = dp;
	dp->b_forw->b_back = bp;
	dp->b_forw = bp;
	bp->b_dev = dev;
	bp->b_blkno = dblkno;
#ifdef UCB_BHASH
	bp->b_link = bhash[j];
	bhash[j] = bp;
#endif
	return(bp);
}

/*
 * Get a block not assigned to any device
 */
struct buf *
geteblk()
{
	register struct buf *bp;
	register struct buf *dp;

loop:
	(void) _spl6();
	while (bfreelist.av_forw == &bfreelist) {
		bfreelist.b_flags |= B_WANTED;
		sleep((caddr_t)&bfreelist, PRIBIO+1);
	}
	(void) _spl0();
	dp = &bfreelist;
	notavail(bp = bfreelist.av_forw);
	if (bp->b_flags & B_DELWRI) {
		bp->b_flags |= B_ASYNC;
		bwrite(bp);
		goto loop;
	}
#ifdef UCB_BHASH
	bunhash(bp);
#endif
	bp->b_flags = B_BUSY;
	bp->b_back->b_forw = bp->b_forw;
	bp->b_forw->b_back = bp->b_back;
	bp->b_forw = dp->b_forw;
	bp->b_back = dp;
	dp->b_forw->b_back = bp;
	dp->b_forw = bp;
	bp->b_dev = (dev_t)NODEV;
#ifdef UCB_BHASH
	bp->b_link = NULL;
#endif
	return(bp);
}

#ifdef UCB_BHASH
bunhash(bp)
register struct buf *bp;
{
	register struct buf *ep;
	register int i;

	if (bp->b_dev == NODEV)
		return;
	i = BUFHASH(dbtofsb(bp->b_blkno));
	ep = bhash[i];
	if (ep == bp) {
		bhash[i] = bp->b_link;
		return;
	}
	for(; ep != NULL; ep = ep->b_link)
		if (ep->b_link == bp) {
			ep->b_link = bp->b_link;
			return;
		}
	panic("bunhash");
}
#endif

/*
 * Wait for I/O completion on the buffer; return errors
 * to the user.
 */
iowait(bp)
register struct buf *bp;
{

	(void) _spl6();
	while ((bp->b_flags&B_DONE)==0)
		sleep((caddr_t)bp, PRIBIO);
	(void) _spl0();
	geterror(bp);
}

/*
 * Unlink a buffer from the available list and mark it busy.
 * (internal interface)
 */
notavail(bp)
register struct buf *bp;
{
	register s;

	s = spl6();
	bp->av_back->av_forw = bp->av_forw;
	bp->av_forw->av_back = bp->av_back;
	bp->b_flags |= B_BUSY;
	splx(s);
}

/*
 * Mark I/O complete on a buffer, release it if I/O is asynchronous,
 * and wake up anyone waiting for it.
 */
iodone(bp)
register struct buf *bp;
{
#ifdef  UNIBUS_MAP
	if(bp->b_flags & (B_MAP|B_UBAREMAP))
		mapfree(bp);
#endif
	bp->b_flags |= B_DONE;
	if (bp->b_flags&B_ASYNC)
		brelse(bp);
	else {
		bp->b_flags &= ~B_WANTED;
		wakeup((caddr_t)bp);
	}
}

/*
 * Zero the core associated with a buffer.
 * Since this routine calls mapin,
 * it cannot be called from interrupt routines.
 */
clrbuf(bp)
register struct buf *bp;
{
	register *p;
	register c;

	p = (int *) mapin(bp);
	c = (BSIZE/sizeof(int)) >> 2;
	do {
		*p++ = 0;
		*p++ = 0;
		*p++ = 0;
		*p++ = 0;
	} while (--c);
	bp->b_resid = 0;
	mapout(bp);
}

/*
 * swap I/O
 */
swap(blkno, coreaddr, count, rdflg)
memaddr blkno, coreaddr;
register count;
{
	register struct buf *bp;
	register tcount;

#ifdef UCB_METER
	if (rdflg) {
		cnt.v_pswpin += count;
		cnt.v_swpin++;
	} else {
		cnt.v_pswpout += count;
		cnt.v_swpout++;
	}
#endif
	bp = &swbuf1;
	if(bp->b_flags & B_BUSY)
		if((swbuf2.b_flags&B_WANTED) == 0)
			bp = &swbuf2;
	(void) _spl6();
	while (bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PSWP+1);
	}
	(void) _spl0();
	while (count) {
		bp->b_flags = B_BUSY | B_PHYS | rdflg;
		bp->b_dev = swapdev;
		tcount = count;
		if (tcount >= 01000)    /* prevent byte-count wrap */
			tcount = 01000;
		bp->b_bcount = ctob(tcount);
		bp->b_blkno = swplo+blkno;
		bp->b_un.b_addr = (caddr_t)(coreaddr<<6);
		bp->b_xmem = (coreaddr>>10) & 077;
		(*bdevsw[major(swapdev)].d_strategy)(bp);
		(void) _spl6();
		while((bp->b_flags&B_DONE)==0)
			sleep((caddr_t)bp, PSWP);
		(void) _spl0();
		if ((bp->b_flags & B_ERROR) || bp->b_resid) {
			extern panicreboot;

			panicreboot++;
			panic("IO err in swap");
		}
		count -= tcount;
		coreaddr += tcount;
		blkno += ctod(tcount);
	}
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= ~(B_BUSY|B_WANTED);
}

/*
 * make sure all write-behind blocks
 * on dev (or NODEV for all)
 * are flushed out.
 * (from umount and update)
 */
bflush(dev)
register dev_t dev;
{
	register struct buf *bp;

loop:
	(void) _spl6();
	for (bp = bfreelist.av_forw; bp != &bfreelist; bp = bp->av_forw) {
		if (bp->b_flags&B_DELWRI && (dev == NODEV||dev==bp->b_dev)) {
			bp->b_flags |= B_ASYNC;
			notavail(bp);
			bwrite(bp);
			goto loop;
		}
	}
	(void) _spl0();
}

/*
 * Raw I/O. The arguments are
 *	The strategy routine for the device
 *	A buffer, which will always be a special buffer
 *	  header owned exclusively by the device for this purpose
 *	The device number
 *	Read/write flag
 * Essentially all the work is computing physical addresses and
 * validating them.
 *
 * physio broken into smaller routines, 3/81 mjk
 *	chkphys(WORD or BYTE) checks validity of word- or byte-
 *	oriented transfer (for physio or device drivers);
 *	physbuf(strat,bp,rw) fills in the buffer header.
 *
 * physio divided into two functions, 1/83 - Mike Edmonds - Tektronix
 *	Physio divided into separate functions:
 *		physio (for WORD i/o)
 *		bphysio (for BYTE i/o)
 *	This allows byte-oriented devices (such as tape drives)
 *	to write/read odd length blocks.
 */

physio(strat, bp, dev, rw)
register struct buf *bp;
int (*strat)();
dev_t dev;
{
	physio1(strat, bp, dev, rw, WORD);
}

bphysio(strat, bp, dev, rw)
register struct buf *bp;
int (*strat)();
dev_t dev;
{
	physio1(strat, bp, dev, rw, BYTE);
}

physio1(strat, bp, dev, rw, kind)
register struct buf *bp;
int (*strat)();
dev_t dev;
{
	if (chkphys(kind))
		return;
	physbuf(bp,dev,rw);
	u.u_procp->p_flag |= SLOCK;
	(*strat)(bp);
	iowait(bp);
	u.u_procp->p_flag &= ~SLOCK;
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	if(runin!=0) {
		runin = 0;
		wakeup((caddr_t)&runin);
	}
	bp->b_flags &= ~(B_BUSY|B_WANTED);
	u.u_count = bp->b_resid;
}

/*
 * check for validity of physical I/O area
 * (modified from physio to use flag for BYTE-oriented transfers)
 */
chkphys(flag)
{
	register unsigned base;
	register int nb;
	register ts;

	base = (unsigned)u.u_base;
	/*
	 * Check odd base, odd count, and address wraparound
	 * Odd base and count not allowed if flag=WORD,
	 * allowed if flag=BYTE.
	 */
	if (flag==WORD && (base&01 || u.u_count&01))
		goto bad;
	if (base>=base+u.u_count)
		goto bad;
	if (u.u_sep)
		ts = 0;
	else
		ts = (u.u_tsize+127) & ~0177;
	nb = (base>>6) & 01777;
	/*
	 * Check overlap with text. (ts and nb now
	 * in 64-byte clicks)
	 */
	if (nb < ts)
		goto bad;
	/*
	 * Check that transfer is either entirely in the
	 * data or in the stack: that is, either
	 * the end is in the data or the start is in the stack
	 * (remember wraparound was already checked).
	 */
	if ((((base+u.u_count)>>6)&01777) >= ts+u.u_dsize && nb < 1024-u.u_ssize)
		if( chkph(base) )
			goto bad;
	return(0);

    bad:
	u.u_error = EFAULT;
	return(-1);
}

/*
 * wait for buffer header, then fill it in to do physical I/O.
 */
physbuf(bp,dev,rw)
register struct buf *bp;
dev_t dev;
{
	register int nb;
	register unsigned base;
	register int ts;

	base = (unsigned)u.u_base;
	nb = (base>>6) & 01777;

	(void) _spl6();
	while (bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO+1);
	}
	(void) _spl0();
	bp->b_flags = B_BUSY | B_PHYS | rw;
	bp->b_dev = dev;
	/*
	 * Compute physical address by simulating
	 * the segmentation hardware.
	 */
	ts = (u.u_sep? UDSA: UISA)[nb>>7] + (nb&0177);
	bp->b_un.b_addr = (caddr_t)((ts<<6) + (base&077));
	bp->b_xmem = (ts>>10) & 077;
	bp->b_blkno = u.u_offset >> PGSHIFT;
	bp->b_bcount = u.u_count;
	bp->b_error = 0;
}

/*
 * Pick up the device's error number and pass it to the user;
 * if there is an error but the number is 0 set a generalized
 * code.
 */
geterror(bp)
register struct buf *bp;
{

	if (bp->b_flags&B_ERROR)
		if ((u.u_error = bp->b_error)==0)
			u.u_error = EIO;
}

/*
 * Count all of requests in device queues
 */
breqcount()
{
	register struct buf *bp;
	register i;
	register count;
	int oldspl;

	count = 0;
	oldspl = spl6();
	for( i = 0 ; i < nblkdev ; i++ ) {
		if( bdevsw[i].d_tab == (struct buf *)0 )
			continue;
		for( bp = bdevsw[i].d_tab->b_actf ;
				bp != (struct buf *)0 ; bp = bp->av_forw )
			count++;
	}
	splx(oldspl);
	return count;
}

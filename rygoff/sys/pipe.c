/*
 * $Log:	pipe.c,v $
 * Revision 1.2  88/01/23  18:57:59  korotaev
 * Состояние перед сливанием с AVG-Э-85.
 * 
 * Revision 1.1  86/04/19  15:50:51  avg
 * Initial revision
 *
 */

#include "param.h"
#include <sys/systm.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/inode.h>
#include <sys/file.h>
#include <sys/reg.h>
#include <sys/inline.h>

/*
 * Max allowable buffering per write on a pipe.
 * This is also roughly the max size of the
 * file created to implement the pipe.
 * If this size is bigger than 4096
 * (5120 without UCB_NKB),
 * pipes will be implemented with large
 * files, which is probably not good.
 */
#define PIPSIZ  4096

/*
 * The sys-pipe entry.
 * Allocate an inode on the root device.
 * Allocate 2 file structures.
 * Put it all together with flags.
 */
pipe()
{
	register struct inode *ip;
	register struct file *rf, *wf;
	register int r;

	ip = ialloc(pipedev);
	if(ip == NULL) {
		/*
		 * Someone may not have mounted pipedev so we
		 * try to get an inode from rootdev.
		 * Getfs() will have already printed a message
		 * about its inability to find pipedev in the
		 * mount table, so don't complain.
		 */
		ip = ialloc(rootdev);
		if (ip == NULL)
			return;
		/*
		 * This is necessary because the ialloc(pipedev) above
		 * set u.u_error to ENOSPC.
		 */
		else
			u.u_error = 0;
	}
	rf = falloc();
	if(rf == NULL) {
		iput(ip);
		return;
	}
	r = u.u_r.r_val1;
	wf = falloc();
	if(wf == NULL) {
		rf->f_count = 0;
		u.u_ofile[r] = NULL;
		iput(ip);
		return;
	}
	u.u_r.r_val2 = u.u_r.r_val1;
	u.u_r.r_val1 = r;
	wf->f_flag = FWRITE|FPIPE;
	wf->f_inode = ip;
	rf->f_flag = FREAD|FPIPE;
	rf->f_inode = ip;
	ip->i_count = 2;
	ip->i_mode = IFREG;
	ip->i_flag = IACC|IUPD|ICHG|IPIPE;
}

/*
 * Read call directed to a pipe.
 */
readp(fp)
register struct file *fp;
{
	register struct inode *ip;

	ip = fp->f_inode;

loop:
	/*
	 * Very conservative locking.
	 */

	plock(ip);
	/*
	 * If nothing in the pipe, wait.
	 */
	if (ip->i_size == 0) {
		/*
		 * If there are not both reader and
		 * writer active, return without
		 * satisfying read.
		 */
		prele(ip);
		if(ip->i_count < 2)
			return;
		ip->i_mode |= IREAD;
		sleep((caddr_t)ip+2, PPIPE);
		goto loop;
	}

	/*
	 * Read and return
	 */

	u.u_offset = fp->f_un.f_offset;
	readi(ip);
	fp->f_un.f_offset = u.u_offset;
	/*
	 * If reader has caught up with writer, reset
	 * offset and size to 0.
	 */
	if (fp->f_un.f_offset == ip->i_size) {
		fp->f_un.f_offset = 0;
		ip->i_size = 0;
		if(ip->i_mode & IWRITE) {
			ip->i_mode &= ~IWRITE;
			wakeup((caddr_t)ip+1);
		}
	}
	prele(ip);
}

/*
 * Write call directed to a pipe.
 */
writep(fp)
register struct file *fp;
{
	register c;
	register struct inode *ip;

	ip = fp->f_inode;
	c = u.u_count;

loop:

	/*
	 * If all done, return.
	 */

	plock(ip);
	if(c == 0) {
		prele(ip);
		u.u_count = 0;
		return;
	}

	/*
	 * If there are not both read and
	 * write sides of the pipe active,
	 * return error and signal too.
	 */

	if(ip->i_count < 2) {
		prele(ip);
		u.u_error = EPIPE;
		psignal(u.u_procp, SIGPIPE);
		return;
	}

	/*
	 * If the pipe is full,
	 * wait for reads to deplete
	 * and truncate it.
	 */

	if(ip->i_size >= PIPSIZ) {
		ip->i_mode |= IWRITE;
		prele(ip);
		sleep((caddr_t)ip+1, PPIPE);
		goto loop;
	}

	/*
	 * Write what is possible and
	 * loop back.
	 * If writing less than PIPSIZ, it always goes.
	 * One can therefore get a file > PIPSIZ if write
	 * sizes do not divide PIPSIZ.
	 */

	u.u_offset = ip->i_size;
	u.u_count = MIN((unsigned)c, (unsigned)PIPSIZ);
	c -= u.u_count;
	writei(ip);
	prele(ip);
	if(ip->i_mode&IREAD) {
		ip->i_mode &= ~IREAD;
		wakeup((caddr_t)ip+2);
	}
	goto loop;
}

#ifdef  plock
#undef  plock
#endif
#ifdef  prele
#undef  prele
#endif
/*
 * Lock a pipe.
 * If its already locked,
 * set the WANT bit and sleep.
 */
plock(ip)
register struct inode *ip;
{
	register s = spl6();

	while(ip->i_flag&ILOCK) {
		ip->i_flag |= IWANT;
		sleep((caddr_t)ip, PINOD);
	}
	ip->i_flag |= ILOCK;
	splx(s);
}

/*
 * Unlock a pipe.
 * If WANT bit is on,
 * wakeup.
 * This routine is also used
 * to unlock inodes in general.
 */
prele(ip)
register struct inode *ip;
{

	ip->i_flag &= ~ILOCK;
	if(ip->i_flag&IWANT) {
		ip->i_flag &= ~IWANT;
		wakeup((caddr_t)ip);
	}
}

/* @(#)sem.c    6.1 */
/*
**      Inter-Process Communication Semaphore Facility.
**      цельнотянуто из системы 5.2
**      $Log:	sem.c,v $
 * Revision 22.6  90/11/12  19:16:31  root
 * Новые вещи для СМ1425 и перемещение include.
 * 
 * Revision 22.5  89/05/11  14:11:48  abs
 * перестановки порядка includ-ов
 * 
 * Revision 22.4  89/05/11  13:47:31  abs
 * завел seminfo
 *
 * Revision 22.3  89/04/27  14:27:20  korotaev
 * Изменения в связи с передвижением каталогов и файлов
 *
 * Revision 22.2  89/04/25  14:02:56  korotaev
 * Появился вместо "NPROC.h" "space.h"
 *
 * Revision 22.1  89/04/12  15:32:19  korotaev
 * "param.h" ==> <sys/param.h>
 *
 * Revision 22.0  89/03/30  16:27:41  korotaev
 * Begining DEMOS 2.2.
 *
 * Revision 1.1  89/03/10  15:50:11  abs
 * Initial revision
 *
*/

#include <sys/param.h>
#include <sys/types.h>
#include <sys/dir.h>
#include "../include/map.h"
#include <errno.h>
#include <signal.h>
#include "h/space.h"
#include <sys/sem.h>
#include <sys/user.h>
#include <sys/proc.h>
#include "../include/buf.h"

#if SEMMNS > 0
#       ifndef _sys_ipc_h_
#include <sys/ipc.h>
#       endif

#define u_rval1 u_r.r_val1
#define u_rval2 u_r.r_val2

#define MOVE    iomove
#define paddr_t caddr_t

typedef unsigned int uint ;

struct semid_ds sema[SEMMNI];     /* semaphore data structure (group headers) */
struct sem      sem[SEMMNS];      /* semaphores */

struct mapent   _semmap[SEMMAP];
struct map      semmap[1] = {          /* sem allocation map */
	_semmap, &_semmap[SEMMAP], "semmap" };

struct  sem_undo        *sem_undo[NPROC];
/* undo table pointers (one per process). Вынесена в param.c,
 * поскольку здесь неизвестна константа NPROC
 */

/*   such a strange kludge puzzle ...
int    _semu[((SEMUSZ*SEMMNU)+NBPW-1)/NBPW];
struct sem_undo *semu = (struct sem_undo *) _semu;
*/
struct sem_undo semu[ SEMMNU ];
	/* operation adjust on exit table */

union {
	short           semvals[SEMMSL];  /* set semaphore values */
	struct semid_ds ds;                     /* set permission values */
	struct sembuf   semops[SEMOPM];   /* operation holding area */
}       semtmp;

struct sem_undo *semunp;                /* ptr to head of undo chain */
struct sem_undo *semfup;                /* ptr to head of free undo chain */

extern time_t   time;                   /* system idea of date */

struct semid_ds *ipcget(),
		*semconv();

struct seminfo seminfo = {
	SEMMAP,
	SEMMNI,
	SEMMNS,
	SEMMNU,
	SEMMSL,
	SEMOPM,
	SEMUME,
	SEMUSZ,
	SEMVMX,
	SEMAEM
};

/*
**      semaoe - Create or update adjust on exit entry.
*/

semaoe(val, id, num)
short   val,    /* operation value to be adjusted on exit */
	num;    /* semaphore # */
int     id;     /* semid */
{
	register struct undo            *uup,   /* ptr to entry to update */
					*uup2;  /* ptr to move entry */
	register struct sem_undo        *up,    /* ptr to process undo struct */
					*up2;   /* ptr to undo list */
	register int                    i,      /* loop control */
					found;  /* matching entry found flag */

	if(val == 0)
		return(0);
	if(val > SEMAEM || val < -SEMAEM) {
		u.u_error = ERANGE;
		return(1);
	}
	if((up = sem_undo[u.u_procp - proc]) == NULL)
		if (up = semfup) {
			semfup = up->un_np;
			up->un_np = NULL;
			sem_undo[u.u_procp - proc] = up;
		} else {
			u.u_error = ENOSPC;
			return(1);
		}
	for(uup = up->un_ent, found = i = 0;i < up->un_cnt;i++) {
		if(uup->un_id < id || (uup->un_id == id && uup->un_num < num)) {
			uup++;
			continue;
		}
		if(uup->un_id == id && uup->un_num == num)
			found = 1;
		break;
	}
	if(!found) {
		if( up->un_cnt >= SEMUME) {
			u.u_error = EINVAL;
			return(1);
		}
		if(up->un_cnt == 0) {
			up->un_np = semunp;
			semunp = up;
		}
		uup2 = &up->un_ent[up->un_cnt++];
		while(uup2-- > uup)
			*(uup2 + 1) = *uup2;
		uup->un_id = id;
		uup->un_num = num;
		uup->un_aoe = -val;
		return(0);
	}
	uup->un_aoe -= val;
	if(uup->un_aoe > SEMAEM || uup->un_aoe < -SEMAEM) {
		u.u_error = ERANGE;
		uup->un_aoe += val;
		return(1);
	}
	if(uup->un_aoe == 0) {
		uup2 = &up->un_ent[--(up->un_cnt)];
		while(uup++ < uup2)
			*(uup - 1) = *uup;
		if(up->un_cnt == 0) {

			/* Remove process from undo list. */
			if(semunp == up)
				semunp = up->un_np;
			else
				for(up2 = semunp;up2 != NULL;up2 = up2->un_np)
					if(up2->un_np == up) {
						up2->un_np = up->un_np;
						break;
					}
			up->un_np = NULL;
		}
	}
	return(0);
}

/*
**      semconv - Convert user supplied semid into a ptr to the associated
**              semaphore header.
*/

struct semid_ds *
semconv(s)
register int    s;      /* semid */
{
	register struct semid_ds        *sp;    /* ptr to associated header */

	sp = &sema[s % SEMMNI] ;
	if((sp->sem_perm.mode & IPC_ALLOC) == 0 ||
		s / SEMMNI != sp->sem_perm.seq) {
		u.u_error = ENOENT;
		return(NULL);
	}
	return(sp);
}

/*
**      semctl - Semctl system call.
*/

semctl()
{
	register struct a {
		int     semid;
		uint    semnum;
		int     cmd;
		int     arg;
	}       *uap = (struct a *)u.u_ap;
	register struct semid_ds        *sp;    /* ptr to semaphore header */
	register struct sem             *p;     /* ptr to semaphore */
	register int                    i;      /* loop control */

	if((sp = semconv(uap->semid)) == NULL)
		return;
	u.u_rval1 = 0;
	switch(uap->cmd) {

	/* Remove semaphore set. */
	case IPC_RMID:
		if(u.u_uid != sp->sem_perm.uid && u.u_uid != sp->sem_perm.cuid
			&& !suser())
			return;
		semunrm(uap->semid, 0, sp->sem_nsems);
		for(i = sp->sem_nsems, p = sp->sem_base;i--;p++) {
			p->semval = p->sempid = 0;
			if(p->semncnt) {
				wakeup(&p->semncnt);
				p->semncnt = 0;
			}
			if(p->semzcnt) {
				wakeup(&p->semzcnt);
				p->semzcnt = 0;
			}
		}
		mfree(semmap, sp->sem_nsems, (sp->sem_base - sem) + 1);
		if(uap->semid + SEMMNI < 0)
			sp->sem_perm.seq = 0;
		else
			sp->sem_perm.seq++;
		sp->sem_perm.mode = 0;
		return;

	/* Set ownership and permissions. */
	case IPC_SET:
		if(u.u_uid != sp->sem_perm.uid && u.u_uid != sp->sem_perm.cuid
			 && !suser())
			return;
		if(copyin((caddr_t)uap->arg, (caddr_t) &semtmp.ds, sizeof(semtmp.ds))) {
			u.u_error = EFAULT;
			return;
		}
		sp->sem_perm.uid = semtmp.ds.sem_perm.uid;
		sp->sem_perm.gid = semtmp.ds.sem_perm.gid;
		sp->sem_perm.mode = semtmp.ds.sem_perm.mode & 0777 | IPC_ALLOC;
		sp->sem_ctime = time;
		return;

	/* Get semaphore data structure. */
	case IPC_STAT:
		if(ipcaccess(&sp->sem_perm, SEM_R))
			return;
		if(copyout((caddr_t) sp, (caddr_t) uap->arg, sizeof(*sp))) {
			u.u_error = EFAULT;
			return;
		}
		return;

	/* Get # of processes sleeping for greater semval. */
	case GETNCNT:
		if(ipcaccess(&sp->sem_perm, SEM_R))
			return;
		if(uap->semnum >= sp->sem_nsems) {
			u.u_error = EINVAL;
			return;
		}
		u.u_rval1 = (sp->sem_base + uap->semnum)->semncnt;
		return;

	/* Get pid of last process to operate on semaphore. */
	case GETPID:
		if(ipcaccess(&sp->sem_perm, SEM_R))
			return;
		if(uap->semnum >= sp->sem_nsems) {
			u.u_error = EINVAL;
			return;
		}
		u.u_rval1 = (sp->sem_base + uap->semnum)->sempid;
		return;

	/* Get semval of one semaphore. */
	case GETVAL:
		if(ipcaccess(&sp->sem_perm, SEM_R))
			return;
		if(uap->semnum >= sp->sem_nsems) {
			u.u_error = EINVAL;
			return;
		}
		u.u_rval1 = (sp->sem_base + uap->semnum)->semval;
		return;

	/* Get all semvals in set. */
	case GETALL:
		if(ipcaccess(&sp->sem_perm, SEM_R))
			return;
		u.u_base = (caddr_t)uap->arg;
		u.u_offset = 0;
		u.u_segflg = 0;
		for(i = sp->sem_nsems, p = sp->sem_base;i--;p++) {
			MOVE((paddr_t)&p->semval, sizeof(p->semval), B_READ);
			if(u.u_error)
				return;
		}
		return;

	/* Get # of processes sleeping for semval to become zero. */
	case GETZCNT:
		if(ipcaccess(&sp->sem_perm, SEM_R))
			return;
		if(uap->semnum >= sp->sem_nsems) {
			u.u_error = EINVAL;
			return;
		}
		u.u_rval1 = (sp->sem_base + uap->semnum)->semzcnt;
		return;

	/* Set semval of one semaphore. */
	case SETVAL:
		if(ipcaccess(&sp->sem_perm, SEM_A))
			return;
		if(uap->semnum >= sp->sem_nsems) {
			u.u_error = EINVAL;
			return;
		}
		if((unsigned)uap->arg > SEMVMX) {
			u.u_error = ERANGE;
			return;
		}
		if((p = sp->sem_base + uap->semnum)->semval = uap->arg) {
			if(p->semncnt) {
				p->semncnt = 0;
				wakeup(&p->semncnt);
			}
		} else
			if(p->semzcnt) {
				p->semzcnt = 0;
				wakeup(&p->semzcnt);
			}
		p->sempid = u.u_procp->p_pid;
		semunrm(uap->semid, uap->semnum, uap->semnum);
		return;

	/* Set semvals of all semaphores in set. */
	case SETALL:
		if(ipcaccess(&sp->sem_perm, SEM_A))
			return;
		u.u_base = (caddr_t)uap->arg;
		u.u_offset = 0;
		u.u_segflg = 0;
		MOVE((paddr_t)semtmp.semvals,
			sizeof(semtmp.semvals[0]) * sp->sem_nsems, B_WRITE);
		if(u.u_error)
			return;
		for(i = 0;i < sp->sem_nsems;)
			if(semtmp.semvals[i++] > SEMVMX) {
				u.u_error = ERANGE;
				return;
			}
		semunrm(uap->semid, 0, sp->sem_nsems);
		for(i = 0, p = sp->sem_base;i < sp->sem_nsems;
			(p++)->sempid = u.u_procp->p_pid) {
			if(p->semval = semtmp.semvals[i++]) {
				if(p->semncnt) {
					p->semncnt = 0;
					wakeup(&p->semncnt);
				}
			} else
				if(p->semzcnt) {
					p->semzcnt = 0;
					wakeup(&p->semzcnt);
				}
		}
		return;
	default:
		u.u_error = EINVAL;
		return;
	}
}

/*
**      semexit - Called by exit(sys1.c) to clean up on process exit.
*/
/*      Имеется такая идея: в структуре sem_perm хранить еще и pid
 *      процесса, первым выполнившего semget()  (процесс-создатель).
 *      И при окончании этого процесса делать (в semexit)
 *      semctl( id, IPC_RMID...) для всех групп семафоров,
 *      создателем которых является этот процесс. В sV этого нет,
 *      но идея так и просится. Однако, как это сделать - очевидно,
 *      и делать это я не буду
 */
semexit()
{
	register struct sem_undo        *up,    /* process undo struct ptr */
					*p;     /* undo struct ptr */
	register struct semid_ds        *sp;    /* semid being undone ptr */
	register int                    i;      /* loop control */
	register long                   v;      /* adjusted value */
	register struct sem             *semp;  /* semaphore ptr */

	if((up = sem_undo[u.u_procp - proc]) == NULL)
		return;
	if(up->un_cnt == 0)
		goto cleanup;
	for(i = up->un_cnt;i--;) {
		if((sp = semconv(up->un_ent[i].un_id)) == NULL)
			continue;
		v = (long)(semp = sp->sem_base + up->un_ent[i].un_num)->semval +
			up->un_ent[i].un_aoe;
		if(v < 0 || v > SEMVMX)
			continue;
		semp->semval = v;
		if(v == 0 && semp->semzcnt) {
			semp->semzcnt = 0;
			wakeup(&semp->semzcnt);
		}
		if(up->un_ent[i].un_aoe > 0 && semp->semncnt) {
			semp->semncnt = 0;
			wakeup(&semp->semncnt);
		}
	}
	up->un_cnt = 0;
	if(semunp == up)
		semunp = up->un_np;
	else
		for(p = semunp;p != NULL;p = p->un_np)
			if(p->un_np == up) {
				p->un_np = up->un_np;
				break;
			}
cleanup:
	up->un_np = semfup;
	semfup = up;
	sem_undo[u.u_procp - proc] = NULL;
}

/*
**      semget - Semget system call.
*/

semget()
{
	register struct a {
		key_t   key;
		int     nsems;
		int     semflg;
	}       *uap = (struct a *)u.u_ap;
	register struct semid_ds        *sp;    /* semaphore header ptr */
	register int                    i;      /* temp */
	int                             s;      /* ipcget status return */

	if((sp = ipcget(uap->key, uap->semflg, sema, SEMMNI, sizeof(*sp), &s))
		== NULL)
		return;
	if(s) {

		/* This is a new semaphore set.  Finish initialization. */
		if(uap->nsems <= 0 || uap->nsems > SEMMSL) {
			u.u_error = EFBIG;
			sp->sem_perm.mode = 0;
			return;
		}
		if((i = malloc(semmap, uap->nsems)) == NULL) {
			u.u_error = ENOSPC;
			sp->sem_perm.mode = 0;
			return;
		}
		sp->sem_base = sem + (i - 1);
		sp->sem_nsems = uap->nsems;
		sp->sem_ctime = time;
		sp->sem_otime = (time_t) 0 ;
	} else
		if(uap->nsems && sp->sem_nsems < uap->nsems) {
			u.u_error = EINVAL;
			return;
		}
	u.u_rval1 = sp->sem_perm.seq * SEMMNI + (sp - sema);
}

/*
**      seminit - Called by main(main.c) to initialize the semaphore map.
*/

seminit()
{
	register i;

	/* mapinit(semmap, SEMMAP);     */
	mfree(semmap, SEMMNS, 1);

	semfup = semu;
	for (i = 0; i < SEMMNU - 1; i++) {
		/*
		semfup->un_np = (struct sem_undo *)((uint)semfup+SEMUSZ);
		*/
		/* совершенно очевидно, что надо писать так: */
		semfup->un_np = semfup+1;
		semfup = semfup->un_np;
		/* id est  semfup++;  */
	}
	semfup->un_np = NULL;
	semfup = semu;
}

/*
**      semop - Semop system call.
*/

semop()
{
	register struct a {
		int             semid;
		struct sembuf   *sops;
		uint            nsops;
	}       *uap = (struct a *)u.u_ap;
	register struct sembuf          *op;    /* ptr to operation */
	register int                    i;      /* loop control */
	register struct semid_ds        *sp;    /* ptr to associated header */
	register struct sem             *semp;  /* ptr to semaphore */
	int     again;
	u_short retvalue;

	if((sp = semconv(uap->semid)) == NULL)
		return;
	if(uap->nsops > SEMOPM) {
		u.u_error = E2BIG;
		return;
	}
	u.u_base = (caddr_t)uap->sops;
	u.u_offset = 0;
	u.u_segflg = 0;
	MOVE((paddr_t)semtmp.semops, uap->nsops * sizeof(*op), B_WRITE);
	if(u.u_error)
		return;

	/* Verify that sem #s are in range and permissions are granted. */
	for(i = 0, op = semtmp.semops;i++ < uap->nsops;op++) {
		if(ipcaccess(&sp->sem_perm, op->sem_op ? SEM_A : SEM_R))
			return;
		if(op->sem_num >= sp->sem_nsems) {
			u.u_error = EINVAL;
			return;
		}
	}
	again = 0;
check:
	/* Loop waiting for the operations to be satisified atomically. */
	/* Actually, do the operations and undo them if a wait is needed
		or an error is detected. */
	if (again) {
		/* Verify that the semaphores haven't been removed. */
		if(semconv(uap->semid) == NULL) {
			u.u_error = EIDRM;
			return;
		}
		/* copy in user operation list after sleep */
		u.u_base = (caddr_t)uap->sops;
		u.u_offset = 0;
		u.u_segflg = 0;
		MOVE((paddr_t)semtmp.semops, uap->nsops * sizeof(*op), B_WRITE);
		if(u.u_error)
			return;
	}
	again = 1;

	for(i = 0, op = semtmp.semops;i < uap->nsops;i++, op++) {
		semp = sp->sem_base + op->sem_num;
		retvalue = semp->semval;
		if(op->sem_op > 0) {
			if(op->sem_op + (long)semp->semval > SEMVMX ||
				(op->sem_flg & SEM_UNDO &&
				semaoe(op->sem_op, uap->semid, op->sem_num))) {
				if(u.u_error == 0)
					u.u_error = ERANGE;
				if(i)
					semundo(semtmp.semops, i, uap->semid, sp);
				return;
			}
			semp->semval += op->sem_op;
			if(semp->semncnt) {
				semp->semncnt = 0;
				wakeup(&semp->semncnt);
			}
			continue;
		}
		if(op->sem_op < 0) {
			if(semp->semval >= -op->sem_op) {
				if(op->sem_flg & SEM_UNDO &&
					semaoe(op->sem_op, uap->semid, op->sem_num)) {
					if(i)
						semundo(semtmp.semops, i, uap->semid, sp);
					return;
				}
				semp->semval += op->sem_op;
				if(semp->semval == 0 && semp->semzcnt) {
					semp->semzcnt = 0;
					wakeup(&semp->semzcnt);
				}
				continue;
			}
			if(i)
				semundo(semtmp.semops, i, uap->semid, sp);
			if(op->sem_flg & IPC_NOWAIT) {
				u.u_error = EAGAIN;
				return;
			}
			semp->semncnt++;
			if(sleep(&semp->semncnt, PCATCH | PSEMN)) {
				if((semp->semncnt)-- <= 1) {
					semp->semncnt = 0;
					wakeup(&semp->semncnt);
				}
				u.u_error = EINTR;
				return;
			}
			goto check;
		}
		if(semp->semval) {
			if(i)
				semundo(semtmp.semops, i, uap->semid, sp);
			if(op->sem_flg & IPC_NOWAIT) {
				u.u_error = EAGAIN;
				return;
			}
			semp->semzcnt++;
			if(sleep(&semp->semzcnt, PCATCH | PSEMZ)) {
				if((semp->semzcnt)-- <= 1) {
					semp->semzcnt = 0;
					wakeup(&semp->semzcnt);
				}
				u.u_error = EINTR;
				return;
			}
			goto check;
		}
	}

	/* All operations succeeded.  Update sempid for accessed semaphores. */
	for(i = 0, op = semtmp.semops;i++ < uap->nsops;
		(sp->sem_base + (op++)->sem_num)->sempid = u.u_procp->p_pid);
	sp->sem_otime = time;
	u.u_rval1 = retvalue;
	/* Вернуть значение, которое было у последнего обработанного
	 * семафора ДО его изменения
	 */
}

/*
**	semsys - System entry point for semctl, semget, and semop system calls.
*/

semsys()
{
	int	semctl(),
		semget(),
		semop();
	static int	(*calls[])() = {semctl, semget, semop};
	register struct a {
		uint	id;	/* function code id */
	}	*uap = (struct a *)u.u_ap;

	if(uap->id > 2) {
		u.u_error = EINVAL;
		return;
	}
	u.u_ap = &u.u_arg[1];
	(*calls[uap->id])();
}

/*
**	semundo - Undo work done up to finding an operation that can't be done.
*/

semundo(op, n, id, sp)
register struct sembuf		*op;	/* first operation that was done ptr */
register int			n,	/* # of operations that were done */
				id;	/* semaphore id */
register struct semid_ds	*sp;	/* semaphore data structure ptr */
{
	register struct sem	*semp;	/* semaphore ptr */

	for(op += n - 1;n--;op--) {
		if(op->sem_op == 0)
			continue;
		semp = sp->sem_base + op->sem_num;
		semp->semval -= op->sem_op;
		if(op->sem_flg & SEM_UNDO)
			semaoe(-op->sem_op, id, op->sem_num);
	}
}

/*
**	semunrm - Undo entry remover.
**
**	This routine is called to clear all undo entries for a set of semaphores
**	that are being removed from the system or are being reset by SETVAL or
**	SETVALS commands to semctl.
*/

semunrm(id, low, high)
int	id;	/* semid */
u_short  low,    /* lowest semaphore being changed */
	high;	/* highest semaphore being changed */
{
	register struct sem_undo	*pp,	/* ptr to predecessor to p */
					*p;	/* ptr to current entry */
	register struct undo		*up;	/* ptr to undo entry */
	register int			i,	/* loop control */
					j;	/* loop control */

	pp = NULL;
	p = semunp;
	while(p != NULL) {

		/* Search through current structure for matching entries. */
		for(up = p->un_ent, i = 0;i < p->un_cnt;) {
			if(id < up->un_id)
				break;
			if(id > up->un_id || low > up->un_num) {
				up++;
				i++;
				continue;
			}
			if(high < up->un_num)
				break;
			for(j = i;++j < p->un_cnt;
				p->un_ent[j - 1] = p->un_ent[j]);
			p->un_cnt--;
		}

		/* Reset pointers for next round. */
		if(p->un_cnt == 0)

			/* Remove from linked list. */
			if(pp == NULL) {
				semunp = p->un_np;
				p->un_np = NULL;
				p = semunp;
			} else {
				pp->un_np = p->un_np;
				p->un_np = NULL;
				p = pp->un_np;
			}
		else {
			pp = p;
			p = p->un_np;
		}
	}
}


#endif SEMMNS

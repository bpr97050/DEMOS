/*
 * $Log:	malloc.c,v $
 * Revision 22.2  90/11/12  19:12:24  root
 * Новые вещи для СМ1425 и перемещение include.
 * 
 * Revision 22.1  89/04/12  15:24:34  korotaev
 * "param.h" ==> <sys/param.h>
 * 
 * Revision 22.0  89/03/30  14:07:36  korotaev
 * Begining DEMOS 2.2
 * 
 * Revision 1.3  89/01/20  20:22:31  abs
 * new malloc (mexpand)
 * 
 * Revision 1.3  89/01/16  11:58:20  abs
 * Новая функция для расширения сегмента data простым увеличением размера, а не переписыванием
 * в другое место.
 * 
 * Revision 1.2  87/01/22  08:35:43  alex
 * Правки под сохранение сегмента text в памяти после выкачки процесса.
 * + введен параметр FXMETER для измерения эффекта от этого нововведения.
 *
 * Revision 1.1  86/04/19  15:50:28  avg
 * Initial revision
 *
 */

#include <sys/param.h>
#include <sys/systm.h>
#include "../include/map.h"
#ifdef UCB_METER
#include <sys/vm.h>
#endif
#define IPK_MEXPAND


/*
 * Resource map handling routines.
 *
 * A resource map is an array of structures each
 * of which describes a segment of the address space of an available
 * resource.  The segments are described by their base address and
 * length, and sorted in address order.  Each resource map has a fixed
 * maximum number of segments allowed.  Resources are allocated
 * by taking part or all of one of the segments of the map.
 *
 * Returning of resources will require another segment if
 * the returned resources are not adjacent in the address
 * space to an existing segment.  If the return of a segment
 * would require a slot which is not available, then one of
 * the resource map segments is discarded after a warning is printed.
 * Returning of resources may also cause the map to collapse
 * by coalescing two existing segments and the returned space
 * into a single segment.  In this case the resource map is
 * made smaller by copying together to fill the resultant gap.
 *
 * N.B.: the current implementation uses a dense array and does
 * not admit the value ``0'' as a legal address or size,
 * since that is used as a delimiter.
 */
extern short xuncount;
/*
 * Allocate 'size' units from the given
 * map. Return the base of the allocated space.
 * In a map, the addresses are increasing and the
 * list is terminated by a 0 size.
 *
 * Algorithm is first-fit.
 */
memaddr
malloc(mp, size)
struct map *mp;
register size_t size;
{
	register memaddr addr;
	register struct mapent *bp;
	int retry = 0;

	if (size == 0)
		panic("malloc");
	/*
	 * Search for a piece of the resource map which has enough
	 * free space to accomodate the request.
	 */
again:
	for (bp = mp->m_map; bp->m_size; bp++) {
		if (bp->m_size >= size) {
			/*
			 * Allocate from the map.
			 * If there is no space left of the piece
			 * we allocated from, move the rest of
			 * the pieces to the left.
			 */
			addr = bp->m_addr;
			bp->m_addr += size;
			if ((bp->m_size -= size) == 0) {
				do {
					bp++;
					(bp-1)->m_addr = bp->m_addr;
				} while ((bp-1)->m_size = bp->m_size);
			}
#ifdef  UCB_METER
			if (mp == coremap)
				freemem -= size;
#endif
			return (addr);
		}
	}
	if (mp == swapmap && retry++ == 0) {
		/*
		 * Attempt to avoid running out of swap--
		 * free ALL unused sticky text segments.
		 */
		printf("short of swap\n");
		xumount(NODEV);
		goto again;
	} else if(mp==coremap && xuncount && xunfree()) goto again;
	return (0);
}

/*
 * Free the previously allocated space at addr
 * of size units into the specified map.
 * Sort addr into map and combine on
 * one or both ends if possible.
 */
mfree(mp, size, addr)
struct map *mp;
size_t size;
register memaddr addr;
{
	struct mapent *firstbp;
	register struct mapent *bp;
	register u_short t;

	if (size == 0)
		return;
#ifdef  DIAGNOSTIC
	/*
	 * The address must be
	 * positive, or the protocol has broken down.
	 */
	if (addr == 0)
		goto badmfree;
#endif
	if (mp == coremap) {
		if (runin) {
			runin = 0;
			wakeup((caddr_t)&runin);
		}
#ifdef  UCB_METER
		freemem += size;
#endif
	}
	/*
	 * Locate the piece of the map which starts after the
	 * returned space (or the end of the map).
	 */
	firstbp = bp = mp->m_map;
	for (; bp->m_addr <= addr && bp->m_size != 0; bp++)
		continue;
	/*
	 * If the piece on the left abuts us,
	 * then we should combine with it.
	 */
	if (bp > firstbp && (bp-1)->m_addr+(bp-1)->m_size >= addr) {
#ifdef  DIAGNOSTIC
		/*
		 * Check no overlap (internal error).
		 */
		if ((bp-1)->m_addr+(bp-1)->m_size > addr)
			goto badmfree;
#endif
		/*
		 * Add into piece on the left by increasing its size.
		 */
		(bp-1)->m_size += size;
		/*
		 * If the combined piece abuts the piece on
		 * the right now, compress it in also,
		 * by shifting the remaining pieces of the map over.
		 */
		if (bp->m_addr && addr+size >= bp->m_addr) {
#ifdef  DIAGNOSTIC
			if (addr+size > bp->m_addr)
				goto badmfree;
#endif
			(bp-1)->m_size += bp->m_size;
			while (bp->m_size) {
				bp++;
				(bp-1)->m_addr = bp->m_addr;
				(bp-1)->m_size = bp->m_size;
			}
		}
		goto done;
	}
	/*
	 * Don't abut on the left, check for abutting on
	 * the right.
	 */
	if (addr+size >= bp->m_addr && bp->m_size) {
#ifdef  DIAGNOSTIC
		if (addr+size > bp->m_addr)
			goto badmfree;
#endif
		bp->m_addr -= size;
		bp->m_size += size;
		goto done;
	}
	/*
	 * Don't abut at all.  Make a new entry
	 * and check for map overflow.
	 */
	do {
		t = bp->m_addr;
		bp->m_addr = addr;
		addr = t;
		t = bp->m_size;
		bp->m_size = size;
		bp++;
	} while (size = t);
	/*
	 * Segment at bp is to be the delimiter;
	 * If there is not room for it
	 * then the table is too full
	 * and we must discard something.
	 */
	if (bp+1 > mp->m_limit) {
		/*
		 * Back bp up to last available segment.
		 * which contains a segment already and must
		 * be made into the delimiter.
		 * Discard second to last entry,
		 * since it is presumably smaller than the last
		 * and move the last entry back one.
		 */
		bp--;
		printf("%s: map ovflo, lost [%d-%d)\n", mp->m_name,
		    (bp-1)->m_addr, (bp-1)->m_addr+(bp-1)->m_size);
		bp[-1] = bp[0];
		bp->m_size = bp->m_addr = 0;
	}
done:
	return;
#ifdef  DIAGNOSTIC
badmfree:
	panic("bad mfree");
#endif
}

#ifdef  VIRUS_VFORK
/*
 *  Allocate resources for the three segments of a process
 *  (data, stack and u.), attempting to minimize the cost of
 *  failure part-way through.
 *  Since the segments are located successively, it is best for the sizes
 *  to be in decreasing order; generally, data, stack, then u. will be best.
 *  Returns NULL on failure, with a[2] set to NULL (since a[2] is for u.).
 *  Returns u. address (a[2]) on success, with all
 *  addresses placed in a[].
 */
memaddr
malloc3(mp,size0,size1,size2,a)
struct map *mp;
size_t size0, size1, size2;
memaddr a[3];
{
	register struct mapent *bp, *ep;
	struct mapent *found[3];
	size_t sizes[3];
	register next;
	int retry = 0;

	sizes[0] = size0, sizes[1] = size1, sizes[2] = size2;

again:
	next = 0;
	for (bp=mp->m_map; bp->m_addr; ) {
		if (bp->m_size >= sizes[next]) {
			found[next] = bp;
			bp->m_size -= sizes[next];
			if (++next > 2)
				break;
			bp = mp->m_map;
		}
		else bp++;
	}
	if (next < 3) {
		/*
		 *  Couldn't get it all.  Restore the old sizes.
		 */
		while (next--)
			found[next]->m_size += sizes[next];
		if (mp == swapmap && retry++ == 0) {
			/*
			 * Attempt to avoid running out of swap--
			 * free ALL unused sticky text segments.
			 */
			printf("short of swap\n");
			xumount(NODEV);
			goto again;
		} else if(mp==coremap && xuncount && xunfree()) goto again;
		a[2] = NULL;
	} else {
		/*
		 *  Got it all.  Update the addresses.
		 */
		for (next=0; next<3; next++) {
			bp = found[next];
			a[next] = bp->m_addr;
			bp->m_addr += sizes[next];
#ifdef  UCB_METER
			if (mp == coremap)
				freemem -= sizes[next];
#endif
		}
		/*
		 * Find the first segment with 0 size.
		 */
		for (bp=mp->m_map; bp->m_size; bp++)
			;
		/*
		 * Copy back any remaining entries to remove entries
		 * with size 0.  addr of 0 terminates.
		 */
		if (bp->m_addr) {
			ep = bp;
			for (;;) {
			    if ((bp->m_size == 0) && (bp->m_addr != 0))
				bp++;
			    else {
				ep->m_size = bp->m_size;
				if ((ep++->m_addr = bp++->m_addr) == 0)
				    break;
			    }
			}
			/*
			 * Eliminate garbage past end.
			 */
			bp--;
			while (ep < bp) {
			    ep->m_size = 0;
			    ep++->m_addr = 0;
			}
		}
	}
	return(a[2]);
}
#endif  VIRUS_VFORK

#ifdef IPK_MEXPAND

/* Гибрид malloc-а и чего-то еще.

   Попробовать расширить уже имеющийся ресурс
   размера size по адресу addr
   до размеров newsize.

   Если удалось -  вернуть addr.
   Если нет - вернуть адрес ДРУГОГО куска размером newsize.
    Тогда следует скопировать в него старые данные и затем
    сделать mfree( mp, size, addr )
*/

/* #define STAT */
/* подсчет эффекта от этого нововведения */
#ifdef STAT
int m_calls=0;
int m_succ =0;
#endif

memaddr
mexpand (mp, addr, size, newsize)
struct map *mp;
register    size_t size,        /* размер имеющегося ресурса */
	    newsize;            /* требуемый размер          */
memaddr addr;                   /* адрес имеющегося ресурса  */
{
    register struct mapent *bp;
    struct mapent  *fit;
    size_t sizediff;
    char    retry = 0;
    char    hope;               /* есть надежда расширить старое */
    if( newsize <= size || !newsize )
	panic( "mexpand");
/*
    if (newsize == size)
	panic( "=");
    if (newsize < size )
	panic( "<");
    if (!newsize)
	panic ("0");
*/

#ifdef STAT
	m_calls ++ ;
#endif

AGAIN:
    fit = NULL;                 /* подходящий кусок размера newsize пока
				   не найден */
    hope = 1;                   /* и есть надежда просто расширить старый
				   кусок */

    for (bp = mp -> m_map; bp -> m_size; bp++) {
    /* на всякий случай подбираем подходящий кусок */
	if (!fit && bp -> m_size >= newsize)
	    fit = bp;           /* кусочек подходящего размера */

	if (hope) {
	    if (bp -> m_addr >= addr + size) {
		hope = 0;       /* это - наш ЕДИНСТВЕННЫЙ шанс */

	    /* либо попали точно на границу (тогда проверим кое-что
	       еще), либо перескочили (это плохо) */

		if (bp -> m_addr == addr + size) {
		/* то свободный кусок пристыковывается прямо к концу уже
		   существующего ресурса. Если размер достаточен ... */

		    if (size + bp -> m_size >= newsize){
				/* ... то можно расшириться */
#ifdef STAT
			m_succ++;
#endif
			goto ALLOC;
		    }
		}               /* иначе ра=змер маловат */
	    }                   /* иначе между нашим отведенным куском и
				   свободным вклинился занятый кусок -
				   расшириться нельзя */
	}

	/* as malloc does */
	if (!hope               /* расшириться не удалось */
		&& fit) {       /* но уже найден подходящий кусок */
	    bp = fit;
	    size = 0;
	    addr = bp-> m_addr;        /* to be returned */
	    goto ALLOC;
	}
    }                           /* end for */

 /* Не удалось ни расширить сегмент, ни найти подходящий кусок */
    if (mp == swapmap && retry++ == 0) {
	printf ("shswap\n");
	xumount (NODEV);
	goto AGAIN;
    }
    else
	if (mp == coremap && xuncount && xunfree ())
	    goto AGAIN;
    return 0;                   /* облом */

ALLOC:  /* allocate bp as new resource */

    sizediff = size + (bp -> m_size) - newsize ;
    if (sizediff) {
	bp -> m_addr = addr + newsize;
	bp -> m_size = sizediff;
    }
    else {                      /* == 0 */
	do {
	    bp++;
	    (bp - 1) -> m_addr = bp -> m_addr;
	} while ((bp - 1) -> m_size = bp -> m_size);
    }
#ifdef UCB_METER
    if (mp == coremap)
	freemem -= newsize-size;
#endif
#ifdef STAT  /* отладочная статистика */
    if( m_calls % 20 == 0 ) printf( "%d/%d\n", m_succ, m_calls );
#endif
    return addr;
}
#endif IPK_MEXPAND

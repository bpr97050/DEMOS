#include <sys/param.h>
#include <sys/systm.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/text.h>
#include <sys/seg.h>

/*
 * $Log:	ureg.c,v $
 * Revision 22.1  89/04/12  15:43:49  korotaev
 * "param.h" ==> <sys/param.h>
 * 
 * Revision 22.0  89/03/30  17:09:40  korotaev
 * Begining DEMOS 2.2.
 * 
 * Revision 1.4  89/02/04  18:50:17  avg
 * Ускорено переключение пользовательских оверлеев.
 * 
 * Revision 1.3  89/01/22  18:12:23  korotaev
 * Егошинские правки для работы с DMA ус-ми и с куском памяти,
 * сдвинутом вызовом phys.
 *
 * Revision 1.2  87/12/09  16:44:06  andrew
 * 31 оверлей
 *
 * Revision 1.1  86/04/19  15:53:40  avg
 * Initial revision
 *
 */

/*
 * Load the user hardware segmentation
 * registers from the software prototype.
 * The software registers must have
 * been setup prior by estabur.
 */
sureg()
{
	register *udp, *uap, *rdp;
	int *rap, *limudp;
	int taddr, daddr;
#ifdef  VIRUS_VFORK
	int saddr;
#endif
	struct text *tp;

#ifdef  VIRUS_VFORK
	taddr = daddr = u.u_procp->p_daddr;
	saddr = u.u_procp->p_saddr;
#else
	taddr = daddr = u.u_procp->p_addr;
#endif
	if ((tp=u.u_procp->p_textp) != NULL)
		taddr = tp->x_caddr;
#ifndef NONSEPARATE
	if (sep_id)
		limudp = &u.u_uisd[16];
	else
#endif
		limudp = &u.u_uisd[8];
	rap = (int *) UISA;
	rdp = (int *) UISD;
	uap = &u.u_uisa[0];
	for (udp = &u.u_uisd[0]; udp < limudp;) {
#ifdef  VIRUS_VFORK
		*rap++ = *uap++ +
		    (*udp & TX? taddr: (*udp&ED? saddr: (*udp&ABS? 0: daddr)));
#else
		*rap++ = *uap++ + (*udp & TX?  taddr: (*udp & ABS?  0: daddr));
#endif
		*rdp++ = *udp++;
	}
	/*
	 *  Since software prototypes are not maintained for overlay
	 *  segments, force overlay change.  The test for TX is because
	 *  there is no text if called from core().
	 */
	u.u_ovdata.uo_xaddr = taddr;
	if (u.u_ovdata.uo_ovbase && (u.u_uisd[0] & TX))
		choverlay(u.u_uisd[0] & ACCESS);
}

/*
 * Set up software prototype segmentation
 * registers to implement the 3 pseudo
 * text,data,stack segment sizes passed
 * as arguments.
 * The argument sep specifies if the
 * text and data+stack segments are to
 * be separated.
 * The last argument determines whether the text
 * segment is read-write or read-only.
 */
estabur(nt, nd, ns, sep, xrw)
unsigned nt, nd, ns;
{
	register a, *ap, *dp;
	unsigned ts;

	if (u.u_ovdata.uo_ovbase && nt)
		ts = u.u_ovdata.uo_dbase;
	else
		ts = nt;
	if(sep) {
#ifndef NONSEPARATE
		if(!sep_id)
			goto err;
		if(ctos(ts) > 8 || ctos(nd)+ctos(ns) > 8)
#endif  NONSEPARATE
			goto err;
	} else
		if(ctos(ts) + ctos(nd) + ctos(ns) > 8)
			goto err;
	if (u.u_ovdata.uo_ovbase && nt)
		ts = u.u_ovdata.uo_ov_offst[u.u_ovdata.uo_maxov];
	if(ts + nd + ns + USIZE > maxmem)
		goto err;
	a = 0;
	ap = &u.u_uisa[0];
	dp = &u.u_uisd[0];
	while(nt >= 128) {
		*dp++ = (127 << 8) | xrw | TX;
		*ap++ = a;
		a += 128;
		nt -= 128;
	}
	if(nt) {
		*dp++ = ((nt - 1) << 8) | xrw | TX;
		*ap++ = a;
	}
#ifdef  NONSEPARATE
	if (u.u_ovdata.uo_ovbase && ts)
#else
	if ((u.u_ovdata.uo_ovbase && ts) && !sep)
#endif
		{
		/*
		 * overlay process, adjust accordingly.
		 * The overlay segment's registers will be set by
		 * choverlay() from sureg().
		 */
		register novlseg;
		for(novlseg = 0; novlseg < u.u_ovdata.uo_nseg; novlseg++) {
			*ap++ = 0;
			*dp++ = 0;
		}
	}
#ifndef NONSEPARATE
	if(sep)
		while(ap < &u.u_uisa[8]) {
			*ap++ = 0;
			*dp++ = 0;
		}
#endif

#ifdef  VIRUS_VFORK
	a = 0;
#else
	a = USIZE;
#endif
	while(nd >= 128) {
		*dp++ = (127 << 8) | RW;
		*ap++ = a;
		a += 128;
		nd -= 128;
	}
	if(nd) {
		*dp++ = ((nd - 1) << 8) | RW;
		*ap++ = a;
		a += nd;
	}
	while(ap < &u.u_uisa[8]) {
		if(*dp & ABS) {
			dp++;
			ap++;
			continue;
		}
		*dp++ = 0;
		*ap++ = 0;
	}
#ifndef NONSEPARATE
	if(sep)
		while(ap < &u.u_uisa[16]) {
			if(*dp & ABS) {
				dp++;
				ap++;
				continue;
			}
			*dp++ = 0;
			*ap++ = 0;
		}
#endif
#ifdef  VIRUS_VFORK
	a = ns;
	while(ns >= 128) {
		a -= 128;
		ns -= 128;
		*--dp = (0 << 8) | RW | ED;
		*--ap = a;
	}
#else
	a += ns;
	while(ns >= 128) {
		a -= 128;
		ns -= 128;
		*--dp = (127 << 8) | RW;
		*--ap = a;
	}
#endif
	if(ns) {
		*--dp = ((128 - ns) << 8) | RW | ED;
		*--ap = a - 128;
	}
#ifndef NONSEPARATE
	if(!sep) {
		ap = &u.u_uisa[0];
		dp = &u.u_uisa[8];
		while(ap < &u.u_uisa[8])
			*dp++ = *ap++;
		ap = &u.u_uisd[0];
		dp = &u.u_uisd[8];
		while(ap < &u.u_uisd[8])
			*dp++ = *ap++;
	}
#endif
	sureg();
	return(0);

err:
	u.u_error = ENOMEM;
	return(-1);
}

/*
 * Routine to change overlays.
 * Only hardware registers are changed;
 * must be called from sureg
 * since the software prototypes will be out of date.
 */
choverlay(xrw)
{
	register int *rap, *rdp;
	register nt;
	int addr, *limrdp;

#ifdef DIAGNOSTIC
	if( u.u_ovdata.uo_ovbase < 0 || u.u_ovdata.uo_ovbase > 7 ||
	    u.u_ovdata.uo_nseg   < 0 || u.u_ovdata.uo_nseg   > 6 ) {
		printf( "ovbase=%d nseg=%d ka6=%o\n",
			u.u_ovdata.uo_ovbase, u.u_ovdata.uo_nseg, *ka6 );
		panic( "ovdata botch" );
	}
#endif /*DIAGNOSTIC*/

	rap = &(UISA[u.u_ovdata.uo_ovbase]);
	rdp = &(UISD[u.u_ovdata.uo_ovbase]);
	limrdp = &(UISD[u.u_ovdata.uo_ovbase + u.u_ovdata.uo_nseg]);

	if( xrw == RO ) {
		/*
		 * If text is read-only, we must set up
		 * all of overlay UISDs with full 8K size
		 */
		if (u.u_ovdata.uo_curov == 0)
			u.u_ovdata.uo_curov = 1;
		addr = u.u_ovdata.uo_ov_offst[u.u_ovdata.uo_curov - 1]
		       + u.u_ovdata.uo_xaddr;
		while (rdp < limrdp) {
			*rap++ = addr;
			*rdp++ = (127 << 8) | RO;
			addr += 128;
		}
	} else {
		/*
		 * Text is R/W, for writting we must use all of checks
		 * (that is for ptrace)
		 */
		if (u.u_ovdata.uo_curov) {
			addr = u.u_ovdata.uo_ov_offst[u.u_ovdata.uo_curov - 1];
			nt   = u.u_ovdata.uo_ov_offst[u.u_ovdata.uo_curov] - addr;
			addr += u.u_ovdata.uo_xaddr;
			while (nt >= 128) {
				*rap++ = addr;
				*rdp++ = (127 << 8) | xrw;
				addr += 128;
				nt -= 128;
			}
			if (nt) {
				*rap++ = addr;
				*rdp++ = ((nt-1) << 8) | xrw;
			}
		}

		/*
		 * Закрыть доступ на остальные листы
		 */
		while (rdp < limrdp) {
			*rap++ = 0;
			*rdp++ = 0;
		}
	}

#ifndef NONSEPARATE
	/*
	 * This section copies the UISA/UISD registers to the
	 * UDSA/UDSD registers.  It is only needed for data fetches
	 * on the overlaid segment, which normally don't happen.
	 *
	 * NOTE: This is used when the RO-data placed into overlay
	 * segment of non-sep process working on the cpu with sep I/D.
	 */
	if (!u.u_sep && sep_id) {
		rdp = &(UISD[u.u_ovdata.uo_ovbase]);
		rap = rdp + 8;
		/* limrdp is still correct */
		while (rdp < limrdp)
			*rap++ = *rdp++;
		rdp = &(UISA[u.u_ovdata.uo_ovbase]);
		rap = rdp + 8;
		limrdp = &(UISA[u.u_ovdata.uo_ovbase + u.u_ovdata.uo_nseg]);
		while (rdp < limrdp)
			*rap++ = *rdp++;
	}
#endif
}

/*
 * Проверить допустимость physio
 * В отличие от старого алгоритма, допускает physio в сегменты, размешенные
 * вызовом phys.
 */
chkph(base)
register unsigned        base;
{
	register int nr;
	register int sep;

	sep = u.u_sep? 8 : 0;
	nr = (base>>13)&7;
cycle:
	if( !(u.u_uisd[nr+sep] & ABS) )
		return(-1);
	if( (((base+u.u_count-1)>>13)&7) != nr ) {
		if( (u.u_uisd[nr+sep] & 077400) != (127<<8) )
			return(-1);
		if( ++nr > 7 )
			return(-1);
		if( (u.u_uisa[nr+sep-1] + 128) != u.u_uisa[nr+sep] )
			return(-1);
		goto cycle;
	}
	if( (((u.u_uisd[nr+sep]>>2)&017700)+0100+(nr<<13)) <
		   (unsigned)(base+u.u_count) )
			return(-1);
	return(0);
}

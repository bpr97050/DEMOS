/*
 *   genassym - генераия таблицы символов для ассемблера
 *
 * $Log:	genassym.c,v $
 * Revision 1.7  90/11/12  18:52:04  root
 * Новые вещи для СМ 1425 и перемещение некоторых include'ов.
 * 
 * Revision 1.6  89/02/04  19:56:13  avg
 * Изменено в связи с принятием мер, посвященных борьбе с оверлеями.
 * 
 * Revision 1.4  88/06/23  21:18:47  ache
 * + 31 оверлей в ядре
 *
 * Revision 1.3  88/01/13  15:40:01  korotaev
 * Исправления ошибок при слиянии ядер ИПК и АЗЛК.
 *
 * Revision 1.2  87/12/09  16:53:59  andrew
 * + 31 оверлей
 *
 * Revision 1.1  86/04/19  15:02:08  avg
 * Initial revision
 *
 */
#include "h/whoami.h"
#include <sys/param.h>
#include <stdio.h>
#include <sys/proc.h>
#include <sys/dir.h>
#include <sys/inode.h>
#include <sys/user.h>
#include <sys/seg.h>
#ifdef  UCB_METER
#include <sys/vm.h>
#include "h/dz.h"
#endif

main()
{
	register struct user *u = (struct user *)0;
	register struct segm *se = (struct segm *)0;
#ifdef  UCB_METER
	register struct vmrate *vm = (struct vmrate *)0;
#endif

	printf("/ Offsets:\n\n");
	printf("U_CUROV = %o\n",&u->u_ovdata.uo_curov);
	printf("U_OVBASE = %o\n",&u->u_ovdata.uo_ovbase);
	printf("U_MAXOV = %o\n",&u->u_ovdata.uo_maxov);
	printf("U_NOVSEG = %o\n", &u->u_ovdata.uo_nseg);
	printf("U_OVOFFSET = %o\n", &u->u_ovdata.uo_ov_offst[0]);
	printf("U_XADDR = %o\n", &u->u_ovdata.uo_xaddr);
	printf("U_SEP = %o\n", &u->u_sep);
#ifdef  NONFP
	printf("U_SIGILL = %o\n", &(u->u_signal[SIGILL]));
#ifdef  UCB_METER
	printf("V_TRAP  = %o\n",&vm->v_trap);
#endif
#endif

#ifdef  UCB_METER
	printf("V_INTR  = %o\n",&vm->v_intr);
#ifdef  DZ_PDMA
	printf("V_PDMA  = %o\n",&vm->v_pdma);
#endif
	printf("V_OVLY  = %o\n",&vm->v_ovly);
#endif
	printf("SE_ADDR = %o\n", &se->se_addr);
	printf("SE_DESC = %o\n", &se->se_desc);

	printf("\n/ Compilation option settings.\n");
	printf("/ These are here so they will appear in the namelist.\n\n");
#if     PDP11==GENERIC
	printf("PDP11 = 0\t\t/ GENERIC\n");
#else
	printf("PDP11 = %d.\n", PDP11);
#endif
#ifdef  NONSEPARATE
	printf("NONSEPARATE = 1\n");
#else
	printf("NONSEPARATE = 0\n");
#endif
#ifdef  KERN_NONSEP
	printf("KERN_NONSEP = 1\n");
#else
	printf("KERN_NONSEP = 0\n");
#endif
#ifdef  UNIBUS_MAP
	printf("UNIBUS_MAP = 1\n");
#else
	printf("UNIBUS_MAP = 0\n");
#endif
#ifdef  MENLO_KOV
	printf("MENLO_KOV = 1\n");
#else
	printf("MENLO_KOV = 0\n");
#endif
#ifdef  IPK_XKOV
	printf("IPK_XKOV = 1\n");
#else
	printf("IPK_XKOV = 0\n");
#endif  /*IPK_XKOV*/
#ifdef  NONFP
	printf("NONFP = 1\n");
#else
	printf("NONFP = 0\n");
#endif
#ifdef  SMALL
	printf("SMALL = 1\n");
#else
	printf("SMALL = 0\n");
#endif
#ifdef  UCB_QUOTAS
	printf("UCB_QUOTAS = 1\n");
#else
	printf("UCB_QUOTAS = 0\n");
#endif
#ifdef  UCB_GRPMAST
	printf("UCB_GRPMAST = 1\n");
#else
	printf("UCB_GRPMAST = 0\n");
#endif
#ifdef  UCB_PGRP
	printf("UCB_PGRP = 1\n");
#else
	printf("UCB_PGRP = 0\n");
#endif
#ifdef  UCB_LOGIN
	printf("UCB_LOGIN = 1\n");
#else
	printf("UCB_LOGIN = 0\n");
#endif
#ifdef  UCB_LOAD
	printf("UCB_LOAD = 1\n");
#else
	printf("UCB_LOAD = 0\n");
#endif
#ifdef  UCB_METER
	printf("UCB_METER = 1\n");
#else
	printf("UCB_METER = 0\n");
#endif
	printf("MENLO_OVLY = 1\n");
	printf("IPK_XOVLY = 1\n");
#ifdef  OLDTTY
	printf("OLDTTY = 1\n");
#else
	printf("OLDTTY = 0\n");
#endif
	printf("UCB_NTTY = 1\n");
	printf("MENLO_JCL = 1\n");
#ifdef  VIRUS_VFORK
	printf("VIRUS_VFORK = 1\n");
#else
	printf("VIRUS_VFORK = 0\n");
#endif
	printf("UCB_AUTOBOOT = 1\n");
#if     UCB_NKB==1
	printf("UCB_NKB = 1\n");
#endif
#if     UCB_NKB==2
	printf("UCB_NKB = 2\n");
#endif
#ifndef UCB_NKB
	printf("UCB_NKB = 0\n");
#endif
#ifdef  UCB_CLIST
	printf("UCB_CLIST = 1\n");
#else
	printf("UCB_CLIST = 0\n");
#endif
#ifdef  UCB_FRCSWAP
	printf("UCB_FRCSWAP = 1\n");
#else
	printf("UCB_FRCSWAP = 0\n");
#endif
#ifdef  UCB_FSFIX
	printf("UCB_FSFIX = 1\n");
#else
	printf("UCB_FSFIX = 0\n");
#endif
#ifdef  CGL_RTP
	printf("CGL_RTP = 1\n");
#else
	printf("CGL_RTP = 0\n");
#endif
#ifdef  NOKA5
	printf("NOKA5 = 1\n");
#else
	printf("NOKA5 = 0\n");
#endif
#ifdef  UCB_UPRINTF
	printf("UCB_UPRINTF = 1\n");
#else
	printf("UCB_UPRINTF = 0\n");
#endif
#ifdef  UCB_DEVERR
	printf("UCB_DEVERR = 1\n");
#else
	printf("UCB_DEVERR = 0\n");
#endif
#ifdef  UCB_VHANGUP
	printf("UCB_VHANGUP = 1\n");
#else
	printf("UCB_VHANGUP = 0\n");
#endif
#ifdef  UNFAST
	printf("UNFAST = 1\n");
#else
	printf("UNFAST = 0\n");
#endif
#ifdef  ACCT
	printf("ACCT = 1\n");
#else
	printf("ACCT = 0\n");
#endif
#ifdef  INSECURE
	printf("INSECURE = 1\n");
#else
	printf("INSECURE = 0\n");
#endif
	printf("MPX_FILS = 0\n");
#ifdef  DIAGNOSTIC
	printf("DIAGNOSTIC = 1\n");
#else
	printf("DIAGNOSTIC = 0\n");
#endif
#ifdef  DISPLAY
	printf("DISPLAY = 1\n");
#else
	printf("DISPLAY = 0\n");
#endif

	printf("\n/ Tunable parameters (other than those in param.c)\n\n");
	printf("MAXMEM = %d.\n", MAXMEM);
	printf("MAXUPRC = %d.\n", MAXUPRC);
	printf("SSIZE = %d.\n", SSIZE);
	printf("SINCR = %d.\n", SINCR);
	printf("NOFILE = %d.\n", NOFILE);
	printf("CANBSIZ = %d.\n", CANBSIZ);
	printf("MSGBUFS = %d.\n", MSGBUFS);
	printf("NCARGS = %d.\n", NCARGS);

	exit(0);
}

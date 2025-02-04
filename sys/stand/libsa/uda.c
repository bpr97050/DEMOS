/*
 * UDA50/RAxx disk device driver
 *
 * A. Skukin (azlk) 03/17/87 (adapted from a uda50 driver BSD4.2)
 *
 * $Log:	uda.c,v $
 * Revision 1.2  90/12/12  17:12:53  korotaev
 * Правки под ДВК (my and dw).
 * 
 * Revision 1.1  88/03/28  11:32:24  korotaev
 * Initial revision
 * 
 * Revision 1.2  87/05/22  12:03:24  av
 * Сделаны правки для осуществления работы dump и restor
 * с двумя устройствами на одном контроллере типа UDA50.
 *
 * Revision 1.1  87/04/27  18:37:15  av
 * Initial revision
 * 
 */
#include <sys/param.h>
#include <sys/inode.h>
#include "../saio.h"

#include "../../include/mscp.h"

#define YES     1
#define NO      0

/*
 * Parameters for the communications area
 */
#define	NRSPL2	0
#define	NCMDL2	0
#define	NRSP	(1<<NRSPL2)
#define	NCMD	(1<<NCMDL2)

/*
 * UQPORT registers and structures
 */

struct	ra_regs {
	int	raaip;		/* initialization and polling */
	int	raasa;		/* status and address */
};

#define	UDA_ERR		0100000	/* error bit */
#define	UD_STEP4	0040000	/* step 4 has started */
#define	UD_STEP3	0020000	/* step 3 has started */
#define	UD_STEP2	0010000	/* step 2 has started */
#define	UD_STEP1	0004000	/* step 1 has started */
#define	UDA_NV		0002000	/* no host settable interrupt vector */
#define	UDA_QB		0001000	/* controller supports Q22 bus */
#define	UDA_DI		0000400	/* controller implements diagnostics */
#define	UDA_IE		0000200	/* interrupt enable */
#define	UDA_PI		0000001	/* host requests adapter purge interrupts */
#define	UDA_GO		0000001	/* start operation, after init */

/*
 * UDA Communications Area
 */

struct udaca {
	short	ca_xxx1;	/* unused */
	char	ca_xxx2;	/* unused */
	char	ca_bdp;		/* BDP to purge */
	short	ca_cmdint;	/* command queue transition interrupt flag */
	short	ca_rspint;	/* response queue transition interrupt flag */
	struct {
		unsigned int rl;
		unsigned int rh;
	} ca_rspdsc[NRSP];	/* response descriptors */
	struct {
		unsigned int cl;
		unsigned int ch;
	} ca_cmddsc[NCMD];	/* command descriptors */
};

#define	ca_ringbase	ca_rspdsc[0].rl

#define	UDA_OWN	0100000	/* UDA owns this descriptor */
#define	UDA_INT	0040000	/* allow interrupt on ring transition */

char    init_done = NO;    /* Initialization flag   */

#define	RAADDR	((struct ra_regs *)0172150)

struct uda {
	struct udaca	uda_ca;
	struct mscp	uda_rsp;
	struct mscp	uda_cmd;
} uda;

int uda_off[] = { 0, 9000, 21600, 60480, 60480, 230176, 400176, 0 };

struct mscp *udcmd();

raopen(io)
	register struct iob *io;
{
	register struct mscp *mp;
	int i;

	if (init_done == NO) {
		RAADDR->raaip = 0;
		while ((RAADDR-> raasa & UD_STEP1) == 0)
			;
		RAADDR->raasa = UDA_ERR;
		while ((RAADDR->raasa & UD_STEP2) == 0)
			;
		RAADDR->raasa = &uda.uda_ca.ca_ringbase;
		while ((RAADDR->raasa & UD_STEP3) == 0)
			;
		RAADDR->raasa = segflag;
		while ((RAADDR->raasa & UD_STEP4) == 0)
			;
		RAADDR->raasa = UDA_GO;
		uda.uda_ca.ca_ringbase = &uda.uda_rsp.m_cmdref;
		uda.uda_ca.ca_rspdsc[0].rh = segflag;
		uda.uda_ca.ca_cmddsc[0].cl = &uda.uda_cmd.m_cmdref;
		uda.uda_ca.ca_cmddsc[0].ch = segflag;
		uda.uda_cmd.m_cntflgs = 0;
		if (udcmd(M_O_STCON) == 0) {
			printf("ra: open error, STCON\n");
			return(-1);
		}
		init_done = YES;
	}
	uda.uda_cmd.m_unit = io->i_unit&7;
	if (udcmd(M_O_ONLIN) == 0) {
		 printf("ra: open error, ONLIN\n");
		 return(-1);
	}
	if (io->i_boff < 0 || io->i_boff > 7 || uda_off[io->i_boff] == -1)
		printf("ra: bad unit\n");
		return(-1);
	io->i_boff = uda_off[io->i_boff];
}

struct mscp *
udcmd(op)
	int op;
{
	struct mscp *mp;
	int i;

	uda.uda_cmd.m_opcode = op;
	uda.uda_rsp.m_header.uda_msglen = sizeof (struct mscp);
	uda.uda_cmd.m_header.uda_msglen = sizeof (struct mscp);
	uda.uda_ca.ca_cmddsc[0].ch |= UDA_OWN|UDA_INT;
rep:    uda.uda_ca.ca_rspdsc[0].rh |= UDA_OWN|UDA_INT;
	i = RAADDR->raaip;
	for (;;) {
		if (uda.uda_ca.ca_cmdint)
			uda.uda_ca.ca_cmdint = 0;
		if (uda.uda_ca.ca_rspint)
			break;
	}
	uda.uda_ca.ca_rspint = 0;
	mp = &uda.uda_rsp;
	if ((mp->m_opcode & 0377) != (op|M_O_END) ||
	    (mp->m_status & M_S_MASK) != M_S_SUCC) {
		if ((mp->m_status & M_S_MASK) == M_S_AVLBL)
			goto rep;
		return(0);
	}
	return(mp);
}

rastrategy(io, func)
	register struct iob *io;
{
	register struct mscp *mp;

	mp = &uda.uda_cmd;
	mp->m_lbn_l = (short)io->i_bn;
	mp->m_lbn_h = (short)(io->i_bn >> 16);
	mp->m_unit = io->i_unit&7;
	mp->m_bytecnt = io->i_cc;
	mp->m_buf_l = io->i_ma;
	mp->m_buf_h = segflag;
	if ((mp = udcmd(func == READ ? M_O_READ : M_O_WRITE)) == 0) {
		printf("ra: I/O error\n");
		return(-1);
	}
	return(io->i_cc);
}

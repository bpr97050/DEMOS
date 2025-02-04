/*
 *      Machine language assist.
 *      Uses the C preprocessor to produce suitable code
 *      for various 11's, mainly dependent on definition
 *      of PDP11, MENLO_KOV and NONSEPARATE.
 *
 * $Log:	mch1.s,v $
 *Revision 22.2  90/11/12  19:12:49  root
 *Новые вещи для СМ1425 и перемещение include.
 *
 *Revision 22.1  89/04/27  14:18:15  korotaev
 *Изменения в связи с передвижением каталогов и файлов
 *
 *Revision 22.0  89/03/30  15:42:49  korotaev
 *Begining DEMOS 2.2.
 *
 *Revision 1.5  89/02/04  18:44:53  avg
 *Ускорено переключение пользовательских оверлеев.
 *
 *Revision 1.4  89/02/03  18:55:41  avg
 *Убрана зависимость расположения регистров относительно u_ar0 от MENLO_KOV
 *===> не требуется более пересобирать отладчики.
 *
 *Revision 1.3  89/01/22  17:34:29  korotaev
 *Егошинские изменения для Separate машин.
 *
 *Revision 1.2  89/01/07  16:13:03  avg
 *Сделан новый (гораздо более быстрый) способ переключения
 *оверлейных сегментов в ядре. Включается #define IPK_FASTKOV
 *в localopts.h .
 *BASIC IDEA IS: хранить во фрейме процедур не номер оверлея, а
 *сразу значение OVLY_PAR. OVLY_PDR сейчас всегда будет 8K.
 *
 * Revision 1.1  86/04/19  16:42:12  avg
 * Initial revision
 *
 */

#define         LOCORE
#include        "h/whoami.h"      /* for localopts */

#if PDP11==GENERIC              /* adapt to any 11 at boot */
#       undef   NONSEPARATE                     /* support sep I/D */
#       define  SPLHIGH         bis     $HIPRI,PS
#       define  SPL7            bis     $340,PS
#       define  SPLLOW          bic     $HIPRI,PS
#       ifdef   UCB_NET
#               define  SPLNET  bis     $NETPRI,PS
#       endif
#else
#   ifdef       NONSEPARATE                     /* 11/40, 34, 23, 24 */
#       define  SPLHIGH         bis     $HIPRI,PS
#       define  SPL7            bis     $340,PS
#       define  SPLLOW          bic     $HIPRI,PS
#       ifdef   UCB_NET
#               define  SPLNET  bis     $NETPRI,PS
#       endif
#       define  mfpd            mfpi
#       define  mtpd            mtpi
#   else                                        /* 11/44, 45, 70, 73 */
#       define  SPLHIGH         spl     HIGH
#       define  SPL7            spl     7
#       define  SPLLOW          spl     0
#       ifdef   UCB_NET
#               define  SPLNET  spl     NET
#       endif
#   endif
#endif

#include        "h/ht.h"
#include        "h/tm.h"
#include        "h/ts.h"
#include        <a.out.h>
#include        "../include/cpu.m"
#include        "../include/trap.h"
#include        <sys/reboot.h>
#include        "../include/iopage.m"
#ifdef  MENLO_KOV
#include        "../include/koverlay.h"
#endif

#define INTSTK  500.                    /* bytes for interrupt stack */

/*
 * non-UNIX instructions
 */
mfpi    = 6500^tst
mtpi    = 6600^tst
#ifndef NONSEPARATE
mfpd    = 106500^tst
mtpd    = 106600^tst
#endif
stst    = 170300^tst
spl     = 230
ldfps   = 170100^tst
stfps   = 170200^tst
mfpt    = 000007                / Move from processor - 11/23+,73,44's only
mfps    = 106700^tst            / Move from PS - 11/23,24,34's only
mtps    = 106400^tst            / Move to PS - 11/23,24,34's only
halt    = 0
wait    = 1
iot     = 4
reset   = 5
rtt     = 6

#ifdef  PROFILE
#define HIGH    6                       / See also the :splfix files
#define HIPRI   300                     / Many spl's are done in-line
#else
#define HIGH    7
#define HIPRI   340
#endif  PROFILE
#ifdef  UCB_NET
#define NET     1
#define NETPRI  40
#endif

/*
 * Mag tape dump
 * Save registers in low core and write core (up to 248K) onto mag tape.
 * Entry is through 044 (physical) with memory management off.
 */
	.globl  dump

#ifndef KERN_NONSEP
	.data
#endif

dump:
#if     NHT > 0 || NTM > 0 || NTS > 0
	/ save regs r0, r1, r2, r3, r4, r5, r6, KIA6
	/ starting at location 4 (physical)
	inc     $-1                     / check for first time
	bne     1f                      / if not, don't save registers again
	mov     r0,4
	mov     $6,r0
	mov     r1,(r0)+
	mov     r2,(r0)+
	mov     r3,(r0)+
	mov     r4,(r0)+
	mov     r5,(r0)+
	mov     sp,(r0)+
	mov     KDSA6,(r0)+
1:

	/ dump all of core (i.e. to first mt error)
	/ onto mag tape. (9 track or 7 track 'binary')
#if     NHT > 0
HT      = 0172440
HTCS1   = HT+0
HTWC    = HT+2
HTBA    = HT+4
HTFC    = HT+6
HTCS2   = HT+10
HTTC    = HT+32
	mov     $HTCS1,r0
	mov     $40,*$HTCS2
	mov     $2300,*$HTTC
	clr     *$HTBA
	mov     $1,(r0)
1:
	mov     $-512.,*$HTFC
	mov     $-256.,*$HTWC
	movb    $61,(r0)
2:
	tstb    (r0)
	bge     2b
	bit     $1,(r0)
	bne     2b
	bit     $40000,(r0)
	beq     1b
	mov     $27,(r0)
#else

#if     NTM > 0
#ifndef MTC
MTC = 172522
#endif /* MTC */
	mov     $MTC,r0
	mov     $60004,(r0)+
	clr     2(r0)
1:
	mov     $-512.,(r0)
	inc     -(r0)
2:
	tstb    (r0)
	bge     2b
	tst     (r0)+
	bge     1b
	reset

	/ end of file and loop
	mov     $60007,-(r0)
#else
#if     NTS > 0
TSDB = 0172520
TSSR = 0172522
IO   = 0177600
UBMR0= 0170200

	/register usage is as follows

	/reg 0 points to UBMAP register 1 low
	/reg 1 is used to calculate the current memory address
	/ for each 512 byte transfer.
	/reg 2 is used to contain and calculate memory pointer
	/ for UBMAP register 1 low
	/reg 3 is used to contain and calculate memory pointer
	/ for UBMAP register 1 high
	/reg 4 points to the command packet
	/reg 5 is used as an interation counter when mapping is enabled


	cmp     _cputype,$44.   /is a 44 ?
	beq     1f              /yes, skip next
	cmp     _cputype,$70.   /is a 70 ?
	bne     2f              /not a 70 either. no UBMAP
1:
/       tst     _ubmaps         /unibus map present ?
/       beq     2f              /no, skip map init
	/this section of code initializes the Unibus map registers
	/and the memory management registers.
	/UBMAP reg 0 points to low memory for the TS11 command,
	/characteristics, and message buffers.
	/UBMAP reg 1 gets updated to point to the current
	/memory area.
	/Kernel I space 0 points to low memory
	/Kernel I space 7 points to the I/O page.

	inc     setmap          /indicate that UB mapping is needed
	mov     $UBMR0,r0       /point to  map register 0
	clr     r2              /init for low map reg
	clr     r3              /init for high map reg
	clr     (r0)+           /load map reg 0 low
	clr     (r0)+           /load map reg 0 high
	mov     $77406,*$KISD0  /set KISDR0
	mov     $77406,*$KISD7  /set KISDR7
	clr     *$KISA0         /point KISAR0 to low memory
	mov     $IO,*$KISA7     /point KISAR7 to IO page
	inc     *$SSR0          /turn on memory mngt
	mov     $60,*$SSR3      /enable 22 bit mapping
	mov     r2,(r0)         /load map reg 1 low
	mov     r3,2(r0)        /load map reg 1 high
2:
	/this section of code initializes the TS11

	tstb    *$TSSR          /make sure
	bpl     2b              /drive is ready
	mov     $comts,r4       /point to command packet
	add     $2,r4           /set up mod 4
	bic     $3,r4           /alignment
	mov     $140004,(r4)    /write characteristics command
	mov     $chrts,2(r4)    /characteristics buffer
	clr     4(r4)           /clear ext mem addr (packet)
	clr     tsxma           /clear extended memory save loc
	mov     $10,6(r4)       /set byte count for command
	mov     $mests,*$chrts  /show where message buffer is
	clr     *$chrts+2       /clear extended memory bits here too
	mov     $16,*$chrts+4   /set message buffer length
	mov     r4,*$TSDB       /start command
	mov     $20,r5          /set up SOB counter for UBMAP
	clr     r1              /init r1 beginning memory address
1:
	tstb    *$TSSR          /wait for ready
	bpl     1b              /not yet
	mov     *$TSSR,tstcc    /error condition (SC) ?
	bpl     2f              /no error
	/ NXM test moved here to help out TK25
	bit     $4000,*$TSSR    /is error NXM ?
	bne     8f
	bic     $!16,tstcc      /yes error, get TCC
	cmp     tstcc,$10       /recoverable error ?
	bne     8f              /no
	mov     $101005,(r4)    /yes, load write data retry command
	clr     4(r4)           /clear packet ext mem addr
	mov     r4,*$TSDB       /start retry
	br      1b
8:
	/bit     $4000,*$TSSR    /is error NXM ?
	/beq     .               /no, hang (not sure of good dump)
	mov     $140013,(r4)    /load a TS init command
	mov     r4,*$TSDB       /to clear NXM error
6:
	tstb    *$TSSR          /wait for ready
	bpl     6b
	mov     $1,6(r4)        /set word count = 1
	mov     $100011,(r4)    /load write EOF command
	mov     r4,*$TSDB       /do write EOF
7:
	tstb    *$TSSR          /wait for ready
	bpl     7b
	halt                    /halt after good dump
9:
	br      1b
2:
	/If mapping is needed this section calculates the
	/ base address to be loaded into map reg 1
	/ the algorithm is (!(r5 - 21))*1000) | 20000
	/ the complement is required because an SOB loop
	/ is being used for the counter
	/This loop causes 20000 bytes to be written
	/before the UBMAP is updated.

	tst     setmap          /UBMAP ?
	beq     3f              /no map
	mov     r2,(r0)         /load map reg 1 low
	mov     r3,2(r0)        /load map reg 1 high
	mov     r5,r1           /calculate
	sub     $21,r1          /address for this pass
	com     r1              /based on current
	mul     $1000,r1        /interation
	bis     $20000,r1       /select map register 1
	clr     4(r4)           /clear extended memory bits
3:
	/This section does the write.
	/ if mapping is needed the sob loop comes in play here
	/ when the sob falls through the UBAMP reg will be
	/ updated by 20000 to point to next loop section.

	/ if mapping not needed then just calculate the
	/ next 512 byte address pointer

	mov     r1,2(r4)        /load mem address
	mov     tsxma,4(r4)     /load ext mem address
	mov     $512.,6(r4)     /set byte count
	mov     $100005,(r4)    /set write command
	mov     r4,*$TSDB       /initiate xfer
	tst     setmap          /mapping?
	beq     4f              /branch if not
	sob     r5,9b           /yes continue loop
	mov     $20,r5          /reset loop count
	add     $20000,r2       /bump low map
	adc     r3              /carry to high map
	br      1b              /do some more
4:
	add     $512.,r1        /bump address for no mapping
	adc     tsxma           /carry to extended memory bits
	br      1b              /do again

/ The following TS11 command and message buffers,
/ must be in initialized data space instead of
/ bss space. This allows them to be mapped by the
/ first M/M mapping register, which is the only one
/ used durring a core dump.

tsxma:  0       /ts11 extended memory address bits
setmap: 0       /UB map usage indicator
tstcc:  0       /ts11 temp location for TCC
comts:          /ts11 command packet
	0 ; 0 ; 0 ; 0 ; 0
chrts:          /ts11 characteristics
	0 ; 0 ; 0 ; 0
mests:          /ts11 message buffer
	0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0
#endif  NTS
#ifdef RADUMP
	/ MSCP disk core dump code

	mov     $20,*$SSR3      / enable 22 bit mapping

	/ Controller initialization

	s1      = 4000
	go      = 1

	mov     dmp_csr,r1      / controller I/O page address
	clr     (r1)+           / start controller init sequence
				/ move pointer to SA register
	mov     $s1,r5          / set cntlr state test bit to step 1
	mov     $1f,r4          / address of init seq table
	br      2f              / branch around table
1:
	100000                  / UDA_ERR, init step 1
	ring                    / address of ringbase
	0                       / hi ringbase address
	go                      / UDA go bit
2:
	tst     (r1)            / error ?
	bmi     .               / yes, hang on init error
	bit     r5,(r1)         / current step done ?
	beq     2b              / no
	mov     (r4)+,(r1)      / yes, load next step info from table
	asl     r5              / change state test bit to next step
	bpl     2b              / if all steps not done, go back
				/ r5 now = 100000, UDA_OWN bit
	mov     $36.,cmdhdr     / command packet length
				/ don't set response packet length,
				/ little shakey but it works.
	mov     dmp_dn,udacmd+4.        / load drive number
	mov     $11,udacmd+8.   / on-line command opcode
	mov     $ring,r2        / initialize cmd/rsp ring
	mov     $udarsp,(r2)+   / address of response packet
	mov     r5,(r2)+        / set UDA owner
	mov     $udacmd,(r2)+   / address of command packet
	mov     r5,(r2)+        / set UDA owner
	mov     -2(r1),r0       / start UDA polling
3:
	tst     ring+2          / wait for response, UDA_OWN goes to zero
	bmi     3b

	/ MSCP disk driver

	mov     $dumplo,ra_blk  /set up start block # (for restart)
	clr     ra_ba
	clr     ra_xba
1:
/       mov     $36.,cmdhdr             / length of command packet
	mov     $42,udacmd+8.           / write opcode
	mov     $512.,udacmd+12.        / byte count
	mov     ra_ba,udacmd+16.        / buffer descriptor, lo bus addr
	mov     ra_xba,udacmd+18.       / buffer descriptor, hi bus addr
	mov     ra_blk,udacmd+28.       / block number low
	clr     udacmd+30.              / block number hi
	mov     $100000,ring+2          / set UDA owner of response
	mov     $100000,ring+6          / set UDA owner of command
	mov     *dmp_csr,r0             / start UDA polling
4:
	tst     ring+2                  / wait for response
	bmi     4b
	tstb    udarsp+10.              / does returned status = SUCCESS ?
	beq     2f                      / yes, no error continue dump
/ *** appears that RQDX1 does not support sub-codes
/ *** new microcode fixed above !
.if RXDUMP
	cmpb    $4,udarsp+10.           / OHMS - ignore attention caused
	bne     3f                      /  by changing floppies
	mov     $100000,ring+2          / set UDA owner of response
	mov     *dmp_csr,r0             / start UDA polling
	br      4b
3:
.endif
	cmpb    $151,udarsp+10.         / (NXM) host buffer access error ?
	bne     .                       / no, hang on bad dump
5:
	halt                            / yes, halt on good dump
2:
	inc     ra_blk                  / increment block number
	cmp     $dumphi,ra_blk          / end of dump area ?
	bgt     3f                      / continue dump if not
.if RXDUMP
	movb    $052,*$177566           / print * on terminal
	halt                            / wait for user to change diskettes
			/ user continues processor
	clr     ra_blk                  / OHMS - need to start at the beginning
	mov     $11,udacmd+8.           / on-line command opcode
	mov     $ring,r2                / initialize cmd/rsp ring
	mov     $udarsp,(r2)+           / address of response packet
	mov     r5,(r2)+                / set UDA owner
	mov     $udacmd,(r2)+           / address of command packet
	mov     r5,(r2)+                / set UDA owner
	mov     *dmp_csr,r0             / start UDA polling
4:
	tst     ring+2          / wait for response, UDA_OWN goes to zero
	bmi     4b
	br      3f
.endif
	br      5b                      / halt if not RXDUMP & end of swap area
3:
	add     $512.,ra_ba             / advance memory address
	bcc     1b                      / and extended address
	inc     ra_xba                  / if necessary
	br      1b                      / do some more

			/ Next two locations used by crash dump copy,
			/ command to locate dump area.
ra_dmplo: dumplo        / starting block of dump area (in swap area)
ra_dmphi: dumphi        / highest possible block of dump area
ra_blk: dumplo          / current block number in dump area
ra_ba:  0               / memory address
ra_xba: 0               / entended memory address
dmp_dn:  dumpdn         / tells Boot: (auto-unit select (unless RX50))
dmp_csr: DSKCSR         / tells Boot: (auto-csr select)
.if RXDUMP
dmp_rx: 0               / tells Boot: (no auto-unit select)
.endif
			/ Controller MSCP communications area
cmdint: .=.+2.          / command ring transition
rspint: .=.+2.          / response ring transition
ring:   .=.+8.          / ring base
rsphdr: .=.+4.          / response header
udarsp: .=.+48.         / response packet
cmdhdr: .=.+4.          / command header
udacmd: .=.+48.         / command packet
#endif  RADUMP
#endif  NTM
#endif  NHT
#endif  NHT || NTM
	br      .               / If no ht and no tm, fall through to here

	.text

/*
 * routine to call panic, take dump and reboot;
 * entered by manually loading the PC with 040 and continuing
 */
	.globl  _panic, _halt, do_panic
do_panic:
	mov     $pmesg, -(sp)
	jsr     pc, _panic
	/* NOTREACHED */

	.data
pmesg:  <forced from console\0>
	.text

_halt:
	halt
	/* NOTREACHED */
	rts     pc

	.globl  _etext, _main, start
#ifdef  MENLO_KOV
	.globl  ovend, ova, ovd
#endif

start:
	bit     $1,SSR0                 / is memory management enabled ?
	beq     .                       / better be !!!

	/  The following two instructions change the contents of
	/  locations 034-037 to read:
	/               syscall; br0+SYSCALL.
	mov     $syscall, 34
	mov     $0+SYSCALL., 36

	/  Turn off write permission on kernel text
	movb    $RO, KISD0

	/  Get a stack pointer
	mov     $usize-1\<8|RW, KDSD6
	mov     $_u+[usize*64.],sp

	/  Clear user block
	mov     $_u,r0
1:
	clr     (r0)+
	cmp     r0,sp
	blo     1b

	.globl  _bootflags, _checkword

	/  Get bootflags;  the bootstrap leave them in r4.
	/  R2 should be the complement of bootflags.
	mov     r4,_bootflags
	mov     r2,_checkword

	/  Check out (and if necessary, set up) the hardware.
	jsr     pc, hardprobe

#ifdef MENLO_KOV
	/  Set up OVLY_PDR page to 8K read only
	mov     $[177\<8]|RO, OVLY_PDR
#endif

	/  Set up previous mode and call main
	/  on return, enter user mode at 0R
	mov     $30340,PS
	jsr     pc,_main
	mov     $170000,-(sp)
	clr     -(sp)
	rtt

/*
 * Emt takes emulator traps, which are normally requests
 * to change the overlay for the current process.
 * Emts are also generated by the separate I/D floating
 * point simulator in fetching instructions from user
 * I space.  In this case, r0 should be -1 and r1 should
 * contain the pc of the instruction to be fetched.
 * If an invalid emt is received, _trap is called.
 */

/ EMT Interrupt Handler

	.globl  emt
emt:
	mov     PS,saveps
	bit     $30000,PS               / verify that emt is not kernel mode
	beq     bademt

	cmp     r0,_u+U_MAXOV           / verify olerlay number
	bhi     fetchi
	tst     r0                      / what's a confuse! return to ov 0
	beq     9f
	SPLLOW

#ifdef UCB_METER
	inc     _cnt+V_OVLY             / for vmstat
#endif
	mov     r0,-(sp)
	mov     r1,-(sp)
	mov     r2,-(sp)
#ifndef NONSEPARATE
	mov     r3,-(sp)
#endif
	mov     r0,_u+U_CUROV           / save current ovno

	asl     r0                      / compute overlay segment address
	mov     _u+U_OVOFFSET-2(r0),r0
	add     _u+U_XADDR,r0

	mov     _u+U_OVBASE,r2          / compute pointers for PARs
	asl     r2

	mov     _u+U_NOVSEG,r1          / jump to code for 1, 2 ... etc segs
	asl     r1
#ifndef NONSEPARATE
	tstb    _u+U_SEP       / IF nonsep process
	beq     1f
	tstb    _sep_id        /      AND separate machine
	bne     1f

	add     $16.,r1        / THEN set up UDSAs (ELSE only UISAs)
	mov     r2,r3
	add     $UDSA,r3
1:
#endif  /* NONSEPARATE */
	add     $UISA,r2
	jmp     *emttab(r1)
.data
emttab:
	badnseg; emt_s1 ; emt_s2 ; emt_s3 ; emt_s4 ; emt_s5 ; emt_s6 ; badnseg
#ifndef NONSEPARATE
	badnseg; emt_ds1; emt_ds2; emt_ds3; emt_ds4; emt_ds5; emt_ds6; badnseg
#endif
.text
/
/ Bad Number of ovly segments ... restore registers and go to trap
/
badnseg:
	clr     _u+U_CUROV
#ifndef NONSEPARATE
	mov     (sp)+,r3
#endif
	mov     (sp)+,r2
	mov     (sp)+,r1
	mov     (sp)+,r0
	br      bademt

#ifndef NONSEPARATE
emt_ds6:        mov r0,(r2)+ ; mov r0,(r3)+ ; add $128.,r0
emt_ds5:        mov r0,(r2)+ ; mov r0,(r3)+ ; add $128.,r0
emt_ds4:        mov r0,(r2)+ ; mov r0,(r3)+ ; add $128.,r0
emt_ds3:        mov r0,(r2)+ ; mov r0,(r3)+ ; add $128.,r0
emt_ds2:        mov r0,(r2)+ ; mov r0,(r3)+ ; add $128.,r0
emt_ds1:        mov r0,(r2)  ; mov r0,(r3)
		br      8f
#endif
emt_s6:         mov r0,(r2)+ ; add $128.,r0
emt_s5:         mov r0,(r2)+ ; add $128.,r0
emt_s4:         mov r0,(r2)+ ; add $128.,r0
emt_s3:         mov r0,(r2)+ ; add $128.,r0
emt_s2:         mov r0,(r2)+ ; add $128.,r0
emt_s1:         mov r0,(r2)
	/ ---- FALL TROUGH -----

8:      / Restore registers
#ifndef NONSEPARATE
	mov     (sp)+,r3
#endif
	mov     (sp)+,r2
	mov     (sp)+,r1
	mov     (sp)+,r0
9:
	rtt                             / return to user

/
/ Fetch I word
/
fetchi:
#if     defined(NONFP) && !defined(NONSEPARATE)
	cmp     $-1, r0
	bne     bademt
	mov     $1f,nofault
	mfpi    (r1)                    / get I-space at (r1)
	mov     (sp)+,r0                / put result in r0
	clr     nofault
	rtt
1:
	clr     nofault
	/* FALL THROUGH */
#endif  defined(NONFP) && !defined(NONSEPARATE)

bademt:
	jsr     r0, call1; jmp  _trap   / invalid emt
	/*NOTREACHED*/

	.globl  syscall, trap, buserr, _syscall, _trap, _panic, _panicstr
/*
 *      System call interface to C code.
 */
syscall:
	mov     r0, -(sp)
	cmp     -(sp), -(sp)    / Dummy arguments.  See trap.c for explanation.
	mov     r1, -(sp)
	mfpd    sp
	tst     -(sp)           / Dummy argument.
	jsr     pc, _syscall
	tst     (sp)+
	mtpd    sp
	mov     (sp)+, r1
	cmp     (sp)+, (sp)+
	mov     (sp)+, r0
	rtt

#ifdef  NONFP
/*
 *      Fast illegal-instruction trap routine for use with interpreted
 *      floating point.  All of the work is done here if SIGILL is caught,
 *      otherwise, trap is called like normal.
 */
	.globl  instrap
instrap:
	mov     PS, saveps
	tst     nofault
	bne     1f                      / branch to trap code
	bit     $30000, PS              / verify not from kernel mode
	beq     3f
	SPLLOW
	tst     _u+U_SIGILL             / check whether SIGILL is being caught
	beq     3f                      / is default
	bit     $1, _u+U_SIGILL
	bne     3f                      / is ignored (or held or deferred!)
	mov     r0, -(sp)
	mfpd    sp
#ifdef  UCB_METER
	inc     _cnt+V_TRAP
	inc     _cnt+V_INTR             / since other traps also count as intr
#endif
	mov     (sp), r0
	sub     $2, r0
	mov     $2f, nofault
	mov     6(sp), -(sp)
	mtpd    (r0)                    / push old PS
	sub     $2, r0
	mov     4(sp), -(sp)
	mtpd    (r0)                    / push old PC
	mov     _u+U_SIGILL, 4(sp)      / new PC
	bic     $TBIT, 6(sp)            / new PS
	mov     r0, (sp)
	mtpd    sp                      / set new user SP
	mov     (sp)+, r0
	clr     nofault
	rtt
2:                                      / back out on error
	clr     nofault
	tst     (sp)+                   / pop user's sp
	mov     (sp)+, r0
3:
	jsr     r0, call1; jmp _trap    / call trap on bad SIGILL
	/*NOTREACHED*/
#endif
buserr:
	mov     PS,saveps
	bit     $30000,PS
	bne     2f      / if previous mode != kernel
	tst     nofault
	bne     1f
	tst     sp
	bne     3f
	/ Kernel stack invalid.
	tst     _panicstr
	beq     4f
	br      .               / Already paniced, don't overwrite anything
4:
	/ Find a piece of stack so we can panic.
	mov     $intstk+[INTSTK\/2], sp
	mov     $redstak, -(sp)
	jsr     pc, _panic
	/*NOTREACHED*/
	.data
redstak: <kernel red stack violation\0>
	.text

/*
 *      Traps without specialized catchers get vectored here.
 */
trap:
	mov     PS,saveps
2:
	tst     nofault
	bne     1f
3:
	mov     SSR0,ssr
#ifndef KERN_NONSEP
	mov     SSR1,ssr+2
#endif
	mov     SSR2,ssr+4
	mov     $1,SSR0
	jsr     r0, call1; jmp _trap
	/*NOTREACHED*/
1:
	mov     $1,SSR0
	mov     nofault,(sp)
	rtt

	.globl  call, _runrun
call1:
	mov     saveps, -(sp)
	SPLLOW
	br      1f

call:
	mov     PS, -(sp)
1:
#ifdef  MENLO_KOV
	mov     OVLY_PAR,-(sp)
#else
	tst     -(sp)                   / Dummy Ovno (see trap.c)
#endif
	mov     r1,-(sp)
	mfpd    sp
#ifdef UCB_METER
	.globl  _cnt
	inc     _cnt+V_INTR             / count device interrupts
#endif UCB_METER
	mov     6(sp), -(sp)
	bic     $!37,(sp)
	bit     $30000,PS
	beq     1f
	jsr     pc,(r0)+
#ifdef  UCB_NET
	jsr     pc,checknet
#endif
	tstb    _runrun
	beq     2f
	mov     $SWITCHTRAP.,(sp)       / give up cpu
	jsr     pc,_trap
2:
	tst     (sp)+
	mtpd    sp
	br      2f
1:
	bis     $30000,PS
	jsr     pc,(r0)+
#ifdef  UCB_NET
	jsr     pc,checknet
#endif
	cmp     (sp)+,(sp)+
2:
	mov     (sp)+,r1
#ifdef  MENLO_KOV
	/ Restore previous overlay number and mapping.
	mov     (sp)+,OVLY_PAR
	tst     (sp)+
#else
	cmp     (sp)+,(sp)+
#endif  MENLO_KOV
	mov     (sp)+,r0
	rtt

/*
 *  Power fail handling.  On power down, we just set up for the next trap.
 */
#define PVECT   24                      /* power fail vector */
	.globl  powrdown, powrup, _panicstr, _rootdev, hardboot
	.text
powrdown:
	mov     $powrup,PVECT
	SPL7
	br      .

	/*
	 *  Power back on... wait a bit, then attempt a reboot.
	 *  Can't do much since memory management is off
	 *  (hence we are in "data" space).
	 */
#ifndef KERN_NONSEP
	.data
#endif
powrup:         /       Здесь сидит клоп - не прочитан 0 блок с rootdev !
	mov     $-1,r0
1:
	dec     r0
	bne     1b

	mov     $RB_POWRFAIL, r4
	mov     _rootdev,r3
	jsr     pc,hardboot
	/* NOTREACHED */
	.text

#ifdef  UCB_NET
	.globl  _netisr,_netintr
checknet:
	mov     PS,-(sp)
	SPL7
	tst     _netisr                 / net requesting soft interrupt
	beq     3f
	bit     $340,18.(sp)
	bne     3f                      / if prev spl not 0
	SPLNET
	jsr     pc,*$_netintr
3:
	mov     (sp)+,PS
	rts     pc
#endif

#include "h/dz.h"
#if     NDZ > 0 && defined(DZ_PDMA)
	/*
	 *  DZ-11 pseudo-DMA interrupt routine.
	 *  Called directly from the interrupt vector;
	 *  the device number is in the low bits of the PS.
	 *  Calls dzxint when the end of the buffer is reached.
	 *  The pdma structure is known to be:
	 *      struct pdma {
	 *              struct  dzdevice *p_addr;
	 *              char    *p_mem;
	 *              char    *p_end;
	 *              struct  tty *p_arg;
	 *      };
	 */
	.globl  dzdma, _dzpdma, _dzxint
dzdma:
	mov     PS, -(sp)
	mov     r0, -(sp)
#ifdef  MENLO_KOV
	mov     OVLY_PAR,-(sp)
#else
	tst     -(sp)
#endif /* MENLO_KOV */
	mov     r1, -(sp)
	mov     r2, -(sp)
	mov     r3, -(sp)
	mov     12(sp), r3              / new PS
#ifdef UCB_METER
	inc     _cnt+V_PDMA             / count pseudo-DMA interrupts
#endif UCB_METER
	bic     $!37, r3                / extract device number
	ash     $3+3, r3                / get offset into dzpdma
	add     $_dzpdma, r3            /       for first line
	mov     (r3)+, r2               / dzaddr in r2; r3 points to p_mem

#ifdef  UCB_CLIST
	.globl  _clststrt, _clstdesc
	mov     KDSA5, -(sp)            / save previous mapping
	mov     KDSD5, -(sp)
	mov     _clststrt, KDSA5        / map in clists
	mov     _clstdesc, KDSD5
#endif  UCB_CLIST

	/ loop until no line is ready
1:
	movb    1(r2), r1               / dzcsr high byte
	bge     3f                      / test TRDY; branch if none
	bic     $!7, r1                 / extract line number
	ash     $3, r1                  / convert to pdma offset
	add     r3, r1                  / r1 is pointer to pdma.p_mem for line
	mov     (r1)+, r0               / pdma->p_mem
	cmp     r0, (r1)+               / cmp p_mem to p_end
	bhis    2f                      / if p_mem >= p_end
	movb    (r0)+, 6(r2)            / dztbuf = *p_mem++
	mov     r0, -4(r1)              / update p_mem
	br      1b

	/ buffer is empty; call dzxint
2:
	mov     (r1), -(sp)             / p_arg
	jsr     pc, _dzxint             / r0, r1 are modified!
	tst     (sp)+
	br      1b

	/ no more lines ready; return
3:
#ifdef  UCB_CLIST
	mov     (sp)+, KDSD5
	mov     (sp)+, KDSA5            / restore previous mapping
#endif  UCB_CLIST
	mov     (sp)+, r3
	mov     (sp)+, r2
	mov     (sp)+, r1
#ifdef  MENLO_KOV
	mov     (sp)+, OVLY_PAR
#else
	tst     (sp)+
#endif /*MENLO_KOV*/
	mov     (sp)+, r0
	tst     (sp)+
	rtt
#endif  DZ_PDMA

#ifndef NONFP
	.globl  _savfp, _restfp, _stst
_savfp:
	tst     fpp
	beq     9f                      / No FP hardware
	mov     2(sp),r1
	stfps   (r1)+
	setd
	movf    fr0,(r1)+
	movf    fr1,(r1)+
	movf    fr2,(r1)+
	movf    fr3,(r1)+
	movf    fr4,fr0
	movf    fr0,(r1)+
	movf    fr5,fr0
	movf    fr0,(r1)+
9:
	rts     pc

_restfp:
	tst     fpp
	beq     9f
	mov     2(sp),r1
	mov     r1,r0
	setd
	add     $8.+2.,r1
	movf    (r1)+,fr1
	movf    (r1)+,fr2
	movf    (r1)+,fr3
	movf    (r1)+,fr0
	movf    fr0,fr4
	movf    (r1)+,fr0
	movf    fr0,fr5
	movf    2(r0),fr0
	ldfps   (r0)
9:
	rts     pc

/*
 * Save floating poing error registers.
 * The argument is a pointer to a two word
 * structure.
 */
_stst:
	tst     fpp
	beq     9f
	stst    *2(sp)
9:
	rts     pc

#endif  NONFP

	.globl  _addupc
_addupc:
	mov     r2,-(sp)
	mov     6(sp),r2                / base of prof with base,leng,off,scale
	mov     4(sp),r0                / pc
	sub     4(r2),r0                / offset
	clc
	ror     r0
	mov     6(r2),r1
	clc
	ror     r1
	mul     r1,r0                   / scale
	ashc    $-14.,r0
	inc     r1
	bic     $1,r1
	cmp     r1,2(r2)                / length
	bhis    1f
	add     (r2),r1                 / base
	mov     nofault,-(sp)
	mov     $2f,nofault
	mfpd    (r1)
	add     12.(sp),(sp)
	mtpd    (r1)
	br      3f
2:
	clr     6(r2)
3:
	mov     (sp)+,nofault
1:
	mov     (sp)+,r2
	rts     pc

#ifdef  DISPLAY
	.globl  _display
_display:
	dec     dispdly
	bge     2f
	clr     dispdly
	mov     PS,-(sp)
	SPLHIGH
	mov     CSW,r1
	bit     $1,r1
	beq     1f
	bis     $30000,PS
	dec     r1
1:
	jsr     pc,fuword
	mov     r0,CSW
	mov     (sp)+,PS
	cmp     r0,$-1
	bne     2f
	mov     $120.,dispdly           / 2 second delay after CSW fault
2:
	rts     pc
#endif

	.globl  _regloc, _backup
#ifndef KERN_NONSEP
/*
 *  Backup routine for use with machines with ssr2.
 */
_backup:
	mov     2(sp),r0
	movb    ssr+2,r1
	jsr     pc,1f
	movb    ssr+3,r1
	jsr     pc,1f
	movb    _regloc+7,r1
	asl     r1
	add     r0,r1
	mov     ssr+4,(r1)
	clr     r0
2:
	rts     pc
1:
	mov     r1,-(sp)
	asr     (sp)
	asr     (sp)
	asr     (sp)
	bic     $!7,r1
	movb    _regloc(r1),r1
	asl     r1
	add     r0,r1
	sub     (sp)+,(r1)
	rts     pc

#else   KERN_NONSEP
/*
 * 11/40 version of backup, for use with no SSR2
 */
_backup:
	mov     2(sp),ssr+2
	mov     r2,-(sp)
	jsr     pc,backup
	mov     r2,ssr+2
	mov     (sp)+,r2
	movb    jflg,r0
	bne     2f
	mov     2(sp),r0
	movb    ssr+2,r1
	jsr     pc,1f
	movb    ssr+3,r1
	jsr     pc,1f
	movb    _regloc+7,r1
	asl     r1
	add     r0,r1
	mov     ssr+4,(r1)
	clr     r0
2:
	rts     pc
1:
	mov     r1,-(sp)
	asr     (sp)
	asr     (sp)
	asr     (sp)
	bic     $!7,r1
	movb    _regloc(r1),r1
	asl     r1
	add     r0,r1
	sub     (sp)+,(r1)
	rts     pc

	/ Hard part:  simulate the ssr2 register missing on 11/40.
backup:
	clr     r2                      / backup register ssr1
	mov     $1,bflg                 / clrs jflg
	clrb    fflg
	mov     ssr+4,r0
	jsr     pc,fetch
	mov     r0,r1
	ash     $-11.,r0
	bic     $!36,r0
	jmp     *0f(r0)
0:              t00; t01; t02; t03; t04; t05; t06; t07
		t10; t11; t12; t13; t14; t15; t16; t17

t00:
	clrb    bflg

t10:
	mov     r1,r0
	swab    r0
	bic     $!16,r0
	jmp     *0f(r0)
0:              u0; u1; u2; u3; u4; u5; u6; u7

u6:                                     / single op, m[tf]pi, sxt, illegal
	bit     $400,r1
	beq     u5                      / all but m[tf], sxt
	bit     $200,r1
	beq     1f                      / mfpi
	bit     $100,r1
	bne     u5                      / sxt

	/ Simulate mtpi with double (sp)+,dd.
	bic     $4000,r1                / turn instr into (sp)+
	br      t01

	/ Simulate mfpi with double ss,-(sp).
1:
	ash     $6,r1
	bis     $46,r1                  / -(sp)
	br      t01

u4:                                     / jsr
	mov     r1,r0
	jsr     pc,setreg               / assume no fault
	bis     $173000,r2              / -2 from sp
	rts     pc

t07:                                    / EIS
	clrb    bflg

u0:                                     / jmp, swab
u5:                                     / single op
#ifndef NONFP
f5:                                     / movei, movfi
ff1:                                    / ldfps
ff2:                                    / stfps
ff3:                                    / stst
#endif
	mov     r1,r0
	br      setreg

t01:                                    / mov
t02:                                    / cmp
t03:                                    / bit
t04:                                    / bic
t05:                                    / bis
t06:                                    / add
t16:                                    / sub
	clrb    bflg

t11:                                    / movb
t12:                                    / cmpb
t13:                                    / bitb
t14:                                    / bicb
t15:                                    / bisb
	mov     r1,r0
	ash     $-6,r0
	jsr     pc,setreg
	swab    r2
	mov     r1,r0
	jsr     pc,setreg

	/ If delta(dest) is zero, no need to fetch source.
	bit     $370,r2
	beq     1f

	/ If mode(source) is R, no fault is possible.
	bit     $7000,r1
	beq     1f

	/ If reg(source) is reg(dest), too bad.
	mov     r2,-(sp)
	bic     $174370,(sp)
	cmpb    1(sp),(sp)+
	beq     u7

	/ Start source cycle.  Pick up value of reg.
	mov     r1,r0
	ash     $-6,r0
	bic     $!7,r0
	movb    _regloc(r0),r0
	asl     r0
	add     ssr+2,r0
	mov     (r0),r0

	/ If reg has been incremented, must decrement it before fetch.
	bit     $174000,r2
	ble     2f
	dec     r0
	bit     $10000,r2
	beq     2f
	dec     r0
2:

	/ If mode is 6,7 fetch and add X(R) to R.
	bit     $4000,r1
	beq     2f
	bit     $2000,r1
	beq     2f
	mov     r0,-(sp)
	mov     ssr+4,r0
	add     $2,r0
	jsr     pc,fetch
	add     (sp)+,r0
2:

	/ Fetch operand. If mode is 3, 5, or 7, fetch *.
	jsr     pc,fetch
	bit     $1000,r1
	beq     1f
	bit     $6000,r1
	bne     fetch
1:
	rts     pc

t17:                                    / floating point instructions
#ifndef NONFP
	clrb    bflg
	mov     r1,r0
	swab    r0
	bic     $!16,r0
	jmp     *0f(r0)
0:              f0; f1; f2; f3; f4; f5; f6; f7

f0:
	mov     r1,r0
	ash     $-5,r0
	bic     $!16,r0
	jmp     *0f(r0)
0:              ff0; ff1; ff2; ff3; ff4; ff5; ff6; ff7

f1:                                     / mulf, modf
f2:                                     / addf, movf
f3:                                     / subf, cmpf
f4:                                     / movf, divf
ff4:                                    / clrf
ff5:                                    / tstf
ff6:                                    / absf
ff7:                                    / negf
	inc     fflg
	mov     r1,r0
	br      setreg

f6:
	bit     $400,r1
	beq     f1                      / movfo
	br      f5                      / movie

f7:
	bit     $400,r1
	beq     f5                      / movif
	br      f1                      / movof

ff0:                                    / cfcc, setf, setd, seti, setl
#endif
u1:                                     / br
u2:                                     / br
u3:                                     / br
u7:                                     / illegal
	incb    jflg
	rts     pc

setreg:
	mov     r0,-(sp)
	bic     $!7,r0
	bis     r0,r2
	mov     (sp)+,r0
	ash     $-3,r0
	bic     $!7,r0
	movb    0f(r0),r0
	tstb    bflg
	beq     1f
	bit     $2,r2
	beq     2f
	bit     $4,r2
	beq     2f
1:
	cmp     r0,$20
	beq     2f
	cmp     r0,$-20
	beq     2f
	asl     r0
2:
#ifndef NONFP
	tstb    fflg
	beq     3f
	asl     r0
	stfps   r1
	bit     $200,r1
	beq     3f
	asl     r0
3:
#endif
	bisb    r0,r2
	rts     pc

0:      .byte   0,0,10,20,-10,-20,0,0

fetch:
	bic     $1,r0
	mov     nofault,-(sp)
	mov     $1f,nofault
	mfpi    (r0)
	mov     (sp)+,r0
	mov     (sp)+,nofault
	rts     pc

1:
	mov     (sp)+,nofault
	clrb    r2                      / clear out dest on fault
	mov     $-1,r0
	rts     pc

	.bss
bflg:   .=.+1
jflg:   .=.+1
fflg:   .=.+1
	.text
#endif  KERN_NONSEP

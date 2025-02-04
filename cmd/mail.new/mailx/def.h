/*
 * Mail -- a mail program
 *
 * $Log: def.h,v $
 * Revision 1.16  1991/01/25  18:18:41  ache
 * Чтобы пользователи больше не спрашивали
 * 	- Что такое Суб`ект: ?
 * Subject, To, Cc и т.п. запрашиваются по ediag по русски
 *
 * Revision 1.15  1991/01/25  18:04:45  ache
 * Убраны старые (4.1) сигналы
 *
 * Revision 1.14  1991/01/19  15:38:23  ache
 * убраны буфера 16к, как не оправдавшие доверия народа
 *
 * Revision 1.13  90/12/23  21:12:15  ache
 * Буферизация IO по 16 К
 * 
 * Revision 1.12  90/12/07  09:38:02  ache
 * Переделана обработка временных файлов и мелочи
 * 
 * Revision 1.11  90/10/13  20:25:27  ache
 * line count -- long now
 * 
 * Revision 1.10  90/09/29  18:22:53  ache
 * <ctype.h> kicked out...
 * 
 * Revision 1.9  90/09/28  17:54:26  ache
 * Lpr,lpr added
 * 
 * Revision 1.8  90/09/13  13:21:12  ache
 * MS-DOS & Unix together...
 * 
 * Revision 1.7  90/08/16  17:31:53  avg
 * Добавлена команда forward.
 * 
 * Revision 1.6  90/08/15  19:44:52  avg
 * Вделан встроенный pager.
 * 
 * Revision 1.5  90/08/07  22:13:25  avg
 * Описание SENDMAIL вынесено в отдельный файл.
 * 
 * Revision 1.4  90/05/31  19:47:29  avg
 * Правлен набор выдаваемых полей заголовков.
 * 
 * Revision 1.3  90/04/20  19:16:46  avg
 * Прикручено под System V
 * 
 * Revision 1.2  88/02/19  16:04:26  avg
 * Для экономии памяти в поле m_size заменен long на unsigned
 * для машин типа pdp11.
 * 
 * Revision 1.1  87/12/25  15:59:05  avg
 * Initial revision
 *
 *
 * Commands are:
 *      t <message list>                print out these messages
 *      r <message list>                reply to messages
 *      m <user list>                   mail to users (analogous to send)
 *      e <message list>                edit messages
 *      c [directory]                   chdir to dir or home if none
 *      x                               exit quickly
 *      w <message list> file           save messages in file
 *      q                               quit, save remaining stuff in mbox
 *      d <message list>                delete messages
 *      u <message list>                undelete messages
 *      h                               print message headers
 *
 */

/*
 * $Header: /usr/src/relcom/mailx/RCS/def.h,v 1.16 1991/01/25 18:18:41 ache Exp $
 */

#include <sys/types.h>
#include <signal.h>
#ifdef  MSDOS
#define SIGQUIT SIGBREAK
#define cfree free
#define SEPCHAR '\\'
#else
#define SEPCHAR '/'
#endif
#include <stdio.h>

#ifndef MSDOS
#ifndef M_SYSV
#include <sgtty.h>
#else
#include <termio.h>
#endif
#endif

#if defined(MSDOS) || defined(M_SYSV)
#define index strchr
#define rindex strrchr
#endif

#include "local.h"

#define ESCAPE          '~'             /* Default escape for sending */
#define NMLSIZE         20              /* max names in a message list */
#define PATHSIZE        100             /* Size of pathnames throughout */
#define NAMESIZE        20              /* Max size of user name */
#define HSHSIZE         19              /* Hash size for aliases and vars */
#define HDRFIELDS       3               /* Number of header fields */
#define LINESIZE        BUFSIZ          /* max readable line width */
#define STRINGSIZE      ((unsigned) 128)/* Dynamic allocation units */
#define MAXARGC         20              /* Maximum list of raw strings */
#define NOSTR           ((char *) NULL) /* Null string pointer */
#define MAXEXP          25              /* Maximum expansion of aliases */
#define equal(a, b)     (strcmp(a,b)==0)/* A nice function to string compare */

struct message {
	short   m_flag;                 /* flags, see below */
	long    m_offset;               /* offset of message */
#ifndef pdp11
	long    m_size;                 /* Bytes in the message */
	long    m_lines;                /* Lines in the message */
#else
	unsigned m_size;
	unsigned m_lines;               /* Lines in the message */
#endif
};

/*
 * flag bits.
 */

#define MUSED           (1<<0)          /* entry is used, but this bit isn't */
#define MDELETED        (1<<1)          /* entry has been deleted */
#define MSAVED          (1<<2)          /* entry has been saved */
#define MTOUCH          (1<<3)          /* entry has been noticed */
#define MPRESERVE       (1<<4)          /* keep entry in sys mailbox */
#define MMARK           (1<<5)          /* message is marked! */
#define MODIFY          (1<<6)          /* message has been modified */
#define MNEW            (1<<7)          /* message has never been seen */
#define MREAD           (1<<8)          /* message has been read sometime. */
#define MSTATUS         (1<<9)          /* message status has changed */
#define MBOX            (1<<10)         /* Send this to mbox, regardless */

/*
 * Format of the command description table.
 * The actual table is declared and initialized
 * in lex.c
 */

struct cmd {
	char    *c_name;                /* Name of command */
	int     (*c_func)();            /* Implementor of the command */
	short   c_argtype;              /* Type of arglist (see below) */
	short   c_msgflag;              /* Required flags of messages */
	short   c_msgmask;              /* Relevant flags of messages */
};

/* Yechh, can't initialize unions */

#define c_minargs c_msgflag             /* Minimum argcount for RAWLIST */
#define c_maxargs c_msgmask             /* Max argcount for RAWLIST */

/*
 * Argument types.
 */

#define MSGLIST  0              /* Message list type */
#define STRLIST  1              /* A pure string */
#define RAWLIST  2              /* Shell string list */
#define NOLIST   3              /* Just plain 0 */
#define NDMLIST  4              /* Message list, no defaults */

#define P       040             /* Autoprint dot after command */
#define I       0100            /* Interactive command bit */
#define M       0200            /* Legal from send mode bit */
#define W       0400            /* Illegal when read only bit */
#define F       01000           /* Is a conditional command */
#define T       02000           /* Is a transparent command */
#define R       04000           /* Cannot be called from collect */

/*
 * Oft-used mask values
 */

#define MMNORM          (MDELETED|MSAVED)/* Look at both save and delete bits */
#define MMNDEL          MDELETED        /* Look only at deleted bit */

/*
 * Structure used to return a break down of a head
 * line (hats off to Bill Joy!)
 */

struct headline {
	char    *l_from;        /* The name of the sender */
	char    *l_tty;         /* His tty string (if any) */
	char    *l_date;        /* The entire date string */
};

#define GTO     1               /* Grab To: line */
#define GSUBJECT 2              /* Likewise, Subject: line */
#define GCC     4               /* And the Cc: line */
#define GBCC    8               /* And also the Bcc: line */
#define GREFS   16              /* And the References: line */
#define GADD    32              /* And the user-defined lines */
#define GMASK   (GTO|GSUBJECT|GCC|GBCC|GREFS|GADD)
				/* Mask of places from whence */

#define GNL     64              /* Print blank line after */
#define GDEL    128             /* Entity removed from list */
#define GCOMMA  256             /* detract puts in commas */

/*
 * Structure used to pass about the current
 * state of the user-typed message header.
 */

struct header {
	char    *h_to;                  /* Dynamic "To:" string */
	char    *h_subject;             /* Subject string */
	char    *h_cc;                  /* Carbon copies string */
	char    *h_bcc;                 /* Blind carbon copies */
	char    *h_refs;                /* References to messages */
	short    h_seq;                 /* Sequence for optimization */
	short    h_resent;              /* Resent-Msg flag */
};

/*
 * Structure of namelist nodes used in processing
 * the recipients of mail and aliases and all that
 * kind of stuff.
 */

struct name {
	struct  name *n_flink;          /* Forward link in list. */
	struct  name *n_blink;          /* Backward list link */
	short   n_type;                 /* From which list it came */
	char    *n_name;                /* This fella's name */
};

/*
 * Structure of a variable node.  All variables are
 * kept on a singly-linked list of these, rooted by
 * "variables"
 */

struct var {
	struct  var *v_link;            /* Forward link to next variable */
	char    *v_name;                /* The variable's name */
	char    *v_value;               /* And it's current value */
};

struct group {
	struct  group *ge_link;         /* Next person in this group */
	char    *ge_name;               /* This person's user name */
};

struct grouphead {
	struct  grouphead *g_link;      /* Next grouphead in list */
	char    *g_name;                /* Name of this group */
	struct  group *g_list;          /* Users in group. */
};

#define NIL     ((struct name *) 0)     /* The nil pointer for namelists */
#define NONE    ((struct cmd *) 0)      /* The nil pointer to command tab */
#define NOVAR   ((struct var *) 0)      /* The nil pointer to variables */
#define NOGRP   ((struct grouphead *) 0)/* The nil grouphead pointer */
#define NOGE    ((struct group *) 0)    /* The nil group pointer */

/*
 * Structure of the hash table of ignored header fields
 */
struct ignore {
	struct ignore   *i_link;        /* Next ignored field in bucket */
	char            *i_field;       /* This ignored field */
};

/*
 * Token values returned by the scanner used for argument lists.
 * Also, sizes of scanner-related things.
 */

#define TEOL    0                       /* End of the command line */
#define TNUMBER 1                       /* A message number */
#define TDASH   2                       /* A simple dash */
#define TSTRING 3                       /* A string (possibly containing -) */
#define TDOT    4                       /* A "." */
#define TUP     5                       /* An "^" */
#define TDOLLAR 6                       /* A "$" */
#define TSTAR   7                       /* A "*" */
#define TOPEN   8                       /* An '(' */
#define TCLOSE  9                       /* A ')' */
#define TPLUS   10                      /* A '+' */

#define REGDEP  2                       /* Maximum regret depth. */
#define STRINGLEN       64              /* Maximum length of string token */

/*
 * Constants for conditional commands.  These describe whether
 * we should be executing stuff or not.
 */

#define CANY            0               /* Execute in send or receive mode */
#define CRCV            1               /* Execute in receive mode only */
#define CSEND           2               /* Execute in send mode only */

/*
 * Kludges to handle the change from setexit / reset to setjmp / longjmp
 */

#define setexit()       setjmp(srbuf)
#define reset(x)        longjmp(srbuf, x)

/*
 * VM/UNIX has a vfork system call which is faster than forking.  If we
 * don't have it, fork(2) will do . . .
 */

#ifndef VMUNIX
#define vfork() fork()
#define sigsys signal
#endif

/*
 * Definitions for flags of send()
 */
#define SF_NONE         0       /* None flags */
#define SF_DOIGN        1       /* Do hiding ignored fields */
#define SF_PAGER        2       /* Use built-in pager */
#define SF_LPR          4       /* Spooled to printer */

/*
 * Forward declarations of routine types to keep lint and cc happy.
 */

FILE    *collect();
FILE    *mesedit();
FILE    *mespipe();
FILE    *popen();
FILE    *setinput();
char    **unpack();
char    *addto();
char    *arpafix();
char    *calloc();
char    *copy();
char    *copyin();
char    *detract();
char    *expand();
char    *gets();
char    *hfield();
char    *index();
char    *name1();
char    *nameof();
char    *nextword();
char    *getenv();
char    *getfilename();
char    *hcontents();
char    *netmap();
char    *netname();
char    *reedit();
char    *netrename();
char    *revarpa();
char    *rindex();
char    *rpair();
char    *salloc();
char    *savestr();
char    *skin();
char    *snarf();
char    *strcat();
char    *strcpy();
char    *value();
char    *vcopy();
char    *yankword();
off_t   fsize();
struct  cmd     *lex();
struct  grouphead       *findgroup();
struct  name    *cat();
struct  name    *delname();
struct  name    *elide();
struct  name    *extract();
struct  name    *gexpand();
struct  name    *map();
struct  name    *outof();
struct  name    *put();
struct  name    *usermap();
struct  name    *verify();
struct  var     *lookup();
long    transmit();
int     icequal();
int     cmpdomain();
long    send();
FILE    *TmpOpen();
void    TmpDel();
void    TmpDelAll();

extern char SENDMAIL[];

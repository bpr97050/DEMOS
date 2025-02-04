/*
 *  $Log:	mkfs.c,v $
 * Revision 1.1  90/11/12  19:31:06  root
 * Initial revision
 * 
 * Revision 1.8  88/12/05  19:54:27  abs
 * Правлена буржуйцкая ошибка в iput(), приводившая к отведению
 * лишнего блока:
 * 	*aibc - КОЛ-ВО блоков в файле, а отнюдь не индекс 
 * 	последнего из них !
 * Как обычно, язык СИ провоцирует на ошибки "промах на 1".
 * В этот раз было исправлено >= на >
 * 
 * Revision 1.7  88/11/28  21:48:25  abs
 * русские диагностики.
 *
 * Revision 1.6  88/11/28  18:47:27  abs
 * Свыше порешили, что устройства есть обычные файлы.
 *
 * Revision 1.5  88/11/21  20:34:15  abs
 * правлена опечатка.
 *
 * Revision 1.4  88/11/21  20:31:37  abs
 * правки под IPK_DIRECTORY (новая структура direct).
 *
 * Revision 1.3  88/09/20  17:47:27  korotaev
 * исправлена проверка magic-number'а.
 *
 */

static char rcsid[] = "$Header: mkfs.c,v 1.1 90/11/12 19:31:06 root Exp $";

/*
 * Make a file system prototype.
 * usage: mkfs filsys proto/size [ m n ]
 */
#include        <whoami.h>
#define NIPB    (BSIZE/sizeof(struct dinode))
#ifndef NINDIR
#  define NINDIR  (BSIZE/sizeof(daddr_t))
#endif
#define NDIRECT (BSIZE/sizeof(struct direct))
#define MAXFN   500
#ifndef UCB_NKB
#define itoo(x) (int)((x+15)&07)
#endif
#ifndef STANDALONE
#ifdef sparc
#include "/usr/include/stdio.h"
#else
#include <stdio.h>
#endif
#include <a.out.h>
#endif

#ifdef STANDALONE
# define ediag( e, r ) (e)
#else
# include <ediag.h>
#endif

#include <sys/param.h>
#include <sys/ino.h>
#include <sys/inode.h>
#include <sys/filsys.h>
#include <sys/fblk.h>
#include <sys/dir.h>

/* Manage values in PDP-11 byte order */
#ifdef sparc
#define GET16(v)   ((((v)&0377)<<8)+((v)>>8&0377))
#define PUT16(v)   ((((v)&0377)<<8)+((v)>>8&0377))
#define GET32(v)   ((((long)(v)&0xFF0000)<<8)+((long)(v)>>8&0xFF0000)\
			+(((v)&0377)<<8)+((v)>>8&0377))
#define PUT32(v)   ((((long)(v)&0xFF0000)<<8)+((long)(v)>>8&0xFF0000)\
			+(((v)&0377)<<8)+((v)>>8&0377))
#else
#define GET16(v)   (v)
#define PUT16(v)   (v)
#define GET32(v)   (v)
#define PUT32(v)   (v)
#endif

/* Машинно-независимая структура каталога */
#define DMI_CLASS  0140000
#define DMI_INO     037777
#define DMI_COMB(x,c)  (((c)<<14) | ((x)&DMI_INO))
struct  direct_mi
{
        char    dmi_ino[2];
        char    dmi_name[DIRSIZ];
};

#define LADDR   (NADDR-3)
time_t  utime;
#ifndef STANDALONE
FILE    *fin;
#else
int     fin;
char    module[] = "Mkfs";
#endif
int     fsi;
int     fso;
char    *charp;
char    buf[BSIZE];

 /* Буфер блока свободных */
union {
    struct fblk fb;
    char pad1[BSIZE];
} fbuf;

#ifndef STANDALONE
struct exec head;
#endif
int bflag = 0;
daddr_t badlist[NINDIR+1];
int mbadlist = 0;
char    string[50], savestr[50];

 /* Буфер суперблока */
union {
    struct filsys fs;
    char pad2[BSIZE];
} filsys_buff;
#define filsys (filsys_buff.fs)  /* Для нелокального использования */

char    *fsys;
char    *proto;
int     f_n     = 10;
int     f_m     = 5;
int     error;
ino_t   ino;            /* номер очередного формируемого I-узла */
long    getnum();
daddr_t alloc();

main(argc, argv)
char *argv[];
{
	int f, c;
	long n;

#ifndef STANDALONE
	time(&utime);
	if(argc < 3) {
		printf( ediag(
"usage: mkfs filsys proto/size [ m n [-c] [-b badlist]]\n",
"вызов: mkfs filsys прототип/размер [ m n [-c] [-b список_плохих]]\n"
		));
		exit(1);
	}
	fsys = argv[1];
	proto = argv[2];
	fso = creat(fsys, 0666);
	if(fso < 0) {
		printf( ediag(
		       "%s: cannot create\n",
		       "%s: не могу создать\n" ), fsys);
		exit(1);
	}
	fsi = open(fsys, 0);
	if(fsi < 0) {
		printf( ediag(
		    "%s: cannot open\n",
		    "%s: не могу открыть\n" ), fsys );
		exit(1);
	}
	fin = fopen(proto, "r");
#else
	{
                char inpbuf[100];
                static char protos[60];
                printf("%s\n",module);

                do {
                        printf( ediag( "file system: ", "файловая система: "));
                        gets(inpbuf);
                        fso = open(inpbuf, 1);
                        fsi = open(inpbuf, 0);
                } while (fso < 0 || fsi < 0);

                printf(ediag("file sys size: ", "размер файловой системы: "));
                gets(protos);
                proto = protos;
                printf( ediag(
                        "interleaving factor (m; %d default): ",
                        "фактор интерливинга (m; %d по умолчанию): "), f_m);
                gets(inpbuf);
                if (inpbuf[0])
                        f_m = atoi(inpbuf);
                printf( ediag(
                        "interleaving modulus (n; %d default): ",
                        "модуль интерливинга (n; %d по умолчанию): "), f_n);
                gets(inpbuf);
                if (inpbuf[0])
                        f_n = atoi(inpbuf);

                if(f_n <= 0 || f_n >= MAXFN)
                        f_n = MAXFN;
                if(f_m <= 0 || f_m > f_n)
                        f_m = 3;

                printf( ediag( "check bad blocks(y/n)?: ",
                               "проверять на плохие блоки(y/n)?: "));
                gets(inpbuf);
                bflag=(inpbuf[0]=='y' || inpbuf[0]=='Y');
	}
	fin = NULL;
	argc = 0;
#endif
	if(fin == NULL) {
		n = 0;
		for(f=0; c=proto[f]; f++) {
			if(c<'0' || c>'9') {
				printf( ediag("%s: cannot open\n",
					      "%s: не могу открыть\n"), proto);
				exit(1);
			}
			n = n*10 + (c-'0');
		}
		filsys.s_fsize = n;
#ifndef UCB_NKB
#define CLSIZE  1
#endif
		/*
		 * Minor hack for standalone root and other
		 * small filesystems: reduce ilist size.
		 */
		if (n <= 5000/CLSIZE)
			n = n/50;
		else
			n = n/25;
		if(n <= 0)
			n = 1;
		if(n > 65500/NIPB)
			n = 65500/NIPB;
		filsys.s_isize = n + 2;
		printf("isize = %D\n", n*NIPB);
		charp = "d--777 0 0 $ ";
		goto f3;
	}

#ifndef STANDALONE
	/*
	 * get name of boot load program
	 * and read onto block 0
	 */

	getstr();
	f = open(string, 0);
	if(f < 0) {
		printf( ediag("%s: cannot  open boot\n",
			      "%s: не могу открыть загрузчик\n"), string);
		goto f2;
	}
#ifdef NODEF
	read(f, (char *)&head, sizeof head);
	if(head.a_magic != A_MAGIC1) {
		printf(ediag( "%s: bad format\n",
			      "%s: плохой формат\n"), string);
		goto f1;
	}
	c = head.a_text + head.a_data;
	if(c > BSIZE) {
		printf( ediag( "%s: too big\n",
			       "%s: слишком большой\n"), string);
		goto f1;
	}
#endif NODEF
	read(f, buf, BSIZE);
	wtfs((long)0, buf);

f1:
	close(f);

	/*
	 * get total disk size
	 * and inode block size
	 */

f2:
	filsys.s_fsize = getnum();
	n = getnum();
	n /= NIPB;
	filsys.s_isize = n + 3;

#endif
f3:
	if(argc >= 5) {
		f_m = atoi(argv[3]);
		f_n = atoi(argv[4]);
		if(f_n <= 0 || f_n >= MAXFN)
			f_n = MAXFN;
		if(f_m <= 0 || f_m > f_n)
			f_m = 3;
	}
	filsys.s_m = f_m;
	filsys.s_n = f_n;
	printf("m/n = %d %d\n", f_m, f_n);
	if(filsys.s_isize >= filsys.s_fsize) {
		printf("%ld/%ld: %s\n", filsys.s_fsize, filsys.s_isize-2,
			       ediag("bad ratio","плохой коэффициент"));
		exit(1);
	}
#ifndef STANDALONE
	if(argc>=6 ) {
		if( argv[5][0]=='-' && argv[5][1]=='c') bflag=1;
		else if(argv[5][0]=='-' && argv[5][1]=='b' )
		{
			register int i=5;
			long atol();
			while(++i<argc)
				badlist[mbadlist++] = dbtofsb(atol(argv[i]));
		}
	}
#endif
	filsys.s_tfree = 0;
	filsys.s_tinode = 0;
	for(c=0; c<BSIZE; c++)
		buf[c] = 0;
	for(n=2; n!=filsys.s_isize; n++) {
		wtfs(n, buf);
		filsys.s_tinode += NIPB;
	}
	ino = 0;

	bflist();

	getstr();
	cfile((struct inode *)0, 0);

	filsys.s_time = utime;

#ifdef sparc
	/* P3: Перед выходом можно окучить прямо на месте */
	filsys.s_isize = PUT16(filsys.s_isize);
	filsys.s_fsize = PUT32(filsys.s_fsize);
	filsys.s_nfree = PUT16(filsys.s_nfree);
	for(n = 0; n < NICFREE; n++) {
		filsys.s_free[n] = PUT32(filsys.s_free[n]);
	}
	filsys.s_ninode = PUT16(filsys.s_ninode);
	for(n = 0; n < NICINOD; n++) {
		filsys.s_inode[n] = PUT32(filsys.s_inode[n]);
	}
	filsys.s_tfree = PUT32(filsys.s_tfree);
	filsys.s_tinode = PUT16(filsys.s_tinode);
	filsys.s_dinfo[0] = PUT16(filsys.s_dinfo[0]);
	filsys.s_dinfo[1] = PUT16(filsys.s_dinfo[1]);
#endif
	wtfs((long)1, (char *)&filsys);
	/* записали суперблок на диск */
	exit(error);
}

/* create file in FS - recursive */
cfile(par, reclevel)
struct inode *par;
{
	struct inode in;
	int dbc, ibc;
	char db[BSIZE];
	daddr_t ib[NINDIR];
	int i, f, c;

	/*
	 * get mode, uid and gid
	 */

	/* getstr(); */
	/* Этот вызов теперь вынесен ПЕРЕД функцией,
	 * т.е. следует использовать
	 *      getstr(); .... cfile( ... );
	 */

	in.i_mode = gmode(string[0], "-bcd", IFREG, IFBLK, IFCHR, IFDIR);
	in.i_mode |= gmode(string[1], "-u", 0, ISUID, 0, 0);
	in.i_mode |= gmode(string[2], "-g", 0, ISGID, 0, 0);
	for(i=3; i<6; i++) {
		c = string[i];
		if(c<'0' || c>'7') {
			printf( ediag("%c/%s: bad octal mode digit\n",
				      "%c/%s: не восьмеричная цифра в кодах доступа\n"),
				c, string);
			error = 1;
			c = 0;
		}
		in.i_mode |= (c-'0')<<(15-3*i);
	}
	in.i_uid = getnum();
	in.i_gid = getnum();

	/*
	 * general initialization prior to
	 * switching on format
	 */

	ino++;
	in.i_number = ino;
	for(i=0; i<BSIZE; i++)
		db[i] = 0;
	for(i=0; i<NINDIR; i++)
		ib[i] = (daddr_t)0;
	in.i_nlink = 1;
	in.i_size = 0;
	for(i=0; i<NADDR; i++)
		in.i_un.i_addr[i] = (daddr_t)0;
	if(par == (struct inode *)0) {
		par = &in;
		in.i_nlink--;
	}
	dbc = 0;
	ibc = 0;
	switch(in.i_mode&IFMT) {

	case IFREG:
		/*
		 * regular file
		 * contents is a file name
		 */

		getstr();
		f = open(string, 0);
		if(f < 0) {
			printf( ediag( "%s: cannot open\n",
				       "%s: не могу открыть\n"),string);
			error = 1;
			break;
		}
		while((i=read(f, db, BSIZE)) > 0) {
			in.i_size += i;
			newblk(&dbc, db, &ibc, ib);
		}
		close(f);
		break;

	case IFBLK:
	case IFCHR:
		/*
		 * special file
		 * content is maj/min types
		 */

		i = getnum() & 0377;
		f = getnum() & 0377;
		in.i_un.i_addr[0] = (i<<8) | f;
		break;

	case IFDIR:
		/*
		 * directory
		 * put in extra links
		 * call recursively until
		 * name of "$" found
		 */

		par->i_nlink++;
		in.i_nlink++;
#ifndef IPK_DIRECTORY
		entry(in.i_number, ".", &dbc, db, &ibc, ib);
		entry(par->i_number, "..", &dbc, db, &ibc, ib);
#else
		entry(in.i_number, ".", &dbc, db, &ibc, ib,    DIR_IFDIR);
		entry(par->i_number, "..", &dbc, db, &ibc, ib, DIR_IFDIR);
#endif
		in.i_size = 2*sizeof(struct direct);
		for(;;) {
			int class;

			getstr();
			if(string[0]=='$' && string[1]=='\0')
				break;

			strcpy( savestr, string );
			getstr();  /* для cfile */
#ifdef IPK_DIRECTORY
			class = gmode( string[0] , "-bcd",
				DIR_IFREG, DIR_IFREG, DIR_IFREG, DIR_IFDIR );
			entry(ino+1, savestr , &dbc, db, &ibc, ib, class);
#else
			entry(ino+1, savestr , &dbc, db, &ibc, ib);
#endif
			in.i_size += sizeof(struct direct);
			cfile(&in, reclevel + 1);
		}
		break;
	}
	if (reclevel == 0) {
#ifndef IPK_DIRECTORY
		entry(ino+1, "lost+found", &dbc, db, &ibc, ib);
#else
		entry(ino+1, "lost+found", &dbc, db, &ibc, ib, DIR_IFDIR );
#endif
		in.i_size += sizeof(struct direct);
		mklost(&in);
	}
	if(dbc != 0)
		newblk(&dbc, db, &ibc, ib);
	iput(&in, &ibc, ib);
}

/*ARGSUSED*/
/*VARARGS3*/
gmode(c, s, m0, m1, m2, m3)
char c, *s;
{
	int i;

	for(i=0; s[i]; i++)
		if(c == s[i])
			return((&m0)[i]);
	printf( ediag( "%c/%s: bad mode\n",
		       "%c/%s: неверные моды\n"), c, string);
	error = 1;
	return(0);
}

long
getnum()
{
	int i, c;
	long n;

	getstr();
	n = 0;
	i = 0;
	for(i=0; c=string[i]; i++) {
		if(c<'0' || c>'9') {
			printf( ediag( "%s: bad number\n",
				       "%s: не цифра в числе\n") , string);
			error = 1;
			return((long)0);
		}
		n = n*10 + (c-'0');
	}
	return(n);
}

getstr()
{
	int i, c;

loop:
	switch(c=getch()) {

	case ' ':
	case '\t':
	case '\n':
		goto loop;

	case '\0':
		printf("EOF\n");
		exit(1);

	case ':':
		while(getch() != '\n');
		goto loop;

	}
	i = 0;

	do {
		string[i++] = c;
		c = getch();
	}
#ifdef  STANDALONE
	while(c!=' '&&c!='\t'&&c!='\n'&&c!='\0');
#else
	while(c!=' '&&c!='\t'&&c!='\n'&&c!='\0' && c != EOF);
#endif
	string[i] = '\0';
}

rdfs(bno, bf)
daddr_t bno;
char *bf;
{
	int n;

	lseek(fsi, bno*BSIZE, 0);
	n = read(fsi, bf, BSIZE);
	if(n != BSIZE) {
		printf( ediag( "read error: %ld\n",
			       "ошибка чтения: %ld\n"), bno);
		exit(1);
	}
}

wtfs(bno, bf)
daddr_t bno;
char *bf;
{
	int n;

	lseek(fso, bno*BSIZE, 0);
	n = write(fso, bf, BSIZE);
	if(n != BSIZE) {
		printf( ediag( "write error: %D\n",
			       "ошибка записи: %D\n"), bno);
		exit(1);
	}
}

daddr_t
alloc()
{
	int i;
	daddr_t bno;

	filsys.s_tfree--;
	bno = filsys.s_free[--filsys.s_nfree];
	if(bno == 0) {
		printf( ediag("out of free space\n","нет свободного места\n"));
		exit(1);
	}
	if(filsys.s_nfree <= 0) {
		rdfs(bno, fbuf.pad1);
		filsys.s_nfree = GET32(fbuf.fb.df_nfree);
		for(i=0; i<NICFREE; i++)
			filsys.s_free[i] = GET32(fbuf.fb.df_free[i]);
	}
	return(bno);
}

bfree(bno)
daddr_t bno;
{
	int i;

	if (bno != 0)
		filsys.s_tfree++;
	if(filsys.s_nfree >= NICFREE) {
		fbuf.fb.df_nfree = PUT16(filsys.s_nfree);
		for(i=0; i<NICFREE; i++)
			fbuf.fb.df_free[i] = PUT16(filsys.s_free[i]);
		wtfs(bno, fbuf.pad1);
		filsys.s_nfree = 0;
	}
	filsys.s_free[filsys.s_nfree++] = bno;
}

#ifndef IPK_DIRECTORY
entry(inum, str, adbc, db, aibc, ib)
#else
entry(inum, str, adbc, db, aibc, ib, class)
#endif

ino_t inum;
char *str;
int *adbc, *aibc;
char *db;
daddr_t *ib;
{
	struct direct *dp;
	int i;

	dp = (struct direct *)db;
	dp += *adbc;
	(*adbc)++;

/*
	dp->d_ino = inum;
#ifdef IPK_DIRECTORY
	dp-> d_class = class;
#endif
*/
#ifdef IPK_DIRECTORY
	i = DMI_COMB(inum,class);
#else
	i = inum;
#endif
	((struct direct_mi *)dp)->dmi_ino[0] = i;
	((struct direct_mi *)dp)->dmi_ino[1] = i >> 8;

	for(i=0; i<DIRSIZ; i++)
		dp->d_name[i] = 0;
	for(i=0; i<DIRSIZ; i++)
		if((dp->d_name[i] = str[i]) == 0)
			break;
	if(*adbc >= NDIRECT)
		newblk(adbc, db, aibc, ib);
}

newblk(adbc, db, aibc, ib)
int *adbc, *aibc;
char *db;
daddr_t *ib;
{
	int i;
	daddr_t bno;

	bno = alloc();
	wtfs(bno, db);
	for(i=0; i<BSIZE; i++)
		db[i] = 0;
	*adbc = 0;
	ib[*aibc] = bno;
	(*aibc)++;
	if(*aibc >= NINDIR) {
		printf(ediag( "indirect block full\n",
			      "переполнился косвенный блок\n"));
		error = 1;
		*aibc = 0;
	}
}

getch()
{

#ifndef STANDALONE
	if(charp)
#endif
		return(*charp++);
#ifndef STANDALONE
	return(getc(fin));
#endif
}

bflist()
{
	struct inode in;
	daddr_t ib[NINDIR];
	int ibc;
	char flg[MAXFN];
	int adr[MAXFN];
	int i, j;
	daddr_t f, d;

	for(i=0; i<f_n; i++)
		flg[i] = 0;
	i = 0;
	for(j=0; j<f_n; j++) {
		while(flg[i])
			i = (i+1)%f_n;
		adr[j] = i+1;
		flg[i]++;
		i = (i+f_m)%f_n;
	}

	ino++;
	in.i_number = ino;
	in.i_mode = IFREG;
	in.i_uid = 0;
	in.i_gid = 0;
	in.i_nlink = 0;
	in.i_size = 0;
	for(i=0; i<NADDR; i++)
		in.i_un.i_addr[i] = (daddr_t)0;

	for(i=0; i<NINDIR; i++)
		ib[i] = (daddr_t)0;
	ibc = 0;
	bfree((daddr_t)0);
	d = filsys.s_fsize-1;
	while(d%f_n)
		d++;
	for(; d > 0; d -= f_n)
	for(i=0; i<f_n; i++) {
		f = d - adr[i];
		if(f < filsys.s_fsize && f >= filsys.s_isize)
			if(badblk(f)) {
				if(ibc >= NINDIR) {
					printf( ediag("too many bad blocks\n",
					  "слишком много плохих блоков\n" ));
					error = 1;
					ibc = 0;
				}
				ib[ibc] = f;
				ibc++;
			} else
				bfree(f);
	}
	if( ibc)
	{ printf( ediag( "Bad blocks (%d total):\n",
			 "Плохие блоки (%d всего):\n") ,ibc);
	  for(i=0;i<ibc;i++)
#ifdef STANDALONE
	  printf("%D%c",ib[i],((i+1)%4?' ':'\n'));
#else
	  printf("%ld %c",ib[i],((i+1)%4?'\t':'\n'));
#endif
	  printf("\n");
	}
	iput(&in, &ibc, ib);
}

iput(ip, aibc, ib)
struct inode *ip;
int *aibc;
daddr_t *ib;			/* May be wasted */
{
	struct dinode *dp;
	daddr_t d;
	int i;

	filsys.s_tinode--;
	d = itod(ip->i_number);
	if(d >= filsys.s_isize) {
		if(error == 0)
			printf( ediag( "ilist too small\n",
				       "мала таблица i-узлов\n"));
		error = 1;
		return;
	}
	rdfs(d, buf);
	dp = (struct dinode *)buf;
	dp += itoo(ip->i_number);

	dp->di_mode = PUT16(ip->i_mode);
	dp->di_nlink = PUT16(ip->i_nlink);
	dp->di_uid = PUT16(ip->i_uid);
	dp->di_gid = PUT16(ip->i_gid);
	dp->di_size = PUT32(ip->i_size);
	dp->di_atime = PUT32(utime);
	dp->di_mtime = PUT32(utime);
	dp->di_ctime = PUT32(utime);

	switch(ip->i_mode&IFMT) {

	case IFDIR:
	case IFREG:

/* (*aibc) - количество блоков в файле (1..4 - прямые блоки, 0 - пуст).
 * адреса блоков содержатся в ib
 * LADDR - кол-во прямых блоков.
 */

		/* прямые блоки */
		for(i=0; i< *aibc ; i++) {
			if(i >= LADDR)
				break; /* нужен косвенный блок */
			ip->i_un.i_addr[i] = ib[i];
		}

		if(*aibc > LADDR) {
			ip->i_un.i_addr[LADDR] = alloc();
			/* косвенный (адресный) блок */

			/* заполняем его */
			for(i=0; i<NINDIR-LADDR; i++) {
				ib[i] = PUT32(ib[i+LADDR]);
                                ib[i+LADDR] = (daddr_t)0;
			}
			wtfs(ip->i_un.i_addr[LADDR], (char *)ib);
		}

	case IFBLK:
	case IFCHR:
		ltol3(dp->di_addr, ip->i_un.i_addr, NADDR);
		break;

	default:
		printf( ediag( "bad mode %o\n",
			       "плохие моды %o\n"), ip->i_mode);
		exit(1);
	}
	wtfs(d, buf);
}

/** ?? *ARGSUSED*/
/** static int i_chk = 0; **/
badblk(bno)
daddr_t bno;
{
	char tbuf[BSIZE];
	if(mbadlist) {
		register int i;
		for(i=0; i<mbadlist;i++) if(badlist[i]==bno) return(1);
	}
	if(bflag){
		lseek(fsi, bno*BSIZE, 0);
		if(read(fsi, tbuf, BSIZE)  != BSIZE)
			return(1);
	}
	return(0);
}

mklost(par)
struct inode *par;
{
	struct inode in;
	int dbc, ibc;
	char db[BSIZE];
	daddr_t ib[NINDIR];
	int i;

	in.i_mode = IFDIR | ISVTX | 0777;
	in.i_uid = 0;
	in.i_gid = 0;
	in.i_number = ++ino;
	for (i = 0; i < BSIZE; i++)
		db[i] = 0;
	for (i = 0; i < NINDIR; i++)
		ib[i] = (daddr_t) 0;
	for (i = 0; i < NADDR; i++)
		in.i_un.i_addr[i] = (daddr_t) 0;
	dbc = 0;
	ibc = 0;
	in.i_nlink = 2;
	/*
	 * blocks 0, ..., NADDR - 4
	 * are direct blocks
	 */
	in.i_size = (off_t) (BSIZE * (NADDR - 4 + 1));
	par->i_nlink++;
#ifndef IPK_DIRECTORY
	entry(in.i_number, ".", &dbc, db, &ibc, ib);
	entry(par->i_number, "..", &dbc, db, &ibc, ib);
#else
	entry(in.i_number, ".", &dbc, db, &ibc, ib,    DIR_IFDIR);
	entry(par->i_number, "..", &dbc, db, &ibc, ib, DIR_IFDIR);
#endif
	for (i = 0; i < NADDR - 4 + 1; i++)
		newblk(&dbc, db, &ibc, ib);
	iput(&in, &ibc, ib);
}

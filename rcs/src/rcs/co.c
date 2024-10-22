/*
 *                     RCS checkout operation
 */
 static char rcsid[]=
 "$Header: /usr/users/korotay/DEMOS_2.2/rcs/src/rcs/RCS/co.c,v 1.3 1986/02/23 22:12:07 avg Exp $";
/*****************************************************************************
 *                       check out revisions from RCS files
 *****************************************************************************
 */

/* $Log: co.c,v $
 * Revision 1.3  1986/02/23  22:12:07  avg
 * Сделаны изменения для МНОС и для работы с U-кодом.
 *
 * Revision 1.1  85/12/26  21:55:35  fox
 * Система хранения версий файлов
 *
 */

#include <pwd.h>
#include "rcsbase.h"
#include "time.h"
#include <sys/types.h>
#include <sys/stat.h>

extern FILE * fopen();
extern int    rename();
extern struct passwd *getpwuid();
extern char * malloc();
extern struct hshentry * genrevs(); /*generate delta numbers                */
extern int  nextc;                  /*next input character                  */
extern int  nerror;                 /*counter for errors                    */
extern char * Kdesc;                /*keyword for description               */
extern char * maketempfile();       /*temporary file name                   */
extern char * buildrevision();      /*constructs desired revision           */
extern int    buildjoin();          /*join several revisions                */
extern char * mktempfile();         /*temporary file name generator         */
extern struct lock * addlock();     /*add a new lock                        */
extern long   maketime();           /*convert parsed time to unix time.     */
extern struct tm * localtime();     /*convert unixtime into a tm-structure  */
extern int StrictLocks;
extern FILE * finptr;               /* RCS input file                       */
extern FILE * frewrite;             /* new RCS file                         */

char * RCSfilename, * workfilename;
char * newRCSfilename, * neworkfilename;
int    rewriteflag; /* indicates whether input should be echoed to frewrite */

char * date, * rev, * state, * author, * join;
char finaldate[datelength];

int lockflag, tostdout;
char * caller;                        /* caller's login;                    */
extern quietflag;

char numericrev[revlength];           /* holds expanded revision number     */
struct hshentry * gendeltas[hshsize]; /* stores deltas to be generated      */
struct hshentry * targetdelta;        /* final delta to be generated        */

char * joinlist[joinlength];          /* pointers to revisions to be joined */
int lastjoin;                         /* index of last element in joinlist  */

main (argc, argv)
int argc;
char * argv[];
{
	register c;
	char * cmdusage;
	struct stat RCSstat;
	struct tm parseddate, *ftm;
	char * rawdate;
	long unixtime;

	catchints();
	cmdid = "co";
	cmdusage = "command format:\nco -l[rev] -p[rev] -q[rev] -r[rev] -ddate -sstate -w[login] -jjoinlist file ...";
	date = rev = state = author = join = nil;
	lockflag = tostdout = quietflag = false;
#ifdef MNOS
	caller=getpwuid(getuid()&0377)->pw_name;
#else
	caller=getpwuid(getuid())->pw_name;
#endif

	while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
		switch ((*argv)[1]) {

		case 'l':
			lockflag=true;
		case 'r':
		revno:  if ((*argv)[2]!='\0') {
				if (rev!=nil) warn("Redefinition of revision number");
				rev = (*argv)+2;
			}
			break;

		case 'p':
			tostdout=true;
			goto revno;

		case 'q':
			quietflag=true;
			goto revno;

		case 'd':
			if ((*argv)[2]!='\0') {
				if (date!=nil) warn("Redefinition of -d option");
				rawdate=(*argv)+2;
			}
			/* process date/time */
			if (partime(rawdate,&parseddate)==0)
				faterror("Can't parse date/time: %s",rawdate);
			if ((unixtime=maketime(&parseddate))== 0L)
				faterror("Inconsistent date/time: %s",rawdate);
			ftm=localtime(&unixtime);
			sprintf(finaldate,DATEFORM,
			ftm->tm_year,ftm->tm_mon+1,ftm->tm_mday,ftm->tm_hour,ftm->tm_min,ftm->tm_sec);
			date=finaldate;
			break;

		case 'j':
			if ((*argv)[2]!='\0'){
				if (join!=nil)warn("Redefinition of -j option");
				join = (*argv)+2;
			}
			break;

		case 's':
			if ((*argv)[2]!='\0'){
				if (state!=nil)warn("Redefinition of -s option");
				state = (*argv)+2;
			}
			break;

		case 'w':
			if (author!=nil)warn("Redefinition of -w option");
			if ((*argv)[2]!='\0')
				author = (*argv)+2;
			else    author = caller;
			break;

		default:
			faterror("unknown option: %s\n%s", *argv,cmdusage);

		};
	} /* end of option processing */

	if (argc<1) faterror("No input file\n%s",cmdusage);

	/* now handle all filenames */
	do {
	rewriteflag=false;
	finptr=frewrite=NULL;
	neworkfilename=nil;

	if (!pairfilenames(argc,argv,true,tostdout)) continue;

	/* now RCSfilename contains the name of the RCS file, and finptr
	 * the file descriptor. If tostdout is false, workfilename contains
	 * the name of the working file, otherwise undefined (not nil!).
	 */
	diagnose("%s  -->  %s", RCSfilename,tostdout?"stdout":workfilename);

	fstat(fileno(finptr),&RCSstat); /* get file status, esp. the mode  */

	if (!tostdout && !trydiraccess(workfilename)) continue; /* give up */
	if (lockflag && !checkaccesslist(caller)) continue;     /* give up */
	if (!trysema(RCSfilename,lockflag)) continue;           /* give up */


	gettree();  /* reads in the delta tree */

	if (Head==nil) {
		/* no revisions; create empty file */
		diagnose("no revisions present; generating empty revision 0.0");
		if (!tostdout)
			if (!creatempty(workfilename)) continue;
		else    putchar('\0'); /* end of file */
		/* Can't reserve a delta, so don't call addlock */
	} else {
		/* expand symbolic revision number */
		if (!expandsym(rev,numericrev))
			continue;
		/* get numbers of deltas to be generated */
		if (!(targetdelta=genrevs(numericrev,date,author,state,gendeltas)))
			continue;
		/* check reservations */
		if (lockflag && !addlock(targetdelta,caller))
			continue;

		if (join && !preparejoin()) continue;

		diagnose("revision %s %s",targetdelta->num,
			 lockflag?"(locked)":"");

		/* remove old working file if necessary */
		if (!tostdout)
			if (!rmoldfile(workfilename)) continue;

		/* prepare for rewriting the RCS file */
		if (lockflag) {
			newRCSfilename=mktempfile(RCSfilename,NEWRCSFILE);
			if ((frewrite=fopen(newRCSfilename, "w"))==NULL) {
				error("Can't open file %s",newRCSfilename);
				continue;
			}
			putadmin(frewrite);
			puttree(Head,frewrite);
			fprintf(frewrite, "\n\n%s%c",Kdesc,nextc);
			rewriteflag=true;
		}

		/* skip description */
		getdesc(false); /* don't echo*/

		if (!(neworkfilename=buildrevision(gendeltas,targetdelta,
		      tostdout?(join!=nil?"/tmp/":nil):workfilename,true)))
				continue;

		if (lockflag&&nerror==0) {
			/* rewrite the rest of the RCSfile */
			fastcopy(finptr,frewrite);
			ffclose(frewrite); frewrite=NULL;
			ignoreints();
			if (rename(newRCSfilename,RCSfilename)<0) {
				error("Can't rewrite %s; saved in: %s",
				RCSfilename, newRCSfilename);
				newRCSfilename[0]='\0'; /* avoid deletion*/
				catchints();
				break;
			}
			newRCSfilename[0]='\0'; /* avoid re-deletion by cleanup()*/
			if (chmod(RCSfilename,RCSstat.st_mode & ~0222)<0)
			    warn("Can't preserve mode of %s",RCSfilename);
			catchints();
		}

#               ifdef SNOOPFILE
		logcommand("co",targetdelta,gendeltas,caller);
#               endif

		if (join) {
			rmsema(); /* kill semaphore file so other co's can proceed */
			if (!buildjoin(neworkfilename,tostdout)) continue;
		}
		if (!tostdout) {
			if (link(neworkfilename,workfilename) <0) {
				error("Can't create %s; see %s",workfilename,neworkfilename);
				neworkfilename[0]= '\0'; /*avoid deletion*/
				continue;
			}
		}
	}
	if (!tostdout)
	    if (chmod(workfilename, WORKMODE(RCSstat.st_mode))<0)
		warn("Can't adjust mode of %s",workfilename);

	if (!tostdout) diagnose("done");
	} while (cleanup(),
		 ++argv, --argc >=1);

	exit(nerror!=0);

}       /* end of main (co) */


/*****************************************************************
 * The following routines are auxiliary routines
 *****************************************************************/

int rmoldfile(ofile)
char * ofile;
/* Function: unlinks ofile, if it exists, under the following conditions:
 * If the file is read-only, file is unlinked.
 * Otherwise (file writable):
 *   if !quietmode asks the user whether to really delete it (default: fail);
 *   otherwise failure.
 * Returns false on failure to unlink, true otherwise.
 */
{
	int response, c;    /* holds user response to queries */
	struct stat buf;

	if (stat (ofile, &buf) < 0)         /* File doesn't exist */
	    return (true);                  /* No problem         */

	if (buf.st_mode & 0222) {            /* File is writable */
	    if (!quietflag) {
		fprintf(stderr,"writable %s exists; overwrite? [ny](n): ",ofile);
		/* must be stderr in case of IO redirect */
		c=response=getchar();
		while (!(c==EOF || c=='\n')) c=getchar(); /*skip rest*/
		if (!(response=='y'||response=='Y')) {
			warn("checkout aborted.");
			return false;
		}
	    } else {
		error("writable %s exists; checkout aborted.",ofile);
		return false;
	    }
	}
	/* now unlink: either not writable, or permission given */
	if (unlink(ofile) != 0) {            /* Remove failed   */
	    error("Can't unlink %s",ofile);
	    return false;
	}
	return true;
}


creatempty(file)
char * file;
/* Function: creates an empty file named file.
 * Removes an existing file with the same name with rmoldfile().
 */
{
	int  fdesc;              /* file descriptor */

	if (!rmoldfile(file)) return false;
	fdesc=creat(file,0666);
	if (fdesc < 0) {
		faterror("Cannot create %s",file);
		return false;
	} else {
		close(fdesc); /* empty file */
		return true;
	}
}



/*****************************************************************
 * The rest of the routines are for handling joins
 *****************************************************************/

char * getrev(sp, tp, buffsize)
register char * sp, *tp; int buffsize;
/* Function: copies a symbolic revision number from sp to tp,
 * appends a '\0', and returns a pointer to the character following
 * the revision number; returns nil if the revision number is more than
 * buffsize characters long.
 * The revision number is terminated by space, tab, comma, colon,
 * semicolon, newline, or '\0'.
 * used for parsing the -j option.
 */
{
	register char c;
	register int length;

	length = 0;
	while (((c= *sp)!=' ')&&(c!='\t')&&(c!='\n')&&(c!=':')&&(c!=',')
		&&(c!=';')&&(c!='\0')) {
		if (length>=buffsize) return false;
		*tp++= *sp++;
		length++;
	}
	*tp= '\0';
	return sp;
}



int preparejoin()
/* Function: Parses a join list pointed to by join and places pointers to the
 * revision numbers into joinlist.
 */
{
	struct hshentry * (* joindeltas)[];
	struct hshentry * tmpdelta;
	register char * j;
	char symbolrev[revlength],numrev[revlength];

	joindeltas = (struct hshentry * (*)[])malloc(hshsize*sizeof(struct hshentry *));
	j=join;
	lastjoin= -1;
	for (;;) {
		while ((*j==' ')||(*j=='\t')||(*j==',')) j++;
		if (*j=='\0') break;
		if (lastjoin>=joinlength-2) {
			error("too many joins");
			return(false);
		}
		if(!(j=getrev(j,symbolrev,revlength))) return false;
		if (!expandsym(symbolrev,numrev)) return false;
		tmpdelta=genrevs(numrev,nil,nil,nil,joindeltas);
		if (tmpdelta==nil)
			return false;
		else    joinlist[++lastjoin]=tmpdelta->num;
		while ((*j==' ') || (*j=='\t')) j++;
		if (*j == ':') {
			j++;
			while((*j==' ') || (*j=='\t')) j++;
			if (*j!='\0') {
				if(!(j=getrev(j,symbolrev,revlength))) return false;
				if (!expandsym(symbolrev,numrev)) return false;
				tmpdelta=genrevs(numrev,nil,nil,nil,joindeltas);
				if (tmpdelta==nil)
					return false;
				else    joinlist[++lastjoin]=tmpdelta->num;
			} else {
				error("join pair incomplete");
				return false;
			}
		} else {
			if (lastjoin==0) { /* first pair */
				/* common ancestor missing */
				joinlist[1]=joinlist[0];
				lastjoin=1;
				/*derive common ancestor*/
				joinlist[0]=malloc(revlength);
				if (!getancestor(targetdelta->num,joinlist[1],joinlist[0]))
				       return false;
			} else {
				error("join pair incomplete");
				return false;
			}
		}
	}
	if (lastjoin<1) {
		error("empty join");
		return false;
	} else  return true;
}



buildjoin(initialfile, tostdout)
char * initialfile; int tostdout;
/* Function: merge pairs of elements in joinlist into initialfile
 * If tostdout==true, copy result to stdout.
 * All unlinking of initialfile, rev2, and rev3 should be done by cleanup().
 */
{       char command[NCPPN+80];
	char subs[revlength];
	char * rev2, * rev3;
	int i;

	rev2=mktempfile("/tmp/",JOINFIL2);
	rev3=mktempfile("/tmp/",JOINFIL3);

	i=0;
	while (i<lastjoin) {
		/*prepare marker for merge*/
		if (i==0)
			strcpy(subs,targetdelta->num);
		else    sprintf(subs, "merge%d",i/2);
		diagnose("revision %s",joinlist[i]);
		sprintf(command,"%s/co -p%s -q  %s > %s\n",TARGETDIR,joinlist[i],RCSfilename,rev2);
		if (system(command)) {
			nerror++;return false;
		}
		diagnose("revision %s",joinlist[i+1]);
		sprintf(command,"%s/co -p%s -q  %s > %s\n",TARGETDIR,joinlist[i+1],RCSfilename,rev3);
		if (system(command)) {
			nerror++; return false;
		}
		diagnose("merging...");
		sprintf(command,"%s %s%s %s %s %s %s\n", MERGE,
			((i+2)>=lastjoin && tostdout)?"-p ":"",
			initialfile,rev2,rev3,subs,joinlist[i+1]);
		if (system(command)) {
			nerror++; return false;
		}
		i=i+2;
	}
	return true;
}

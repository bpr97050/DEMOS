/*
 *                     RCS file comparison
 */

/* $Header: /usr/users/korotay/DEMOS_2.2/rcs/src/rcs/RCS/rcsfcmp.c,v 1.3 1986/02/23 22:17:42 avg Exp $ */

/*****************************************************************************
 *                       rcsfcmp()
 *                       Testprogram: define FCMPTEST
 *****************************************************************************
 */

/* $Log: rcsfcmp.c,v $
 * Revision 1.3  1986/02/23  22:17:42  avg
 * Сделаны изменения для МНОС и для работы с U-кодом.
 *
 * Revision 1.2  85/12/29  15:02:51  fox
 * Убран rcsid
 * 
 * Revision 1.1  85/12/26  22:05:40  fox
 * Система хранения версий файлов
 * 
 */

/*
#define FCMPTEST
/* Testprogram; prints out whether two files are identical,
 * except for keywords
 */

#include  "rcsbase.h"
extern FILE * fopen();

rcsfcmp(xfname,uxfname,delta)
char * xfname, *uxfname; struct hshentry *delta;
/* Function: compares the files xfname and uxfname. Returns true
 * if xfname has the same contents as uxfname, while disregarding
 * keyword values. For the LOG-keyword, rcsfcmp skips the log message
 * given by the parameter delta in xfname. Thus, rcsfcmp returns true
 * if xfname contains the same as uxfname, with the keywords expanded.
 */
{
    register int xc,uxc;
    char xkeyword[keylength+2], uxkeyword[keylength+2];
    register char * tp;
    FILE * xfp, * uxfp;
    int result;

    if ((xfp=fopen(tp=xfname,"r"))==NULL || (uxfp=fopen(tp=uxfname,"r"))==NULL) {
       faterror("Can't open %s\n", tp);
       return false;
    }
    result=false;
    xc=getc(xfp); uxc=getc(uxfp);
    while( xc == uxc) { /* comparison loop */
        if (xc==EOF) { /* finished; everything is the same*/
            result=true;
            break;
        }
        if ( xc!=KDELIM) {
            /* get the next characters */
            xc=getc(xfp); uxc=getc(uxfp);
        } else {
            /* try to get both keywords */
            tp = xkeyword;
            while( ctab[(xc=getc(xfp))]==LETTER && tp< xkeyword+keylength)
                *tp++ = xc;
            *tp='\0';
            tp = uxkeyword;
            while( ctab[(uxc=getc(uxfp))]==LETTER && tp< uxkeyword+keylength)
                *tp++ = uxc;
            *tp='\0';
            /* now we have 2 keywords, or something thal looks like it.*/
            if (strcmp(xkeyword,uxkeyword)!=0) break; /* not the same! */
            /* now check whether it's really a keyword */
            if (!(strcmp(xkeyword,AUTHOR)==0 || strcmp(xkeyword,DATE)    ==0 ||
                  strcmp(xkeyword,HEADER)==0 || strcmp(xkeyword,LOCKER)  ==0 ||
                  strcmp(xkeyword,LOG)   ==0 || strcmp(xkeyword,REVISION)==0 ||
                  strcmp(xkeyword,SOURCE)==0 || strcmp(xkeyword,STATE)   ==0 )) {
                /* it's not a keyword, so continue normally */
                continue;
            } else {
                /* it's a keyword, so skip value */
                while (xc!=KDELIM && xc!='\n' && xc!=EOF) xc=getc(xfp);
                while (uxc!=KDELIM && uxc!='\n' && uxc!=EOF) uxc=getc(uxfp);
                if (xc==uxc && xc==KDELIM) {
                    xc=getc(xfp); uxc=getc(uxfp); /* skip KDELIM */
                    /* if the keyword is LOG, also skip the log message in xfp*/
                    if (strcmp(xkeyword,LOG)==0) {
                        /* first, compute the number of line feeds in log msg */
                        int lncnt;
                        lncnt=2; tp=delta->log;
                        while(*tp) if(*tp++=='\n') lncnt++;
                        while(xc!=EOF) {
                            if (xc=='\n')
                                if(--lncnt==0) break;
                            xc=getc(xfp);
                        }
                        /* skip last comment leader */
                        for (lncnt=strlen(Comment); lncnt>=0; lncnt--) xc=getc(xfp);
                    }
                }
            }
        }
    }
    fclose(xfp);fclose(uxfp);
    return result;
}



#ifdef FCMPTEST
cleanup(){} /* dummy */

char * Comment;

main(argc, argv)
int  argc; char  *argv[];
/* first argument: comment leader; 2nd: log message, 3rd: expanded file,
 * 4th: unexpanded file
 */
{       struct hshentry delta;

        cmdid="rcsfcmp";
        Comment=argv[1];
        delta.log=argv[2];
        if (rcsfcmp(argv[3],argv[4],&delta))
                printf("files are the same\n");
        else    printf("files are different\n");
}
#endif

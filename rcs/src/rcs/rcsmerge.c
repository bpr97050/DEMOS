/*
 *                       rcsmerge operation
 */
 static char rcsid[]=
 "$Header: /usr/users/korotay/DEMOS_2.2/rcs/src/rcs/RCS/rcsmerge.c,v 1.3 1986/02/23 22:20:33 avg Exp $";
/*****************************************************************************
 *                       join 2 revisions with respect to a third
 *****************************************************************************
 */

/* $Log: rcsmerge.c,v $
 * Revision 1.3  1986/02/23  22:20:33  avg
 * Сделаны изменения для МНОС и для работы с U-кодом.
 *
 * Revision 1.1  85/12/26  22:12:55  fox
 * Система хранения версий файлов
 * 
 */

#include "rcsbase.h"

extern int  cleanup();              /* cleanup after signals                */
extern char * mktempfile();         /*temporary file name generator         */
extern struct hshentry * genrevs(); /*generate delta numbers                */
extern int  nerror;                 /*counter for errors                    */

char *RCSfilename;
char *workfilename;
char * temp1file, * temp2file;

main (argc, argv)
int argc; char **argv;
{
        char * cmdusage;
        char command[NCPPN+revlength+40];
        int  revnums; /* counter for revision numbers given */
        int tostdout;
        char * rev1, * rev2; /*revision numbers*/
        char numericrev[revlength];   /* holds expanded revision number     */
        struct hshentry * gendeltas[hshsize];/*stores deltas to be generated*/
        struct hshentry * target;

        catchints();
        cmdid = "rcsmerge";
        cmdusage = "command format:\n    rcsmerge -p -rrev1 -rrev2 file\n    rcsmerge -p -rrev1 file";
        revnums=0;tostdout=false;

        while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
                switch ((*argv)[1]) {
                case 'p':
                        tostdout=true;
                        /* falls into -r */
                case 'r':
                        if ((*argv)[2]!='\0') {
                            if (revnums==0) {
                                    rev1= *argv+2; revnums=1;
                            } elif (revnums==1) {
                                    rev2= *argv+2; revnums=2;
                            } else {
                                    faterror("too many revision numbers");
                            }
                        } /* do nothing for empty -r or -p */
                        break;

                default:
                        faterror("unknown option: %s\n%s", *argv,cmdusage);
                };
        } /* end of option processing */

        if (argc<1) faterror("No input file\n%s",cmdusage);
        if (revnums<1) faterror("no base revision number given");

        /* now handle all filenames */

        if (pairfilenames(argc,argv,true,false)==1) {

                if (argc>2 || (argc==2&&argv[1]!=nil))
                        warn("too many arguments");
                diagnose("RCS file: %s",RCSfilename);
                if (!(access(workfilename,4)==0))
                        faterror("Can't open %s",workfilename);

                if (!trysema(RCSfilename,false)) goto end; /* give up */

                gettree();  /* reads in the delta tree */

                if (Head==nil) faterror("no revisions present");


                if (!expandsym(rev1,numericrev)) goto end;
                if (!(target=genrevs(numericrev,nil,nil,nil,gendeltas))) goto end;
                rev1=target->num;
                if (revnums==1) rev2=Head->num; /* default for rev2 */
                if (!expandsym(rev2,numericrev)) goto end;
                if (!(target=genrevs(numericrev,nil,nil,nil,gendeltas))) goto end;
                rev2=target->num;

                temp1file=mktempfile("/tmp/",TMPFILE1);
                temp2file=mktempfile("/tmp/",TMPFILE2);

                diagnose("retrieving revision %s",rev1);
                sprintf(command,"%s/co -q -p%s %s > %s\n",
                        TARGETDIR,rev1,RCSfilename,temp1file);
                if (system(command)){
                        faterror("co failed");
                }
                diagnose("retrieving revision %s",rev2);
                sprintf(command,"%s/co -q -p%s %s > %s\n",
                         TARGETDIR,rev2,RCSfilename,temp2file);
                if (system(command)){
                        faterror("co failed");
                }
                diagnose("Merging differences between %s and %s into %s%s",
                         rev1, rev2, workfilename,
                         tostdout?"; result to stdout":"");

                sprintf(command,"%s %s%s %s %s %s %s\n",MERGE,tostdout?"-p ":"",
                        workfilename,temp1file,temp2file,workfilename,rev2);
                if (system(command)) {
                        faterror("merge failed");
                }
        }

end:
        cleanup();
        exit(nerror!=0);

}


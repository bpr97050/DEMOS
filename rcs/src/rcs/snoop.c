/*
 *         Запись протокола использования команд co и ci
 */
 static char rcsid[]=
 "$Header: /usr/users/korotay/DEMOS_2.2/rcs/src/rcs/RCS/snoop.c,v 1.1 1985/12/26 22:29:14 fox Rel $ KIAE";
/********************************************************************
 * Программа дописывает информацию заданную argv[1] в файл SNOOPFILE.
 * Для борьбы с коллизиями создается файл блокировки.  Имя файла для
 * записи протокола (SNOOPFILE)  должно  задаваться при компиляции в
 * команде cc. Если после MAXTRIES попыток, файл блокировки так и не
 * удалось уничтожить, выдается сообщение об ошибке.
 ********************************************************************
 */

/* $Log: snoop.c,v $
 * Revision 1.1  1985/12/26  22:29:14  fox
 * Система хранения версий файлов
 *
 */

#include "rcsbase.h"
/* отменяет переопределение putc в rcsbase.h */
#define fflsbuf _flsbuf

char  lockfname[NCPPN];
FILE * logfile;
int lockfile;

#define MAXTRIES 20

main(argc,argv)
int argc; char * argv[];

/* Пишет argv[1] в SNOOPFILE и добавляет "\n".
 *
 * Обращение:
 *
 *      rcslog logmessage
 */
{       int tries;
        register char * lastslash, *sp;

        strcpy(lockfname,SNOOPFILE);
        lastslash = sp = lockfname;
	while (*sp) if (*sp++ =='/') lastslash=sp;
        strcpy(lastslash,",lockfile");
        tries=0;
	while (((lockfile=creat(lockfname, 000))==-1) && (tries<=MAXTRIES)){
                tries++;
                sleep(5);
        }
        if (tries<=MAXTRIES) {
                close(lockfile);
                if ((logfile=fopen(SNOOPFILE,"a")) ==NULL) {
                        fprintf(stderr,"Can't open logfile %s\n",SNOOPFILE);
                } else {
                        fputs(argv[1],logfile);
                        putc('\n',logfile);
                        fclose(logfile);
                }
                unlink(lockfname);
        } else {
                fprintf(stderr,"RCS logfile %s seems permanently locked.\n",SNOOPFILE);
                fprintf(stderr,"Please alert system administrator\n");
        }
}

/*
**      Удаление таблицы символов об'ектного файла.
**
**      strip файл ...
**
**      Стандартное местоположение в системе:  /bin
**
**      Метод сборки:  cc -O -n -s
**
**      $Header$
**      $Log$
**
*/

static  char Rcs_id[] = "$Header$";

#ifdef sparc
#include "/home/mellorn/zaitcev/d22/include/a.out.h"
#else
#include <a.out.h>
#endif
#include <signal.h>

/* Manage values in PDP-11 byte order */
#ifdef sparc
#define GET16(v)   ((((v)&0377)<<8)+((v)>>8&0377))
#define PUT16(v)   ((((v)&0377)<<8)+((v)>>8&0377))
#else
#define GET16(v)   (v)
#define PUT16(v)   (v)
#endif

char   *tname;
char   *mktemp ();
struct exec head;
int     a_magic[] = {
    A_MAGIC1, A_MAGIC2, A_MAGIC3, A_MAGIC4, 0
};
int     status;
int     tf;

main (argc, argv)
char   *argv[];
{
    register    i;

    signal (SIGHUP, SIG_IGN);
    signal (SIGINT, SIG_IGN);
    signal (SIGQUIT, SIG_IGN);
    tname = mktemp ("/tmp/sXXXXX");
    close (creat (tname, 0600));
    tf = open (tname, 2);
    if (tf < 0) {
        printf ("cannot create temp file\n");
        exit (2);
    }
    for (i = 1; i < argc; i++) {
        strip (argv[i]);
        if (status > 1)
            break;
    }
    close (tf);
    unlink (tname);
    exit (status);
}

strip (name)
char   *name;
{
    register    f;
    long    size;
    int     i;

    f = open (name, 0);
    if (f < 0) {
        printf ("cannot open %s\n", name);
        status = 1;
        goto out;
    }
    read (f, (char *) & head, sizeof (head));
#ifdef sparc
    head.a_magic = GET16(head.a_magic);
    head.a_text = GET16(head.a_text);
    head.a_data = GET16(head.a_data);
    head.a_bss = GET16(head.a_bss);
    head.a_syms = GET16(head.a_syms);
    head.a_entry = GET16(head.a_entry);
    head.a_flag = GET16(head.a_flag);
#endif
    for (i = 0; a_magic[i]; i++)
        if (a_magic[i] == head.a_magic)
            break;
    if (a_magic[i] == 0) {
        printf ("%s not in a.out format\n", name);
        status = 1;
        goto out;
    }
    if (head.a_syms == 0 && (head.a_flag & 1) != 0) {
        printf ("%s already stripped\n", name);
        goto out;
    }
    size = (long) head.a_text + head.a_data;
    head.a_syms = 0;
    head.a_flag |= 1;

#ifdef sparc
    head.a_magic = PUT16(head.a_magic);
    head.a_text = PUT16(head.a_text);
    head.a_data = PUT16(head.a_data);
    head.a_bss = PUT16(head.a_bss);
    head.a_syms = PUT16(head.a_syms);
    head.a_entry = PUT16(head.a_entry);
    head.a_flag = PUT16(head.a_flag);
#endif
    lseek (tf, (long) 0, 0);
    write (tf, (char *) & head, sizeof (head));
    if (copy (name, f, tf, size)) {
        status = 1;
        goto out;
    }
    size += sizeof (head);
    close (f);
    f = creat (name, 0666);
    if (f < 0) {
        printf ("%s cannot recreate\n", name);
        status = 1;
        goto out;
    }
    lseek (tf, (long) 0, 0);
    if (copy (name, tf, f, size))
        status = 2;

out: 
    close (f);
}

copy (name, fr, to, size)
char   *name;
long    size;
{
    register    s,
                n;
    char    buf[512];

    while (size != 0) {
        s = 512;
        if (size < 512)
            s = size;
        n = read (fr, buf, s);
        if (n != s) {
            printf ("%s unexpected eof\n", name);
            return (1);
        }
        n = write (to, buf, s);
        if (n != s) {
            printf ("%s unexpected write eof\n", name);
            return (1);
        }
        size -= s;
    }
    return (0);
}

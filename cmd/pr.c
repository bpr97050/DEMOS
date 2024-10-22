/*
**      Печать файлов.
**
**      pr [ ключи ] [ файл ... ]
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

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

/* Making putcp a macro sped things up by 14%. */
#define putcp(c)  if (page >= fpage) putchar(c)
#define SYEF 0271
#define SYEB 0272

int     ncol = 1;
char   *header;
int     col;
int     icol;
FILE * file;
char   *bufp;
#define BUFS    6720
char    buffer[BUFS];           /* for multi-column output */
char    obuf[BUFSIZ];
#define FF      014
int     line;
char   *colp[72];
int     coln[72];
int     nofile;
char    isclosed[10];
FILE * ifile[10];
char  **lastarg;
int     peekc;
int     fpage;
int     page;
int     colw;
int     nspace;
int     width = 132;
int     length = 66;
int     plength = 61;
int     margin = 10;
int     ntflg,
        numflg,
        num0,
        iflg;
int     fflg;
int     mflg;
int     tabc;
char   *tty;
int     mode;
char   *ttyname ();
char   *ctime ();

main (argc, argv)
char  **argv;
{
    int     nfdone;
    int     onintr ();

    setbuf (stdout, obuf);
    if (signal (SIGINT, SIG_IGN) != SIG_IGN)
        signal (SIGINT, onintr);
    lastarg = &argv[argc - 1];
    fixtty ();
    for (nfdone = 0; argc > 1; argc--) {
        argv++;
        if (**argv == '-') {
            switch (*++*argv) {
                case 'h': 
                    if (argc >= 2) {
                        header = *++argv;
                        argc--;
                    }
                    continue;

                case 't': 
                    ntflg++;
                    continue;

                case 'f': 
                    fflg++;
                    plength = 60;
                    continue;

                case 'l': 
                    length = atoi (++*argv);
                    continue;

                case 'w': 
                    width = atoi (++*argv);
                    continue;

                case 's': 
                    if (*++*argv)
                        tabc = **argv;
                    else
                        tabc = '\t';
                    continue;

                case 'm': 
                    mflg++;
                    continue;
                case 'n': 
                    numflg++;
                    continue;
                case 'i': 
                    numflg++;
                    iflg++;
                    if (*++*argv)
                        num0 = atoi (*argv);
                    else
                        num0 = 1;
                    continue;
                default: 
                    ncol = atoi (*argv);
                    continue;
            }
        }
        else
            if (**argv == '+') {
                fpage = atoi (++*argv);
            }
            else {
                if (!iflg)
                    num0 = 1;
                print (*argv, argv);
                nfdone++;
                if (mflg)
                    break;
            }
    }
    if (nfdone == 0)
        print ((char *) 0, (char **) 0);
    done ();
}

done () {

    if (tty)
        chmod (tty, mode);
    exit (0);
}

onintr () {

    if (tty)
        chmod (tty, mode);
    _exit (1);
}

fixtty () {
    struct stat sbuf;

    tty = ttyname (1);
    if (tty == 0)
        return;
    stat (tty, &sbuf);
    mode = sbuf.st_mode & 0777;
    chmod (tty, 0600);
}

print (fp, argp)
char   *fp;
char  **argp;
{
    extern char *sprintf ();
    struct stat sbuf;
    register    sncol;
    register char  *sheader;
    register char  *cbuf;
    char    linebuf[150],
           *cp;

    if (ntflg)
        margin = 0;
    else
        margin = 10;
    if (length <= margin)
        length = 66;
    if (width <= 0)
        width = 132;
    if (ncol > 72 || ncol > width) {
        fprintf (stderr, "pr: No room for columns.\n");
        done ();
    }
    if (mflg) {
        mopen (argp);
        ncol = nofile;
    }
    colw = width / (ncol == 0 ? 1 : ncol);
    sncol = ncol;
    sheader = header;
    plength = length - 5;
    if (ntflg)
        plength = length;
    if (--ncol < 0)
        ncol = 0;
    if (mflg)
        fp = 0;
    if (fp) {
        if ((file = fopen (fp, "r")) == NULL) {
            if (tty == NULL)
                fprintf (stderr, "pr: can't open %s\n", fp);
            ncol = sncol;
            header = sheader;
            return;
        }
        stat (fp, &sbuf);
    }
    else {
        file = stdin;
        time (&sbuf.st_mtime);
    }
    if (header == 0)
        header = fp ? fp : "";
    cbuf = ctime (&sbuf.st_mtime);
    cbuf[16] = '\0';
    cbuf[24] = '\0';
    page = 1;
    icol = 0;
    colp[ncol] = bufp = buffer;
    coln[ncol] = num0;
    if (mflg == 0)
        nexbuf ();
    while (mflg && nofile || (!mflg) && tpgetc (ncol) > 0) {
        if (mflg == 0) {
            colp[ncol]--;
            if (colp[ncol] < buffer)
                colp[ncol] = &buffer[BUFS];
        }
        line = 0;
        if (ntflg == 0) {
            if (fflg) {
            /* Assume a ff takes two blank lines at the top of
               the page. */
                line = 2;
                sprintf (linebuf, "%s %s  %s Page %d Line %d\n\n\n",
                        cbuf + 4, cbuf + 20, header, page, num0);
            }
            else
                sprintf (linebuf, "\n\n%s %s  %s Page %d Line %d\n\n\n",
                        cbuf + 4, cbuf + 20, header, page, num0);
            for (cp = linebuf; *cp;)
                put (*cp++);
        }
        putpage ();
        if (ntflg == 0) {
            if (fflg)
                put ('\f');
            else
                while (line < length)
                    put ('\n');
        }
        page++;
    }
    fclose (file);
    ncol = sncol;
    header = sheader;
}

mopen (ap)
char  **ap;
{
    register char **p,
                   *p1;

    p = ap;
    while ((p1 = *p) && p++ <= lastarg) {
        if ((ifile[nofile] = fopen (p1, "r")) == NULL) {
            isclosed[nofile] = 1;
            nofile--;
        }
        else {
            isclosed[nofile] = 0;
            coln[nofile] = num0;
        }
        if (++nofile >= 10) {
            fprintf (stderr, "pr: Too many args\n");
            done ();
        }
    }
}

putpage () {
    register int    lastcol,
                    i,
                    c;
    int     j;

    if (ncol == 0) {
        while (line < plength) {
            if (c = tpgetc (0)) {
                if (numflg)
                    putd (num0);
                num0++;
            }
            while ((c = (c ? c : (c = tpgetc (0))))
&& c != '\n' && c != FF) {
                putcp (c);
                c = 0;
            }
            putcp ('\n');
            line++;
            if (c == FF) {
                num0--;
                break;
            }
        }
        return;
    }
    colp[0] = colp[ncol];
    if (!mflg)
        coln[0] = coln[ncol];
    if (mflg == 0)
        for (i = 1; i <= ncol; i++) {
            colp[i] = colp[i - 1];
            coln[i] = coln[i - 1];
            for (j = margin; j < length; j++) {
                while ((c = tpgetc (i)) != '\n')
                    if (c == 0)
                        break;
                coln[i]++;
            }
        }
    while (line < plength) {
        lastcol = colw;
        for (i = 0; i < ncol; i++) {
            if (peekc || (peekc = tpgetc (i))) {
                if (numflg)
                    putd (coln[i]);
                num0 = ++coln[i];
            }
            while ((c = pgetc (i)) && c != '\n')
                if (col < (lastcol - 1) || tabc != 0)
                    put (c);
            if (c == 0)
                continue;
            if (tabc)
                put (tabc);
            else
                while (col < lastcol)
                    put (' ');
            lastcol += colw;
        }
        if (peekc || (peekc = tpgetc (i))) {
            if (numflg)
                putd (coln[i]);
            num0 = ++coln[i];
        }
        while ((c = pgetc (ncol)) && c != '\n')
            if ((col < lastcol) || tabc != 0)
                put (c);
        put ('\n');
    }
}

nexbuf () {
    register int    n;
    register char  *rbufp;

    rbufp = bufp;
    n = &buffer[BUFS] - rbufp;
    if (n > 512)
        n = 512;
    if ((n = fread (rbufp, 1, n, file)) <= 0) {
        fclose (file);
        *rbufp = SYEF;
    }
    else {
        rbufp += n;
        if (rbufp >= &buffer[BUFS])
            rbufp = buffer;
        *rbufp = SYEB;
    }
    bufp = rbufp;
}

tpgetc (ai) {
    register char **p;
    register int    c,
                    i;

    i = ai;
    if (mflg) {
        if ((c = getc (ifile[i])) == EOF) {
            if (isclosed[i] == 0) {
                isclosed[i] = 1;
                if (--nofile <= 0)
                    return (0);
            }
            return ('\n');
        }
        if (c == FF && ncol > 0)
            c = '\n';
        return (c);
    }
loop: 
    c = **(p = &colp[i]) & 0377;
    if (c == SYEB) {
        nexbuf ();
        c = **p & 0377;
    }
    if (c == SYEF)
        return (0);
    (*p)++;
    if (*p >= &buffer[BUFS])
        *p = buffer;
    if (c == 0)
        goto loop;
    return (c);
}

pgetc (i) {
    register int    c;

    if (peekc) {
        c = peekc;
        peekc = 0;
    }
    else
        c = tpgetc (i);
    if (tabc)
        return (c);
    switch (c) {

        case '\t': 
            icol++;
            if ((icol & 07) != 0)
                peekc = '\t';
            return (' ');

        case '\n': 
            icol = 0;
            break;

        case 010: 
        case 033: 
            icol--;
            break;
    }
    if (c >= ' ')
        icol++;
    return (c);
}
put (ac) {
    register int    ns,
                    c;

    c = ac;
    if (tabc) {
        putcp (c);
        if (c == '\n')
            line++;
        return;
    }
    switch (c) {

        case ' ': 
            nspace++;
            col++;
            return;

        case '\n': 
            col = 0;
            nspace = 0;
            line++;
            break;

        case 010: 
        case 033: 
            if (--col < 0)
                col = 0;
            if (--nspace < 0)
                nspace = 0;

    }
    while (nspace) {
        if (nspace > 2 && col > (ns = ((col - nspace) | 07))) {
            nspace = col - ns - 1;
            putcp ('\t');
        }
        else {
            nspace--;
            putcp (' ');
        }
    }
    if (c >= ' ')
        col++;
    putcp (c);
}

/* This routine convert int id to ascii number consisted of 5 char's */
char    putdb[5];
putd (id)
int     id;
{
    register int    j,
                    i = id;
    for (j = 4; j >= 0; j--) {
        if (i) {
            putdb[j] = i % 10 + '0';
            i = i / 10;
        }
        else
            putdb[j] = ' ';
    }
    if (ncol != 0) {
        for (j = 0; j < 5; j++)
            put (putdb[j]);
        put (' ');
    }
    else {
        putcp (' ');
        for (j = 0; j < 5; j++)
            putcp (putdb[j]);
        putcp (' ');
        putcp (' ');
    }
}

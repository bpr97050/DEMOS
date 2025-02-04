# include	"curses.ext"

/*
 * Смещение окна (scrolls).
 */
scroll (win)
reg WINDOW * win; {

    reg char   *sp;
    reg int i;
    reg char   *temp;

    if (!win -> _scroll)
	return ERR;
    temp = win -> _y[0];
    for (i = 1; i < win -> _maxy; i++)
	win -> _y[i - 1] = win -> _y[i];
    for (sp = temp; sp < &temp[win -> _maxx];)
	*sp++ = ' ';
    win -> _y[win -> _maxy - 1] = temp;
    win -> _cury--;
    if (win == curscr) {
	if (NL)
	    _puts (NL);
	else
	    putchar ('\n');
	if (!NONL)
	    win -> _curx = 0;
# ifdef DEBUG
	fprintf (outf,
		"SCROLL: win == curscr\n");
# endif
    }
# ifdef DEBUG
    else
	fprintf (outf,
		"SCROLL: win [0%o] != curscr [0%o]\n",
		win, curscr);
# endif
    touchwin (win);
    return OK;
}

/*
 * Проецирование окна "win" на экран терминала.
 */

# include	"curses.ext"
# include       <ctype.h>

# ifdef DEBUG
# define	STATIC
# else
# define	STATIC	static
# endif

STATIC short    ly,
                lx;

STATIC bool curwin;

WINDOW * _win = NULL;

wrefresh (win)
reg WINDOW * win;
{
    reg short   wy;
    reg int retval;

 /* 
  * Восстановление режима адресации курсора.
  */
    if (_endwin) {
	_puts (VS);
	_puts (TI);
	_endwin = FALSE;
    }

    ly = curscr -> _cury;
    lx = curscr -> _curx;
    wy = 0;
    _win = win;
    curwin = (win == curscr);

    if (win -> _clear || curscr -> _clear || curwin) {
	if ((win -> _flags & _FULLWIN) || curscr -> _clear) {
	    _puts (CL);
	    ly = 0;
	    lx = 0;
	    if (!curwin) {
		curscr -> _clear = FALSE;
		curscr -> _cury = 0;
		curscr -> _curx = 0;
		werase (curscr);
	    }
	    touchwin (win);
	}
	win -> _clear = FALSE;
    }
/*
    if (!CA) {
	if (win -> _curx != 0)
	    putchar ('\n');
	if (!curwin)
	    werase (curscr);
    }
*/
# ifdef DEBUG
    fprintf (outf,
	    "REFRESH(%0.2o): curwin = %d\n",
	    win, curwin);
    fprintf (outf,
	    "REFRESH:\n\tfirstch\tlastch\n");
# endif
    for (wy = 0; wy < win -> _maxy; wy++) {
# ifdef DEBUG
	fprintf (outf,
		"%d\t%d\t%d\n",
		wy, win -> _firstch[wy], win -> _lastch[wy]);
# endif
	if (win -> _firstch[wy] != _NOCHANGE)
	    if (makech (win, wy) == ERR)
		return ERR;
	    else
		win -> _firstch[wy] = _NOCHANGE;
    }
    if (win == curscr)
/*----------------------------------------------------------*\
 *      domvcur (ly, lx, win -> _cury, win -> _curx);       *
\*----------------------------------------------------------*/
	mvcur (ly, lx, win -> _cury, win -> _curx);
    else
	if (win -> _leave) {
	    curscr -> _cury = ly;
	    curscr -> _curx = lx;
	    ly -= win -> _begy;
	    lx -= win -> _begx;
	    if (ly >= 0 && ly < win -> _maxy
		    && lx >= 0 && lx < win -> _maxx) {
		win -> _cury = ly;
		win -> _curx = lx;
	    }
	    else
		win -> _cury = win -> _curx = 0;
	}
	else {
/*----------------------------------------------------------*\
 *          domvcur (ly, lx, win -> _cury + win -> _begy,   *
 *                  win -> _curx + win -> _begx);           *
\*----------------------------------------------------------*/
	    mvcur (ly, lx, win -> _cury + win -> _begy,
		    win -> _curx + win -> _begx);
	    curscr -> _cury = win -> _cury + win -> _begy;
	    curscr -> _curx = win -> _curx + win -> _begx;
	}
    retval = OK;
    _win = NULL;
    fflush (stdout);
    return retval;
}

/*
 * Изменение очередной строки на экране.
 */
STATIC
makech (win, wy)
reg WINDOW * win;
short   wy;
{
    reg char   *nsp,
               *csp,
               *ce;
    reg short   wx,
                lch,
                y;
    reg int nlsp,
            clsp;		/* последняя строка */

    wx = win -> _firstch[wy];
    y = wy + win -> _begy;
    lch = win -> _lastch[wy];
    if (curwin)
	csp = " ";
    else
	csp =
	    &curscr ->
	    _y[wy + win -> _begy][wx + win -> _begx];
    nsp = &win -> _y[wy][wx];
    if (CE && !curwin) {
	for (ce = &win -> _y[wy][win -> _maxx - 1];
		*ce == ' '; ce--)
	    if (ce <= win -> _y[wy])
		break;
	nlsp = ce - win -> _y[wy];
    }
    if (!curwin)
	ce = CE;
    else
	ce = NULL;
    while (wx <= lch) {
	if (*nsp != *csp) {
/*----------------------------------------------------------*\
 *          domvcur (ly, lx, y, wx + win -> _begx);         *
\*----------------------------------------------------------*/
	    mvcur (ly, lx, y, wx + win -> _begx);
# ifdef DEBUG
	    fprintf (outf,
		    "MAKECH: 1: wx = %d, lx = %d\n", wx, lx);
# endif	
	    ly = y;
	    lx = wx + win -> _begx;
	    while (*nsp != *csp && wx <= lch) {
		if (ce != NULL && wx >= nlsp && *nsp == ' ') {
		/* 
		 * Стирание до конца строки
		 */
		    ce = &curscr -> _y[ly][COLS - 1];
		    while (*ce == ' ')
			if (ce-- <= csp)
			    break;
		    clsp =
			ce - curscr -> _y[ly] - win -> _begx;
# ifdef DEBUG
		    fprintf (outf,
			    "MAKECH: clsp = %d, nlsp = %d\n",
			    clsp, nlsp);
# endif
		    if (clsp - nlsp >= strlen (CE)
			    && clsp < win -> _maxx) {
# ifdef DEBUG
			fprintf (outf,
				"MAKECH: using CE\n");
# endif
			_puts (CE);
			lx = wx + win -> _begx;
			while (wx++ <= clsp)
			    *csp++ = ' ';
			goto ret;
		    }
		    ce = NULL;
		}
	    /* 
	     * Установка/отмена режима выделения.
	     */
/*----------------------------------------------------------*\
 *              if (SO && (*nsp & _STANDOUT)                *
 *                      != (curscr -> _flags & _STANDOUT)) {*
 *                  if (*nsp & _STANDOUT) {                 *
 *                      _puts (SO);                         *
 *                      curscr -> _flags |= _STANDOUT;      *
 *                  }                                       *
 *                  else {                                  *
 *                      _puts (SE);                         *
 *                      curscr -> _flags &= ~_STANDOUT;     *
 *                  }                                       *
 *              }                                           *
\*----------------------------------------------------------*/
		wx++;
		if (wx >= win -> _maxx
			&& wy == win -> _maxy - 1)
		    if (win -> _scroll) {
/*------------------------------------------------------------*\
 *                      if ((curscr -> _flags & _STANDOUT) && *
 *                              (win -> _flags & _ENDLINE))   *
 *                          if (!MS) {                        *
 *                              _puts (SE);                   *
 *                              curscr -> _flags              *
 *                                  &= ~_STANDOUT;            *
 *                          }                                 *
\*------------------------------------------------------------*/
			if (!curwin)
			    putchar ((*csp = *nsp));
			else
			    putchar (*nsp);
			scroll (win);
			if (win -> _flags & _FULLWIN
				&& !curwin)
			    scroll (curscr);
			ly = win -> _begy + win -> _cury;
			lx = win -> _begx + win -> _curx;
			return OK;
		    }
		    else
			if (win -> _flags & _SCROLLWIN) {
			    lx = --wx;
			    return ERR;
			}
		if (!curwin)
		    putchar ((*csp++ = *nsp));
		else
		    putchar (*nsp);
/*------------------------------------------------------------*\
 *              if (UC && (*nsp & _STANDOUT)) {               *
 *                  if (BC)                                   *
 *                      _puts (BC);                           *
 *                  else                                      *
 *                      putchar ('\b');                       *
 *                  _puts (UC);                               *
 *              }                                             *
\*------------------------------------------------------------*/
		nsp++;
	    }
# ifdef DEBUG
	    fprintf (outf,
		    "MAKECH: 2: wx = %d, lx = %d\n",
		    wx, lx);
# endif	
	    if (lx == wx + win -> _begx)
				/* не было изменений */
		break;
	    lx = wx + win -> _begx;
	}
	else
	    if (wx < lch)
		while (*nsp == *csp) {
		    nsp++;
		    if (!curwin)
			csp++;
		    ++wx;
		}
	    else
		break;
# ifdef DEBUG
	fprintf (outf,
		"MAKECH: 3: wx = %d, lx = %d\n",
		wx, lx);
# endif	
    }
ret: 
    return OK;
}

/*----------------------------------------------------------*\
 * static                                                   *
 *         domvcur (oy, ox, ny, nx)                         *
 * int     oy,                                              *
 *         ox,                                              *
 *         ny,                                              *
 *         nx; {                                            *
 *                                                          *
 *     if (curscr -> _flags & _STANDOUT && !MS) {           *
 *         _puts (SE);                                      *
 *         curscr -> _flags &= ~_STANDOUT;                  *
 *     }                                                    *
 *     mvcur (oy, ox, ny, nx);                              *
 * }                                                        *
\*----------------------------------------------------------*/

# include	"curses.ext"

/*
 *   Построение рамки по границам окна  "win".
 * Для  построения вертикальных сторон исполь-
 * зуется символ "vert", для горизонтальных  -
 * "hor".
 */

box (win, vert, hor)
reg WINDOW * win;
char    vert,
        hor; {

    reg int i;
    reg int endy,
            endx;
    reg char   *fp,
               *lp;

    endx = win -> _maxx;
    endy = win -> _maxy - 1;
    fp = win -> _y[0];
    lp = win -> _y[endy];
    for (i = 0; i < endx; i++)
	fp[i] = lp[i] = hor;
    endx--;
    for (i = 0; i <= endy; i++)
	win -> _y[i][0] = (win -> _y[i][endx] = vert);
    if (!win -> _scroll && (win -> _flags & _SCROLLWIN))
	fp[0] = fp[endx] = lp[0] = lp[endx] = ' ';
    touchwin (win);
}

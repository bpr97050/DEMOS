/*
 * Нормальное завершение работы с экраном.
 */

# include	"curses.ext"

endwin () {
    resetty ();
    _puts (VE);
    _puts (TE);
    if (curscr) {
/*---------------------------------------------*\
 *      if (curscr -> _flags & _STANDOUT) {    *
 *          _puts (SE);                        *
 *          curscr -> _flags &= ~_STANDOUT;    *
 *      }                                      *
\*---------------------------------------------*/
	_endwin = TRUE;
    }
}

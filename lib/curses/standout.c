/*
 * Включение/выключение режима выделения символов
 */

# include	"curses.ext"

char   *
        wstandout (win)
        reg WINDOW * win;
{
/*--------------------------------*\
 *  if (!SO && !UC)               *
\*--------------------------------*/
	return FALSE;

/*--------------------------------*\
 *  win -> _flags |= _STANDOUT;   *
 *  return (SO ? SO : UC);        *
\*--------------------------------*/
}

char   *
        wstandend (win)
        reg WINDOW * win;
{
/*--------------------------------*\
 *  if (!SO && !UC)               *
\*--------------------------------*/
	return FALSE;

/*--------------------------------*\
 *  win -> _flags &= ~_STANDOUT;  *
 *  return (SE ? SE : UC);        *
\*--------------------------------*/
}

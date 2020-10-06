# include	"monitor.h"
# include	<defines.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)newdirec.c	7.1	2/5/81)



/*
**  CHANGE WORKING DIRECTORY
*/

newdirec()
{
	register char	*direc;
	extern char	*getfilenm();

	direc = getfilenm(0);
	if (chdir(direc))
		printf("Cannot access directory \"%s\"\n", direc);
}

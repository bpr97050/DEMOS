# include <defines.h>
# include <access.h>
# include <aux.h>
# include <sccs.h>

SCCSID(@(#)get_reltup.c	7.1	2/5/81)

/*
**  GET_RELTUP -- get appropriate tuple from relation catalog
**
**	Get the tuple for the relation specified by 'name'
**	and put it in the descriptor 'dx'.
**
**	First a relation named 'name' owned
**	by the current user is searched for. If that fails,
**	then a relation owned by the dba is searched for.
*/

get_reltup(d, name)
DESC *d;
char *name;
{
	int i;
	i = get_rtupl(&d->reldum,name,&d->reltid.s_tupid) ;
	return(i);
}
/*
 * Функция общаа для get_reltup и ser_next (serial.c)
 */
get_rtupl(prel, name,ptid)
register struct relation *prel;
char		*name;
TID             *ptid;
{
	struct relation	rel;
	register int	i;

	clearkeys(&Admin.adreld);

	/* make believe relation relation is read only for concurrency */
	Admin.adreld.relopn = abs(Admin.adreld.relopn);

	/* relation relation is open. Search for relation 'name' */
	setkey(&Admin.adreld, (char *) &rel, name, RELID);
	setkey(&Admin.adreld, (char *) &rel, Usercode, RELOWNER);

	if ((i = getequal(&Admin.adreld, (char *) &rel, (char *)prel, ptid)) == 1)
	{
		/* not a user relation. try relation owner by dba */
		setkey(&Admin.adreld, (char *) &rel, Admin.adhdr.adowner, RELOWNER);
		i = getequal(&Admin.adreld, (char *) &rel, (char *)prel, ptid);
	}

	flush_rel(&Admin.adreld, TRUE);

#	ifdef xATR1
	if (tTf(21, 1))
		printf("get_reltup: %d\n", i);
#	endif

	/* restore relation relation to read/write mode */
	Admin.adreld.relopn = -Admin.adreld.relopn;
	return (i);
}

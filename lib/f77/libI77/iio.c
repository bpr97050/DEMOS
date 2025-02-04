/*
 * 21.11.86 Исправлена ошибка в дополнении записи пробелами до конца:
 * теперь "/", ":" работают как переход к следующей записи,
 * а "$" вообще блокирует дополнение строки пробелами до конца.
 * Руднев .
 */
#include "fio.h"
#include "fmt.h"
char *icptr,*icend;
icilist *svic;
extern int rd_ed(),rd_ned(),w_ed(),w_ned(),y_err();
extern int z_wnew(),z_wSL();
int icnum,icpos;
z_getc()
{
	if(icptr >= icend) err(svic->iciend,(EOF),"endfile");
	if(icpos++ < svic->icirlen)
		return(*icptr++);
	else	err(svic->icierr,110,"recend");
}
z_putc(c)
{
	if(icptr >= icend) err(svic->icierr,110,"inwrite");
	if(icpos++ < svic->icirlen)
		*icptr++ = c;
	else	err(svic->icierr,110,"recend");
	return(0);
}
z_rnew(sy)char sy;
{
	icptr = svic->iciunit + (++icnum)*svic->icirlen;
	icpos = 0;
}
s_rsfi(a) icilist *a;
{	int n;
	if(n=c_si(a)) return(n);
	reading=1;
	doed=rd_ed;
	doned=rd_ned;
	getn=z_getc;
	dorevert = donewrec = y_err;
	doend = z_rnew;
	return(0);
}
s_wsfi(a) icilist *a;
{	int n;
	if(n=c_si(a)) return(n);
	reading=0;
	doed=w_ed;
	doned=w_ned;
	putn=z_putc;
	dorevert = z_wnew;
	donewrec = z_wSL;
	doend = z_wnew;
	return(0);
}
c_si(a) register icilist *a;
{
	fmtbuf=a->icifmt;
	if(pars_f(fmtbuf)<0)
		err(a->icierr,100,"startint");
	fmt_bg();
	sequential=formatted=1;
	external=0;
	cblank=cplus=scale=0;
	svic=a;
	icnum=icpos=0;
	icptr=svic->iciunit;
	icend=icptr+svic->icirlen*svic->icirnum;
	return(0);
}
z_wnew(sy)char sy;
{
	if ( sy == '\n') {
		while(icpos++ < svic->icirlen)
			*icptr++ = ' ';
		icpos = 0;
		icnum++;
	}
	return(0);
}
z_wSL(sy) char sy;
{ 
	return( !z_wnew('\n'));
}
e_rsfi()
{	int n;
	n = en_fio();
	fmtbuf = NULL;
	return(n);
}
e_wsfi()
{
	int n;
	n = en_fio();
	fmtbuf = NULL;
/*
	while(icpos++ < svic->icirlen)
		*icptr++ = ' ';
 */
	return(n);
}
y_err()
{
	err(elist->cierr, 110, "iio");
}

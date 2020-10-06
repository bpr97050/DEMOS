/*
*  Это простой пример программы на EQUEL. Она работает с
*  отношением СЛУЖАЩИЕ в базе данных "demo".
*  После вызова программы введите фамилию служащего, и программа либо
*  выдаст его оклад, либо сообщение о том, что такого служащего нет.
*  Список служпщих можно получить, набрав "?" в ответ на запрос имени.
*
*  Для выхода введите CTRL/D.
*
*  Трансляция и запуск этой программы на счет осуществляются
*  командами:
*
*  equel equeldemo.q
*  cc    equeldemo.c -lq
*  a.out
*/
main(ac,av)
int ac;
##char *av[];
{
##	char	NAME[20];
##	int	SAL;
	char	flag;

	if ( ac != 2)
##      rubin demo
	else
##      rubin av[1]
##      range of e is служащие
	while (eread(NAME))
	{
		if(NAME[0] == '?')
		{
##                      retrieve (NAME=e.имя)
##			{
				printf("%s\n",NAME);
##			}
			continue;
		}
		flag = 0;
##              retrieve (SAL = e.оклад) where
##              e.имя = NAME
##		{
			printf("Тов. %s имеет оклад %d руб.\n",NAME,SAL);
			flag = 1;
##		}
	if(!flag) printf("Тов. %s неизвестен системе\n",NAME);
	}
##	exit
}

eread(p)
char	*p;
{
	int    c;
	printf("Введите имя служащего:");
	while((c = getchar()) > 0)
	{
		if(c == '\n')
		{
			*p = 0;
			return(1);
		}
		*p++ = c;
	}
	return(0);
}

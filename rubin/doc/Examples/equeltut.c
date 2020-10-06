/*
** Этот файл содержит программу-пример, написанную на Equel'е.
** Для того, чтобы понять, что делают примеры- выполните программу.
** Для компиляции и исполнения этого примера выполните следующие
** действия:
**
**              equel equeltut.q
**              cc equeltut.c -lq
**              a.out
**
** Первая команда вызывает препроцессор Equel, который вставляет
** вызовы функций, посылающих запросы RUBIN`у. Результат работы
** препроцессора в нашем случае помещается в файл "equeltut.c".
** Общая форма вызова препроцессора:
**
**              equel [-d] [-f] [-r] file1.q [file2.q ...]
**
** Результат помещается в файл "file1.c", и т.д. При указании
** ключа -d Equel оставляет в файле информацию о номерах строк
** для того, чтобы ошибки прогона могли быть связаны с
** соответствующим запросом.
**
** Для включения в программы файлов, содержащих операторы Equel'а и,
** возможно, об'явления, используется C-препроцессор ;
** имена этих файлов должны оканчиваться расширением ".q.h". После
** их обработки препроцессором equel эти файлы вместо расширения
** ".q.h" получат расширения ".c.h". Файлы с расширением ".c.h"
** будут указаны в операторах #include C-версий программ, преобразованных
** Equel'ом. Имена файлов, указанные в операторах #include Equel-программ,
** но не заканчивающиеся расширением ".q.h" будут игнорироваться equel'ом.
*/


/*
** Синтаксис Equel'а почти полностью совпадает с синтаксисом
** Quel'а, хотя между Equel и Quel также имеются и некоторые
** различия. При работе с Equel'ом возникают определенные тонкости
** во взаимодействии с конструкциями Quel и C.  Вот наиболее
** важные из них:
**
**      Об'явленные Equel'у C-переменные используются как переменные
** во всех операторах Equel, кроме того случая, когда они находятся
** внутри строк или если им предшествует оператор Equel'а  '#'.
** Особенно, будте осторожны с именами переменных, совпадающими с
** именами доменов.
**
**      Все строки, передаваемые из RUBIN'а C-переменным должны
** заканчиваться символом '\0'. Это делает их на один байт длиннее,
** чем они были в отношении. Т.о. символьный массив нужно об'являть
** на байт длиннее размерности домена, из которого в него будут
** поступать данные.
**
**      Операторы retrieve, в которых не задано результирующее
** отношение, в Equel интерпретируются иначе, чем в Quel. Различия
** будут продемонстрированы в следующих примерах.
*/




/*
** Для того, чтобы продемонстрировать сходства и различия между
** двумя способами работы с RUBIN'ом некоторые из запросов,
** приведенных в "Учебном введении в RUBIN", будут переписаны на
** Equel'е.
*/






/*
** Мы начинаем с об'явления некоторых прерменных, которые понадобятся
** нам для работы. Отметим, что эти переменные будут глобальными
** для Equel, т.е. их об'явления будут действительны во всем файле.
*/
# include <rubin/equel.h>

char pname[21];















char col[9];








main(argc, argv)
int     argc;
char*argv[];

{

	/*
	** Взаимодействие с RUBIN'ом мы начинаем с использования базы
	** данных demo.
	*/
	if ( argc != 2)
{IIrubin("-i210","demo",0);}
	else
{IIrubin("-i210",argv[1],0);}

	/*
	** При вызове RUBIN'а может быть указано до 9 аргументов.
	** В данном случае мы изменили формат вывода целых переменных.
	** Флаги должны быть взяты в кавычки для того, чтобы правильно
	** происходил разбор знаков '+'и '-'.
	*/



	/*
	** Tакже, как и в "Учебном введении", можно напечатать
	** отношение части:
	*/

{IIwrite("print части");IIsync(0);}

	/*
	** Отметим, что здесь все аналогично оператору Quel, за
	** исключением того, что строка начинается с символов "##",
	** сообщающих препроцессору Equel о том, что необходимо
	** перевести эту строку в стандартный C.
	*/

	/*
	** Следующая часть программы аналогична третьему запросу из
	** "Введения" [стр. 3]
	*/

{IIwrite("range of p=части");IIsync(0);}






	/*
	 ** Обратите внимание на то, что в запросе первое имя pname
	 ** относится к переменной "pname", в то время как #имя является
	 ** именем домена "имя" даже если бы существовала переменная Си "имя"
	 ** из-за наличия символа "#" - признака константы QUEL.
	 */
{IIwrite("retrieve(pname=p.имя)");IIsetup();while(IIn_get(0)){IIn_ret(pname,3);if(IIerrtest())continue;

		/*
		** Все находящиеся внутри фигурных скобок операторы,
		** выполняются для каждой получаемой выборки.
		*/

		printf("%s\n", pname);

		/*
		** pname- корректно закрытая C-строка. Equel
		** закрывает пустым символом ВСЕ передаваемые из
		** RUBIN'а строки. Строки будут на единицу длиннее
		** размерности атрибута. Предполагается, что
		** пользователь отвел для них достаточно места!!!
		*/
}IIflushtup(0);IIsync(0);}



	/*
	** Получим цвета и названия частей. Сделаем как
	** и во "Введении" ошибку и заметим, что Equel
	** отрабатывает ее, как и на странице 4:
	**   ##      retrieve pname = p.#имя, col =  p.цвет
	** с сообщением:
	**   IS = '=' : line 7, syntax error
	** почти столь же полезным, что и сообщение Quel.
	*/




{IIwrite("retrieve(pname=p.имя,col=p.цвет)");IIsetup();while(IIn_get(0)){IIn_ret(pname,3);IIn_ret(col,3);if(IIerrtest())continue;

















		printf(" %s имеет  %s цвет \n", pname,col);
}IIflushtup(0);IIsync(0);}

	/*
	** Если даже вы хотите поместить только одну строку
	** C-программы внутри запроса retrieve, то последовательности
	** ##{ и ##} все равно необходимы.
	*/

	/*
	** Для того, чтобы получить и распечатать части серого цвета
	** пишется следующий запрос:
	*/

	printf("Части, окрашенные в серый цвет:\n");
{IIwrite("retrieve(pname=p.имя)where p.цвет=\"серый\"");IIsetup();while(IIn_get(0)){IIn_ret(pname,3);if(IIerrtest())continue;


		printf("\t%s\n", pname);
}IIflushtup(0);IIsync(0);}


	/*
	** Предыдущий запрос аналогичен запросу со страницы 5
	** "Введения".
	*/


	/*
	** В Equel нет понятия "буфера запросов", который имеется
	** в Теминальном Мониторе RUBIN'а. Если мы хотим выполнить
	** запрос со страницы 6 "Введения", весь запрос должен быть
	** введен сразу (кроме операторов диапазона):
	*/

{IIwrite("retrieve(pname=p.имя,col=p.цвет)where p.цвет=\"серый\"or p.цвет=\"розовый\"");IIsetup();while(IIn_get(
0)){IIn_ret(pname,3);IIn_ret(col,3);if(IIerrtest())continue;


		printf("Деталь: %s, цвет %s\n", pname, col);
}IIflushtup(0);IIsync(0);}



	/*
	** Оставим пока "Введение" в покое и познакомимся с
	** некоторыми особенностями Equel.
	*/


	example1();

	/*
	** Далее идет пример интерактивной работы с БД...
	*/

	raise();

	/*
	** А в этй программе содержатся примеры "параметризированных"
	** операторов Equel
	*/
	param_ex();
}


/*
** Предположим, что мы хотим собрать в одно место некоторые атрибуты отношения
** для какого-нибудь арифметического вычисления, которое трудно провести в
** RUBIN'е.
** В примере example1() некоторые атрибуты отношения supply помещаются
** в массив структур.
*/
# define        MAXDATA         20




/*
** Так определяются для Equel поля "Nчасти", "Nпост", и "размер".
*/
struct supply{int pnum,snum;int quan;};





/*
** Последовательности ##{ и ##} в начале и в конце функции example1()
** указывают область действия об'явленных внутри них переменных.
** Т.о. данные рассматриваются Equel как локальные для example1.
** Каждый свободный блок (последовательность ##{...##} не обязательно
** следующая за оператором ##retrieve без результирующего
** отношения [ into] ) делает локальными об'явленные в нем переменные
** (существует лищь один уровень локальности; т.е. либо переменная
** является глобальной для всего файла, либо она является локальной
** для самого внешнего включающего ее блока).
*/

example1()
{

struct supply data[MAXDATA + 1];
	register int    i;

	i = 0;

{IIwrite("range of s=поставки");IIsync(0);}

	/*
	** Имена полей структуры известны как поля структуры, т.к.
	** они были об'явлены таковыми, и следуют за именем
	** структуры "data". Справа от знака равенства (=) они
	** не могут по контексту быть полями структуры и рассматриваются
	** как имена доменов, хотя для ясности тут может использоваться
	** оператор '#'
	*/

{IIwrite("retrieve(pnum=s.Nчасти,snum=s.Nпост,quan=s.размер)where s.дата_пост<=\"76-12-10\"");IIsetup();while(IIn_get(
0)){IIn_ret(&data[i].pnum,1);IIn_ret(&data[i].snum,1);IIn_ret(&data[i].quan,1);if(IIerrtest())continue;



		printf("Поставщик N%d, поставляет %d частей N%d.\n",
		data [i].snum, data [i].quan, data [i].pnum);
		if (i++ >= MAXDATA - 1)
		{
			printf("Много данных!\n");
			break;
			/*
			** Оператор break здесь применяется
			** корректно, т.к. retrieve  преобразуется
			** в оператор "while".  Break - единственно
			** возможный способ выйти из retrieve по
			** обнаруженной пользователем ошибке.
			** Стоящие за "while" операторы нужны для
			** сброса посланных RUBIN'ом данных, которые
			** не были использованы процессом Equel.
			*/
		}

}IIflushtup(0);IIsync(0);}}








/*
** В программе raise() реализован сеанс изменения зарплаты в интерактивном
** режиме. Существуют и другие способы реализации подобного сеанса
** интерактивной работы, но этот способ акцентирует внимание на некоторых из
** возможных ловушек.
*/

#include <setjmp.h>

static jmp_buf ___lj ;

#define setexit() setjmp( ___lj );

reset(val){ longjmp( ___lj, val ); }

raise()
{
	int             flag;
	int             per;
char percent[10];char rname[21];
char ename[21];
int sal;
char domain[20];
char info[255];

	extern          reset();

{IIwrite("range of e=служащие");IIsync(0);}

	/*
	** Т.к. оператор range действует все время работы RUBIN'а,
	** то мы об'являем его перед циклом, а не в его теле.
	*/





	/*
	** Перед входом в цикл мы готовимся продолжить обработку
	** после получения от пользователя сигнала прерывания. Нам
	** в этой точке поймать сигнал невозможно, т.к. RUBIN его
	** перехватит и попытается синхронизоваться с процессом Equel.
	** Когда процесс Equel синхронизируется, он вызовет
	** (*IIinterrupt)().
	*/

	IIinterrupt = reset;
	setexit();
loop:
	printf("Введите имя служащего (CTRL/D - выход, ? - дай список)?");

	if (eread(ename))
		return (0);

	if (ename[0] == '?' && ename[1] == '\0')
{IIwrite("print служащие");IIsync(0);}
	else
	{
		flag = 0;

		/*
		** В этом сеансе пользователь выполняет три запроса,
		** а RUBIN - арифметические действия.  имя помещается
		** в rname, т.к. ename может содержать совпадающие
		** с образцом симвлы и может быть получено более
		** одного значения переменной name. Например, может
		** быть введена последовательность символов "Ross*",
		** и тогда как Stanley, так и Stuart получат прибвку к
		** зарплате.
		*/
{IIwrite("retrieve(rname=e.имя,sal=e.оклад)where e.имя=");IIcvar(ename,3,0);IIwrite("");IIsetup();while(IIn_get(
0)){IIn_ret(rname,3);IIn_ret(&sal,1);if(IIerrtest())continue;

			printf("Тов.  %s имеет оклад %d\n",
				rname, sal);
			flag = 1;
}IIflushtup(0);IIsync(0);}

		if (!flag)
		{
			printf("Нет такого служащего\n");
			goto loop;
		}
		printf("Введите процент изменения оклада (+/-число) =");
		if (eread(percent))
			goto loop;


		/*
		** В Equel нет средств для просмотра, изменения
		** и возврата выборки назад в отношение. Оператор replace
		** должен содержать условие, т.к. нет связи между
		** предыдущим оператором retrieve и replace.
		*/

{IIwrite("replace e(оклад=e.оклад+float8(");IIcvar(percent,3,0);IIwrite(")/100.0*e.оклад)where e.имя=");IIcvar(
ename,3,0);IIwrite("");IIsync(0);}


		per = atoi(percent);

{IIwrite("retrieve(rname=e.имя,sal=e.оклад)where e.имя=");IIcvar(ename,3,0);IIwrite("");IIsetup();while(IIn_get(
0)){IIn_ret(rname,3);IIn_ret(&sal,1);if(IIerrtest())continue;

			printf("После такого ");
			if (abs(per) < 5)
				printf("ничтожного");
			else if (abs(per) < 10)
				printf("заметного");
			else if (abs(per) < 30)
				printf("большого");
			else
				printf("огромного");
			printf(" изменения, %s теперь получает %d\n",rname,sal);
}IIflushtup(0);IIsync(0);}


		printf("Вам нужна другая информация о %s?\n"
			, ename);

		if (eread(domain) || domain[0] == 'n' )
			goto loop;

		printf("Введите имя домена:  ");

		if (eread(domain))
			goto loop;

		/*
		** Если пользователь введет символ '?', тогда
		** ему показывают все возможные домены, распечатывая
		** атрибуты этого отношения из выборки в отношении
		** "attribute".
		*/

		if (domain[0] == '?' &&	domain[1] == '\0')
		{

{IIwrite("range of a=attribute");IIsync(0);}

{IIwrite("retrieve(domain=a.attname)where a.attrelid=\"служащие\"");IIsetup();while(IIn_get(0)){IIn_ret(domain,
3);if(IIerrtest())continue;

				printf("\t%s\n", domain);
}IIflushtup(0);IIsync(0);}
			printf("Введите домен:  ");

			if (eread(domain))
				goto loop;
		}


		/*
		** В этом случае C-переменная используется в качестве
		** имени домена. Значение переменной передается RUBIN'у
		** и интерпретируется как часть запроса.
		*/



		/*
		** Функция ascii используется потому, что тип домена
		** не известен. Если применить функцию ascii к символьному
		** домену, то она не сделает ничего.
		*/

{IIwrite("retrieve(rname=e.имя,info=ascii(e.");IIwrite(domain);IIwrite("))where e.имя=");IIcvar(ename,3,0);IIwrite("");
IIwrite("");IIsetup();while(IIn_get(0)){IIn_ret(rname,3);IIn_ret(info,3);if(IIerrtest())continue;


			printf("%s\t%s = %s\n",	rname, domain, info);
}IIflushtup(0);IIsync(0);}

	}
	goto loop;
}

/*
** На примере программы param_ex() демонстрируется использование параметризованных
** операторов equel. В примере приведены операторы equel, в которых целевой
** список к моменту прогона не определен. Таким образом в одних и тех же
** операторах Equel могжет участвовать разное число доменов или домены
** разных типов.
*/

param_ex()
{
	char		name [25];	/*
					 ** Переменные, использованные
					 ** в целевом списке
					 ** параметризированного
					 ** оператора могут и не
					 ** об'являться equel'у.
					*/
	register char	*string;
	short           empno;
	char		*tl_vector [100];

	/*
	** Другой способ:
	**  ##  retrieve (name = e.#имя, empno =e.номер)
	**  ##	{
	**		printf("employee #%d  is  called  %s.\n",
	** 		empno, name);
	**  ##	}
	*/

	/*
	** Этот оператор инициализирует переменную целевого списка.
	** Последовательности, начинающиеся с символа '%' указывают
	** тип следующего за ним соответствующего аргумента. Имеющиеся
	** типы :
	**      %c -- строка любой длины
	**      %i2, %i4 -- short или long
	**      %f4, %f8 -- float или double
	*/

	string = "%c is e.имя, %i2 = e.номер";
	tl_vector [0] = name;
	tl_vector [1] = &empno;

{IIwrite("retrieve(");IIw_left(string, tl_vector);IIwrite(")");IIsetup();while(IIgettup(0)){






		printf("Служащий N%d по имени %s.\n", empno, name);
}IIflushtup(0);IIsync(0);}





	/*
	** Параметризированными могут быть следующие операторы
	** append,  copy,  create, define view,
	** retrieve с результирующим отношением, и replace.
	**
	** А можно написать :
	** ##  param append to employee ("имя is %c, номер is %i2",
	** ##	  tl_vector)
	*/
}



/*
** Эта программа читает строку с терминала и закрывает ее пустым
** символом. При прочтении символа EOF возвращается 1.
*/

eread(p)
char	*p;
{
	int  c;
	while((c = getchar()) > 0)
	{
		if(c ==	'\n')
		{
			*p = 0;
			return(0);
		}
		*p++ = c;
	}
	return(1);
}
# include <defines.h>
# include <datetime.h>
# include "../iutil/form_defs.h"

/*
 * Программы перевода из текстового формата в формат
 * даты или времени
 *
 * Некоторые тонкости
 * 1. При вводе те символы в шаблоне и в тексте
 *    которые не являются символами преобразования,
 *    должны совпадать
 *    Исключение составляет символ шаблона ?,
 *    который означает "любой символ"
 * 2. При вводе через форматные поля предполагается, что
 *    формат имеет вид %.Nd и не проверяется. Проверяется
 *    только длина, причем реальное число может быть и
 *    меньшей длины, если за ним идет (и в строке, и в
 *    шаблоне) символ пунктуации. Ни при каких условиях
 *    поле числа в строке не может быть длиннее, чем в
 *    шаблоне.
 * 3. При вводе по полю типа "список", если в указанном списке
 *    нет требуемого слова, проверяются все остальные списки в
 *    таблице, относящиеся к тому же полю структуры. То есть по
 *    шаблону "MON" правильно введутся (как январь) следующие
 *    слова: Jan January Янв Январь Января.
 *
 *    Функция: to_long(char *строка_ввода, char *скомп_шаблон,
 *                     struct datebase *t)
 * Код ответа: -1 - ошибка, >=0 - результат
 */
long to_long(p0,s,db)
char *p0;               /* Входная строка - тек. символ */
char *s;                /* Шаблон - текущий символ      */
struct datebase *db;    /* указатель на таблицу         */
{
	register char *p;   /* Анализируемый символ входной строки */
	int kod;            /* Полученный результат                */
	int len;            /* Длина очередного поля               */
	register struct tabform *t0;
	struct tabform *t;
	register char *f;
	short cs;           /* Символ шаблона */
	/* 1. Есть ли шаблон и скомпилирован и он */
	if (!s || s[0] != D_COMPL || !s[1])
		return(-1l);
	s += 2;
	/* 2. Обнулим структуру */
	(*(db->fromlong))(0l,db->convbuf);
	/* 3. Поехали по шаблону */
	while (cs=(*s++ & 0377))
	{
		if (cs >= 040)
		{
			if (*p0 && (cs == '?' || cs == *p0))
			{
				p0++;
				continue;
			}
			goto ErrS;
		}
		t0 = &(db->tb[cs-1]);
		f = t0->da_form;
		/* 1. Формат - десятичное число */
		if ( *f >= ' ')
		{
			len = t0->da_flen;
			kod = 0;
			p = p0;
			while (len-- && *p >= '0' && *p <= '9')
				kod = kod*10 + (*p++ - '0');
			if (p == p0)goto ErrS;
			goto WrKod;
		}
		/* 2. Формат - список */
		t = db->tb;
FindTab:
		/* Попробуем найти очередную таблицу */
		while ( t->da_name== NULL || t->da_addr!=t0->da_addr
			|| *(f=t->da_form) >= ' ' )
		{
			if ( t->da_name == NULL ) goto ErrS;
			t++;
		};
		while ( *f )
		{
			kod = *f++;
			p = p0;
			while ( (*f & 0340) && *p == *f)
				f++, p++;
			if ((*f & 0340) == 0)
				goto WrKod;
			while ( (*f&0340) ) f++;
		}
		t++;
		goto FindTab;
WrKod:
		*(t0->da_addr) = kod - t0->da_off;
		p0 = p;
		continue;
	}
	return( (*db->tolong)(db->convbuf) );
ErrS:
	return(-1);
}

/*
 * long char_date(s,p)  - ввести дату из строки s по
 * шаблону p или по умолчанию (при p==NULL)
 */
long char_date(s,p)
char *s;
char *p; /* Шаблон */
{
	return(to_long(s,do_compl(p,&b_date),&b_date));
}

/*
 * long char_time(s,p)  - ввести время из строки s по
 * шаблону p или по умолчанию (при p==NULL)
 */
long char_time(s,p)
char *s;
char *p; /* Шаблон */
{
	return(to_long(s,do_compl(p,&b_time),&b_time));
}

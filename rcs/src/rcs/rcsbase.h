
/*
 *             Общие определения и структуры данных RCS
 */
#define RCSBASE "$Header: /usr/users/korotay/DEMOS_2.2/rcs/src/rcs/RCS/rcsbase.h,v 1.4 1988/09/21 20:37:38 avg Exp $"
/****************************************************************************
 * ИНСТРУКЦИЯ:
 * ===========
 * Для ЭВМ типа Электроника-82, задайте #define Е_82. В этом случае
 * будет производится замена функций getwd() и rename() на
 * аналогичные из библиотеки C. Кроме того, макро E_82 управляет
 * заданием максимального числа байтов в базовом и полном именах файлов
 * (макро NCPFN и NCPPN).
 * Макро STRICT_LOCKING задает режим включения блокировки файла
 * по умолчанию (см. STRICT_LOCKING).
 * При необходимости можно изменить размер байта (макро BYTESIZ).
 ****************************************************************************
 */


/* $Log: rcsbase.h,v $
 * Revision 1.4  1988/09/21  20:37:38  avg
 * Выключил STRICT_LOCKING. ДОСТАЛО!
 *
 * Revision 1.3  86/02/23  22:23:22  avg
 * Сделаны изменения для МНОС и для работы с U-кодом.
 *
 * Revision 1.2  86/01/02  15:50:52  fox
 * Введено E_82 в качестве имени весии для машин
 * совместимых с семейством VAX.
 *
 * Revision 1.1  85/12/26  22:02:36  fox
 * Система хранения версий файлов
 *
 */

#include <stdio.h>
#undef putc         /* будет переопределяться */

/* P3 */
#ifdef sparc
#define E82
#endif

/* Проверьте какой из вариантов макро DATEFORM печатает
 * двузначные числа с лидирующими нулями (00, 01 ...).
 * Если это макро заданно не верно, могут возникнуть проблемы
 * при работе с датами.
 */
#ifdef USG
#       define rindex    strrchr
#       define DATEFORM  "%.2d.%.2d.%.2d.%.2d.%.2d.%.2d"
#else
#       define DATEFORM  "%02d.%02d.%02d.%02d.%02d.%02d"
#endif

/* Запись в строку file даты и времени в формате RCS                        */
#define PRINTDATE(file,date) fprintf(file,"%.2s/%.2s/%.2s",date,date+3,date+6)
#define PRINTTIME(file,date) fprintf(file,"%.2s:%.2s:%.2s",date+9,date+12,date+15)

/*
 * Параметры
 */
#define BYTESIZ             8 /* число битов в байте                        */

#define STRICT_LOCKING      0 /* 0 выключает механизм блокировки по         */
			      /* умолчанию, этот режим используется         */
			      /* в отладочных версиях.                      */
			      /* 1 режим включения блокировки по            */
			      /* умолчанию.                                 */
#define hshsize           119 /* размер hash-таблицы; hshsize = 4*k -1      */
			      /* например: 547 или 719                      */
#define strtsize (hshsize * 30) /* размер таблицы строк                     */
#define logsize           512 /* мак. размер комментария                    */
#define revlength          30 /* мак. длина номера версии                   */
#define datelength         20 /* длина даты в формате RCS                   */
#define joinlength         20 /* число элементов списка ключа -j            */
#define RCSDIR         "RCS/" /* имя подсправочника  для архива RCS         */
#define RCSSUF            'v' /* суффикс архивных файлов RCS                */
#define RCSSEP            ',' /* разделитель для RCSSUF                     */
#define KDELIM            '$' /* символ для ключевый параметров             */
#define VDELIM            ':' /* разделитель ключевого слова и значения     */
#define DEFAULTSTATE    "Exp" /* статус версии по умолчанию                 */
#ifdef E_82
#  define NCPFN           256 /* макс. число байтов в имени файла           */
#  define NCPPN          1024 /* макс. число байтов в полном имени файла    */
#else
#  define NCPFN            14 /* макс. число байтов в имени файла           */
#  define NCPPN       6*NCPFN /* макс. число байтов в полном имени файла    */
#endif
#define keylength          20 /* размер буфера для расширения кл. параметров*/
#define keyvallength NCPPN+revlength+datelength+60
			      /* размер буфера под значение кл. параметра   */



#define true     1
#define false    0
#define nil      0
#define elsif    else if
#define elif     else if


/* Имена временных файлов */

#define NEWRCSFILE  ",RCSnewXXXXXX"
#define DIFFILE     ",RCSciXXXXXX"
#define TMPFILE1    ",RCSt1XXXXXX"
#define TMPFILE2    ",RCSt2XXXXXX"
#define TMPFILE3    ",RCSt3XXXXXX"
#define JOINFIL2    ",RCSj2XXXXXX"
#define JOINFIL3    ",RCSj3XXXXXX"


/* Версия putc. Печатает символ, но вылетает при ошибках записи             */
int     _cc_;
#define putc(x,p) (--(p)->_cnt>=0?((*(p)->_ptr++=(_cc_=(x))?_cc_:0)):fflsbuf((x),p))

/* GETC зписывает символ del (код 0177) в конец файла                       */
#define GETC(in,out,echo) (echo?putc(getc(in),out):getc(in))

/* Готовит режим доступа рабочего файла из RCSmod, при необходимости
убирается разрешение записи (в зависимости от lockflag и StrictLocks)       */
#define WORKMODE(RCSmode) (RCSmode&~0222)|((lockflag||!StrictLocks)?0600:0000)


/* Классы символов и коды лексем */
enum tokens {
/* классы символов*/ DIGIT, IDCHAR, NEWLN, LETTER, PERIOD, SBEGIN, SPACE, UNKN,
/* лексемы */       COLON, DATE, EOFILE, ID, KEYW, NUM, SEMI, STRING,
};

/* класс SBEGIN (начало строки) возвращется лексическим анализатором        */
#define AT      SBEGIN

/* символ необходимый для обработки строк */
#define SDELIM  '@'

/* спец. символы могут заменяться, например:
 * #define DQUOTE       SBEGIN
 * #define SDELIM       '"'
 * #define AT           IDCHAR
 * они не должны пересекаться с SDELIM, KDELIM и VDELIM
 */

/* Прочие символы */

#define ACCENT   IDCHAR
#define AMPER    IDCHAR
#define BACKSL   IDCHAR
#define BAR      IDCHAR
#define COMMA    UNKN
#define DIVIDE   IDCHAR
#define DOLLAR   IDCHAR
#define DQUOTE   IDCHAR
#define EQUAL    IDCHAR
#define EXCLA    IDCHAR
#define GREAT    IDCHAR
#define HASH     IDCHAR
#define INSERT   UNKN
#define LBRACE   IDCHAR
#define LBRACK   IDCHAR
#define LESS     IDCHAR
#define LPARN    IDCHAR
#define MINUS    IDCHAR
#define PERCNT   IDCHAR
#define PLUS     IDCHAR
#define QUEST    IDCHAR
#define RBRACE   IDCHAR
#define RBRACK   IDCHAR
#define RPARN    IDCHAR
#define SQUOTE   IDCHAR
#define TILDE    IDCHAR
#define TIMES    IDCHAR
#define UNDER    IDCHAR
#define UPARR    IDCHAR




/****************************************
 * Структуры данных для таблицы символов
 ****************************************/


/* Структура hash таблицы */
struct hshentry {
	char              * num;      /* указатель на номер версии (в КОИ-8)*/
	char              * date;     /* указатель на дату записи           */
	char              * author;   /* id записавшего пользователя        */
	char              * lockedby; /* кто зарезервировал версию          */
	char              * log;      /* комментарий, полученный при записи */
	char              * state;    /* статус версии (по умолчанию Exp)   */
	struct branchhead * branches; /* список первых версий ветвей        */
	struct hshentry   * next;     /* следующая версия этой ветви        */
	int                 insertlns;/* число вставленных строк (выч. rlog)*/
	int                 deletelns;/* число исключенных (вычисляет rlog) */
	char                selector; /* метка для selection/deletion       */
};

/* Список элементов для списка ветвей */
struct branchhead {
        struct hshentry   * hsh;
        struct branchhead * nextbranch;
};

/* Элемент списка доступа */
struct access {
        char              * login;
        struct access     * nextaccess;
};

/* Элемент списка для блокировки */
struct lock {
        char              * login;
        struct hshentry   * delta;
        struct lock       * nextlock;
};

/* Элемент списка символьных имен */
struct assoc {
        char              * symbol;
        struct hshentry   * delta;
        struct assoc      * nextassoc;
};


/* Общие переменные (getadmin и getdelta())*/
extern char            * Comment;
extern struct access   * AccessList;
extern struct assoc    * Symbols;
extern struct lock     * Locks;
extern struct hshentry * Head;
extern int               StrictLocks;
extern int               TotalDeltas;

/* Обшие переменные (лексический анализатор)*/
extern enum tokens map[];
#define ctab (&map[1])
extern struct hshentry   hshtab[];
extern struct hshentry * nexthsh;
extern enum tokens       nexttok;
extern int               hshenter;
extern char            * NextString;
extern char            * cmdid;

/* Общие функции */
extern int serror();
extern int faterror();
extern int fatserror();

/*
 * Ключевые параметры (используются в co и ident)
 */
#define AUTHOR          "Author"
#define DATE            "Date"
#define HEADER          "Header"
#define LOCKER          "Locker"
#define LOG             "Log"
#define REVISION        "Revision"
#define SOURCE          "Source"
#define STATE           "State"

enum markers { Nomatch, Author, Date, Header,
               Locker, Log, Revision, Source, State };

/* используется в putdtext и scanlogtext */
#define DELNUMFORM      "\n\n%s\n%s\n"

/* устанавливается rcs -o, используется puttree() в rcssyn */
#define DELETE          'D'


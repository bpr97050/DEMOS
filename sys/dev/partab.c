/*
 * $Log:	partab.c,v $
 * Revision 22.1  89/04/12  14:29:56  korotaev
 * "param.h" ==> <sys/param.h>
 * 
 * Revision 22.0  89/03/25  12:30:16  korotaev
 * Отсюда начинается версия 2.2
 * 
 * Revision 1.2  88/07/15  16:32:00  alex
 * Внесены правки для работы с КОИ-8/QWERTY терминалы
 * 
 * Revision 1.1  86/04/19  17:54:44  avg
 * Initial revision
 * 
 *
 * Автор: Антонов В.Г. (@VG)
 */

#include <sys/param.h>
#include <sys/tty.h>

#define O  0200|
#define E  0000|

#define __    ORDINARY
#define co    CONTROL
#define bs    BACKSPACE
#define nl    NEWLINE
#define ht    TAB
#define vt    VTAB
#define cr    RETURN

/*
 * Parity table - high order bit.
 * Input mapping table-- if an entry is >= 040, when the
 * corresponding character is typed preceded by "\" the escape
 * sequence is replaced by the table value.  Only used for
 * upper-case only terminals.
 */

char partab[] = {
/*000*/ E   co, O   co, O   co, E   co, O   co, E   co, E   co, O   co,
/*010*/ O   bs, E   ht, E   nl, O   co, E   vt, O   cr, O   co, E   co,
/*020*/ O   co, E   co, E   co, O   co, E   co, O   co, O   co, E   co,
/*030*/ E   co, O   co, O   co, E   co, O   co, E   co, E   co, O   co,
/*040*/ O   __, E  '|', E   __, O   __, E   __, O   __, O   __, E  '`',
/*050*/ E  '{', O  '}', O   __, E   __, O   __, E   __, E   __, O   __,
/*060*/ E   __, O   __, O   __, E   __, O   __, E   __, E   __, O   __,
/*070*/ O   __, E   __, E   __, O   __, E   __, O   __, O   __, E   __,
/*100*/ O   __, E   __, E   __, O   __, E   __, O   __, O   __, E   __,
/*110*/ E   __, O   __, O   __, E   __, O   __, E   __, E   __, O   __,
/*120*/ E   __, O   __, O   __, E   __, O   __, E   __, E   __, O   __,
/*130*/ O   __, E   __, E   __, O   __, E   __, O   __, O  '~', E   __,
/*140*/ E   __, O  'A', O  'B', E  'C', O  'D', E  'E', E  'F', O  'G',
/*150*/ O  'H', E  'I', E  'J', O  'K', E  'L', O  'M', O  'N', E  'O',
/*160*/ O  'P', E  'Q', E  'R', O  'S', E  'T', O  'U', O  'V', E  'W',
/*170*/ E  'X', O  'Y', O  'Z', E   __, O   __, E   __, E   __, O   co,

/*200*/ E   __, O   __, O   __, E   __, O   __, E   __, E   __, O   __,
/*210*/ O   __, E   __, E   __, O   __, E   __, O   __, O   __, E   __,
/*220*/ O   __, E   __, E   __, O   __, E   __, O   __, O   __, E   __,
/*230*/ E   __, O   __, O   __, E   __, O   __, E   __, E   __, O   __,
/*240*/ O   __, E   __, E   __, O   __, E   __, O   __, O   __, E   __,
/*250*/ E   __, O   __, O   __, E   __, O   __, E   __, E   __, O   __,
/*260*/ E   __, O   __, O   __, E   __, O   __, E   __, E   __, O   __,
/*270*/ O   __, E   __, E   __, O   __, E   __, O   __, O   __, E   __,

/*300*/ O 0340, E 0341, E 0342, O 0343, E 0344, O 0345, O 0346, E 0347,
/*310*/ E 0350, O 0351, O 0352, E 0353, O 0354, E 0355, E 0356, O 0357,
/*320*/ E 0360, O 0361, O 0362, E 0363, O 0364, E 0365, E 0366, O 0367,
/*330*/ O 0370, E 0371, E 0372, O 0373, E 0374, O 0375, O 0376, E 0377,

/*340*/ E   __, O   __, O   __, E   __, O   __, E   __, E   __, O   __,
/*350*/ O   __, E   __, E   __, O   __, E   __, O   __, O   __, E   __,
/*360*/ O   __, E   __, E   __, O   __, E   __, O   __, O   __, E   __,
/*370*/ E   __, O   __, O   __, E   __, O   __, E   __, E   __, O   __

};

char    qwerty_in[] = {
    ' ', '!', 'Э', '?', '$', '%', '&', 'э',     /* 040 - 047 */
    '(', ')', '*', '+', 'б', '-', '.', 'ю',     /* 050 - 057 */
    '0', '1', '2', '3', '4', '5', '6', '7',     /* 060 - 067 */
    '8', '9', 'Ж', 'ж', 'Б', '=', '>', 'Ю',     /* 070 - 077 */
    '/', 'Ф', 'И', 'С', 'В', 'У', 'А', 'П',     /* 100 - 107 */
    'Р', 'Ш', 'О', 'Л', 'Д', 'Ь', 'Т', 'Щ',     /* 110 - 117 */
    'З', 'Й', 'К', 'Ы', 'Е', 'Г', 'М', 'Ц',     /* 120 - 127 */
    'Ч', 'Н', 'Я', 'х', ',', 'ъ', '^', '_',     /* 130 - 137 */
    '`', 'ф', 'и', 'с', 'в', 'у', 'а', 'п',     /* 140 - 147 */
    'р', 'ш', 'о', 'л', 'д', 'ь', 'т', 'щ',     /* 150 - 157 */
    'з', 'й', 'к', 'ы', 'е', 'г', 'м', 'ц',     /* 160 - 167 */
    'ч', 'н', 'я', 'Х', '"', ']', '~', ' '      /* 170 - 177 */
};

#ifdef Ucode

/*
 * Coding from and to KOI-8 code.
 */
static char IN_tab[] = {
	0376, 0340, 0341, 0366, 0344, 0345, 0364, 0343,
	0365, 0350, 0351, 0352, 0353, 0354, 0355, 0356,
	0357, 0377, 0360, 0361, 0362, 0363, 0346, 0342,
	0374, 0373, 0347, 0370, 0375, 0371, 0367, 0372,
	0336, 0300, 0301, 0326, 0304, 0305, 0324, 0303,
	0325, 0310, 0311, 0312, 0313, 0314, 0315, 0316,
	0317, 0337, 0320, 0321, 0322, 0323, 0306, 0302,
	0334, 0333, 0307, 0330, 0335, 0331, 0327, 0332
};

static char OUT_tab[] = {
	0341, 0342, 0367, 0347, 0344, 0345, 0366, 0372,
	0351, 0352, 0353, 0354, 0355, 0356, 0357, 0360,
	0362, 0363, 0364, 0365, 0346, 0350, 0343, 0376,
	0373, 0375, 0377, 0371, 0370, 0374, 0340, 0361,
	0301, 0302, 0327, 0307, 0304, 0305, 0326, 0332,
	0311, 0312, 0313, 0314, 0315, 0316, 0317, 0320,
	0322, 0323, 0324, 0325, 0306, 0310, 0303, 0336,
	0333, 0335, 0337, 0331, 0330, 0334, 0300, 0321
};

/*
 * Transliterate from KOI-8 to Ucode
 */
conv_in(c)
register c;
{
	if ((c&0300) == 0300)
		c = IN_tab[c&077];
	c &= 0377;
	return(c);
}

/*
 * Transliterate from Ucode to KOI-8
 */
conv_out(c)
register c;
{
	if ((c&0300) == 0300)
		c = OUT_tab[c&077];
	c &= 0377;
	return(c);
}
#endif Ucode

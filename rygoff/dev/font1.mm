*
* Программа для перекодировки таблицы символов
*
* $Log:	font1.mm,v $
* Revision 22.1  89/04/10  18:26:48  avg
* М. Коротаев неправильно не указал комментария для RCS.
* 
* Revision 22.0  89/03/25  12:25:11  korotaev
* Отсюда начинается версия 2.2
*
* Revision 1.2  89/03/24  15:34:45  korotaev
* *** empty log message ***
*
# Revision 1.1  87/01/21  17:38:21  avg
# Initial revision
#
* Revision 1.1  86/07/13  20:51:18  avg
* Initial revision
*
*

/ -?
 .\/* ~1 *\/

/ [0-7]|?
 g~2

/ !|?

/ ?
 .~1

/g|?
 c~1|

/g ?|?
 g~1|F~2

/g!?|?
 g~2|T~3

/c?FFF|?
 c~1|0~2
/c?FFT|?
 c~1|1~2
/c?FTF|?
 c~1|2~2
/c?FTT|?
 c~1|3~2
/c?TFF|?
 c~1|4~2
/c?TFT|?
 c~1|5~2
/c?TTF|?
 c~1|6~2
/c?TTT|?
 c~1|7~2

/c|?
 .\t0~1,

/c?|?
 cF~1|~2

 26.01.90
     Имеет  смысл  в будущем сделать оптимизацию на использование общих
 буферов в Tcopy, Next0 и Relocate.

 27.01.90
     Проблема общего буфера чтения решена путем виртуализации ModRead.
     Выписал копирование модуля с определением базирования.
     Выписал чтение таблицы символов с перемещением символов.

 28.01.90
     Bug: надо сначала искать модули всей пачкой, а потом грузить (??)

 01.02.90
     Использование  неинициированного  static'а, который в MISS конечно
 не равен нулю.

 03.02.90
     Подряд  две ошибки на одну тему: в цикле с двумя индексами я забыл
 увеличить один из них. Мораль - не пользоваться таким циклами. Правда,
 пока  я  только  поправил  ошибки, так как в обоих случаях для цикла с
 одним индексом страдает ясность (NextSymbol и Reloc).
     Замечательная   ошибка:   использование   перечислимого  типа  как
 логического плюс неудачная мнемоника (Mlibflag==0, когда библиотека).
     Промашка с порядком операций:  n = a[i++] + a[i++]; - нельзя!

 09.02.90
     Работает более-менее.
     Куча  ошибок  с  чтением  об'ектных модулей, причем все они вокруг
 оптимизации на статический буфер.
     Другого  типа  ошибка  в  том  же  модуле  -  Неудачное именование
 переменной: flen для библиотечного модуля означает не длину, а стопор.
 Далее конфликт между ModSeek и ненулевыми смещениями для библиотек.

 11.02.90
     Ошибка   из-за   отсутствия   спецификации  т.е.  описания  смысла
 переменной lastfill во всех ситуациях. Где муть, там ошибки!

 24.05.92
     Перевод на трансляцию в режиме Strict Check в Vengerov's C в связи
 с переходом на IBM PC.

 29.05.92
     Грубейшая  ошбка  в  модуле  загрузки  символов  SymGet.   Счетчик
 символов увеличивался только для глобальных.

 31.05.92
     В ModIni() запарывалось имя модуля,  передававшееся  в  аргументе,
 из-за чего  не  выдавалось  правильной  диагностики  об  отсутствующих
 модулях.
     Крайне коряво поправлено несколько модулей на  предмет  правильной
 настройки bss. Раньше получалось, что bss лежит с нуля.

 01.06.92
     Ошибка в секции работы с  выходным  файлом.  Длина  модуля  теперь
 подновляется  при  любой  записи  и  правлена  небольшая  путаница   в
 проверках во время закрытия файла.
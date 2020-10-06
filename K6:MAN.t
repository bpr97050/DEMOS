
+              УСТРОЙСТВО ПЕРЕДАЧИ ДАННЫХ ВОСЬМИКАНАЛЬНОЕ
+                    Руководство по программированию

+     Адресация устройства

 Рядом с 580ИР82, 1 .. 4 в ряд, 5 - отдельно
 Перемычка    Базовый    Верхний
              адрес      адрес
   1 - 5      176000     176076
   2 - 5      176100     176176
   3 - 5      176200     176276
   4 - 5      176300     176376

+     Загрузка счетчиков

 Загрузка счетчиков делается раньше всего. Сначала  следует  установить
 режим генератора меандра, затем загрузить счетчик.

 В асинхронном режиме:  счетчик = 1536000 / (Скорость * 16)
 В синхронном режиме:   счетчик = 1536000 / Скорость
 Для асинхронного режима и скорости 19200 бод счетчик = 5.

 Формат регистра режима:
 BIN    =    0      /16-разрядный двоичный счетчик
 BCD    =    1      /4-декадный BCD счетчик
 MEANDR =   06      /Режим генератора
 LSB    =  020      /Загрузить только младший байт
 MSB    =  040      /Загрузить только старший байт
 ML2    =  060      /Загрузка сначала младшего, потом старшего байта
 CNTSEL = 0100      /Выборка счетчика = N*CNTSEL

 Адрес счетчика:
 Чип Регистр    Смещ.  Канал
 D16 Режим      +066
     счетчик 0  +060    1
     счетчик 1  +062    2
     счетчик 2  +064   +3
 D17 Режим      +076
     счетчик 0  +070    4
     счетчик 1  +072    5
     счетчик 2  +074    6,7,8

 Пример. Загрузка 3-го канала на 9600 бод.
 OURCNT = 2
 OUROFF = 60
   mov $176000+OUROFF,r5
   mov $CNTSEL*OURCNT+ML2+MEANDR+BIN,6(r5)
   add $OURCNT*2,r5
   mov $12,*r5
   mov $0,*r5

+Установка USART

 8251 имеет два регистра - данные и команда.  По  умолчанию  в  регистр
 команд может быть  записана  команда.  Для  установки  USART  выдается
 соответствующая команда, а затем  в  регистр  управления  записывается
 байт параметров установки. Установку USART следует делать при закрытых
 в контроллере прерываниях.

 Формат слова команд.
 Бит  7  6  5  4  3  2  1  0
      !  !  !  !  !  !  !  +-- Разрешение передачи
      !  !  !  !  !  !  +----- RTS (?)
      !  !  !  !  !  +-------- Разрешение приема
      !  !  !  !  +----------- BREAK
      !  !  !  +-------------- Сброс ошибок
      !  !  +----------------- DTR (?)
      !  +-------------------- Общий внутренний сброс
      +----------------------- Разрешение поиска символов SYN

 Формат слова параметров
 Бит  7  6  5  4  3  2  1  0
      !  !  !  !  !  !  +--+-- Делитель или режим работы
      !  !  !  !  !  !         00=SyncMode, 01=1:1, 10=1:16, 11=1:64
      !  !  !  !  +--+-------- Длина данных, бит
      !  !  !  !               00=5, 01=6, 10=7, 11=8
      !  !  !  +-------------- Разрешение контроля четности
      !  !  +----------------- 0=четность, 1=нечетность
      +--+-------------------- Для асинхр. режима - число стоп-битов.
      :  :                     00=нет, 01=1, 10=1.5, 11=2
      !  +-------------------- Для синхр. режима -
      !                        разрешение внешней синхронизации
      +----------------------- Для синхр. режима -
                               0=два синхросимвола, 1=один синхросимвол

 Адресация микросхем
 Канал  Данные  Управление
   1     +000    +002
   2     +004    +006
   3     +010    +012
   4     +014    +016
   5     +020    +022
   6     +024    +026
   7     +030    +032
   8     +034    +036

 Пример:
   mov $376,*$176002      /8e2, 1:16 для канала 1
   mov $116 *$176012      /8n1, 1:16 для канала 3

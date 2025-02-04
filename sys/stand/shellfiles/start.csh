#! /бин/цсх
# kОМАНДНЫЙ ФАЙЛ ДЛЯ НАЧАЛЬНОЙ НАСТРОЙКИ СИСТЕМЫ
#
уналиас *
сет патх=(/етц $патх)
сет PROFILE=/.профиле
сет RC=/етц/рц
сет TTYS=/етц/ттыс
сет ROOT=/
сет TUSR=7
сет TSYS=8
сет TUSR1=9
сет STTY=()
ецхо "\
           ***  ****** *     *  ****   ****           ****  \
          *  *  *      **   ** *    * *    *         *    * \
         *   *  *      * * * * *    * *                   * \
         *   *  ****   *  *  * *    * *                  *  \
         *   *  *      *     * *    * *                *    \
        ******* *      *     * *    * *    *          *     \
        *     * ****** *     *  ****   ****          ****** \
"
#
# 1. uСТАНОВКА РЕЖИМОВ ТЕРМИНАЛА.
#
алиас едиаг 'ецхо \!:1'
иф ( $?MSG != 0 ) тхен
иф ( $MSG == р ) алиас едиаг 'ецхо \!:2'
ендиф
ецхо "IF YOU WANT TO SET A TTY KEYS"
ецхо "      ENTER      0<BK> OR 0<RETURN>"
ецхо "      ELSE ENTER 1<BK> OR 1<RETURN>"
ецхо -н "(0 1) ?"
сет анс=$<
иф ( "$анс" == "1" ) гото адате
старт:
стты цел15 дец табс видтх 80
ецхо "           uСТАНОВКА РЕЖИМОВ РАБОТЫ ТЕРМИНАЛА"
ецхо "wВЕДИТЕ 0, ЕСЛИ ТЕРМИНАЛ ИМЕЕТ РУССКИЕ И ЛАТИНСКИЕ ПРОПИСНЫЕ "
ецхо "           И СТРОЧНЫЕ БУКВЫ (|ЛЕКТРОНИКА-15-i|-0013 И Т.П.   "
стты дец цырилл лцасе уцасе сисо црт видтх 80
ецхо "           uСТАНОВКА РЕЖИМОВ РАБОТЫ ТЕРМИНАЛА"
ецхо "wВЕДИТЕ 1, ЕСЛИ ТЕРМИНАЛ ИМЕЕТ РУССКИЕ И ЛАТИНСКИЕ ПРОПИСНЫЕ "
ецхо "           БУКВЫ И НЕ ИМЕЕТ СТРОЧНЫХ БУКВ. (sm-7209, ...    )"
стты -лцасе -цырилл дец црт видтх 80
ецхо "           TTY SET UP      "
ецхо "Eнтер 2, иф ыоу тты хаже онлы енглисх леттерс "
ецхо "          (аттансион: DEC VT-220 цан спеак руссион, иф ыоу  )"
ецхо "          (лоад фонт, вхицх ыоу цан таке он AZLK, MOSCOW    )"
стты дец црт видтх 80 лцасе
ецхо "           TTY SET UP      "
ецхо "Eнтер 3, иф ыоу тты хаже онлы BIG енглисх леттерс "
стты цел15 дец видтх 80
ецхо -н "\
(0 1 2 3 ) ?"
сет анс=$<
свитцх ($анс)
цасе 0:
    стты цел15 дец видтх 80
сети:
    ецхо "\
        uТОЧНИМ РЕЖИМ ВВОДА, ДЛЯ ЧЕГО ПРОВЕДЕМ СЛЕДУЮЩИЙ ТЕСТ.\
        sНАЧАЛА НАЖМИТЕ НА ТЕРМИНАЛЕ КЛАВИШУ rus ИЛИ <CTRL-N>, \
        ЕСЛИ ТАКОЙ КЛАВИШИ НЕТ, И wwod (RETURN). zАТЕМ, ПОЛУЧИВ СЛОВО тест,\
        НАЖМИТЕ НИЖНИЙ РЕГИСТР И ЗАТЕМ КЛАВИШИ <Т> <Е> <С> <Т> <wwod>"
    ецхо "wОЗМОЖНО, ПРИ ВВОДЕ ЧАСТЬ БУКВ БУДУТ НЕ НА ТОМ РЕГИСТРЕ"
    стты цел15и
    ецхо -н "nАЧАЛИ. nАЖМИТЕ rus"
    сет ааа=$<
    ецхо -н "тест "
    сет анс1=$<
    сет STTY=нил
    иф( $анс1 == "ТЕСТ" ) сет STTY=("-рав -нл ецхо дец цел15и табс" "дец,цел15и,табс")
    иф( $анс1 == "Тest" ) сет STTY=("-рав -нл ецхо дец цел15 табс" "дец,цел15,табс")
    иф( $анс1 == "test" ) сет STTY=("-рав -нл ецхо дец цел15к табс" "дец,цел15к,табс")
    иф( "$STTY" == "нил"  ) тхен
        ецхо "wЫ ОШИБЛИСЬ, ПОВТОРИТЕ. dОЛЖНО ВЫЙТИ СЛОВО ТЕСТ, Тest ИЛИ test"
        гото сети
    ендиф
    алиас едиаг 'ецхо \!:2'
    ецхо "dЛЯ ПЕРЕКЛЮЧЕНИЯ НА ВВОД РУССКИХ/ЛАТИНСКИХ БУКВ"
    ецхо "ИСПОЛЬЗУЙТЕ КЛАВИШИ rus/lat, ЕСЛИ ОНИ ЕСТЬ, ИНАЧЕ <CTRL>N/<CTRL>O"
    бреаксв
цасе 1:
    стты цырилл лцасе уцасе
    сет STTY=("-рав -нл ецхо лцасе дец цырилл уцасе црт" "лцасе,дец,цырилл,уцасе,црт")
    ецхо "\
    wВЕДИТЕ 1, ЕСЛИ ДЛЯ ПЕРЕКЛЮЧЕНИЯ РУССКИХ/АНГЛИЙСКИХ БУКВ ВЫ БУДЕТЕ\
    ИСПОЛЬЗОВАТЬ КЛАВИШУ rus/lat, А ПЕРЕД ПРОПИСНЫМИ БУКВАМИ ПИСАТЬ \
    СИМВОЛ \\ (ТЕРМИНАЛЫ wta); \
    wВЕДИТЕ 2 ИЛИ wk, ЕСЛИ НА ТЕРМИНАЛЕ ЕСТЬ КЛАВИША SHIFT \
    (ПЕРЕХОД В ВЕРХНИЙ РЕГИСТР), РУССКИЕ/АНГЛИЙСКИЕ БУКВЫ В \
    ЭТОМ СЛУЧАЕ ПЕРЕКЛЮЧАЮТСЯ КОДАМИ:\
    <CTRL>N - РУССКИЕ, <CTRL>O - АНГЛИЙСКИЕ"
    ецхо -н "(1 2)?"
    сет анс1=$<
    иф( $анс1 != 1 ) тхен
    сет STTY[1]="$STTY[1] сисо"
    сет STTY[2]="$STTY[2],сисо"
    ендиф
    алиас едиаг 'ецхо \!:2'
    бреаксв
цасе 2:
    сет STTY=("-рав -нл ецхо -лцасе -цырилл дец црт" "дец,црт")
    алиас едиаг 'ецхо \!:1'
    бреаксв
цасе 3:
    сет STTY=("-рав -нл ецхо лцасе -цырилл дец уцасе црт" "лцасе,дец,уцасе,црт")
    алиас едиаг 'ецхо \!:1'
    бреаксв
ендсв

стты $STTY[1]
стты видтх 0
едиаг "Nеьт тест: иф ыоу тты аутоматицаллы бегин нев лине \
 афтер ласт ис фулл, ентер 0" ""
едиаг "" "sЛЕД. ТЕСТ. eСЛИ ВАШ ТЕРМИНАЛ АВТОМАТИЧЕСКИ \
ПЕРЕВОДИТ СТРОКУ ПРИ ЗАПОЛНЕНИИ ОЧЕРЕДНОЙ СТРОКИ, ВВЕДИТЕ 0"
едиаг "Eлсе ентер 1" "iНАЧЕ ВВЕДИТЕ 1"
едиаг "0 - иф ыоу вилл реад AUTO, 1 - до нот" "0 - ЕСЛИ ВЫ ПРОЧТЕТЕ AUTO, ИНАЧЕ 1"
ецхо "-------------------------------------------------------------------------------------    AUTO"
сет анс=$<
иф ( "$анс" == "1" ) тхен
сет STTY[1]="$STTY[1] видтх 80"
сет STTY[2]="$STTY[2],видтх=80"
ендиф
стты $STTY[1]
едиаг "Rемемдер ыоу цонсоле тты флагс - $STTY[1]" "zАПОМНИТЕ ТЕРМИНАЛЬНЫЕ КЛЮЧИ ДЛЯ КОНСОЛИ - $STTY[1]"
едиаг "Rемемдер ыоу цонсоле /етц/ттыс флагс - $STTY[2]" "zАПОМНИТЕ ТЕРМИНАЛЬНЫЕ КЛЮЧИ В ФАЙЛЕ /етц/ттыс ДЛЯ КОНСОЛИ - $STTY[2]"
слееп 5
#
аттытыпе:
едиаг "Eнтер тты тыпе, оне оф десцрибед ин /етц/термцап" "wВЕДИТЕ ТИП ТЕРМИНАЛА (ОДИН ИЗ ИМЕЮЩИХСЯ В /етц/термцап)"
стты нлделаы 5
греп "#+" /етц/термцап
стты нлделаы 0
ецхо -н "Tыпе="
сет ттытыпе=$<
греп -с "$ттытыпеэ" /етц/термцап
иф ( $статус != 0 ) тхен
едиаг "Uнкновн тыпе $ттытыпе, репеат" "nЕИЗВЕСТНЫЙ ТИП $ттытыпе, ПОВТОРИТЕ"
гото аттытыпе
ендиф
едиаг "Tхис тыпе вилл бе врите ин /етц/ттыс" "tИП ЗАПИСЫВАЕТСЯ В /етц/ттыс"
#
ецхо "\
---------------------------------\
"
#
# pРОВЕРКА И ЗАПРОС ДАТЫ, ЕСЛИ ОНА НУЛЕВАЯ
#
адате:
    дате э греп -с '1970$'
    иф ( $статус == 0 ) тхен
        едиаг "ZERO DATA\!\!\!" "dАТА НЕ УСТАНОВЛЕНА\!\!\!"
        едиаг "Eнтер дата ин 'дата' формат: YYMMDDHHMM" "wВЕДИТЕ ДАТУ В ФОРМАТЕ КОМАНДЫ дате : ГГММДДЧЧММ"
        едиаг "2 дигит - ыеар, 2 дигит - монтх, 2 дигит -даы"  ""
        едиаг "2 дигит - хоур, 2 дигит - минутес"  ""
        едиаг "" "2 ЦИФРЫ - ГОД, 2 ЦИФРЫ - МЕСЯЦ, 2 ЦИФРЫ - ДЕНЬ ,"
        едиаг "" "2 ЦИФРЫ - ЧАСОВ, 2 ЦИФРЫ - МИНУТ."
        едиаг "Iф фирст фиелдс нот цхангед, ит цан бе скиппед ин футуре" "w ДАЛЬНЕЙШЕМ МОЖНО БУДЕТ ОПУСКАТЬ ЦИФРЫ СЛЕВА, ЕСЛИ ОНИ НЕ ИЗМЕНЯЛИСЬ"
        едиаг "Dата ин тхе сыстем=" "dАТА В СИСТЕМЕ= "
        дате
        едиаг " Eнтер дате" " wВЕДИТЕ ДАТУ"
        ецхо -н "_"
        сет анс=$<
        дате $анс
        гото адате
    ендиф
иф ( $#STTY != 0 ) тхен
едиаг "BEGIN OF EDITING" "nАЧАЛО РЕДАКТИРОВАНИЯ"
ецхо едитинг $PROFILE
ед - $PROFILE << %енд
/STTY=/ц
STTY="$STTY[1]"
.
в
я
%енд
ецхо едитинг $RC
ед - $RC << %енд
/STTY=/ц
STTY="$STTY[1]"
.
в
я
%енд
ецхо едитинг $TTYS
ед - $TTYS << %енд
/1цонсоле/ц
1цонсоле:9600:-:$шттытыпещ:$STTY[2]
.
в
я
%енд
едиаг "Wхен еьит, ентер TERM=$ттытыпе;еьпорт TERM" "pОСЛЕ КОНЦА старт НАБЕРИТЕ: TERM=$ттытыпе;еьпорт TERM"
едиаг "Eдитион анд ттысет енд" "kОНЕЦ НАСТРОЙКИ ТЕРМИНАЛА И РЕДАКТИРОВАНИЯ"
ендиф

ецхо "\
\
\"
едиаг "Eнтер 0, иф ыоу вант цонтинуе (сет дискс ор реад тапе)" ""
едиаг "" "wВЕДИТЕ 0, ЕСЛИ НУЖНО ПРОДОЛЖИТЬ (НАСТРОИТЬ СИСТЕМНЫЕ ДИСКИ ИЛИ ЧИТАТЬ ЛЕНТУ)"
едиаг "Eлсе ентер 1" "iНАЧЕ ВВЕДИТЕ 1"
ецхо -н "(0 1) ?"
сет анс=$<
иф ( "$анс" == 1 ) еьит 0
асктдиск:
едиаг "еьит то тхе сыстем. Eнтер: MSG=л;еьпорт MSG" "wЫХОДИМ В СИСТЕМУ. wВЕДИТЕ: MSG=р;еьпорт MSG"
едиаг "Tо цонтинуе сет уп, ентер: старт1" "dЛЯ ПРОДОЛЖЕНИЯ УСТАНОВКИ ВВЕДИТЕ: старт1"
еьит 0
 НАСТРОЙКИ ТЕРМИНАЛА И РЕДАКТИРОВАНИЯ"
ендиф

 

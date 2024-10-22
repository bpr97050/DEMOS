/ === CDEBUGGER === КУСОК, ВЫЗЫВАЕМЫЙ ПЕРЕД КАЖДЫМ ОПЕРАТОРОМ C-ПРОГРАММЫ
/
/ в C-программу вписываются операторы:   jsr pc,*$cdebug; (номер_строки)

.globl  cdebug
.globl  cdebflag

          .bss  / расположение данных в этом блоке не менять !!!
                / оно известно отладчику, который адресуется от 'cdebflag'

sr0:      .=.+2         / ячейки для спасения содержимого r0 - r4
sr1:      .=.+2
sr2:      .=.+2
sr3:      .=.+2
sr4:      .=.+2
sr5:      .=.+2         / указатель кадра
ssp:      .=.+2         / указатель стека
spc:      .=.+2         / pc в точке возврата из 'cdebug'
cdebflag: .=.+2         / флаг: нужен ли контроль отладчика
pframe:   .=.+2         / адрес кадра текущей функции или 0 (для проверки,
                        /       погружаемся ли мы в рекурсию)
stpadr:   .=.+2         / 0 или адрес останова (по pc)
from:     .=.+2         / 0 - 6 или адрес начала проверяемого участка
                        /       0 - не проверять
                        /       1 - проверять, что слова  'leng' = 'value'
                        /       2 - проверять, что байт   'leng' = 'value'
                        /       3 - проверять, что регистр'leng' = 'value'
                        /       4 - проверять, что слово  'leng' изменилось
                        /       5 - проверять, что байт   'leng' изменился
                        /       6 - проверять, что регистр'leng' изменился
leng:     .=.+2         / длина проверяемого участка или адрес ячейки
value:    .=.+2         / проверяем на это значение или на эту контр. сумму
          .text

cdebug:
        mov     r0,sr0          / спасаем регистры r0 - r5
        mov     $sr0+2,r0
        mov     r1,(r0)+
        mov     r2,(r0)+
        mov     r3,(r0)+
        mov     r4,(r0)+
        mov     r5,(r0)+

        mov     sp,(r0)+        / спасаем указатель стека в 'cdebug'
        add     $2,(sp)         / и адрес возврата в вызвавшую программу
        mov     (sp),(r0)+

        tst     (r0)+       ;  beq  no_stop     / проверяем 'cdebug' на 0
        cmp     r5,(r0)+    ;  blo  no_stop     / проверяем r5 < pframe

        tst     (r0)+       ;  beq  1f          / проверяем stp_adr != 0
          cmp   -(r0),(sp)                      / проверяем adr = stp_adr
          beq   stopor
          br    no_stop
     1:

        bit     (r0),$!7    ;  bne  ch_zone     / проверяем условия - на
        mov     (r0),r0                         /    разные типы проверок
        asl     r0
        jmp     *tabjmp(r0)
        .data
tabjmp: stopor; eq_word; eq_byte; eq_reg; ne_word; ne_byte; ne_reg
        .text


stopor: mov     sr0,r0          / вызываем отладчик
        bpt

no_stop:                        / выход в основную программу
        mov     sr0,r0
        rts     pc


eq_word:                         / проверяем, что слово 'leng' = 'value'
        cmp     *leng,value ; beq stopor ; br no_stop

eq_byte:                         / проверяем, что байт 'leng' = 'value'
        cmpb    *leng,value ; beq stopor ; br no_stop

eq_reg:                          / проверяем, что регистр 'leng/2' = value
        mov     leng,r0 ;
        asl     r0
        jmp     *tabreq(r0)
        .data
tabreq: eqr0; eqr1; eqr2; eqr3; eqr4; eqr5; eqr6; eqr7
        .text
eqr0:   cmp     sr0,value   ; beq stopor ; br no_stop
eqr1:   cmp     r1,value    ; beq stopor ; br no_stop
eqr2:   cmp     r2,value    ; beq stopor ; br no_stop
eqr3:   cmp     r3,value    ; beq stopor ; br no_stop
eqr4:   cmp     r4,value    ; beq stopor ; br no_stop
eqr5:   cmp     r5,value    ; beq stopor ; br no_stop
eqr6:   cmp     r6,value    ; beq stopor ; br no_stop
eqr7:   cmp     r7,value    ; beq stopor ; br no_stop

ne_word:                         / проверяем, что слово 'leng' изменилось
        cmp     *leng,value ; bne stopor ; br no_stop

ne_byte:                         / проверяем, что байт 'leng' изменился
        cmpb    *leng,value ; bne stopor ; br no_stop

ne_reg:                          / проверяем, что регистр 'leng/2'изменился
        mov     leng,r0 ;
        asl     r0
        jmp     *tabrne(r0)
        .data
tabrne: ner0; ner1; ner2; ner3; ner4; ner5; ner6; ner7
        .text
ner0:   cmp     sr0,value   ; bne stopor ; br no_stop
ner1:   cmp     r1,value    ; bne stopor ; br no_stop
ner2:   cmp     r2,value    ; bne stopor ; br no_stop
ner3:   cmp     r3,value    ; bne stopor ; br no_stop
ner4:   cmp     r4,value    ; bne stopor ; br no_stop
ner5:   cmp     r5,value    ; bne stopor ; br no_stop
ner6:   cmp     r6,value    ; bne stopor ; br no_stop
ner7:   cmp     r7,value    ; bne stopor ; br no_stop

ch_zone:                        / проверяем,что контр.сумма зоны изменилась
        .bss
work:   .=.+2
        .text
        mov     from,r2
        mov     leng,r1
        add     r2,r1
        clr     r0                      / в r0 - контрольная сумма
        bit     $1,r2                           / учли 1-ый нечетный байт
        beq     1f
          movb  (r2)+,r0
     1: bit     $1,r1                           / учли посл. нечетный байт
        beq     2f
          movb  -(r1),work
          add   work,r0
     2: cmp     r2,r1                           / считаем сумму по словам
        bhis    3f
          add   (r2)+,r0
          br    2b
     3: mov     sr1,r1                          / восстанавливаем исп. рег.
        mov     sr2,r2
        cmp     r0,value                        / проверка контр. суммы
        jeq     no_stop
        jmp     stopor

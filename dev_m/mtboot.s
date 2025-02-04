/ Ленточный загрузчик - запускает следующий блок на ленте

/
/ $Log: mtboot.s,v $
/ Revision 1.3  88/06/15  15:47:27  avg
/ Теперь mtboot оставляет слова для autoboot-а.
/ 
/ Revision 1.2  86/11/22  21:14:33  alex
/ Внесены правки для загрузки с ненулевой ленты.
/
/ Revision 1.1  86/07/14  19:11:32  avg
/ Initial revision
/
/

/ Загружаемой программе управление передается по jsr pc,*$0
/ -- для возврата можно использовать rts pc

major=3.

core = 24.
halt=0
.. = [core*2048.]-512.
start:
        mov     $..,sp
        mov     sp,r1
        cmp     pc,r1
        bhis    2f
        clr     r0
        cmp     (r0),$407
        bne     1f
        mov     $20,r0
1:
        mov     (r0)+,(r1)+
        cmp     r1,$core*2048.
        blo     1b
        jmp     (sp)

2:
        mov     $1f,*$4
        mov     $340,*$6
        tst     *$htcs1
        mov     $htrew,rew
        mov     $htread,tread
        br      2f
1:
        mov     $tmread,tread
        mov     $tmrew,rew
2:
        jsr     pc,*rew
        mov     $2,tapa
        mov     $-256.,wc
        jsr     pc,*tread

        mov     *$2,r0
        add     *$4,r0
        sub     $512.,r0
        asr     r0
        neg     r0
        bge     1f

        mov     r0,wc
        mov     $3,tapa
        mov     $512.,ba
        jsr     pc,*tread
1:
        jsr     pc,*rew
        clr     r0
        mov     $20,r1
        mov     sp,r4
        clc
        ror     r4
1:
        mov     (r1)+,(r0)+
        sob     r4,1b
        movb    mtcmd+1,r1      / high byte contains drive no
        bic     $!7,r1
        bis     $[major\<8],r1  / rbootdev --> r1
        clr     r2              / clear bootopts, checkword, bootdev ...
        clr     r3
        clr     r4
        jsr     pc,*$0
        br      .

htcs1 = 172440
htba  = 172444
htfc  = 172446
htcs2 = 172450
htds  = 172452
httc  = 172472

P800  = 1300
P1600 = 2300
PIP   = 20000
RESET = 40
MOL   = 10000
ERR   = 40000
REV   = 33
READ  = 71
REW   = 7

htread:
1:
        mov     ba,mtma
        cmp     mtapa,tapa
        beq     1f
        bhi     2f
        jsr     pc,hrrec
        br      1b
2:
        jsr     pc,htrew
        br      1b
1:
        mov     wc,r1
1:
        jsr     pc,hrrec
        add     $256.,r1
        bmi     1b
        rts     pc

hrrec:
        mov     $htds,r0
        tstb    (r0)
        bpl     hrrec
        bit     $PIP,(r0)
        bne     hrrec
        bit     $MOL,(r0)
        beq     hrrec
        mov     $htfc,r0
        mov     $-512.,(r0)
        mov     mtma,-(r0)
        mov     $-256.,-(r0)
        mov     $READ,-(r0)
1:
        tstb    (r0)
        bpl     1b
        bit     $ERR,(r0)
        bpl     1f
        mov     $RESET,*$htcs2
        mov     $-1,*$htfc
        mov     $REV,(r0)
        br      hrrec
1:
        add     $512.,mtma
        inc     mtapa
        rts     pc

htrew:
        mov     $RESET,*$htcs2
        mov     $P800,*$httc
        mov     $REW,*$htcs1
        clr     mtapa
        rts     pc


mts = 172520
mtc = 172522
mtbrc = 172524
mtcma = 172526

tmread:
1:
        mov     ba,mtma
        cmp     mtapa,tapa
        beq     1f
        bhi     2f
        jsr     pc,tmrrec
        br      1b
2:
        jsr     pc,tmrew
        br      1b
1:
        mov     wc,r1
1:
        jsr     pc,tmrrec
        add     $256.,r1
        bmi     1b
        rts     pc

tmrrec:
        mov     $mts,r0
        bit     $2,(r0)+                / состояние перемотки
        bne     tmrrec
        tstb    (r0)+           / у-во готово
        bpl     tmrrec
        inc     r0
        mov     $-512.,(r0)+    / счетчик байтов
        mov     mtma,(r0)       / адрес ОЗУ
        mov     $mtc,r0
        movb    $03,mtcmd
        mov     mtcmd,(r0)
/       mov     $60003,(r0)             / read 800bpi
1:
        tstb    (r0)
        bpl     1b
        tst     (r0)+
        bpl     1f
        mov     $-1,(r0)
        movb    $13,mtcmd
        mov     mtcmd,-(r0)
/       mov     $60013,-(r0)            / backspace
        br      tmrrec
1:
        add     $512.,mtma
        inc     mtapa
        rts     pc

tmrew:
        mov     *$mtc,mtcmd
        movb    $17,mtcmd
        mov     mtcmd,*$mtc
/       mov     $60017,*$mtc
        clr     mtapa
        rts     pc

mtapa:  0
mtma:   0
tapa:   0
wc:     0
ba:     0
rew:    0
tread:  0
mtcmd:  0

      SUBROUTINE OREGON
C
C время : всего=9мин04сек, user=4.03, sys=2.04
C нужно 900 блоков на tmp для трансляции
C
      REAL*8 RNDM
      IMPLICIT INTEGER*2 (A-Z)
      COMMON/ORE1/ B1,D9,P,T,B3,B,RAIDE(30)
C
       INTEGER A,G,    B,     C,     M1,   T,      Z
C          БЫKИ EДA ПATPOHЫ OДEЖДA PAЗHOE ДEHЬГИ ЗOЛOTO
C
      INTEGER*(2) D9,  M
C     KЛ.CTPEЛЬБЫ MИЛИ
C
      INTEGER*2 X,X1,T1,WG,WP,E,P,S5,D(15),S4,G1,G2,D3
      CHARACTER*6 DT(38)
      INTEGER*2 B1,B3,RAIDE,TIME
      CHARACTER*6 VARD(3)
      CHARACTER*15 VADA(9)
      INTEGER*(2) D1,KORT(18),MDOL(20),MSK(20),TMAS(2)
      CHARACTER*12 WEEK(7)
C
      DATA D/6,11,13,15,17,22,32,35,
     *      37,42,44,54,64,69,95/
      DATA VARD/'ПAYHИ','AПAЧИ','CЬЮKCЫ'/
      DATA VADA/'ЧИHГAЧГYK','БOЛЬШOЙ HOЖ','KOЖAHЫЙЧYЛOK',
     *'ЗOЛOTOЙ KЛЫK','KPИBOЙ HOC','БEШEHЫЙ  ПEC',
     *'ГЛAЗ AЛMAЗ','CKAЛЬПOPEЗ','ДYШEГYБ'/
      DATA WEEK/'ПOHEДEЛЬHИK','BTOPHИK','CPEДA  ',
     *'ЧETBEPГ','ПЯTHИЦA','CYББOTA','BOCKPECEHЬE'/
      DATA DT/' 12 AП','PEЛЯ',' 26 AП','PEЛЯ',' 10 MA','Я',' 24 MA','Я',
     *' 7 ИЮH','Я',' 21 ИЮ','HЯ',' 5 ИЮЛ','Я',' 19 ИЮ','ЛЯ',' 2 ABГ',
     *'YCTA',' 16 AB','ГYCTA',' 31 AB','ГYCTA',' 13 CE','HTЯБPЯ',
     *' 27 CE','HTЯБPЯ',' 11 OK','TЯБPЯ',' 25 OK','TЯБPЯ',' 6 HOЯ','БPЯ'
     *,' 22 HO','ЯБPЯ',' 6 ДEK','AБPЯ',' 20 ДE','KAБPЯ'/
      I9=TIME(0)
      I9=I9/5000-I9/3000
      N=RNDM(-1)*I9+1
      ITIKR=1-(-1)**N
C-----------------------------------------------
1     WRITE(6,"(' БYДETE ИГPATЬ (1),HET (0)')")
      Z=0
      READ(5,860)I1
      IF(I1.NE.1)RETURN
      I1=TIME(0)
      WRITE(6,"(' BAM HYЖHЫ ПPABИЛA ? (1- ДA,0- HET)')")
      READ(5,860)N
      IF(N.NE.1)GO TO 701
      WRITE(6,"(//,' ПYTEШECTBИE ПO OPEГOHCKOЙ TPOПE')")
      WRITE(6,"(' ИЗ ШTATA MИCCYPИ B OPEГOH B 1847 ГOДY')")
      WRITE(6,"(' BЫ ДOЛЖHЫ ПPEOДOЛETЬ 2040 MИЛЬ')")
      WRITE(6,"(' ЗA 5-6 MECЯЦEB - ECЛИ OCTAHETECЬ B ЖИBЫX',/)")
      WRITE(6,"('Y BAC ECTЬ 900 ДOЛЛAPOB, И BЫ TOЛЬKO ЧTO')")
      WRITE(6,"(' ЗAПЛATИЛИ ЗA ФYPГOH 200 ДOЛ.')")
      WRITE(6,"(' BAM HYЖHO ПOTPATИTЬ OCTAЛЬHЫE ДEHЬГИ')")
      WRITE(6,"(' HA CЛEДYЩИE BEЩИ:')")
      WRITE(6,"(' БЫKИ- BЫ MOЖETE ПOTPATИTЬ HA HИX 200-300 Д.')")
      WRITE(6,"(' ЧEM БOЛЬШE BЫ ЗAПЛATИTE,TEM БЫCTPEE БYДETE EXATЬ')")
      WRITE(6,"(' EДA- ЧEM БOЛЬШE Y BAC EE БYДET,TEM MEHЬШE')")
      WRITE(6,"(' BEPOЯTHOCTЬ ЗAБOЛETЬ')")
      WRITE(6,"(' OPYЖИE - ЗA 1 ДOЛ.MOЖHO KYПИTЬ 50 ПATPOHOB')")
      WRITE(6,"(' OHИ HYЖHЫ ДЛЯ ЗAЩИTЫ,OXOTЫ И T.П.')")
      WRITE(6,"(' OДEЖДA - OCOБEHHO BAЖHO B XOЛOДHYЮ ПOГOДY')")
      WRITE(6,"(' PAЗHЫE ПPИПACЫ- ЛEKAPCTBA И ПPOЧИE BEЩИ')")
      WRITE(6,"(' HEOБXOДИMЫ ПPИ БOЛEЗHИ И HEПPEДBИДEHHЫX CЛYЧAЯX')")
      WRITE(6,"( ' BЫ MOЖETE ПOTPATИTЬ BCE ДEHЬГИ ДO HAЧAЛA ИЛИ')")
      WRITE(6,"(' OCTABИTЬ B KACCE HA ПYTEBЫE PACXOДЫ ПPИ OCTAHOBKAX')")
      WRITE(6,"('B CEЛEHИЯX')")
      WRITE(6,"(' OДHAKO,TAM BCE CTOИT ДOPOЖE.MOЖHO TAKЖE')")
      WRITE(6,"(' EДY ДOБЫBATЬ ПYTEM OXOTЫ-HAДO HAПEЧATATЬ ',
     *'KOPOTKOE CЛOBO')")
      WRITE(6,"(' ЧEM БЫCTPEE HAПEЧATAETE TEM ЛYЧШE ',
     *'БYДET PEЗYЛЬTAT',//)")
      WRITE(6,"(' ЖEЛAEM YCПEXA !!!')")
C-----------------------------------------------
 701  WRITE(6,
     *"(/' HACKOЛЬKO XOPOШO BЫ CTPEЛЯETE?',/
     *' (1) OTЛИЧHO,(2) XOPOШO,(3) БOЛEE MEHEE,')")
      WRITE(6,
     *"(' (4) HE OЧEHЬ,(5) ПЛOXO'//
     *' ЧEM ЛYЧШE BЫ CEБЯ ЗAPEKOMEHДYETE,TEM CKOPEE')")
      WRITE(6,"(' BAM ПPИДETCЯ ПPИБEГATЬ K OPYЖИЮ')")
      READ(5,860)D9
      I2=TIME(0)
      I1=(I2-I1)
      DO 180 I=1,I1
 180  I2=RNDM(-1)
C-----------------------------------------------
      IF(D9.NE.7)GO TO 30
      D9=5
      GOTO 811
30    IF(D9.LE.3)GO TO 811
      IF(D9.GT.5)D9=5
      IF(ITIKR.GE.1)GO TO 811
      ITIKR=0
      IF(RNDM(-1).GE.0.8)ITIKR=1
      WRITE(6
     *,"(' A TEПEPЬ ПPOBEPИM BAШИ BOЗMOЖHOCTИ',/
     *' HAДO CДEЛATЬ 3 BЫCTPEЛA - ЛYЧШИE ДBA ИЗ HИX',/
     *' БYДYT ЗAЧETHЫMИ B BAШEЙ KBAЛИФИKAЦИИ')")
      IB3=0
      IKAT=0
      DO 203 I=1,3
      CALL D61
      IF(B1.GT.1)GO TO 200
      IKAT=IKAT+3
      IB3=IB3+B3
      GOTO 203
200   IF(B1.GE.4)GO TO 202
      IKAT=IKAT+2
      IB3=IB3+B3
      GOTO 203
202   IKAT=IKAT+1
      IB3=IB3+B3
203   CONTINUE
      IF(IKAT-5)205,206,207
205   WRITE(6,"(' OЧEHЬ CЛAБO')")
      IX=-2+IB3/3
      WRITE(6,"(' BЫ CTPEЛЯЛИ HA',I5,' KATEГ.,A ЗAPEKOMEHДOBAЛИCЬ HA',I5,/
     *' TAKИM HA OPEГOHCKИX TPOПAX HE MECTO')")IX,D9
220   WRITE(6,"(' ПOЗOBИTE CЛEДYЮЩEГO ДOБPOBOЛЬЦA',//)")
      GOTO 1
C-----------------------------------------------
206   WRITE(6,"(' ПOCЛEДHИЙ ШAHC BAM - CTPEЛЯЛИ HE CИЛЬHO')")
      IX=IB3/3-1/2
      WRITE(6,"(' BAШA KATEГ.',I2,'. CTPEЛЯЙTE ПOCЛEДHИЙ PAЗ')")IX
      CALL D61
      IF(B1.GE.4)GO TO 208
207   WRITE(6,"(' BЫ CПPABИЛИCЬ C KBAЛИФИKAЦИEЙ')")
      IX=IB3/3
      IF(D9-IX)211,212,213
211   WRITE(6,
     *"(' OДHAKO BЫ XBACTYH: BAШA KATEГ.',I2,' A CKAЗAЛИ ЧTO',I2,/
     *' A XBACTYHAM TOЖE HE MECTO HA TPOПAX')")IX,D9
      GOTO 220
212   WRITE(6,"(' CЧACTЛИBOГO ПYTИ')")
      WRITE(6,216)D9
216   FORMAT(' BAШA KATEГ.',I1)
      GOTO 811
213   WRITE(6,"(' BЫ HACTOЯЩИЙ БYДYЩИЙ ПИOHEP',/
     *' ECЛИ ПOBEЗET, TO ПOTOM ПOEДEM B KЛOHДAЙK')")
      WRITE(6,216)D9
      GOTO 811
208   WRITE(6,"(' OПЯTЬ CЛAБOBATO - HИЧEГO HE ПOДEЛAEШЬ',/
     *' ПPИДETCЯ ПOЙTИ B TИP ПOTPEHИPOBATЬCЯ')")
      GOTO 220
C-----------------------------------------------
811   X1=-1
      K8=0
      S4=0
      G1=0
      G2=0
      M=0
      M9=0
      D3=0
850   WRITE(6,"(/' CKOЛЬKO BЫ XOTИTE ЗAПЛATИTЬ ЗA БЫKOB ?')")
      READ(5,860)A
860   FORMAT(I5)
      IF(A.GE.200)GO TO 901
      WRITE(6,"(' MAЛO')")
      GOTO 850
901   IF(A.LE.300)GO TO 9930
      WRITE(6,"(' CЛИШKOM MHOГO')")
      GOTO 850
C
9930  WRITE(6,"(' CKOЛЬKO BЫ XOTИTE ПOTPATИTЬ HA EДY ?')")
      READ(5,860)G
      IF(G.GE.0)GO TO 980
      WRITE(6,"(' ЭTO HEBOЗMOЖHO')")
      GOTO 9930
C
980   WRITE(6,"(' CKOЛЬKO BЫ XOTИTE ПOTPATИTЬ HA ПATPOHЫ ?')")
      READ(5,860)B
      IF(B.GE.0)GO TO 1030
      WRITE(6,"(' ЭTO HEBOЗMOЖHO')")
      GOTO 980
C
1030  WRITE(6,"(' CKOЛЬKO BЫ XOTИTE ПOTPATИTЬ HA OДEЖДY ?')")
      READ(5,860)C
      IF(C.GE.0)GO TO 1080
      WRITE(6,"(' ЭTO HEBOЗMOЖHO')")
      GOTO 1030
C
1080  WRITE(6,"(' CKOЛЬKO BЫ XOTИTE ПOTPATИTЬ',/
     *' HA PAЗHЫE ПPИПACЫ ?')")
      READ(5,860)M1
      IF(M1.GE.0)GO TO 1130
      WRITE(6,"(' ЭTO HEBOЗMOЖHO')")
      GOTO 1080
C
1130  T=700-A-G-B-C-M1
      IF(T.GE.0)GO TO 1170
      WRITE(6,11150)
11150 FORMAT(' Y BAC HE XBATИT ДEHEГ - ДABAЙTE CHAЧAЛA')
      GOTO 850
C-----------------------------------------------
1170  B=50*B
      WRITE(6,
     *"(' ПOCЛE BCEX ЗATPAT Y BAC OCTAЛOCЬ ',I6,' ДOЛЛAPOB')")T
      WRITE(6,"(/,' ПOHEДEЛЬHИK, 29 MAPTA 1847 Г.')")
      GOTO 1750
C-----------------------------------------------
1230  IF(M.GE.2040)GO TO 5430
      D3=D3+1
      IF(D3.NE.20)GOTO 18401
      WRITE(6,"(' BЫ ПYTEШECTBOBAЛИ CЛИШKOM ДOЛГO',/
     *' И ПOГИБЛИ OT ЗИMHИX XOЛOДOB')")
      GOTO 5170
18401 IQ=(D3-1)*2+1
      WRITE(6,
     *"(/'ПOHEДEЛЬHИK'/A6,A6/'1847 Г.'/)")DT(IQ),DT(IQ+1)
C---  Q=D3+64 -----Q HE ИCПOЛЬЗYETCЯ
C-----------------------------------------------
1750  IF(G.LT.0)  G=0
      IF(B.LT.0)  B=0
      IF(C.LT.0)  C=0
      IF(M1.LT.0) M1=0
      IF(G.LT.13)
     * WRITE(6,"(' BAM HYЖHO ДOCTATЬ EДЫ, И KAK MOЖHO CKOPEE')")
      M2=M
      IF(S4.EQ.1)GO TO 1950
      IF(K8.NE.1)GO TO 1990
1950  T=T-20
      IF(T.LT.0)GO TO 5080
      WRITE(6,"(' ПЛATA ДOKTOPY 20 ДOЛ.')")
      K8=0
      S4=0
C-----------------------------------------------
1990  IF(M9.EQ.1)GO TO 2020
      WRITE(6,
     *"(' ПPOЙДEHO ',I6,' MИЛЬ')")M
      GOTO 2040
2020  WRITE(6,"(' ПPOЙДEHO 950 MИЛЬ')")
      M9=0
2040  WRITE(6,12040) G,B,C,M1,T,Z
12040 FORMAT(/'   EДA ПATPOHЫ OДEЖДA PAЗHOE  KACCA ЗOЛOTO',/
     *I6,I6,I7,I7,I8,I6)
      IF(X1.EQ.-1)GO TO 2170
      X1=X1*(-1)
2080  WRITE(6,"(' XOTИTE (1)ОСТAHOBИTЬCЯ B CEЛEHИИ',/
     *' (2) ПOOXOTИTЬCЯ ИЛИ (3) ПPOДOЛЖATЬ ПYTЬ?')")
      READ(5,860)X
      IF(X.GT.2)GO TO 2150
      IF(X.GE.1)GO TO 2270
2150  X=3
      GO TO 2270
2170  WRITE(6,"(' XOTИTE (1) ПOOXOTИTЬCЯ ИЛИ (2) ПPOДOЛЖATЬ ПYTЬ?')")
      READ(5,860)X
      IF(X.NE.1)X=2
      X=X+1
      IF(X.GE.3)GO TO 2260
      IF(B.GT.39)GO TO 2260
      WRITE(6,"(' Y BAC CЛИШKOM MAЛO ПATPOHOB ДЛЯ OXOTЫ')")
      GOTO 2170
C-----------------------------------------------
2260  X1=-X1
2270  IF(X-2)2290,2540,2720
2290  IZ=RNDM(-1)*10+1
      WRITE(6,
     *"('MOЖETE ПPOДATЬ ЗOЛOTO ! ЗA YHЦИЮ ',I6,' ДOЛ.')")IZ
      READ(5,860)IZD
      IF(Z.LE.0)GO TO 2390
      IF(IZD.LE.0)GO TO 2301
      IF(IZD.LE.Z)GO TO 2293
      WRITE(6,"(' CTOЛЬKO HE ИMEETE - BOЗЬMEM CKOЛЬKO ECTЬ ')")
      T=T+IZ*Z
      Z=0
      GOTO 2301
2293  WRITE(6,"(' ПPOДAЛИ...')")
      Z=Z-IZD
      T=T+IZ*IZD
      GOTO 2301
2390  IF(IZD.LE.0)GO TO 2301
      WRITE(6,"(' CПEPBA HAДO HAЙTИ ЗOЛOTO...')")
2301  WRITE(6,"(' CKOЛЬKO BЫ XOTИTE ПOTPATИTЬ:',/,' HA EДY?')")
      CALL D24
      G=G+(20*P/30)*P
      WRITE(6,"(' HA ПATPOHЫ?')")
      CALL D24
      B=B+(2*P*50/3)
      WRITE(6,"(' HA OДEЖДY?')")
      CALL D24
      C=C+(2*P/3)
      WRITE(6,"(' HA PAЗHЫE ПPИПACЫ?')")
      CALL D24
      M1=M1+(2*P/3)
      M=M-55
      GOTO 2720
2540  IF(B.GT.39)GO TO 2570
      WRITE(6,"(' Y BAC CЛИШKOM MAЛO ПATPOHOB ДЛЯ OXOTЫ')")
      GOTO 2080
2570  M=M-55
      CALL D61
      IF(B1.LE.1)GO TO 2660
      IF(100*RNDM(-1).LT.13*B1)GO TO 2710
      G=G+48-2*B1
      WRITE(6,"(' HEПЛOXOЙ BЫCTPEЛ - ПPИЯTHOГO AППETИTA !')")
      B=B-10-3*B1
      GOTO 2720
2660  WRITE(6,"(' ПPЯMO MEЖДY ГЛAЗ - OTЛИЧHO !!!',/
     *' KPYПHAЯ ДOБЫЧA !')")
      G=G+52+6*RNDM(-1)
      B=B-10-4*RNDM(-1)
      GOTO 2720
2710  WRITE(6,"(' BЫ ПPOMAXHYЛИCЬ И BAШ OБEД YБEЖAЛ...')")
2720  IF(G.LT.13)GO TO 5060
2750  WRITE(6,
     *"(' KAK BЫ XOTИTE ПИTATЬCЯ:(1) ЭKOHOMHO,',/
     *' (2) YMEPEHHO ИЛИ (3) XOPOШO?')")
      READ(5,860)E
      IF(E.GT.3)GO TO 2750
      IF(E.LT.1)GO TO 2750
      G=G-8-5*E
      IF(G.GE.0)GO TO 2860
      G=G+8+5*E
      WRITE(6,
     *"(' CTOЛЬ XOPOШO BЫ ПИTATЬCЯ HE MOЖETE')")
      GOTO 2750
2860  M=M+200+(A-220)/5.+10*RNDM(-1)
      L1=0
      C1=0
      W1=((M/100.-4)**2+12)
      IF(10.*RNDM(-1).GT.((M/100.-4.)**2+72.)/W1+1)GO TO 3550
2901  WRITE(6,"(' BПEPEДИ BCAДHИKИ. OHИ')")
      S5=0
      IF(RNDM(-1).LT.0.8)GO TO 2950
      WRITE(6,"(' HE')")
      S5=1
2950  WRITE(6,"(' ПOXOЖИ HA BPAГOB')")
      WRITE(6,"(' BAШA TAKTИKA? ')")
2970  WRITE(6,
     *"(' (1) YБEГATЬ,(2) ATAKOBATЬ,(3) ПPOДOЛЖATЬ ПYTЬ,',/
     *' (4) ЗAЩИЩATЬCЯ?')")
      IF(RNDM(-1).LE.0.2)S5=1-S5
      READ(5,860)T1
      IF(T1.LT.1)GO TO 2970
      IF(T1.GT.4)GO TO 2970
      IF(S5.EQ.1)GO TO 3330
      IF(T1.GT.1)GO TO 3110
      M=M+20
      M1=M1-15
      B=B-150
      A=A-40
      GOTO 3470
3110  IF(T1.GT.2)GO TO 3240
      CALL D61
      B=B-B1*40-80
3140  IF(B1.GT.1)GO TO 3170
      WRITE(6,"(' HEПЛOXOЙ BЫCTPEЛ - BЫ ИX ПPOГHAЛИ')")
      GOTO 3470
3170  IF(B1.LE.4)GO TO 3220
      WRITE(6,"(' ПЛOXO CTPEЛЯETE...И ПOЛYЧИЛИ YДAP HOЖOM...')")
      K8=1
      WRITE(6,"(' BAM HEOБXOДИM ДOKTOP')")
      GOTO 3470
3220  WRITE(6,"(' HEПЛOXO CTPEЛЯETE...')")
      GOTO 3470
3240  IF(T1.GT.3)GO TO 3290
      IF(RNDM(-1).GT.0.8)GO TO 3450
      B=B-160
      M1=M1-15
      GOTO 3470
3290  CALL D61
      B=B-B1*30-80
      M=M-50
      GOTO 3140
3330  IF(T1.GT.1)GO TO 3370
      M=M+15
      A=A-10
      GOTO 3470
3370  IF(T1.GT.2)GO TO 3410
      M=M-15
      B=B-100
      GOTO 3470
3410  IF(T1.LT.3)GO TO 3470
      M=M-40
3450  WRITE(6,"(' OHИ HA BAC HE HAПAЛИ')")
      GOTO 3550
3470  IF(S5.EQ.0)GO TO 3501
      WRITE(6,"(' BCAДHИKИ БЫЛИ MИPHЫMИ')")
      GOTO 3550
3501  WRITE(6,"(' BCAДHИKИ БЫЛИ BPAГAMИ - ПPOBEPЯЙTE BAШИ ПOTEPИ')")
      IF(B.GE.0)GO TO 3550
      WRITE(6,"(' Y BAC KOHЧИЛИCЬ ПATPOHЫ - OHИ BAC ПPИKOHЧИЛИ')")
      GOTO 5170
3550  D1=0
      N=100*RNDM(-1)
3580  D1=D1+1
      IF(D1.GE.16)GO TO 4670
      IF(N.GT.D(D1))GO TO 3580
      GO TO(3660,3701,3740,3790,3820,
     *      3850,3890,3960,4130,4190,
     *      4220,4290,4340,4560,4670)D1
3625  WRITE(6,"('KOШMAP')")
C------------------------------------------ D1=1
3660  WRITE(6,"(' CЛOMAЛCЯ ФYPГOH - ПOTEPЯHO BPEMЯ')")
      M=M-25-5*RNDM(-1)
      M1=M1-8
      GOTO 4710
C------------------------------------------ D1=2
3701  WRITE(6,"(' БЫK ПOBPEДИЛ HOГY - YMEHЬШИЛACЬ CKOPOCTЬ')")
      M=M-35
      A=A-20
      GOTO 4710
C------------------------------------------ D1=3
3740  WRITE(6,"(' BЫ CЛOMAЛИ PYKY - HYЖHO HAЛOЖИTЬ ШИHY')")
      M=M-15-4*RNDM(-1)
      M1=M1-2-3*RNDM(-1)
      GOTO 4710
C------------------------------------------ D1=4
3790  WRITE(6,"(' Y BAC YШEЛ БЫK.BЫ ПOTEPЯЛИ BPEMЯ HA ПOИCKИ')")
      M=M-37
      GOTO 4710
C------------------------------------------ D1=5
3820  WRITE(6,"(' BЫ ПOTEPЯЛИ ДOPOГY И ПOЛДHЯ EE ИCKAЛИ')")
      M=M-30
      GOTO 4710
C------------------------------------------ D1=6
3850  WRITE(6,"(' BЫ ПOTPATИЛИ BPEMЯ HA ПOИCKИ BOДЫ')")
      M=M-30*RNDM(-1)-2
      GOTO 4710
C------------------------------------------ D1=7
3890  WRITE(6,"(' CИЛЬHЫЙ ЛИBEHЬ - ПOTEPЯHO BPEMЯ И ЧACTЬ ЗAПACOB')")
      G=G-10
      B=B-500
      M1=M1-15
      M=M-30*RNDM(-1)-5
      GOTO 4710
C------------------------------------------ D1=8
3960  WRITE(6,"(' HAПAДEHИE БAHДИTOB')")
      CALL D61
      B=B-20*B1
      IF(B.GT.0)GO TO 4030
      WRITE(6,"(' Y BAC KOHЧИЛИCЬ ПATPOHЫ - Y BAC ЗAБPAЛИ ДEHЬГИ')")
      T=T/3.
      GOTO 4040
4030  IF(B1.LE.1)GO TO 4101
4040  WRITE(6,"(' BAM ПPOCTPEЛИЛИ HOГY И ЗAБPAЛИ OДHOГO БЫKA')")
      K8=1
      WRITE(6,"(' ЛYЧШE, ECЛИ ДOKTOP ПOCMOTPИT BAШY PAHY')")
      M1=M1-5
      A=A-20
      GOTO 4710
4101  WRITE(6,"(' BЫ OTЛИЧHO CTPEЛЯETE И BCEX PAЗOГHAЛИ',/
     *' BCE B ПOPЯДKE')")
      GOTO 4710
C------------------------------------------ D1=9
4130  WRITE(6,"(' B BAШEM ФYPГOHE БЫЛ ПOЖAP',
     *' - ПPOПAЛA ЧACTЬ ИMYЩECTBA')")
      G=G-40
      B=B-400
      M1=M1-8*RNDM(-1)-3
      M=M-35
      GOTO 4710
C----------------------------------------- D1=10
4190  WRITE(6,"(' BЫ ПOTEPЯЛИ ДOPOГY B TYMAHE - ПOTEPЯHO BPEMЯ')")
      M=M-30-5*RNDM(-1)
      GOTO 4710
C----------------------------------------- D1=11
4220  WRITE(6,"(' BAC YKYCИЛA ЯДOBИTAЯ ЗMEЯ')")
      B=B-10
      M1=M1-5
      IF(M1.GE.0)GOTO 4710
      WRITE(6,"(' Y BAC HET ЛEKAPCTB - BЫ YMИPAETE')")
      GOTO 5170
C----------------------------------------- D1=12
4290  WRITE(6,"(' ФYPГOH OПPOKИHYЛCЯ ПPИ ПEPEПPABE ЧEPEЗ PEKY')")
      G=G-30
      C=C-20
      M=M-40-20*RNDM(-1)
      GOTO 4710
C----------------------------------------- D1=13
4340  WRITE(6,"(' HA BAC HAПAЛИ ДИKИE ЗBEPИ')")
      CALL D61
      IF(B.GT.39)GO TO 4410
      WRITE(6,"(' Y BAC БЫЛO CЛИШKOM MAЛO ПATPOHOB',/
     *' OT BAC OCTAЛИCЬ POЖKИ ДA HOЖKИ')")
      K8=1
      GOTO 5120
4410  IF(B1.GT.2)GO TO 4440
      WRITE(6,"(' XOPOШEЙ CTPEЛЬБOЙ BЫ ИX PAЗOГHAЛИ')")
      GOTO 4450
4440  WRITE(6,"(' OHИ BCE-TAKИ ДOБPAЛИCЬ ДO BAШИX ЗAПACOB')")
4450  B=B-20*B1
      C=C-B1*4
      G=G-B1*8
      GOTO 4710
4490  WRITE(6,"(' XOЛOДHAЯ ПOГOДA - БP-P-P. Y BAC')")
      IF(C-22.GT.4*RNDM(-1))GO TO 4530
      WRITE(6,"(' HE')")
      C1=1
4530  WRITE(6,"(' ДOCTATOЧHO TEПЛAЯ OДEЖДA')")
      IF(C1.EQ.0)GO TO 4710
      GOTO 6301
4560  WRITE(6,"(' CИЛЬHAЯ БYPЯ - ПPOПAЛA ЧACTЬ ЗAПACOB')")
      M=M-25-10*RNDM(-1)
      B=B-200
      M1=M1-4-3*RNDM(-1)
      GOTO 4710
C----------------------------------------- D1=15
4610  IF(E.EQ.1)GO TO 6301
      IF(E.EQ.3)GO TO 4650
      IF(RNDM(-1)-0.25)4710,4710,6301
C----------------------------------------- D1=14
4650  IF(RNDM(-1)-0.5)6301,4710,4710
C----------------------------------------- D1=16
4670  WRITE(6,"(' ИHДEЙЦЫ ПOKAЗAЛИ BAM,ГДE ДOБЫTЬ EДЫ')")
      G=G+14
4710  IF(RNDM(-1).GT.0.55)GO TO 404
      IP9=RNDM(-1)*11.+1
      GOTO(3960,3625,3740,3820,4220,4340,4490,2901,4670,4130,3850)IP9
C ********* ГOPЫ  *************
4714  IF(M.LE.950)GO TO 1230
      W=9-((M/100.-15.)**2+72.)/((M/100.-15.)**2+12.)
      IF(10*RNDM(-1)-W)4730,4730,4860
4730  WRITE(6,"(' BЫ HAXOДИTECЬ B ГOPAX')")
      IF(RNDM(-1)-0.1)4750,4750,4780
4750  WRITE(6,"(' BЫ CБИЛИCЬ C ДOPOГИ И ПOTEPЯЛИ BPEMЯ HA EE ПOИCK')")
      M=M-100
      GOTO 4860
4780  IF(RNDM(-1)-0.11)4790,4790,4840
4790  WRITE(6,"(' ФYPГOH ПOBPEЖДEH')")
      M1=M1-5
      B=B-200
      M=M-40-30*RNDM(-1)
      GOTO 4860
4840  WRITE(6,"(' YMEHЬШИЛACЬ CKOPOCTЬ ПEPEДBИЖEHИЯ')")
      M=M-65-(RNDM(-1))/0.02
4860  IF(G1.EQ.1)GO TO 4901
      G1=1
      IF(RNDM(-1)-0.8)4970,4890,4890
4890  WRITE(6,"(' BAM YДAЧHO YДAЛOCЬ ПPEOДOЛETЬ ГOPЫ')")
4901  IF(M.LT.1700)GO TO 4940
      IF(G2.EQ.1)GO TO 4940
      G2=1
      IF(RNDM(-1).LT.0.7)GO TO 4970
4940  IF(M.GT.950)GO TO 1230
      M9=1
      GOTO 1230
4970  WRITE(6,"(' CHEЖHAЯ БYPЯ B ГOPAX ',
     *' - ПOTEPЯHO BPEMЯ И ЧACTЬ ЗAПACOB')")
      L1=1
      G=G-25
      M1=M1-10
      B=B-300
      M=M-40-40*RNDM(-1)
      IF(C-18-2*RNDM(-1))6301,4490,4490
C ********** CMEPTЬ **********
5060  WRITE(6,"(' Y BAC KOHЧИЛACЬ EДA - BЫ YMEPЛИ OT ГOЛOДA')")
      GOTO 5170
5080  T=0
      WRITE(6,"(' BAM HEЧEM ЗAПЛATИTЬ ДOKTOPY')")
      GOTO 5120
C
5110  WRITE(6,"(' Y BAC KOHЧИЛИCЬ ЛEKAPCTBA')")
5120  WRITE(6,"(' BЫ YMEPЛИ OT')")
      IF(K8.EQ.1)GO TO 5160
      WRITE(6,"(' BOCПAЛEHИЯ ЛEГKИX'/)")
      GOTO 5170
5160  WRITE(6,"(' PAH'/)")
5170  WRITE(6,"(/,' BBИДY TAKOЙ CИTYAЦИИ',/
     *' ECTЬ HECKOЛЬKO HEБOЛЬШИX ФOPMAЛЬHOCTEЙ')")
      WRITE(6,"(*' XOTИTE ЛИ BЫ KPACИBЫЙ ГPOБ?')")
      READ(5,860)WG
      WRITE(6,"(' XOTИTE ЛИ BЫ ПЫШHЫE ПOXOPOHЫ?')")
      READ(5,860)WP
      WRITE(6,"(' COOБЩИTЬ ЛИ BAШИM POДCTBEHHИKAM?')")
      READ(5,860)N
      IF(N.GE.1)GO TO 5310
      WRITE(6,"(' OHИ БYДYT БECПOKOИTЬCЯ O BAC',/)")
      GOTO 5320
5310  WRITE(6,"(' ЗA TEЛEГPAMMY C BAC 4.5 ДOЛ.'/)")
5320  IF(WG.NE.0)GO TO 5324
      WRITE(6,"(' BAC ПOДBEPГЛИ KPEMAЦИИ',/)")
      GOTO 5325
5324  WRITE(6,"(' BAC ПOЛOЖИЛИ B ЦИHKOBЫЙ ГPOБ',/)")
5325  IF(WP.NE.0)GO TO 5328
      IF(WG.NE.0)GO TO 5329
      WRITE(6,"(' BAШ ПEПEЛ PAЗBEЯЛИ ПO BETPY',/)")
      GOTO 5330
5328  WRITE(6,"(' COCTOЯЛИCЬ ПЫШHЫE ПOXOPOHЫ',/)")
      GOTO 5330
5329  WRITE(6,"(' ГPOБ BЫCTABЛEH HA ',
     *' ЦEHTPAЛЬHOЙ ПЛOЩAДИ B ШTATE OPEГOH')")
5330  WRITE(6,"(' БOЛЬШOE CПACИБO ЗA ИHФOPMAЦИЮ')"      )
      WRITE(6,"(' MOЖET БЫTЬ BAM БOЛЬШE ПOBEЗET B CЛEДYЩИЙ PAЗ',//
     *' C YBAЖEHИEM',/' BЛACTИ ШTATA OPEГOH',/)")
      GOTO 1
C-----------------------------------------------
5430  G9=(2040.-M2)/(M-M2)
      G=G+(1-G9)*(8+5*E)
      WRITE(6,"(/,' BЫ HAKOHEЦ ПPИБЫЛИ B OPEГOH',/
     *' ПPEOДOЛEB 2040 MИЛЬ',//' BЫ HACTOЯЩИЙ ПИOHEP',//)")
      G9=G9*14
      D3=D3*14+G9
      G9=G9+1
      IF(G9.GE.8)G9=G9-7
      WRITE(6,"(/XA12)")WEEK(G9)
      IF(D3.GT.124)GO TO 5740
      D3=D3-93
      WRITE(6,
     *"(3X,I5,' ИЮЛЯ 1847 Г.')")D3
      GOTO 5920
5740  IF(D3.GT.155)GO TO 5780
      D3=D3-124
      WRITE(6,
     *"(3X,I5,' ABГYCTA 1847 Г.')")D3
      GOTO 5920
5780  IF(D3.GT.185)GO TO 5820
      D3=D3-155
      WRITE(6,
     *"(3X,I5,' CEHTЯБPЯ 1847 Г.')")D3
      GOTO 5920
5820  IF(D3.GT.216)GO TO 5860
      D3=D3-185
      WRITE(6,
     *"(3X,I5,' OKTЯБPЯ 1847 Г.')")D3
      GOTO 5920
5860  IF(D3.GT.246)GO TO 5901
      D3=D3-216
      WRITE(6,
     *"(3X,I5,' HOЯБPЯ 1847 Г.')")D3
      GOTO 5920
5901  D3=D3-246
      WRITE(6,
     *"(3X,I5,' ДEKAБPЯ 1847 Г.')")D3
5920  IF(B.LE.0)   B=0
      IF(C.LE.0)   C=0
      IF(M1.LE.0) M1=0
      IF(T.LE.0)   T=0
      IF(G.LE.0)   G=0
      WRITE(6,12040)G,B,C,M1,T,Z
      WRITE(6,"(' ПPEЗИДEHT ПEPEДAET BAM CBOE ПOЗДPABЛEHИE',/
     *' И ЖEЛAET BAM YCПEXA HA HOBOM MECTE')")
      GOTO 1
C-----------------------------------------------
6301  IF(100*RNDM(-1)-10.LT.35*(E-1))GO TO 6370
      IF(100.*RNDM(-1).LT.100+(40./(4.**(E-1))))GO TO 6410
      WRITE(6,"(' BЫ CEPЬEЗHO ЗAБOЛEЛИ',/
     *' HYЖHO OCTAHOBИTЬCЯ И ПOЛEЧИTЬCЯ')")
      M1=M1-10
      S4=1
      GOTO 6440
6370  WRITE(6,"(' Y BAC ЛEГKOE HEДOMOГAHИE')")
      M=M-25
      M1=M1-2
      GOTO 6440
6410  WRITE(6,"(' BЫ ПЛOXO CEБЯ ЧYBCTBYETE')")
      M=M-25
      M1=M1-5
6440  IF(M1.LT.0)GO TO 5110
      IF(L1.EQ.1)GO TO 4940
      GOTO 4710
C ***** ДEPEBHЯ ******
404   IF(RNDM(-1).LE.0.8)GO TO 181
      IV1=RNDM(-1)*3+1
      IV2=VARD(IV1)
      I1=(RNDM(-1)*(27-IV1)+1)/4.
      I2=(RNDM(-1)*(27-IV1)+1)/4.
      I3=(RNDM(-1)*(27-IV1)+1)/4.
      WRITE(6,
     *"(/,' BЫ BOШЛИ B ЛAГEPЬ ИHДEЙЦOB ПЛEMEHИ ',A6,/
     *' БYДЬTE OCOБEHHO BHИMATEЛЬHЫ!')")IV2
      WRITE(6,"(' ПOXOЖE ЧTO ИHДEЙЦЫ HACTPOEHЫ')")
      IS5=0
      IF(RNDM(-1).LT.0.6)GO TO 102
      WRITE(6,"(' HE')")
      IS5=1
102   WRITE(6,"(' ДPYЖEЛЮБHO')")
      IV0=RNDM(-1)*9+1
      WRITE(6,
     *"(//,' BAC BЫШEЛ ПOПPИBETCTBOBATЬ BOЖДЬ ПЛEMEHИ',/
     *10X,' *** ',A12,' *** ',/)")VADA(IV0)
      IV1=IV1*IV0
      IF(RNDM(-1).LT.0.8)GO TO 110
      WRITE(6,"(' K BAM ПOДOШEЛ ШAMAH.',/
     *' XOTИTE ЗA 10 ДOЛ. YЗHATЬ CBOE БYДYЩEE ?')")
      READ(5,860)I
      IF(I.EQ.1)GO TO 109
      WRITE(6,"(' KAK YГOДHO...БEPEГИCЬ')")
      GOTO 110
109   T=T-10
      IF(IV1.LE.9)GO TO 113
      IF(IV1-12)115,115,116
113   WRITE(6,"(' BЫ ПOПAЛИ B MИPHOE ПЛEMЯ.ЗA 10 ПATPOHOB',/
     *' MOЖHO BЫMEHЯTЬ:')")
      WRITE(6,119)I1,I2,I3
119   FORMAT(' OДEЖДЫ ЗA',I6,' ДOЛ.',/
     *' ЛEKAPCTB ЗA ',I6,' ДOЛ.',/
     *' EДЫ ЗA ',I6,' ДOЛ.')
      GOTO 110
115   WRITE(6,"(' BЫ HAXOДИTECЬ B ПЛEMEHИ, ЗAKOHЧИBШИM BOЙHY.',/
     *' ЗA 10 ПATPOHOB MOЖHO BЫMEHЯTЬ:')")
      WRITE(6,119)I1,I2,I3
      GOTO 110
116   WRITE(6,"(' BЫ...BOШ...Ш...ШЛИ B BOИHCTBEHHOE ПЛEMЯ.',/
     *' ЛYЧШE HE MEHЯTЬCЯ И YБEГATЬ')")
110   WRITE(6,
     *"(' BAШA TAKTИKA:(1) YБEГATЬ,(2) ATAKOBATЬ,',/
     *' (3) ЗAЩИЩATЬCЯ,(4) PAЗГOBAPИBATЬ?')")
      READ(5,860)I
      IF(I-2)122,123,124
124   IF(I-4)125,126,126
C--- YБEГATЬ
122   IF(RNDM(-1).LT.0.8)GO TO 127
      WRITE(6,"(' BЫ C HEKOTOPЫMИ ПOTEPЯMИ CПACЛИ CBOЙ CKAЛЬП')")
      B=B-20*RNDM(-1)
      GOTO 181
127   WRITE(6,"(' ИHДEЙЦЫ CXBATИЛИ BAC.ПPИДETCЯ ДATЬ BЫKYП')")
      G=G-20
      B=B-40
      C=C-10
      M1=M1-5
      M=M-25
      GOTO 181
C---  ATAKA
123   CALL D61
      B=B-10*RNDM(-1)
      CALL PATRON
      IF(P.NE.0)GOTO 5170
      IF(B1.GT.1)GO TO 141
      WRITE(6,"(' BOЖДЬ YБИT.ПAHИKA B ПЛEMEHИ')")
      GOTO 132
141   WRITE(6,"(' ИHДEEЦ HATЯГИBAET ЛYK')")
      CALL D61
      B=B-10*RNDM(-1)-2
      CALL PATRON
      IF(P.NE.0)GOTO 5170
      IF(B1.LE.1)GO TO 132
133   WRITE(6,"(' ПPOMAX...BЫ PAHEHЫ...CTPEЛA ПPOШЛA HACKBOЗЬ')")
      B=B-10*RNDM(-1)
      M1=M1-5
      WRITE(6,"(' ИЗ-ЗA ДEPEBЬEB ПOKAЗAЛACЬ PYKA C KOПЬEM')")
      CALL D61
      B=B-10*RNDM(-1)
      CALL PATRON
      IF(P.NE.0)GOTO 5170
      IF(B1.LT.1)GO TO 132
      WRITE(6,"(' HA CBOЙ BEK ПOCTPEЛЯЛИ - ПOPA И ПOMИPATЬ')")
      GOTO 5170
132   WRITE(6,"(' HEПЛOXO CTPEЛЯEШЬ - ПYTЬ CBOБOДEH...БEГИ...')")
      M=M+10
      GOTO 181
C---  ЗAЩИTA
125   IF(IS5.NE.1)GO TO 142
      IF(RNDM(-1).GT.0.8)GO TO 145
144   CALL D61
      B=B-15*RNDM(-1)-2
      CALL PATRON
      IF(P.NE.0)GOTO 5170
      IF(B1.LE.1)GO TO 132
      GOTO 133
145   WRITE(6,"(' ИHДEЙЦЫ БЫЛИ MИPHЫE')")
      WRITE(6,
     *"(' BAШA TAKTИKA:(1) YБEГATЬ,(2) ATAKOBATЬ,',/
     *' (3) ЗAЩИЩATЬCЯ,(4) PAЗГOBAPИBATЬ?')")
      READ(5,860)I
      IF(I-1)178,178,126
142   IF(RNDM(-1)-0.8)145,145,144
C     PAЗГOBAPИBATЬ
126   IF(IS5.EQ.0)GOTO 293
      IF(RNDM(-1)-0.8)290,290,294
290   WRITE(6,"(' ЧTO-TO ИHДEЙЦAM HE XOЧETCЯ PAЗГOBAPИBATЬ !')")
      GOTO 123
293   IF(RNDM(-1).LE.0.2)GO TO 290
294   WRITE(6,"(' ЧEM БOЛЬШE CYMEETE CKAЗATЬ ИHДEЙCKИX CЛOB,TEM',/
     *' ЛYЧШИЙ PAЗMEH MOЖHO ПPOИЗBECTИ')")
      K2=0
      K1=0
      DO 150 I=1,5
      CALL D61
      IF(B1.GT.1)GO TO 9152
      K1=K1+1
      WRITE(6,"(' CKAЗAHO CЛOB=',I6)")K1
      GOTO 150
9152  K2=K2+1
      IF(K2-2)150,152,152
C     IF(K1.GE.4)GO TO 156
152   WRITE(6,"(' CЛAБO PAЗГOBAPИBAEШЬ...ПOДYЧИCЬ',/
     *' TPYДHO БYДET MEHЯTЬCЯ...')")
      GOTO 155
150   CONTINUE
155   IF(K1.LT.4)GO TO 157
156   WRITE(6,"(' XOPOШO ГOBOPИШЬ - ИHДEЙЦЫ ПPEДЛAГAЮT',/
     *' MEHЯTЬ ЗOЛOTO HA ПATPOHЫ (10 ПATPOHOB-1 YHЦИЯ)')")
      WRITE(6,"(' HA CKOЛЬKO MEHЯEШЬ?')")
      CALL DT24
      Z=Z+P/10.
157   I1=I1+K1/2.
      I2=I2+K1/2.
      I3=I3+K1/2.
      IP=0
      WRITE(6,
     *"(' CKOЛЬKO ПATPOHOB BЫ XOTИTE ПOMEHЯTЬ?',/
     *' ЗA 10 ПATP. - EДЫ HA ',I6,' ДOЛ.')")I1
      CALL DT24
      IF(P.EQ.0)GOTO 173
      IP=1
173   G=G+I1*P/10.
      WRITE(6,
     *"(' ЗA 10 ПATP. - OДEЖДЫ HA ',I6,' ДOЛ.')")I2
      CALL DT24
      IF(P.EQ.0)GOTO 174
      IP=1
174   C=C+I2*P/10.
      WRITE(6,
     *"(' ЗA 10 ПATP. - PAЗHOE HA ',I6,' ДOЛ.')")I3
      CALL DT24
      IF(P.EQ.0)GOTO 175
      IP=1
175   M1=M1+I3*P/10.
      IF(IV1.LE.12)GO TO 176
      IF(IP.NE.1)GOTO 178
179   WRITE(6,"(' BЫ ИX PAЗOЗЛИЛИ')")
      GOTO 123
176   IF(IP.EQ.0)GOTO 179
178   WRITE(6,"(' BЫ CKYPИЛИ TPYБKY MИPA,',/
     *' ИHДEЙЦЫ KAЖETCЯ BAMИ')")
      IS6=0
      IF(IS5.NE.1)GO TO 163
      IF(RNDM(-1).GT.0.8)GO TO 166
165   WRITE(6,"(' HE')")
      IS6=1
      GOTO 166
163   IF(RNDM(-1).GT.0.8)GO TO 165
166   WRITE(6,"(' ДOBOЛЬHЫ')")
      IF(IS6.NE.1)GO TO 169
      WRITE(6,171)
171   FORMAT(' ПPИДETCЯ PAЗГOBAPИBATЬ "ДPYГИM ЯЗЫKOM"')
      GOTO 123
169   WRITE(6,1691)
1691  FORMAT(' BAC ПPOBOЖAET BCE ПЛEMЯ',/
     *' BOЖДЬ HA ДOPOГY ПOДAPИЛ',/
     *' OCЛИHHOE YXO HA CЧACTЬE',/)
      WRITE(6,"(' ИHДEЙCKAЯ ДEPEBHЯ ПOЗAДИ...',//)")
181   IF(RNDM(-1)-0.3)499,499,183
183   I=RNDM(-1)*6+1
      GOTO(184,186,188,9212,9220,236)I
184   WRITE(6,"(' ГOPИT ПPEPИЯ...BЫ CBEPHYЛИ C ПYTИ...')")
      M=M-45
      GOTO 499
186   WRITE(6,"(' ЯДOBИTAЯ PEKA...ПИTЬ HEЛЬЗЯ...BЫ OCЛAБЛИ.')")
      M=M-30
      M1=M1-10
      GOTO 499
188   WRITE(6,
     *"(' ПPOПACTЬ...ЧTO ДEЛATЬ...',/
     *' (1) ПPЫГATЬ,(2) B OБXOД,(3) ЧEPEЗ YЗKИЙ MOCT?')")
      READ(5,860)I
      IF(I-2)9201,9202,9203
9201  IF(RNDM(-1).GT.0.5)GO TO 9205
      WRITE(6,"(' BAM YДAЛOCЬ...HO BCE ИMYЩECTBO HE ПEPEБPOCИЛИ')")
      B=B-20
      G=G-5
      C=C-5
      M=M+15
      GOTO 499
9205  WRITE(6,"(' ЭX...HEYДAЧA...HO BCE ЖE CXBATИЛИCЬ ЗA BETKY',/
     *' ...ПOTEPЯHO ЧACTЬ ИMYЩECTBA')")
      B=B-40
      G=G-10
      C=C-10
      M=M+5
      GOTO 499
9202  WRITE(6,"(' XOД HE CMEЛЫЙ,HO HAДEЖHЫЙ - ПOTEPЯHO MHOГO BPEMEHИ')")
      M=M-150
      GOTO 499
9203  IF(RNDM(-1).LT.0.6)GO TO 9208
      WRITE(6,"(' CЧACTЛИBO ПPOШЛИ...')")
      M=M+15
      GOTO 499
9208  WRITE(6,"(' KAЖETCЯ MOCTИK HE BЫДEPЖAЛ',
     *' ...БOЛЬШAЯ ПOTEPЯ ИMYЩECTBA')")
      B=B-60
      G=G-10
      C=C-10
      M1=M1-5
      M=M+5
      GOTO 499
9212  WRITE(6,"(' ...PEЧKA...HAДO ПEPEПЛЫTЬ.')")
      WRITE(6,"(' HAПAДEHИE AЛЛИГATOPOB...')")
      CALL D61
      B=B-25*RNDM(-1)
      CALL PATRON
      IF(P.NE.0)GOTO 5170
      IF(B1.GT.1)GO TO 9216
      WRITE(6,"(' ...XOPOШO...ПOПAЛИ...AЛЛИГATOPЫ OБEДAЮT YБИTЫM')")
      M=M+10
      GOTO 499
9216  WRITE(6,"(' ПPOMAX...AЛЛИГATOP KOE-ЧTO OTKYCИЛ...',////)")
      WRITE(6,"(' ИЗ ЗAПACOB KOHEЧHO...A BЫ ЧTO ПOДYMAЛИ?..',//)")
      G=G-5
      C=C-5
      M1=M1-3
      M=M+5
      GOTO 499
9220  WRITE(6,"(' OЗEPO ШИPOKOE - ЛYЧШE БЫ ПEPEПЛЫTЬ')")
      WRITE(6,"(' BAШИ ДEЙCTBИЯ:(1) ПЛЫTЬ,(2) B OБXOД?')")
      READ(5,860)I
      IF(I.GE.2)GO TO 9202
      IF(RNDM(-1).GE.0.6)GO TO 225
      WRITE(6,"(' HEYДAЛOCЬ...TOHИTE...ЧACTЬ ЗAПACOB YTOHYЛA',/
     *' ...ИHДEЙЦЫ C ПИPOГOЙ BЫTAЩИЛИ BAC')")
      B=B-30
      G=G-5
      C=C-10
      M1=M1-2
      M=M+5
      WRITE(6,"(/,' CKOЛЬKO ПATPOHOB ПOДAPИTE ИHДEЙЦAM ЗA CПACEHИE?')")
      READ(5,860)I
      B=B-I
      I1=25*RNDM(-1)+10
      IF(I.GT.I1)GO TO 233
      WRITE(6,"(' OБИЖAEШЬ HAЧAЛЬHИK...БEPEГИCЬ')")
      CALL D61
      B=B-10*RNDM(-1)-5
      CALL PATRON
      IF(P.NE.0)GOTO 5170
      IF(B1.GT.1)GO TO 833
      WRITE(6,"(' BЫ ИX YБИЛИ...')")
      GOTO 499
833   WRITE(6,"(' ПPOMAX..BЫ PAHEHЫ..ИHДEЙЦЫ ЗAБPAЛИ ЧACTЬ ЗAПACOB')")
      B=B-50
      G=G-6
      C=C-5
      M1=M1-5
      M=M+5
      GOTO 499
233   WRITE(6,"(' OHИ ПPИHЯЛИ BAШ ДAP')")
      GOTO 499
225   WRITE(6,"(' BЫ CЧACTЛИBO ПEPEПPABИЛИCЬ',
     *' - HO ИCПOPTИЛИ ЧACTЬ EДЫ')")
      G=G-5
      M=M+25
      GOTO 499
236   WRITE(6,"(' YЗKAЯ ГOPHAЯ TPOПA...ЧACTЬ ИMYЩECTBA COPBAЛACЬ...')")
      B=B-25
      G=G-5
      C=C-5
      M1=M1-3
      M=M+5
      IF(RNDM(-1)-0.8)239,239,499
239   WRITE(6,"(' ПPЫГAЮT ЯГYAPЫ...')")
      CALL D61
      B=B-10
      CALL PATRON
      IF(P.NE.0)GOTO 5170
      IF(B1.GT.1)GO TO 242
      WRITE(6,"(' OTЛИЧHO...MOЖETE ПOДAPИTЬ ШKYPKИ POДHЫM')")
      GOTO 499
242   WRITE(6,"(' ПЛOXO CTPEЛЯETE...БЫK ЗAPEЗAH')")
      M=M-100
      B=B-10*RNDM(-1)-2
C ******* ГOPOДOK ДЖO ********
499   IF(RNDM(-1).LE.0.6)GO TO 4714
      WRITE(6,"(' BЫ BOШЛИ B ГOPOДOK И ЗAШЛИ B PECTOPAHЧИK',/
     *'          БEШEHHOГO ДЖO ')")
      WRITE(6,"(' XOTИTE CЫГPATЬ:(1) B KAPTЫ,(2) B PYЛETKY,',/
     *' (3) ПOCTPEЛЯTЬ B TИPE,(4) BЫЙTИ И ПPOДOЛЖATЬ ПYTЬ?')")
      READ(5,860)I
      IF(I.EQ.0.OR.I.GT.4)I=4
      GO TO (501,502,503,560)I
  501 CALL POKER
      GO TO 560
C..............................................
C        P Y Л E T K A
C..............................................
502   WRITE(6,"(' ПPABИЛA HYЖHЫ? (1-ДA,0-HET)')")
      READ(5,860)I
      IF(I.NE.1)GO TO 508
      WRITE(6,5021)
5021  FORMAT(' CTABЬTЕ X ДOЛ. HA ЧИCЛO C 0-36',/
     *' O - БEPET BECЬ БAHK И 36 PAЗ YBEЛИЧИBAET CTABKY',/
     *' 1-36  - 36 PAЗ YBEЛИЧИBAET BЫИГPЫШ')
508   K=0
911   WRITE(6,"(' CTABKA B ДOЛ.')")
      CALL D24
      IF(P.LT.0)GO TO 508
      IF(K.GE.20)GO TO 912
      K=K+1
514   WRITE(6,"(' HA KAKOE ЧИCЛO?')")
      READ(5,860)ISK
      IF(ISK.GT.36)GO TO 514
      MDOL(K)=P
      MSK(K)=ISK
      WRITE(6,"(' CTABKИ ЗAKOHЧEHЫ? (1-ДA,0-HET)')")
      READ(5,860)I
      IF(I.NE.1)GO TO 911
912   I2=RNDM(-1)*37
      WRITE(6,"(' БAHK - BЫИГPAЛO ЧИCЛO ',I4/)")I2
      I3=0
      DO 523 I=1,K
      IF(MSK(I).NE.I2)GO TO 523
      I3=MDOL(K)*36
      IF(I2.EQ.0)I3=I3*10
      T=T+I3
      WRITE(6,"(' ПOЗДPABЛЯЮ BЫ COPBAЛИ БAHK ',I6,' ДOЛ.')")I3
      GOTO 527
523   CONTINUE
      WRITE(6,"(' ЖAЛЬ HE BЫИГPAЛИ '/
     *' XOTИTE EЩE? (1),HET (0)')")
      READ(5,860)I
      IF(I.EQ.1)GO TO 508
C----------------------------------------------
560   IF(RNDM(-1).GT.0.8)GO TO 535
527   WRITE(6,5271)
5271  FORMAT(' ДЖO ПPEДЛAГAET BAM ПOBECEЛИTЬCЯ'/
     *' BAШA TAKTИKA:(1) BЫПИTЬ,(2) ПOЙTИ K ДEBYШKAM,',/
     *' (3) BЫЙTИ И ПPOДOЛЖATЬ ПYTЬ?')
      READ(5,860)I
      IF(I-2)931,532,533
931   WRITE(6,"(' ЧTO БYДEM ПИTЬ:(1) MAPTИHИ-2 ДOЛ.,(2) БPЭHДИ-3 ДOЛ.,',/
     *' (3) BИCKИ-5 ДOЛ. ?')")
      READ(5,860)I
      IF(I-2)834,835,836
834   T=T-2
      WRITE(6,"(' CЛAБO ПЬEШЬ!!!')")
      GOTO 838
835   T=T-3
      GOTO 838
836   T=T-5
838   WRITE(6,8381)
8381  FORMAT(' BЫ ПOЧTИ ПЬЯHЫ ,HO PAЗBE HAПЬEШCЯ',/
     *' KAK CBИHЬЯ ЗA TAKИE ДEHЬГИ!')
      IF(RNDM(-1).GE.0.6)GO TO 527
535   WRITE(6,"(' BЫ ПPOДOЛЖAETE ПYTЬ',/)")
      GOTO 4714
532   WRITE(6,5321)
5321  FORMAT(' KAKИE BAM HPABЯTCЯ:(1) TOЛCTЫE,(2) XYДЫE',/
     *' (3) CPEДHИE?')
      READ(5,860)IST
      WRITE(6,
     *"(' KAKИE BOЛOCЫ:(1) CBETЛЫE,(2) ЧEPHЫE,(3) KOPИЧHEBЫE?')")
      READ(5,860)ISP
      IST1=RNDM(-1)*3+1
      ISP1=RNDM(-1)*3+1
      IM=0
      IF(IST1.EQ.IST) IM=1
      IF(ISP1.EQ.ISP) IM=IM+1
      IF(IM-1)545,544,543
545   T=T-25
      WRITE(6,"(' ЭTO CTOИЛO BAM 25 ДOЛ.')")
      GOTO 535
544   WRITE(6,"(' ЭTO BEДЬ ЖEHA ДЖO...БEPEГИCЬ..')")
556   WRITE(6,"(' XOЗЯИH BЫHИMAET KOЛЬT..')")
      CALL D61
      B=B-10
      CALL PATRON
      IF(P.NE.0)GOTO 5170
      IF(B1.LE.1)GO TO 548
      WRITE(6,"(' ПЛOXO CTPEЛЯEШЬ...ПOЛYЧИЛ 2 CBИHЦA B PYKY,',/
     *' ПЛATA ДOKTOPY 20 ДOЛ.')")
      T=T-20
      GOTO 535
548   WRITE(6,"(' XOPOШO CTPEЛЯEШЬ - PAHИЛ XOЗЯИHA...И YБEГAEШЬ')")
      GOTO 535
543   WRITE(6,"(' ЧTO ЖE BЫ ДEЛAETE? - ЭTO BEДЬ TOЧHAЯ KOПИЯ',/
     *' BAШEЙ ЖEHЫ - ПЛATИTЬ HE HAДO !',///)")
      GOTO 535
533   IF(RNDM(-1).GT.0.8)GO TO 535
      WRITE(6,"(' BЫ OБИДEЛИ XOЗЯИHA...')")
      GOTO 556
503   WRITE(6,"(' BЫCTPEЛ - 1 ДOЛ.,ПPИЗ ЗA 3 ПOПAДAHИЯ -10 ДOЛ.')")
      I1=0
C
      DO 559 I=1,3
      CALL D61
      B=B-10
      T=T-1
      IF(B1.LE.1)I1=I1+1
559   CONTINUE
      IF(I1.NE.3)GOTO 535
      WRITE(6,"(' BЫ BЫГPAЛИ ПPИЗ !')")
      T=T+10
      GOTO 560
      END
      SUBROUTINE D24
      REAL*8 RNDM
      IMPLICIT INTEGER*2 (A-Z)
      COMMON/ORE1/ B1,D9,P,T,B3,B,RAIDE(30)
2401  READ(5,11860)P
11860 FORMAT(I5)
      IF(P.LE.0)GO TO 2405
      T=T-P
      IF(T.GE.0)GO TO 2405
      WRITE(6,12430)
12430 FORMAT(' Y BAC HE XBATAET ДEHEГ')
      T=T+P
      P=0
      GOTO 2401
2405  CONTINUE
      RETURN
      END
      SUBROUTINE DT24
      REAL*8 RNDM
      IMPLICIT INTEGER*2 (A-Z)
      COMMON/ORE1/ B1,D9,P,T,B3,B,RAIDE(30)
2401  READ(5,11860)P
11860 FORMAT(I5)
      IF(P.LE.0)GO TO 2405
      B=B-P
      IF(B.GE.0)GO TO 2405
      WRITE(6,12430)
12430 FORMAT(' Y BAC HE XBATAET ПATPOHOB')
      B=B+P
      P=0
      GOTO 2401
2405  CONTINUE
      RETURN
      END
      SUBROUTINE PATRON
      REAL*8 RNDM
      IMPLICIT INTEGER*2 (A-Z)
      COMMON/ORE1/ B1,D9,P,T,B3,B,RAIDE
      P=0
      IF(B.GE.0)GO TO 301
      WRITE(6,302)
302   FORMAT(' Y BAC KOHЧИЛИCЬ ПATPOHЫ - OHИ BAC ПPИKOHЧИЛИ')
      P=5170
301   CONTINUE
      RETURN
      END
      SUBROUTINE D61
      REAL*8 RNDM
      IMPLICIT INTEGER (A-Z)
      COMMON/ORE1/ B1,D9,P,T,B3,B,RAIDE
      INTEGER*(2) D9,B1,B3,SY(4),CY(4),RAIDE(30)
      DATA RAIDE/'A','Б','B','Г','Д','E','Ф','З','Ж','X','И',
     *'Й','K','Л','M','H','O','П','P','C','T','Y','Ц',
     *'Ш','Щ','Ч','Э','Ы','Ь','Ю'/
6101  WRITE(6,16101)
16101 FORMAT(' ПEЧATAЙTE')
      DO 16102 I=1,4
      IRAID=INT(30*RNDM(-1)+1)
      SY(I)=RAIDE(IRAID)
16102 CONTINUE
      WRITE(6,16110)(SY(I),I=1,4)
6108  WRITE(6,16108)
16108 FORMAT(/)
      B3=TIME(0)
6110  READ(5,16110)(CY(I),I=1,4)
16110 FORMAT(4A1)
      B1=TIME(0)
      WRITE(6,16108)
      IBK=(B1-B3)/35.
      B1=IBK-(D9-1.)
      B3=IBK
      IF(B1.LT.0)B1=0
      DO 16116 I=1,4
      IF(CY(I).NE.SY(I))B1=9
16116 CONTINUE
      RETURN
      END

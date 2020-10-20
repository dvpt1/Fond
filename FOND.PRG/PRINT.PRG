*---------------------------------------------------------*
procedure AccoLPT1
private str_file,str_prn
if .not.QuesPrint()
  return
endif
MsgWrite(msgprn)
str_file=memoread("account.prn")
if empty(str_file)
  return
endif
NN=1
Nz=recno()
if Mark != 0
  go top
endif
do while .not.eof()
  if delete().or.Mark = 0
    str_prn=strfrm(str_file)
    if .not.mprints(str_prn)
      exit
    endif
  endif
  if Mark != 0
    skip
  else
    exit
  endif
  NN=NN+1
enddo
go Nz
return
*---------------------------------------------------------*
procedure FondLPT1
private str_file,i,j,i_min,i_max,old_Nz,NN,var,fld,str_prn,isprn
numdic=Dicfrag(9,42,12,61)
if numdic=0
  return
endif
MsgWrite("Расчитываю...")
old_select=select()
old_Nz=recno()
***
select 2
use &OborDBF
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
total on str(NUMBER)+str(DEBET)+str(KREDIT) fields SUMMA to temp.dbf
use
select 2
use temp.dbf
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
select 3
if numdic=1
 use division.dic index division.ntx
else
 use species.dic index species.ntx
endif
select 4
use balans.dic index balans.ntx
****
declare format_str[5]
MsgWrite(msgprn)
str_file=memoread("fond.prn")
if empty(str_file)
  return
endif
for i=1 to 5
  i_min=at(chr(i),str_file)
  i_max=rat(chr(i),str_file)
  if i_min != i_max.and.i_min != (i_max - 1)
    format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
  else
    format_str[i]=""
  endif
next
***
if QuesPrint()
***
sASUMMA=0
sDEBET =0
sKREDIT=0
sSALDO =0
dFILTER=I->L_FILTER
select 3
go top
isprn=.T.
do while (.not.eof()).and.isprn
 if at("DIVISION",I->L_FILTER) != 0
   locate for C->D1=A->DIVISION
 endif
 str_prn=strfrm(format_str[1]+format_str[2])
 isprn=mprints(str_prn)
 iASUMMA=0
 iDEBET =0
 iKREDIT=0
 iSALDO =0
 select(old_select)
 if at("DIVISION",I->L_FILTER) = 0
  if at("SPECIES",I->L_FILTER) = 0
   set filter to A->DIVISION=C->D1
  else
   set filter to A->DIVISION=C->D1.and.&dFILTER
  endif
 endif
 go top
 do while (.not.eof()).and.isprn
  dASUMMA=0
  dDEBET =0
  dKREDIT=0
  dSALDO =0
  select 4
  go top
  dRECSUM=0
  do while .not.eof()
   select 2
   seek str(A->NUMBER)+str(I->DEBET)+str(D->D1)
   dRECSUM=dRECSUM+SUMMA
   select 4
   skip
  enddo
  select 1
  repl I->FORMAT with DEBET+dRECSUM
  dDEBET=I->FORMAT
  select 4
  go top
  dEXPSUM=0
  do while .not.eof()
   select 2
   seek str(A->NUMBER)+str(D->D1)+str(I->KREDIT)
   dEXPSUM=dEXPSUM+SUMMA
   select 4
   skip
  enddo
  dASUMMA=dEXPSUM
  select 1
  repl I->FORMAT with KREDIT+dEXPSUM
  dKREDIT=I->FORMAT
  repl I->FORMAT with SALDO+dRECSUM-dEXPSUM
  dSALDO=I->FORMAT
  str_prn=strfrm(format_str[3])
  isprn=mprints(str_prn)
  iASUMMA=iASUMMA+dASUMMA
  iDEBET =iDEBET +dDEBET
  iKREDIT=iKREDIT+dKREDIT
  iSALDO =iSALDO +dSALDO
  select 1
  skip
 enddo
 if isprn
  repl I->FORMAT with iASUMMA
  iASUMMA=I->FORMAT
  repl I->FORMAT with iDEBET
  iDEBET=I->FORMAT
  repl I->FORMAT with iKREDIT
  iKREDIT=I->FORMAT
  repl I->FORMAT with iSALDO
  iSALDO=I->FORMAT
  select 3
  str_prn=strfrm(format_str[4]+iif(I->IS_LIST,chr(12),""))
  mprints(str_prn)
  sASUMMA=sASUMMA+iASUMMA
  sDEBET =sDEBET +iDEBET
  sKREDIT=sKREDIT+iKREDIT
  sSALDO =sSALDO +iSALDO
  if at("DIVISION",I->L_FILTER) != 0
   isprn=.F.
  else
   skip
  endif
 endif
enddo
if isprn
 repl I->FORMAT with sASUMMA
 sASUMMA=I->FORMAT
 repl I->FORMAT with sDEBET
 sDEBET=I->FORMAT
 repl I->FORMAT with sKREDIT
 sKREDIT=I->FORMAT
 repl I->FORMAT with sSALDO
 sSALDO=I->FORMAT
 str_prn=strfrm(format_str[1]+format_str[5]+iif(I->IS_LIST,chr(12),""))
 mprints(str_prn)
endif
***
endif
***
select 2
use
select 3
use
select 4
use
dele file temp.dbf
dele file temp.ntx
***
select(old_select)
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure OborbLPT1
private str_file,i,j,i_min,i_max,old_Nz,NN,var,fld,str_prn,isprn
numdic=Dicfrag(9,42,12,61)
if numdic=0
  return
endif
MsgWrite("Расчитываю...")
old_select=select()
old_Nz=recno()
***
select 2
use &OborDBF
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
total on str(NUMBER)+str(DEBET)+str(KREDIT) fields SUMMA to temp.dbf
use
select 2
use temp.dbf
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
select 3
if numdic=1
 use division.dic index division.ntx
else
 use species.dic index species.ntx
endif
select 4
use balans.dic index balans.ntx
****
declare format_str[5]
MsgWrite(msgprn)
str_file=memoread("oborotb.prn")
if empty(str_file)
  return
endif
for i=1 to 5
  i_min=at(chr(i),str_file)
  i_max=rat(chr(i),str_file)
  if i_min != i_max.and.i_min != (i_max - 1)
    format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
  else
    format_str[i]=""
  endif
next
***
if QuesPrint()
***
sASUMMA=0
sDEBET =0
sKREDIT=0
sSALDOB=0
sSALDOE=0
dFILTER=I->L_FILTER
select 3
go top
isprn=.T.
do while (.not.eof()).and.isprn
 if at("DIVISION",I->L_FILTER) != 0
   locate for C->D1=A->DIVISION
 endif
 str_prn=strfrm(format_str[1]+format_str[2])
 isprn=mprints(str_prn)
 iASUMMA=0
 iDEBET =0
 iKREDIT=0
 iSALDOB =0
 iSALDOE =0
 select(old_select)
 if at("DIVISION",I->L_FILTER) = 0
  if at("SPECIES",I->L_FILTER) = 0
   set filter to A->DIVISION=C->D1
  else
   set filter to A->DIVISION=C->D1.and.&dFILTER
  endif
 endif
 go top
 do while (.not.eof()).and.isprn
  dASUMMA=0
  dDEBET =0
  dKREDIT=0
  dSALDOB=DEBET
  dSALDOE=0
  select 4
  go top
  dRECSUM=0
  do while .not.eof()
   select 2
   seek str(A->NUMBER)+str(I->DEBET)+str(D->D1)
   dRECSUM=dRECSUM+SUMMA
   select 4
   skip
  enddo
  repl I->FORMAT with dRECSUM
  dDEBET=I->FORMAT
  select 4
  go top
  dEXPSUM=0
  do while .not.eof()
   select 2
   seek str(A->NUMBER)+str(D->D1)+str(I->DEBET)
   dEXPSUM=dEXPSUM+SUMMA
   select 4
   skip
  enddo
  dASUMMA=dEXPSUM
  repl I->FORMAT with dEXPSUM
  dKREDIT=I->FORMAT
  repl I->FORMAT with dSALDOB+dRECSUM-dEXPSUM
  dSALDOE=I->FORMAT
  select 1
  str_prn=strfrm(format_str[3])
  isprn=mprints(str_prn)
  iASUMMA=iASUMMA+dASUMMA
  iDEBET =iDEBET +dDEBET
  iKREDIT=iKREDIT+dKREDIT
  iSALDOB =iSALDOB +dSALDOB
  iSALDOE =iSALDOE +dSALDOE
  select 1
  skip
 enddo
 if isprn
  repl I->FORMAT with iASUMMA
  iASUMMA=I->FORMAT
  repl I->FORMAT with iDEBET
  iDEBET=I->FORMAT
  repl I->FORMAT with iKREDIT
  iKREDIT=I->FORMAT
  repl I->FORMAT with iSALDOB
  iSALDOB=I->FORMAT
  repl I->FORMAT with iSALDOE
  iSALDOE=I->FORMAT
  select 3
  str_prn=strfrm(format_str[4]+iif(I->IS_LIST,chr(12),""))
  mprints(str_prn)
  sASUMMA=sASUMMA+iASUMMA
  sDEBET =sDEBET +iDEBET
  sKREDIT=sKREDIT+iKREDIT
  sSALDOB =sSALDOB +iSALDOB
  sSALDOE =sSALDOE +iSALDOE
  if at("DIVISION",I->L_FILTER) != 0
   isprn=.F.
  else
   skip
  endif
 endif
enddo
if isprn
 repl I->FORMAT with sASUMMA
 sASUMMA=I->FORMAT
 repl I->FORMAT with sDEBET
 sDEBET=I->FORMAT
 repl I->FORMAT with sKREDIT
 sKREDIT=I->FORMAT
 repl I->FORMAT with sSALDOB
 sSALDOB=I->FORMAT
 repl I->FORMAT with sSALDOE
 sSALDOE=I->FORMAT
 str_prn=strfrm(format_str[1]+format_str[5]+iif(I->IS_LIST,chr(12),""))
 mprints(str_prn)
endif
***
endif
***
select 2
use
select 3
use
select 4
use
dele file temp.dbf
dele file temp.ntx
***
select(old_select)
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure OboraLPT1
private str_file,i,j,i_min,i_max,old_Nz,NN,var,fld,str_prn,isprn
numdic=Dicfrag(9,42,12,61)
if numdic=0
  return
endif
MsgWrite("Расчитываю...")
old_select=select()
old_Nz=recno()
***
select 2
use &OborDBF
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
total on str(NUMBER)+str(DEBET)+str(KREDIT) fields SUMMA to temp.dbf
use
select 2
use temp.dbf
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
select 3
if numdic=1
 use division.dic index division.ntx
else
 use species.dic index species.ntx
endif
select 4
use balans.dic index balans.ntx
****
declare format_str[5]
MsgWrite(msgprn)
str_file=memoread("oborota.prn")
if empty(str_file)
  return
endif
for i=1 to 5
  i_min=at(chr(i),str_file)
  i_max=rat(chr(i),str_file)
  if i_min != i_max.and.i_min != (i_max - 1)
    format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
  else
    format_str[i]=""
  endif
next
***
if QuesPrint()
***
sASUMMA=0
sDEBET =0
sKREDIT=0
sSALDOB=0
sSALDOE=0
dFILTER=I->L_FILTER
select 3
go top
isprn=.T.
do while (.not.eof()).and.isprn
 if at("DIVISION",I->L_FILTER) != 0
   locate for C->D1=A->DIVISION
 endif
 str_prn=strfrm(format_str[1]+format_str[2])
 isprn=mprints(str_prn)
 iASUMMA=0
 iDEBET =0
 iKREDIT=0
 iSALDOB =0
 iSALDOE =0
 select(old_select)
 if at("DIVISION",I->L_FILTER) = 0
  if at("SPECIES",I->L_FILTER) = 0
   set filter to A->DIVISION=C->D1
  else
   set filter to A->DIVISION=C->D1.and.&dFILTER
  endif
 endif
 go top
 do while (.not.eof()).and.isprn
  dASUMMA=0
  dDEBET =0
  dKREDIT=0
  dSALDOB=KREDIT
  dSALDOE=0
  select 4
  go top
  dRECSUM=0
  do while .not.eof()
   select 2
   seek str(A->NUMBER)+str(D->D1)+str(I->KREDIT)
   dRECSUM=dRECSUM+SUMMA
   select 4
   skip
  enddo
  repl I->FORMAT with dRECSUM
  dDEBET=I->FORMAT
  select 4
  go top
  dEXPSUM=0
  do while .not.eof()
   select 2
   seek str(A->NUMBER)+str(I->KREDIT)+str(D->D1)
   dEXPSUM=dEXPSUM+SUMMA
   select 4
   skip
  enddo
  dASUMMA=dEXPSUM
  repl I->FORMAT with dEXPSUM
  dKREDIT=I->FORMAT
  repl I->FORMAT with dSALDOB+dRECSUM-dEXPSUM
  dSALDOE=I->FORMAT
  select 1
  str_prn=strfrm(format_str[3])
  isprn=mprints(str_prn)
  iASUMMA=iASUMMA+dASUMMA
  iDEBET =iDEBET +dDEBET
  iKREDIT=iKREDIT+dKREDIT
  iSALDOB =iSALDOB +dSALDOB
  iSALDOE =iSALDOE +dSALDOE
  select 1
  skip
 enddo
 if isprn
  repl I->FORMAT with iASUMMA
  iASUMMA=I->FORMAT
  repl I->FORMAT with iDEBET
  iDEBET=I->FORMAT
  repl I->FORMAT with iKREDIT
  iKREDIT=I->FORMAT
  repl I->FORMAT with iSALDOB
  iSALDOB=I->FORMAT
  repl I->FORMAT with iSALDOE
  iSALDOE=I->FORMAT
  select 3
  str_prn=strfrm(format_str[4]+iif(I->IS_LIST,chr(12),""))
  mprints(str_prn)
  sASUMMA=sASUMMA+iASUMMA
  sDEBET =sDEBET +iDEBET
  sKREDIT=sKREDIT+iKREDIT
  sSALDOB =sSALDOB +iSALDOB
  sSALDOE =sSALDOE +iSALDOE
  if at("DIVISION",I->L_FILTER) != 0
   isprn=.F.
  else
   skip
  endif
 endif
enddo
if isprn
 repl I->FORMAT with sASUMMA
 sASUMMA=I->FORMAT
 repl I->FORMAT with sDEBET
 sDEBET=I->FORMAT
 repl I->FORMAT with sKREDIT
 sKREDIT=I->FORMAT
 repl I->FORMAT with sSALDOB
 sSALDOB=I->FORMAT
 repl I->FORMAT with sSALDOE
 sSALDOE=I->FORMAT
 str_prn=strfrm(format_str[1]+format_str[5]+iif(I->IS_LIST,chr(12),""))
 mprints(str_prn)
endif
***
endif
***
select 2
use
select 3
use
select 4
use
dele file temp.dbf
dele file temp.ntx
***
select(old_select)
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
procedure OpisjLPT1
private str_file,i,j,i_min,i_max,old_Nz,NN,var,fld,str_prn,isprn
numdic=Dicfrag(9,42,12,61)
if numdic=0
  return
endif
MsgWrite("Расчитываю...")
old_select=select()
old_Nz=recno()
***
select 2
use &OborDBF
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
total on str(NUMBER)+str(DEBET)+str(KREDIT) fields SUMMA to temp.dbf
use
select 2
use temp.dbf
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
select 3
if numdic=1
 use division.dic index division.ntx
else
 use species.dic index species.ntx
endif
select 4
use balans.dic index balans.ntx
****
declare format_str[5]
MsgWrite(msgprn)
str_file=memoread("opisj.prn")
if empty(str_file)
  return
endif
for i=1 to 5
  i_min=at(chr(i),str_file)
  i_max=rat(chr(i),str_file)
  if i_min != i_max.and.i_min != (i_max - 1)
    format_str[i]=substr(str_file,i_min+1,i_max-i_min-1)
  else
    format_str[i]=""
  endif
next
***
if QuesPrint()
***
sASUMMA=0
sDEBET =0
sKREDIT=0
sSALDO =0
dFILTER=I->L_FILTER
select 3
go top
isprn=.T.
do while (.not.eof()).and.isprn
 if at("DIVISION",I->L_FILTER) != 0
   locate for C->D1=A->DIVISION
 endif
 str_prn=strfrm(format_str[1]+format_str[2])
 isprn=mprints(str_prn)
 iASUMMA=0
 iDEBET =0
 iKREDIT=0
 iSALDO =0
 select(old_select)
 if at("DIVISION",I->L_FILTER) = 0
  if at("SPECIES",I->L_FILTER) = 0
   set filter to A->DIVISION=C->D1
  else
   set filter to A->DIVISION=C->D1.and.&dFILTER
  endif
 endif
 go top
 do while (.not.eof()).and.isprn
  dASUMMA=0
  dDEBET =0
  dKREDIT=0
  dSALDO =0
  repl I->FORMAT with DEBET
  dDEBET=I->FORMAT
  repl I->FORMAT with DEBET-KREDIT
  SALDO=I->FORMAT
  repl I->FORMAT with dKREDIT+KREDIT
  dKREDIT=I->FORMAT
  repl I->FORMAT with dDEBET-dKREDIT
  dSALDO=I->FORMAT
  str_prn=strfrm(format_str[3])
  isprn=mprints(str_prn)
  iASUMMA=iASUMMA+dASUMMA
  iDEBET =iDEBET +dDEBET
  iKREDIT=iKREDIT+dKREDIT
  iSALDO =iSALDO +dSALDO
  select 1
  skip
 enddo
 if isprn
  repl I->FORMAT with iASUMMA
  iASUMMA=I->FORMAT
  repl I->FORMAT with iDEBET
  iDEBET=I->FORMAT
  repl I->FORMAT with iKREDIT
  iKREDIT=I->FORMAT
  repl I->FORMAT with iSALDO
  iSALDO=I->FORMAT
  select 3
  str_prn=strfrm(format_str[4]+iif(I->IS_LIST,chr(12),""))
  mprints(str_prn)
  sASUMMA=sASUMMA+iASUMMA
  sDEBET =sDEBET +iDEBET
  sKREDIT=sKREDIT+iKREDIT
  sSALDO =sSALDO +iSALDO
  if at("DIVISION",I->L_FILTER) != 0
   isprn=.F.
  else
   skip
  endif
 endif
enddo
if isprn
 repl I->FORMAT with sASUMMA
 sASUMMA=I->FORMAT
 repl I->FORMAT with sDEBET
 sDEBET=I->FORMAT
 repl I->FORMAT with sKREDIT
 sKREDIT=I->FORMAT
 repl I->FORMAT with sSALDO
 sSALDO=I->FORMAT
 str_prn=strfrm(format_str[1]+format_str[5]+iif(I->IS_LIST,chr(12),""))
 mprints(str_prn)
endif
***
endif
***
select 2
use
select 3
use
select 4
use
dele file temp.dbf
dele file temp.ntx
***
select(old_select)
InitFilter()
go old_Nz
return
*---------------------------------------------------------*
function Order1LPT1
private old_color,old_Nz,str_file,i,j,i_min,i_max,str_prn,isprn
numdic=Dicfrag(8,42,11,61)
if numdic=0
  return
endif
MsgWrite("Расчитываю...")
nBALANS=I->DEBET
old_select=select()
old_Nz=recno()
set filter to
go top
select 2
use &OborDBF
index on str(NUMBER)+str(DEBET)+str(KREDIT) to &OborNTX
total on str(NUMBER)+str(DEBET)+str(KREDIT) fields SUMMA to temp.dbf
use
select 2
use temp.dbf
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
select 3
if numdic=1
 use division.dic index division.ntx
else
 use species.dic index species.ntx
endif
len_dev=lenvar(D2)
nul_dev=space(len_dev)
select 4
use balans.dic index balans.ntx
count_rec=reccount()
count_exp=reccount()
*** Расчет фактического числа rec и exp
decl iREC[count_rec]
afill(iREC,0)
decl iEXP[count_exp]
afill(iEXP,0)
select 1
go top
do while (.not.eof())
  select 4
  go top
  i=1
  do while .not.eof()
    select 2
    seek str(A->NUMBER)+str(I->DEBET)+str(D->D1)
    iREC[i]=iREC[i]+SUMMA
    i=i+1
    select 4
    skip
  enddo
  select 4
  go top
  i=1
  do while .not.eof()
    select 2
    seek str(A->NUMBER)+str(D->D1)+str(I->DEBET)
    iEXP[i]=iEXP[i]+SUMMA
    i=i+1
    select 4
    skip
  enddo
  select 1
  skip
enddo
j=0
for i=1 to count_rec
  if iREC[i] != 0
    j=j+1
  endif
next
cnt_rec=j
j=0
for i=1 to count_exp
  if iEXP[i] != 0
    j=j+1
  endif
next
cnt_exp=j
***  формирование строки таблицы
select(old_select)
str_len=(len_dev)+4*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
*** 1-заголовок
str_file=memoread("order.prn")
str_prn=""
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
***
* 1 строка
str_prn=str_prn+replicate("-",str_len)+chr(13)+chr(10)
* 2 строка
str_prn=str_prn+"    Hаименование    !"
str_prn=str_prn+"    Сальдо    !"
if cnt_rec<>0
 str_prn=str_prn+zentstr(cnt_rec*(len_sum+1)-1,"Приход")+"!"
endif
str_prn=str_prn+"    Итого     !"
if cnt_exp<>0
 str_prn=str_prn+zentstr(cnt_exp*(len_sum+1)-1,"Расход")+"!"
endif
str_prn=str_prn+"    Итого     !    Сальдо    "
str_prn=str_prn+chr(13)+chr(10)
* 3 строка
str_prn=str_prn+nul_dev+"!"
str_prn=str_prn+"      на      !"
if cnt_rec<>0
 str_prn=str_prn+replicate("-",cnt_rec*(len_sum+1)-1)+"!"
endif
str_prn=str_prn+"    приход    !"
if cnt_exp<>0
 str_prn=str_prn+replicate("-",cnt_exp*(len_sum+1)-1)+"!"
endif
str_prn=str_prn+"    расход    !      на      "
str_prn=str_prn+chr(13)+chr(10)
* 4 строка
str_prn=str_prn+nul_dev+"!"
str_prn=str_prn+"    начало    !"
if cnt_rec<>0
 select 4
 go top
 i=1
 do while .not.eof()
  if iREC[i] != 0
   str_prn=str_prn+"     "+strroun(D1)+"   !"
  endif
  skip
  i=i+1
 enddo
endif
str_prn=str_prn+"              !"
if cnt_exp<>0
 select 4
 go top
 i=1
 do while .not.eof()
  if iEXP[i] != 0
   str_prn=str_prn+"     "+strroun(D1)+"   !"
  endif
  skip
  i=i+1
 enddo
endif
str_prn=str_prn+"              !    конец      "
str_prn=str_prn+chr(13)+chr(10)
* 5 строка
str_prn=str_prn+replicate("-",str_len)+chr(13)+chr(10)
***
decl dREC[cnt_rec]
decl dEXP[cnt_exp]
iRECSUM=0
iEXPSUM=0
iSALBEG=0
iSALEND=0
select 3
go top
do while (.not.eof())
  str_prn=str_prn+D2+"!"
select 1
if numdic=1
set filter to A->DIVISION=C->D1
else
set filter to A->SPECIES=C->D1
endif
go top
afill(dREC,0)
afill(dEXP,0)
dRECSUM=0
dEXPSUM=0
dSALBEG=0
dSALEND=0
do while (.not.eof())
  dSALBEG=dSALBEG+DEBET
  select 4
  go top
  i=1
  ii=1
  do while .not.eof()
    if iREC[i] != 0
      select 2
      seek str(A->NUMBER)+str(I->DEBET)+str(D->D1)
      dREC[ii]=dREC[ii]+SUMMA
      dRECSUM=dRECSUM+SUMMA
      ii=ii+1
      select 4
    endif
    skip
    i=i+1
  enddo
  select 4
  go top
  i=1
  ii=1
  do while .not.eof()
    if iEXP[i] != 0
      select 2
      seek str(A->NUMBER)+str(D->D1)+str(I->DEBET)
      dEXP[ii]=dEXP[ii]+SUMMA
      dEXPSUM=dEXPSUM+SUMMA
      ii=ii+1
      select 4
    endif
    skip
    i=i+1
  enddo
  select 1
  skip
enddo
  iSALBEG=iSALBEG+dSALBEG
  repl I->FORMAT with dSALBEG
  str_prn=str_prn+strnum(I->FORMAT)+"!"
  i=1
  ii=1
  do while i<=count_rec
    if iREC[i] != 0
      repl I->FORMAT with dREC[ii]
      str_prn=str_prn+strnum(I->FORMAT)+"!"
      ii=ii+1
    endif
    i=i+1
  enddo
  iRECSUM=iRECSUM+dRECSUM
  repl I->FORMAT with dRECSUM
  str_prn=str_prn+strnum(I->FORMAT)+"!"
  i=1
  ii=1
  do while i<=count_exp
    if iEXP[i] != 0
      repl I->FORMAT with dEXP[ii]
      str_prn=str_prn+strnum(I->FORMAT)+"!"
      ii=ii+1
    endif
    i=i+1
  enddo
  iEXPSUM=iEXPSUM+dEXPSUM
  repl I->FORMAT with dEXPSUM
  str_prn=str_prn+strnum(I->FORMAT)+"!"
  dSALEND=dSALBEG+dRECSUM-dEXPSUM
  iSALEND=iSALEND+dSALEND
  repl I->FORMAT with dSALEND
  str_prn=str_prn+strnum(I->FORMAT)
  str_prn=str_prn+chr(13)+chr(10)
  select 3
  skip
enddo
* "Итого"
str_prn=str_prn+replicate("-",str_len)+chr(13)+chr(10)
str_prn=str_prn+"       ИТОГО        !"
repl I->FORMAT with iSALBEG
str_prn=str_prn+strnum(I->FORMAT)+"!"
i=1
do while i<=count_rec
  if iREC[i] != 0
    repl I->FORMAT with iREC[i]
    str_prn=str_prn+strnum(I->FORMAT)+"!"
  endif
  i=i+1
enddo
repl I->FORMAT with iRECSUM
str_prn=str_prn+strnum(I->FORMAT)+"!"
i=1
do while i<=count_exp
  if iEXP[i] != 0
    repl I->FORMAT with iEXP[i]
    str_prn=str_prn+strnum(I->FORMAT)+"!"
  endif
  i=i+1
enddo
repl I->FORMAT with iEXPSUM
str_prn=str_prn+strnum(I->FORMAT)+"!"
repl I->FORMAT with iSALEND
str_prn=str_prn+strnum(I->FORMAT)
str_prn=str_prn+chr(13)+chr(10)
* 3 - END строка
str_prn=str_prn+replicate("-",str_len)+chr(13)+chr(10)
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=str_prn+strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
***
select 2
use
select 3
use
select 4
use
dele file temp.dbf
dele file temp.ntx
***
str_prn=formprn(@str_prn,I->LEN_LPT1,I->IS_LIST)
ed(Y_min,X_min,Y_max,X_max,@str_prn)
***
setcolor(old_color)
select(old_select)
InitFilter()
go old_Nz
return " "
*---------------------------------------------------------*
function Order2LPT1
private old_color,old_Nz,str_file,i,j,i_min,i_max,str_prn,isprn
numdic=Dicfrag(9,42,12,61)
if numdic=0
  return
endif
nBALANS=I->KREDIT
MsgWrite("Расчитываю...")
old_select=select()
old_Nz=recno()
set filter to
go top
select 2
use &OborDBF
index on str(NUMBER)+str(DEBET)+str(KREDIT) to &OborNTX
total on str(NUMBER)+str(DEBET)+str(KREDIT) fields SUMMA to temp.dbf
use
select 2
use temp.dbf
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
select 3
if numdic=1
 use division.dic index division.ntx
else
 use species.dic index species.ntx
endif
len_dev=lenvar(D2)
nul_dev=space(len_dev)
select 4
use balans.dic index balans.ntx
count_rec=reccount()
count_exp=reccount()
*** Расчет фактического числа rec и exp
decl iREC[count_rec]
afill(iREC,0)
decl iEXP[count_exp]
afill(iEXP,0)
select 1
go top
do while (.not.eof())
  select 4
  go top
  i=1
  do while .not.eof()
    select 2
    seek str(A->NUMBER)+str(D->D1)+str(I->KREDIT)
    iREC[i]=iREC[i]+SUMMA
    i=i+1
    select 4
    skip
  enddo
  select 4
  go top
  i=1
  do while .not.eof()
    select 2
    seek str(A->NUMBER)+str(I->KREDIT)+str(D->D1)
    iEXP[i]=iEXP[i]+SUMMA
    i=i+1
    select 4
    skip
  enddo
  select 1
  skip
enddo
j=0
for i=1 to count_rec
  if iREC[i] != 0
    j=j+1
  endif
next
cnt_rec=j
j=0
for i=1 to count_exp
  if iEXP[i] != 0
    j=j+1
  endif
next
cnt_exp=j
***  формирование строки таблицы
select(old_select)
str_len=(len_dev)+4*(len_sum+1)+(cnt_rec+cnt_exp)*(len_sum+1)
*** 1-заголовок
str_file=memoread("order.prn")
str_prn=""
i_min=at(chr(1),str_file)
i_max=rat(chr(1),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
***
* 1 строка
str_prn=str_prn+replicate("-",str_len)+chr(13)+chr(10)
* 2 строка
str_prn=str_prn+"    Hаименование    !"
str_prn=str_prn+"    Износ     !"
if cnt_rec<>0
 str_prn=str_prn+zentstr(cnt_rec*(len_sum+1)-1,"Hачислено")+"!"
endif
str_prn=str_prn+"    Итого     !"
if cnt_exp<>0
 str_prn=str_prn+zentstr(cnt_exp*(len_sum+1)-1,"Списано")+"!"
endif
str_prn=str_prn+"    Итого     !    Износ     "
str_prn=str_prn+chr(13)+chr(10)
* 3 строка
str_prn=str_prn+nul_dev+"!"
str_prn=str_prn+"      на      !"
if cnt_rec<>0
 str_prn=str_prn+replicate("-",cnt_rec*(len_sum+1)-1)+"!"
endif
str_prn=str_prn+"  начислено   !"
if cnt_exp<>0
 str_prn=str_prn+replicate("-",cnt_exp*(len_sum+1)-1)+"!"
endif
str_prn=str_prn+"   списано    !      на      "
str_prn=str_prn+chr(13)+chr(10)
* 4 строка
str_prn=str_prn+nul_dev+"!"
str_prn=str_prn+"    начало    !"
if cnt_rec<>0
 select 4
 go top
 i=1
 do while .not.eof()
  if iREC[i] != 0
   str_prn=str_prn+"     "+strroun(D1)+"   !"
  endif
  skip
  i=i+1
 enddo
endif
str_prn=str_prn+"    износ     !"
if cnt_exp<>0
 select 4
 go top
 i=1
 do while .not.eof()
  if iEXP[i] != 0
   str_prn=str_prn+"     "+strroun(D1)+"   !"
  endif
  skip
  i=i+1
 enddo
endif
str_prn=str_prn+"    износ     !    конец      "
str_prn=str_prn+chr(13)+chr(10)
* 5 строка
str_prn=str_prn+replicate("-",str_len)+chr(13)+chr(10)
***
decl dREC[cnt_rec]
decl dEXP[cnt_exp]
iRECSUM=0
iEXPSUM=0
iSALBEG=0
iSALEND=0
select 3
go top
do while (.not.eof())
  str_prn=str_prn+D2+"!"
select 1
if numdic=1
set filter to A->DIVISION=C->D1
else
set filter to A->SPECIES=C->D1
endif
go top
afill(dREC,0)
afill(dEXP,0)
dRECSUM=0
dEXPSUM=0
dSALBEG=0
dSALEND=0
do while (.not.eof())
  dSALBEG=dSALBEG+KREDIT
  select 4
  go top
  i=1
  ii=1
  do while .not.eof()
    if iREC[i] != 0
      select 2
      seek str(A->NUMBER)+str(D->D1)+str(I->KREDIT)
      dREC[ii]=dREC[ii]+SUMMA
      dRECSUM=dRECSUM+SUMMA
      ii=ii+1
      select 4
    endif
    skip
    i=i+1
  enddo
  select 4
  go top
  i=1
  ii=1
  do while .not.eof()
    if iEXP[i] != 0
      select 2
      seek str(A->NUMBER)+str(I->KREDIT)+str(D->D1)
      dEXP[ii]=dEXP[ii]+SUMMA
      dEXPSUM=dEXPSUM+SUMMA
      ii=ii+1
      select 4
    endif
    skip
    i=i+1
  enddo
  select 1
  skip
enddo
  iSALBEG=iSALBEG+dSALBEG
  repl I->FORMAT with dSALBEG
  str_prn=str_prn+strnum(I->FORMAT)+"!"
  i=1
  ii=1
  do while i<=count_rec
    if iREC[i] != 0
      repl I->FORMAT with dREC[ii]
      str_prn=str_prn+strnum(I->FORMAT)+"!"
      ii=ii+1
    endif
    i=i+1
  enddo
  iRECSUM=iRECSUM+dRECSUM
  repl I->FORMAT with dRECSUM
  str_prn=str_prn+strnum(I->FORMAT)+"!"
  i=1
  ii=1
  do while i<=count_exp
    if iEXP[i] != 0
      repl I->FORMAT with dEXP[ii]
      str_prn=str_prn+strnum(I->FORMAT)+"!"
      ii=ii+1
    endif
    i=i+1
  enddo
  iEXPSUM=iEXPSUM+dEXPSUM
  repl I->FORMAT with dEXPSUM
  str_prn=str_prn+strnum(I->FORMAT)+"!"
  dSALEND=dSALBEG+dRECSUM-dEXPSUM
  iSALEND=iSALEND+dSALEND
  repl I->FORMAT with dSALEND
  str_prn=str_prn+strnum(I->FORMAT)
  str_prn=str_prn+chr(13)+chr(10)
  select 3
  skip
enddo
* "Итого"
str_prn=str_prn+replicate("-",str_len)+chr(13)+chr(10)
str_prn=str_prn+"       ИТОГО        !"
repl I->FORMAT with iSALBEG
str_prn=str_prn+strnum(I->FORMAT)+"!"
i=1
do while i<=count_rec
  if iREC[i] != 0
    repl I->FORMAT with iREC[i]
    str_prn=str_prn+strnum(I->FORMAT)+"!"
  endif
  i=i+1
enddo
repl I->FORMAT with iRECSUM
str_prn=str_prn+strnum(I->FORMAT)+"!"
i=1
do while i<=count_exp
  if iEXP[i] != 0
    repl I->FORMAT with iEXP[i]
    str_prn=str_prn+strnum(I->FORMAT)+"!"
  endif
  i=i+1
enddo
repl I->FORMAT with iEXPSUM
str_prn=str_prn+strnum(I->FORMAT)+"!"
repl I->FORMAT with iSALEND
str_prn=str_prn+strnum(I->FORMAT)
str_prn=str_prn+chr(13)+chr(10)
* 3 - END строка
str_prn=str_prn+replicate("-",str_len)+chr(13)+chr(10)
i_min=at(chr(3),str_file)
i_max=rat(chr(3),str_file)
if i_min != i_max.and.i_min != (i_max - 1)
  str_prn=str_prn+strfrm(substr(str_file,i_min+1,i_max-i_min-1))
endif
***
select 2
use
select 3
use
select 4
use
dele file temp.dbf
dele file temp.ntx
***
str_prn=formprn(@str_prn,I->LEN_LPT1,I->IS_LIST)
ed(Y_min,X_min,Y_max,X_max,@str_prn)
***
setcolor(old_color)
select(old_select)
InitFilter()
go old_Nz
return " "
*-------------------------------------------------------------*
function Dicfrag
parameters Y1,X1,Y2,X2
private otwet
set color to &cmenu
BoxTenj(Y1,X1,Y2,X2,tbox)
@ Y1+1,   X1+1 prompt " по подpазделению "
@ row()+1,X1+1 prompt " по виду          "
menu to otwet
return otwet

*---------------------------------------------------------*
function NewFond
private var_retr
save screen
if Yes_Or_No(' ë é á Ñ Ä í ú ','çÆ¢„Ó ™†p‚ÆÁ™„?',1)
  MsgWrite("ëÆß§†Ó...")
  var_retr=.T.
  append blank
  if SIZE_FILTER!=0.and.(.not.empty(I->N_FILTER))  && á≠†Á. ‰®´Ï‚‡† ¢ ãë
    fil_string=alltrim(I->L_FILTER)
    fil_string=strtran(fil_string,".or.","    ")
    for i=1 to SIZE_FILTER
      new_var=psubstr(fil_string,i)
      new_var=strtran(new_var,filter_fld[i])
      new_var=strtran(new_var,"=")
      var_fld=filter_fld[i]
      type_var=&var_fld
      do case
        case type("type_var")="C"
          replace &var_fld with strtran(new_var,'"')
        case type("type_var")="N"
          replace &var_fld with val(new_var)
        case type("type_var")="D"
          new_var=strtran(new_var,"ctod()")
          new_var=strtran(new_var,'"')
          replace &var_fld with ctod(new_var)
        case type("type_var")="L"
          if new_var="()"     && "(.not.)"
            replace &var_fld with .T.
          else
            replace &var_fld with .F.
          endif
      endcase
    next
  endif
  RETR = 0
else
  var_retr=.F.
endif
restore screen
return var_retr
*---------------------------------------------------------*
procedure InpFond
private old_color,InpPage,done,otwet,var_fld,var_prm,var_file,var_iif
old_color = setcolor(cinp)
save screen to InpPage
DrawInp()
keyboard chr(27)
Oborot()
otwet=1
done=.T.
do while  done
for i=1 to SIZE_ACCO
  setcolor(cinp)
  var_fld=acco_fld[i]
  if .not.empty(acco_dbf[i])
    var_file=acco_dbf[i]
    if file(var_file)
      var_prm=mDict(acco_scr[i],&var_fld,acco_cod[i],var_file,acco_ntx[i])
    else
      var_prm=space(21)
    endif
  else
    var_prm=&var_fld
  endif
  @ Y_min+acco_row[i],X_min+acco_col[i] say acco_nam[i]
  @ row(), col()+1 say var_prm
next
do while otwet!=0
  setcolor(cinp)
  var_fld=acco_fld[otwet]
  do case
  case .not.empty(acco_dbf[otwet])
    var_file=acco_dbf[otwet]
    if file(var_file)
      var_prm=mDict(acco_scr[otwet],&var_fld,acco_cod[otwet],var_file,acco_ntx[otwet])
    else
      var_prm=space(21)
    endif
  othe
    var_prm=&var_fld
  endcase
  @ Y_min+acco_row[otwet],X_min+acco_col[otwet] say acco_nam[otwet]
  @ row(), col()+1 get var_prm
  clear gets
  key_last = inkey(0)
  @ Y_min+acco_row[otwet],X_min+acco_col[otwet] say acco_nam[otwet]
  @ row(), col()+1 say var_prm
  do case
  case key_last = 27
    otwet = 0
    done=.F.
  case key_last =  5.or.key_last = 19   && UP or RIGHT
    if otwet = 1
      otwet = SIZE_ACCO
    else
      otwet = otwet - 1
    endif
  case key_last = 24.or.key_last =  4   && DOWN or LEFT
    if otwet = SIZE_ACCO
      otwet = 1
    else
      otwet = otwet + 1
    endif
  case key_last = 13.or.isdata(key_last)
    if key_last <> 13
      keyboard chr(key_last)
    endif
    var_fld=acco_fld[otwet]
    old_var=&var_fld
    do case
    case .not.empty(acco_dbf[otwet])
      var_file=acco_dbf[otwet]
      if file(var_file)
        repl &var_fld with DictFind(&var_fld,acco_cod[otwet],acco_scr[otwet],acco_nam[otwet],var_file,acco_ntx[otwet])
      endif
    othe
      set cursor on
      @ Y_min+acco_row[otwet],X_min+acco_col[otwet] say acco_nam[otwet]
      @ row(), col()+1 get &var_fld
      read
      set cursor off
    endcase
    if old_var != &var_fld
      RETR = 0
    endif
    if lastkey()=13.or.lastkey()=7.or.isdata(lastkey())
      keyboard chr(24)
      if old_var != &var_fld
        exit
      endif
    endif
  case key_last =  9
    do Calculator
  case key_last = -1
    Oborot()
  case key_last = -2
    repl A->NOTES with ed(Y_srm-5,X_srm-25,Y_srm+5,X_srm+25,A->NOTES)
  case key_last = -3
    Raschet()
    Oborot()
  endcase
enddo
enddo
if RETR=0
  Nz=recno()
  skip
  commit
  go Nz
endif
restore screen from InpPage
set color to &old_color
return
*---------------------------------------------------------*
procedure DrawInp
setcolor(cinp)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
MSG=' à H Ç Ö H í Ä ê H Ä ü   ä Ä ê í é ó ä Ä '
@ Y_min,int((X_min+X_max)/2-len(MSG)/2) say MSG
@ Y_max,X_min+8 say ' å Ö ç û: '
setcolor(cmenu)
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ç¢Æ§'
MSG='F2'
@ row(),col() get MSG
@ row(),col() say 'èpÆ¢Æ§™®'
MSG='F3'
@ row(),col() get MSG
@ row(),col() say 'èp®¨•Á†≠®•'
MSG='F4'
@ row(),col() get MSG
@ row(),col() say 'ê†·Á®‚†‚Ï'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'ÇÎÂÆ§'
clear gets
return
*---------------------------------------------------------*
procedure Oborot
private old_color,old_select
old_select=select()
old_color = setcolor(cinp)
l=savescreen(Y_max,Y_min,Y_max,X_max)
Y1=Y_min+2
X1=X_max-34
Y2=Y_max-2
X2=X_max-1
DrawObor()
declare dict_fld[3]
declare dict_nam[3]
dict_nam[1]=' Ñ•°•‚'
dict_nam[2]='äp•§®‚'
dict_nam[3]='ë„¨¨†'
dict_fld[1]='DEBET'
dict_fld[2]='KREDIT'
dict_fld[3]='SUMMA'
select 2
use &OborDBF index &OborNTX
set filter to A->NUMBER=B->NUMBER
go top
setcolor(cl)
dbedit(Y1+1,X1+1,Y2-1,X2-1,dict_fld,"OborFunc","",dict_nam)
use
restscreen(Y_max,Y_min,Y_max,X_max,l)
setcolor(old_color)
select(old_select)
return
*---------------------------------------------------------*
function OborFunc
parameters mode,fld_ptr
private cur_field,key_last
key_last=LASTKEY()
do case
case mode = 0
case mode <= 3
  return 1
case LASTKEY() = 27
  return 0
case LASTKEY() = 13.or.isdata(key_last)
  if key_last <> 13
     keyboard chr(key_last)
  endif
  do case
  case fld_ptr=1
    repl DEBET with DictFind(DEBET,"D1","strroun(D1)+' '+D2","ÇÎ°•p®‚• ·Á•‚","BALANS.DIC","BALANS.NTX")
  case fld_ptr=2
    repl KREDIT with DictFind(KREDIT,"D1","strroun(D1)+' '+D2","ÇÎ°•p®‚• ·Á•‚","BALANS.DIC","BALANS.NTX")
  case fld_ptr=3
   set cursor on
   @ row(),col() get SUMMA
   read
   set cursor off
  endcase
  return 2
case lastkey() = 22        && Ins
  append blank
  repl B->NUMBER with A->NUMBER
  return 2
case lastkey() = 7         && Del
  DelGloss()
  return 2
endcase
return 1
*---------------------------------------------------------*
procedure DrawObor
DrawBox(Y1,X1,Y2,X2,dbox)
@ Y_max,X_min say replicate('€',80)
setcolor(cl)
@ Y_max,X_min+15 say ' å Ö ç û: '
setcolor(cmenu)
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ç¢Æ§'
MSG='INS'
@ row(),col() get MSG
@ row(),col() say 'ëÆß§†‚Ï'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'ì§†´®‚Ï'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'ÇÎÂÆ§'
clear gets
return
*---------------------------------------------------------*
procedure Raschet
private Nz,i,var,fld
save screen to RasPage
if Yes_Or_No('','ë§•´†‚Ï p†Á•‚?',1)
 MsgWrite("ëÁ®‚†Ó...")
 old_select=select()
 Nz=recno()
 select 8
 use come.fnd
 nucount=reccount()
 decl nam[nucount]
 decl deb[nucount]
 decl kre[nucount]
 decl frm[nucount]
 i=1
 do while .not.eof()
  nam[i]=NAME
  deb[i]=DEBET
  kre[i]=KREDIT
  frm[i]=translate(SUMMA)
  i=i+1
  skip
 enddo
 use
 if nucount <> 0
  select 2
  use &OborDBF index &OborNTX
  select 3
  use &RashetDIC index &RashetNTX
  select(old_select)
  if Mark != 0
    go top
  endif
  do while .not.eof()
    if delete().or.Mark = 0
     for i=1 to nucount
      if (.not.empty(frm[i])).and.empty(nam[i])
       var = formula(frm[i])
       if TokenError
         exit
       endif
       select 3
       if RashetDIC='species.dic'
        seek A->SPECIES
       else
        seek A->DIVISION
       endif
       if deb[i]=0
        debt=C->DEBET
       else
        debt=deb[i]
       endif
       if kre[i]=0
        kret=C->KREDIT
       else
        kret=kre[i]
       endif
       select 2
       seek str(A->NUMBER)+str(debt)+str(kret)
       if .not.found()
        if var != 0
         append blank
         repl B->NUMBER with A->NUMBER
         repl B->DEBET with debt
         repl B->KREDIT with kret
         repl B->SUMMA with var
        endif
       else
        if var != 0
         repl B->NUMBER with A->NUMBER
         repl B->DEBET with debt
         repl B->KREDIT with kret
         repl B->SUMMA with var
        else
         delete
        endif
       endif
       select(old_select)
      endif
     next
     if TokenError
       exit
     endif
    endif
    if Mark != 0
      skip
    else
      exit
    endif
  enddo
  select 3
  use
  select 2
  pack
  use
 else
  WarnWrite("H• ¢¢•§•≠Î ‰Æp¨„´Î p†·Á•‚†...")
 endif
 select(old_select)
 go Nz
endif
restore screen from RasPage
return
*---------------------------------------------------------*
function SummaFond
parameters Y1,X1,Y2,X2
private old_color,ptr,currprow,str_file,i,j,indx,i_min,i_max,old_Nz,str_prn,isprn
if reccount() = 0
  WarnWrite("H•Á•£Æ p†·Á®‚Î¢†‚Ï...")
  return " "
endif
save screen to InpPage
MsgWrite("ê†·Á®‚Î¢†Ó...")
old_color=setcolor(cw)
old_select=select()
old_Nz=recno()
***
nBALANS=I->DEBET
select 2
use &OborDBF
index on str(NUMBER)+str(DEBET)+str(KREDIT) to &OborNTX
total on str(NUMBER)+str(DEBET)+str(KREDIT) fields SUMMA to temp.dbf
use
select 2
use temp.dbf
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
select 4
use balans.dic index balans.ntx
count_rec=reccount()
count_exp=reccount()
*** ê†·Á•‚ ‰†™‚®Á•·™Æ£Æ Á®·´† rec ® exp
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
select(old_select)
decl dREC[cnt_rec]
decl dEXP[cnt_exp]
afill(dREC,0)
afill(dEXP,0)
dRECSUM=0
dEXPSUM=0
dSALBEG=0
dSALEND=0
go top
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
dSALEND=dSALBEG+dRECSUM-dEXPSUM
***
select 2
use
select 4
use
dele file temp.dbf
dele file temp.ntx
***
nBALANS=I->KREDIT
select 2
use &OborDBF
index on str(NUMBER)+str(DEBET)+str(KREDIT) to &OborNTX
total on str(NUMBER)+str(DEBET)+str(KREDIT) fields SUMMA to temp.dbf
use
select 2
use temp.dbf
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
select 4
use balans.dic index balans.ntx
count_rec=reccount()
count_exp=reccount()
*** ê†·Á•‚ ‰†™‚®Á•·™Æ£Æ Á®·´† rec ® exp
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
select(old_select)
decl dREC[cnt_rec]
decl dEXP[cnt_exp]
afill(dREC,0)
afill(dEXP,0)
kRECSUM=0
kEXPSUM=0
kSALBEG=0
kSALEND=0
go top
do while (.not.eof())
  kSALBEG=kSALBEG+KREDIT
  select 4
  go top
  i=1
  ii=1
  do while .not.eof()
    if iREC[i] != 0
      select 2
      seek str(A->NUMBER)+str(D->D1)+str(I->KREDIT)
      dREC[ii]=dREC[ii]+SUMMA
      kRECSUM=kRECSUM+SUMMA
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
      kEXPSUM=kEXPSUM+SUMMA
      ii=ii+1
      select 4
    endif
    skip
    i=i+1
  enddo
  select 1
  skip
enddo
kSALEND=kSALBEG+kRECSUM-kEXPSUM
***
select 2
use
select 4
use
dele file temp.dbf
dele file temp.ntx
***
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
@ Y1,X1+9 say 'éëçéÇçõÖ ëêÖÑëíÇÄ'
@ Y1,X1+48 say 'àáçéë'
set color to &cinp
@ Y1+ 1,X1+1 say "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
@ Y1+ 2,X1+1 say "∫     èêàïéÑ    ≥    êÄëïéÑ     ∫∫   HÄóàëãÖHé   ≥    ëèàëÄHé    ∫"
@ Y1+ 3,X1+1 say "«ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ∂«ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ∂"
@ Y1+ 4,X1+1 say "∫               ≥               ∫∫               ≥               ∫"
@ Y1+ 5,X1+1 say "«ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ∂«ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ∂"
@ Y1+ 6,X1+1 say "∫ëÄãúÑé çÄ çÄóÄã≥ëÄãúÑé çÄ äéçÖñ∫∫àáHéë çÄ çÄóÄãé≥àáHéë çÄ äéçÖñ ∫"
@ Y1+ 7,X1+1 say "«ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ∂«ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ∂"
@ Y1+ 8,X1+1 say "∫               ≥               ∫∫               ≥               ∫"
@ Y1+ 9,X1+1 say "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"

repl I->FORMAT with dRECSUM
@ Y1+ 4,X1+ 3 say I->FORMAT
repl I->FORMAT with dEXPSUM
@ Y1+ 4,X1+19 say I->FORMAT
repl I->FORMAT with dSALBEG
@ Y1+ 8,X1+ 3 say I->FORMAT
repl I->FORMAT with dSALEND
@ Y1+ 8,X1+19 say I->FORMAT

repl I->FORMAT with kRECSUM
@ Y1+ 4,X1+36 say I->FORMAT
repl I->FORMAT with kEXPSUM
@ Y1+ 4,X1+52 say I->FORMAT
repl I->FORMAT with kSALBEG
@ Y1+ 8,X1+36 say I->FORMAT
repl I->FORMAT with kSALEND
@ Y1+ 8,X1+52 say I->FORMAT

inkey(0)

restore screen from InpPage
setcolor(old_color)
select(old_select)
go old_Nz
return " "


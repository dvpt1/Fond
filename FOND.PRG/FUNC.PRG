*---------------------------------------------------------*
procedure InpFilter
parameters Y1,X1,Y2,X2
private var_fld,var_prm,is_var
private fil_string,type_var
private otwet
private old_color
private del_filter
del_filter=.F.
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG=' В Ы Б О Р К А '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cl
@ Y2,X1+1 say ' М Е Н Ю: '
set color to &cmenu
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ввод'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удалить'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
declare var_filter[SIZE_FILTER]  && Массив ключей фильтра
fil_string=alltrim(I->L_FILTER)  && Разбор строки фильтра
fil_string=strtran(fil_string,".and.","    ")
for i=1 to SIZE_FILTER
  var_filter[i]=psubstr(fil_string,i)
  var_filter[i]=strtran(var_filter[i],filter_fld[i])
  var_filter[i]=strtran(var_filter[i],"=")
  var_fld=filter_fld[i]
  type_var=&var_fld
  do case
  case type("type_var")="C"
    var_filter[i]=strtran(var_filter[i],'"')
  case type("type_var")="N"
    var_filter[i]=val(var_filter[i])
    var_filter[i]=val(transform(var_filter[i],pictn(type_var)))  && для форматирования
  case type("type_var")="D"
    var_filter[i]=strtran(var_filter[i],"ctod()")
    var_filter[i]=strtran(var_filter[i],'"')
    var_filter[i]=ctod(var_filter[i])
  case type("type_var")="L"
    if var_filter[i]="()"     && "(.not.)"
      var_filter[i]=strtran(filter_iif[i],filter_fld[i],".T.")
    else
      var_filter[i]=strtran(filter_iif[i],filter_fld[i],".F.")
    endif
  endcase
next
otwet=1
do while otwet!=0   && Собственно меню выбора нового фильтра
  set color to &cfr
  for i=1 to SIZE_FILTER
    @ Y1+i+1,X1+2 say filter_nam[i]
    do case
    case .not.empty(filter_dbf[i])
      var_prm=mDict(filter_scr[i],var_filter[i],filter_cod[i],filter_dbf[i],filter_ntx[i])
    case .not.empty(filter_iif[i])
      var_fld=var_filter[i]
      var_prm=&var_fld
    othe
      var_prm=var_filter[i]
    endcase
    @ row(),col()+1 prompt varstr(var_prm)
  next
  set key 7 to DelFilter
  menu to otwet
  set key 7 to
  if otwet != 0
    do case
    case .not.empty(filter_dbf[otwet])
      var_filter[otwet]=DictFind(var_filter[otwet],filter_cod[otwet],filter_scr[otwet],;
                        filter_nam[otwet],filter_dbf[otwet],filter_ntx[otwet])
    case .not.empty(filter_iif[otwet])
      if var_filter[otwet]="iif(.T.,"
        var_filter[otwet]=strtran(var_filter[otwet],"iif(.T.,","iif(.F.,")
      else
        var_filter[otwet]=strtran(var_filter[otwet],"iif(.F.,","iif(.T.,")
      endif
    othe
      set cursor on
      @ Y1+otwet+1,X1+3+len(filter_nam[otwet]) get var_filter[otwet]
      read
      set cursor off
    endcase
  endif
enddo
MSG=""          && Строка названия фильтра
fil_string=""   && Строка логического выражения фильтра
if .not.del_filter
is_var=.F.
for i=1 to SIZE_FILTER
  if .not.empty(var_filter[i])
  if i != 1.and.is_var
    fil_string=fil_string+".and."
    MSG=MSG+"  "
  endif
  is_var=.T.
  var_fld=filter_fld[i]
  type_var=&var_fld
  do case
  case type("type_var")="C"
    fil_string=fil_string+filter_fld[i]+"="+'"'+var_filter[i]+'"'
  case type("type_var")="N"
    fil_string=fil_string+filter_fld[i]+"="+ltrim(str(var_filter[i]))
  case type("type_var")="D"
    fil_string=fil_string+filter_fld[i]+"="+"ctod("+"'"+dtoc(var_filter[i])+"'"+")"
  case type("type_var")="L"
    if var_filter[i]="iif(.T.,"
      fil_string=fil_string+"("+filter_fld[i]+")"
    else
      fil_string=fil_string+"(.not."+filter_fld[i]+")"
    endif
  endcase
  if .not.empty(var_filter[i])
  MSG=MSG+filter_nam[i]+" "
  do case
  case .not.empty(filter_dbf[i])
    var_fld=mDict(filter_scr[i],var_filter[i],filter_cod[i],filter_dbf[i],filter_ntx[i])
  case .not.empty(filter_iif[i])
    var_fld=var_filter[i]
    var_fld=&var_fld
  othe
    var_fld=var_filter[i]
  endcase
  do case
  case type("var_fld")="C"
    MSG=MSG+'"'+alltrim(var_fld)+'"'
  case type("var_fld")="N"
    MSG=MSG+ltrim(str(var_fld))
  case type("var_fld")="D"
    MSG=MSG+dtoc(var_fld)
  case type("var_fld")="L"
    MSG=MSG+'"'+alltrim(var_fld)+'"'
  endcase
  endif
  endif
next
endif
restore screen from InpPage
if fil_string!=I->L_FILTER.or.del_filter        && Если фильтр изменен
  replace I->L_FILTER with fil_string
  replace I->N_FILTER with MSG
  MarkRecall()
  InitFilter()
  RETR=0
endif
set color to &old_color
return
*---------------------------------------------------------*
procedure DelFilter
del_filter=.T.
keyboard chr(27)
return
*---------------------------------------------------------*
procedure InpMonth
private old_color,ch,otwet,ptr,old_date,old_select,old_filter,j,i,new_ptr
old_color=setcolor(cw)
save screen to InpPage
DrawBox(Y_min,X_max-24,Y_min+2,X_max,ibox)
setcolor(cl)
MSG=' ТЕКУЩАЯ ДАТА '
@ Y_min,int((X_max-24+X_max)/2-len(MSG)/2) say MSG
??chr(7)
old_date=I->T_DATE
otwet=1
do while otwet!=0
  setcolor(cw)
  @ Y_min+1,X_max-19 prompt Month[month(I->T_DATE)]
  @ row(),col()+1 prompt str(year(I->T_DATE),4)
  @ row(),col() say " г."
  menu to otwet
  if otwet != 0
    ptr=month(I->T_DATE)
    ch=str(year(I->T_DATE),4)
    do case
    case otwet=1
      save screen
      setcolor(cdict)
      BoxTenj(Y_min+1,X_max-20,Y_min+14,X_max-11,ibox)
      new_ptr=achoice(Y_min+2,X_max-19,Y_min+13,X_max-12,month,"","",ptr)
      if new_ptr != 0
        ptr=new_ptr
      endif
      rest screen
    case otwet=2
      set cursor on
      @ Y_min+1,X_max-19+len(Month[month(I->T_DATE)])+1 get ch picture '####'
      read
      set cursor off
    endcase
    if lastkey() != 27
      replace I->T_DATE with ctod("01."+str(ptr,2)+"."+ch)
    endif
  endif
enddo
if old_date != I->T_DATE
  setcolor(old_color)
  restore screen from InpPage
  DrawDate(Y_min,X_max-24,Y_min+2,X_max)
  save screen to InpPage
  restore screen from InpPage
  if Fragen('','Будем переводить на новый месяц?')
    restore screen from InpPage
    MsgWrite("Ж д и т е . . .  Перевожу все на новый месяц...")
    CloseDBF()  && закрываю старый месяц
    old_path=bal_path
    bal_path=cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
    use_path=bal_path+"\"
    set path to &bal_path
    mkdir(bal_path)
    deles("*.*",bal_path)
    copys("*.*",old_path,bal_path)
    CreateNTX(.T.)
    OpenDBF()   && откpываю новый месяц
    OpenNTX()
    select 1
    set filter to
    go top
***
select 2
use_dbf=use_path+OborDBF
use_ntx=use_path+OborNTX
use &use_dbf index &use_ntx
total on str(NUMBER)+str(DEBET)+str(KREDIT) fields SUMMA to temp.dbf
use
select 2
use temp.dbf
index on str(NUMBER)+str(DEBET)+str(KREDIT) to temp.ntx
select 8
use_com=use_path+'come.fnd'
use &use_com
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
select 1
go top
do while (.not.eof())
  repl ASUMMA with 0
  for i=1 to nucount
   if .not.empty(nam[i])
    select 2
    if deb[i] != 0 .and. kre[i] != 0
     seek str(A->NUMBER)+str(deb[i])+str(kre[i])
     if found()
      M->summa=B->SUMMA
      var=nam[i]
      select 1
      if type("&var") = "N"
       if .not.empty(frm[i])
        repl &var with formula(frm[i])
       endif
      endif
     endif
    else
     M->summa=0
     var=nam[i]
     select 1
     if type("&var") = "N"
      if .not.empty(frm[i])
       repl &var with formula(frm[i])
      endif
     endif
    endif
    select 1
   endif
  next
  skip
enddo
***
select 2
use
dele file temp.dbf
dele file temp.ntx
***
    select 2
    use_dbf=use_path+OborDBF
    use_ntx=use_path+OborNTX
    use &use_dbf index &use_ntx
    zap
    use
    select 1
    CreateNTX(.T.)
    OpenNTX()
    InitFilter()
    RETR=0
  endif
endif
restore screen from InpPage
setcolor(old_color)
return
*---------------------------------------------------------*
function FindFond
parameters fld_ptr,Y1,X1,Y2,X2
if fld_ptr=1
  return " "
endif
private var_find,var_fld,otwet
private old_color,old_index
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG='  И С К А Т Ь '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
var_fld=jour_fld[fld_ptr]
var_find=nulvar(&var_fld)
@ Y1+2,X1+2 say jour_nam[fld_ptr]
set cursor on
@ row(),col()+2 get var_find
read
set cursor off
if lastkey() != 27
  MsgWrite("Сейчас поищем...")
  Nz=recno()
  log=0   && Есть ли индекс по этому полю
  if SIZE_INDX != 0
    for i=1 to SIZE_INDX
      if jour_fld[fld_ptr]==index_key[i]
        log=i
      endif
    next
  endif
  old_index=indexord()
  set order to log
  var_fld=jour_fld[fld_ptr]
  if indexord() != 0
    seek var_find
  else
    locate for var_find=&var_fld
  endif
  if found()
    RETR=2
  else
    ??chr(7)
    WarnWrite("Поиск не удачен...")
    go Nz
  endif
  set order to old_index
endif
restore screen from InpPage
set color to &old_color
return " "
*---------------------------------------------------------*
procedure DelFond
save screen
if Yes_Or_No(' У Д А Л И Т Ь ','Лицевые счета?',2)
  MsgWrite("Ждите... Удаляю...")
  if Mark=0
    delete
  else
    Mark=0
  endif
  select 2
  use &OborDBF index &OborNTX
  select 1
  go top
  do while .not.eof()
   if delete()
    select 2
    delete for A->NUMBER=B->NUMBER
    select 1
   endif
   skip
  enddo
  select 2
  pack
  use
  select 1
  pack
  go top
  RETR=2
else
  RETR=1
endif
restore screen
return
*---------------------------------------------------------*
function Selekt
parameters log,fld_ptr,Y1,X1,Y2,X2
if fld_ptr = 1
  WarnWrite("По полю метки селектирование не осуществляется...")
  RETR=1
  return " "
endif
private var_sele1,var_sele2,var_fld,var_num,otwet
private old_color
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
if log
  MSG=' З А С Е Л Е К Т И Р О В А Т Ь '
else
  MSG=' Р А З С Е Л Е К Т И Р О В А Т Ь '
endif
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
var_fld=jour_fld[fld_ptr]
var_sele1=nulvar(&var_fld)
var_sele2=var_sele1
set cursor on
@ Y1+2,X1+2 say jour_nam[fld_ptr]+" с "
@ row(),col() get var_sele1
@ Y1+4,X1+2+len(jour_nam[fld_ptr]) say "по "
@ row(),col() get var_sele2
read
set cursor off
if lastkey() != 27
  if log
    MsgWrite("Сейчас заселектируем...")
  else
    MsgWrite("Сейчас разселектируем...")
  endif
  Nz=recno()
  go top
  var_fld=jour_fld[fld_ptr]
  if empty(var_sele2)
    var_sele2=var_sele1
  endif
  if log
    do while .not.eof()
      if &var_fld.>=var_sele1.and.&var_fld.<=var_sele2
        if .not.delete()
          delete
          Mark=Mark+1
        endif
      endif
      skip
    enddo
  else
    do while .not.eof()
      if &var_fld.>=var_sele1.and.&var_fld.<=var_sele2
        if delete()
          recall
          Mark=Mark-1
        endif
      endif
      skip
    enddo
  endif
  go Nz
  RETR=2
else
  RETR=1
endif
restore screen from InpPage
set color to &old_color
return " "
*---------------------------------------------------------*
function GlossFunc
parameters mode,fld_ptr
private cur_field,key_last
key_last=LASTKEY()
do case
case mode <= 3
  return 1
case LASTKEY() = 27
  return 0
case LASTKEY() = 13.or.isdata(key_last)
  if key_last <> 13
     keyboard chr(key_last)
  endif
  set cursor on
  cur_field=dict_fld[fld_ptr]
  @ row(),col() get &cur_field
  read
  set cursor off
  return 2
case lastkey() = 22        && Ins
  append blank
  return 2
case lastkey() = -4        && F5
  dblpt1(dict_fld,dict_nam,TITL1)
case lastkey() = -5        && F6
  FindGloss(fld_ptr,Y_srm-2,X_srm-30,Y_srm+2,X_srm+30)
  return 2
case lastkey() = 7         && Del
  DelGloss()
  return 2
endcase
return 1
*---------------------------------------------------------*
function MenuGloss
set color to &cl
@ Y_max,X_min+10 say ' МЕНЮ:'
set color to &cmenu
MSG='INS'
@ row(),col() get MSG
@ row(),col() say 'Созд.'
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say 'Ввод'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say 'Удал.'
MSG='F5'
@ row(),col() get MSG
@ row(),col() say 'Печать'
MSG='F6'
@ row(),col() get MSG
@ row(),col() say 'Искать'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'Выход'
clear gets
return " "
*---------------------------------------------------------*
function FindGloss
parameters fld_ptr,Y1,X1,Y2,X2
private var_find,var_fld,otwet
private old_color
old_color = setcolor()
save screen to InpPage
set color to &cfr
BoxTenj(Y1,X1,Y2,X2,ibox)
set color to &cl
MSG=' И С К А Т Ь '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
set color to &cfr
var_fld=dict_fld[fld_ptr]
var_find=nulvar(&var_fld)
@ Y1+2,X1+2 say dict_nam[fld_ptr]
set cursor on
@ row(),col()+2 get var_find
read
set cursor off
if lastkey() != 27
  MsgWrite("Сейчас поищем...")
  Nz=recno()
  if type("var_find")="L"
    if var_find
      locate for &var_fld
    else
      locate for .not.&var_fld
    endif
  else
    locate for var_find=&var_fld
  endif
  if .not.found()
    ??chr(7)
    WarnWrite("Поиск не удачен...")
    go Nz
  endif
endif
restore screen from InpPage
set color to &old_color
return " "
*---------------------------------------------------------*
procedure DelGloss
save screen
if Yes_Or_No(' У Д А Л И Т Ь ','Текущую запись?',2)
  MsgWrite("Ждите... Удаляю...")
  delete
  pack
  go top
endif
restore screen
return

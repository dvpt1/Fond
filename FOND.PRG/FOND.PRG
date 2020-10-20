*----------------------------------------Конюхов Д.Л.-----*
*             О С H О В H Ы Е   Ф О H Д Ы                 *
*---------------------------------------------------------*
*=========================================================*
*                     CLIPPER  2.0                        *
*=========================================================*
external mdict,strnum,nulvar,strroun,strline
*---------------------------------------------------------*
#define F_NAME 1
#define F_SIZE 2
#define F_DATE 3
#define F_TIME 4
#define F_ATT  5
*---------------------------------------------------------*
public is_arhiv
*---------------------------------------------------------*
parameters param1
if type("param1") <> "C"
  param1 = ""
endif
************************
***InitTest()
************************
OpenMouse()
InitSets()
InitVars()
InitColor(param1)
InitFormul()
InitCalc()
InitMenu()
InitConfig()
*********************************************
*name_firm = __pinit(direxe()+'FOND.',__panti())
*********************************************
cur_path=getdir()
bal_path=cur_path+"\"+str(year(I->T_DATE),4)+"."+strtran(str(month(I->T_DATE),2)," ","0")
use_path=bal_path+"\"
set path to &bal_path
mkdir(bal_path)
CreateDBF()
OpenDBF()
CreateNTX(upper(param1)="/A")
OpenNTX()
ReDraw=.T.
PrgExit=.T.
do while PrgExit
 if ReDraw
  DrawFond()
  DrawDate()
  InitFilter()
  ReDraw=.F.
 endif
 setcolor(c)
 dbedit(Y_min+1,X_min+1,Y_max-3,X_max-1,jour_jou,"JourFunc","",jour_nam)
enddo
CloseDBF()
set color to w/n
clear
set cursor On
CloseMouse()
Quit

*---------------------------------------------------------*
function JourFunc
parameters mode,fld_ptr
cur_row=row()
RETR=1
Retr_User=.T.
Last_User=0
do while Retr_User
Retr_User=.F.
do case
case mode = 0
  @ Y_max, 2 say '██████████████'
  if mark != 0
    setcolor(cl)
    @ Y_max, 2 say "Помечено "+ltrim(str(Mark))
    setcolor(c)
  endif
case mode = 3
case mode = 4
do case
case (Last_User = 32).or.(lastkey() = 32)       && SPACE
  if delete()
    recall
    Mark=Mark-1
  else
    delete
    Mark=Mark+1
  endif
  RETR=2
case (Last_User = 22).or.(lastkey() = 22)       && INS
  if NewFond()
    InpFond()
  endif
case (Last_User = 13).or.(lastkey() = 13)       && Enter
  InpFond()
case (Last_User =  7).or.(lastkey()=   7)       && DEL
  DelFond()
case (Last_User = -2).or.(lastkey() = -2)       && F3
  SummaFond(Y_srm-5,X_srm-34,Y_srm+5,X_srm+33)
case (Last_User = -3).or.(lastkey() = -3)       && F4
  Raschet()
case (Last_User = -4).or.(lastkey() = -4)       && F5
  ComeTable()
case (Last_User = -5).or.(lastkey() = -5)       && F6
  InpFilter(Y_srm-3,X_srm-20,Y_srm+SIZE_FILTER,X_srm+20)
case (Last_User = -6).or.(lastkey() = -6)       && F7
  FindFond(fld_ptr,Y_srm-2,X_srm-30,Y_srm+2,X_srm+30)
case (Last_User = -7).or.(lastkey() = -7)       && F8
  InpMonth()
case (Last_User = -8).or.(lastkey() = -8)       && F9
  Tune(Y_srm-6,X_srm-20,Y_srm+6,X_srm+20)
case (Last_User = -9).or.(lastkey() = -9)       && F10
  MenuMain()
case (Last_User = 45).or.(lastkey() = 45)       && -
  Selekt(.F.,fld_ptr,Y_srm-3,X_srm-30,Y_srm+3,X_srm+30)
case (Last_User = 43).or.(lastkey() = 43)       && +
  Selekt(.T.,fld_ptr,Y_srm-3,X_srm-30,Y_srm+3,X_srm+30)
case (Last_User = 27).or.(lastkey() = 27)       && ESC
  if Fragen(' В Ы Х О Д ','Завершить работу с программой?')
    PrgExit=.F.
    RETR=0
  endif
endcase
endcase
enddo
return RETR
*---------------------------------------------------------*
procedure CreateDBF
select 1
use_dbf=use_path+MainDBF
if .not.file(use_dbf)
  creat &use_dbf from &MainSTR
endif
use_dbf=use_path+OborDBF
if .not.file(use_dbf)
  creat &use_dbf from &OborSTR
endif
if SIZE_DICT != 0
  for i=1 to SIZE_DICT
    name_dbf=use_path+dict_dbf[i]
    if .not.file(name_dbf)
      name_str=dict_str[i]
      creat &name_dbf from &name_str
    endif
  next
endif
return
*---------------------------------------------------------*
procedure OpenDBF
select 1
use_dbf=use_path+MainDBF
use &use_dbf
return
*---------------------------------------------------------*
procedure CreateNTX
parameters log
private old_color,old_select
old_color = setcolor()
old_select=select()
save screen
if SIZE_INDX != 0
  for i=1 to SIZE_INDX
    name_NTX=use_path+index_ntx[i]
    if (.not.file(name_NTX)).or.log
      name_key = index_key[i]
      setcolor(cw)
      BoxTenj(Y_srm-2,X_srm-10,Y_srm+2,X_srm+10,ibox)
      setcolor(cl)
      @ Y_srm-2,X_srm-5 say 'Сортирую...'
      setcolor(cw)
      @ Y_srm,int(X_srm-len(index_ntx[i])/2) say index_ntx[i]
      index on &name_key to &name_NTX
    endif
  next
  go top
endif
use_dbf=use_path+OborDBF
use_ntx=use_path+OborNTX
if (.not.file(use_ntx)).or.log
  select 8
  use &use_dbf
  setcolor(cw)
  BoxTenj(Y_srm-2,X_srm-10,Y_srm+2,X_srm+10,ibox)
  setcolor(cl)
  @ Y_srm-2,X_srm-5 say 'Сортирую...'
  setcolor(cw)
  @ Y_srm,int(X_srm-len(OborNTX)/2) say OborNTX
  index on str(NUMBER)+str(DEBET)+str(KREDIT) to &use_ntx
  use
endif
if SIZE_DICT != 0
  select 8
  for i=1 to SIZE_DICT
    if .not.empty(dict_ntx[i])
      name_NTX=use_path+dict_ntx[i]
      if .not.file(name_NTX).or.log
        name_dbf=use_path+dict_dbf[i]
        name_key=dict_key[i]
        setcolor(cw)
        BoxTenj(Y_srm-2,X_srm-10,Y_srm+2,X_srm+10,ibox)
        setcolor(cl)
        @ Y_srm-2,X_srm-5 say 'Сортирую...'
        setcolor(cw)
        @ Y_srm,int(X_srm-len(dict_ntx[i])/2) say dict_ntx[i]
        use &name_dbf
        index on &name_key to &name_NTX
        use
      endif
    endif
  next
endif
restore screen
select(old_select)
setcolor(old_color)
return
*---------------------------------------------------------*
procedure OpenNTX
for i=1 to SIZE_INDX
  ch = ltrim(str(i))
  Fl_NTX&ch = use_path+index_ntx[i]
next
do case
case SIZE_INDX=1
  set index to &Fl_NTX1
case SIZE_INDX=2
  set index to &Fl_NTX1,&Fl_NTX2
case SIZE_INDX=3
  set index to &Fl_NTX1,&Fl_NTX2,&Fl_NTX3
case SIZE_INDX=4
  set index to &Fl_NTX1,&Fl_NTX2,&Fl_NTX3,&Fl_NTX4
case SIZE_INDX=5
  set index to &Fl_NTX1,&Fl_NTX2,&Fl_NTX3,&Fl_NTX4,&Fl_NTX5
endcase
set order to I->REGIM_SORT
return
*---------------------------------------------------------*
procedure DrawFond
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
MSG=' В Е Д О М О С Т Ь '
@ Y_min,int(X_srm-len(MSG)/2) say MSG
@ Y_max,X_max-16 say '║ F10-М Е Н Ю ║'
return
*---------------------------------------------------------*
procedure DrawDate
setcolor(c)
@ Y_min,X_max-15 say replicate("█",15)
setcolor(cl)
MSG=alltrim(Month[month(I->T_DATE)])+" "+str(year(I->T_DATE),4)+"г."
@ Y_min,X_max-15 say MSG
return
*---------------------------------------------------------*
procedure InitFilter
setcolor(c)
@ Y_max-2,X_min say replicate('█',80)
@ Y_max-1,X_min+1 say replicate(' ',78)
setcolor(cl)
MSG=' В Ы Б О Р К А '
@ Y_max-2,int(X_srm-len(MSG)/2) say MSG
name_filter=alltrim(I->L_FILTER)
if empty(name_filter).or.SIZE_FILTER=0
  set filter to
else
  set filter to &name_filter
endif
go top
MSG=alltrim(I->N_FILTER)
setcolor(c)
@ Y_max-1,int(X_srm-len(MSG)/2)+1 say MSG
return
*---------------------------------------------------------*
function CloseDBF
MarkRecall()
select 1
use
return " "
*---------------------------------------------------------*
function MarkRecall
if Mark # 0
  save screen
  MsgWrite('Удаляю метки...')
  recall for delete()
  Mark = 0
  restore screen
endif
return " "

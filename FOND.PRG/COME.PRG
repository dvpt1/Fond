*---------------------------------------------------------*
function ComeTable
private old_color,old_select,InpPage,otwet,DoneCome,old_otwet,log_clear
old_color = setcolor()
save screen to InpPage
old_select = select()
select 8
use come.fnd
setcolor(c)
DrawBox(Y_min,X_min,Y_max,X_max,ibox)
setcolor(cl)
TITL='                 '
@ Y_min,int((X_min+X_max)/2-len(TITL)/2) say TITL
MenuTable()
setcolor(c)
declare name_var[4],name_fld[4]
name_var[1]="¬ï"
name_var[2]="¥¡¥â"
name_var[3]="p¥¤¨â"
name_var[4]="®p¬ã«  p áç¥â "
name_fld[1]="NAME"
name_fld[2]="DEBET"
name_fld[3]="KREDIT"
name_fld[4]="translate(substr(SUMMA,1,50))"
dbedit(Y_min+1,X_min+1,Y_max-1,X_max-1,name_fld,"TableFunc","",name_var,"ÄÄÄ","  ")
use
select(old_select)
restore screen from InpPage
setcolor(old_color)
return " "
*---------------------------------------------------------*
function MenuTable
setcolor(cl)
@ Y_max,X_min+10 say ': '
setcolor(cmenu)
MSG='Enter'
@ row(),col() get MSG
@ row(),col() say '¢®¤'
MSG='INS'
@ row(),col() get MSG
@ row(),col() say 'áâ ¢¨âì'
MSG='DEL'
@ row(),col() get MSG
@ row(),col() say '¤ «¨âì'
MSG='F5'
@ row(),col() get MSG
@ row(),col() say '¥ç âì'
MSG='ESC'
@ row(),col() get MSG
@ row(),col() say 'ëå®¤'
clear gets
return " "
*---------------------------------------------------------*
function TableFunc
parameters mode,ptr
private old_color,var,new_cod,log_prn,key_last
key_last=LASTKEY()
do case
case mode <= 3
case lastkey()=27
  return 0
case LASTKEY() = 13.or.isdata(key_last)
  do case
  case ptr=4
    repl H->SUMMA with ed(Y_srm-5,X_srm-25,Y_srm+5,X_srm+25,H->SUMMA)
  othe
    if key_last <> 13
      keyboard chr(key_last)
    endif
    var=name_fld[ptr]
    set cursor on
    @ row(),col() get &var
    read
    set cursor off
  endcase
case lastkey()=22   && INS
  insert()
  return 2
case lastkey()=7    && DEL
  DelGloss()
  return 2
case lastkey()=-4    && F5
  dblpt1(name_fld,name_var,TITL)
endcase
return 1

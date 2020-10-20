*---------------------------------------------------------*
procedure Tune
parameters Y1,X1,Y2,X2
private otwet,old_color
old_color = setcolor(cmenu)
save screen to OnePage
BoxTenj(Y1,X1,Y2,X2,ibox)
setcolor(cl)
MSG='  € ‘ ’  Ž ‰ Š € '
@ Y1,int((X1+X2)/2-len(MSG)/2) say MSG
otwet=1
do while (otwet != 0)
   setcolor(cmenu)
   @ Y1+ 2,X1+2 prompt "„¥¡¥â®¢ë©  áç¥â:"
   @ row(),col()+1 say I->DEBET
   @ Y1+ 4,X1+2 prompt "Šp¥¤¨â®¢ë© áç¥â:"
   @ row(),col()+1 say I->KREDIT
   @ Y1+ 6,X1+2 prompt "P áç¥â ¯®:"
   @ row(),col()+1 say iif(RashetDIC='species.dic','‚ˆ„“ Ž‘      ','Ž„P€‡„…‹…Hˆž')
   @ Y1+ 8,X1+2 prompt "˜¨à¨­  ¯¥ç â¨:"
   @ row(),col()+1 say I->LEN_LPT1
   @ Y1+10,X1+2 prompt "¥à¥¢®¤ «¨áâ :"
   @ row(),col()+1 say iif(I->IS_LIST,"„€ ","…’")
   menu to otwet
   if otwet!=0
     set cursor on
     do case
     case otwet=1
      @ Y1+2,X1+19 get I->DEBET
      read
     case otwet=2
      @ Y1+4,X1+19 get I->KREDIT
      read
     case otwet=3
      if RashetDIC='species.dic'
       store 'division.dic' to RashetDIC
       store 'division.ntx' to RashetNTX
      else
       store 'species.dic' to RashetDIC
       store 'species.ntx' to RashetNTX
      endif
     case otwet=4
      @ Y1+6,X1+17 get I->LEN_LPT1
      read
     case otwet=5
      repl I->IS_LIST with .not.(I->IS_LIST)
     endcase
     set cursor off
   endif
enddo
restore screen from OnePage
setcolor(old_color)
return

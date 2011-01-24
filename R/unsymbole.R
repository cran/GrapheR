unsymbole <-
function(symbol,col) {
  Fen31=tktoplevel()
  tkwm.resizable(Fen31,FALSE,FALSE)
  tktitle(Fen31)="GrapheR"
  tkgrab.set(Fen31)
  tkfocus(Fen31)
  symbol2=symbol
  col2=tclVar(col)
  col.choose=tkcanvas(Fen31,width="40",height="25",bg=tclvalue(col2))
  tkbind(col.choose,"<ButtonRelease-1>",function() {couleur(fen=Fen31,titre=229,var=col2,widg=col.choose,type="can",plusieurs=FALSE)})
  liste.symb=list()
  for (i in 1:8) {liste.symb[[i]]=tklabel(Fen31,height=35,width=35,font=Env$police,relief="groove",borderwidth=0,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Symbole",i,".gif",sep=""),fsep=.Platform$file.sep)))}
  if (symbol2==1) {tkconfigure(liste.symb[[1]],borderwidth=2)}
  if (symbol2==22) {tkconfigure(liste.symb[[2]],borderwidth=2)}
  if (symbol2==2) {tkconfigure(liste.symb[[3]],borderwidth=2)}
  if (symbol2==3) {tkconfigure(liste.symb[[4]],borderwidth=2)}
  if (symbol2==16) {tkconfigure(liste.symb[[5]],borderwidth=2)}
  if (symbol2==15) {tkconfigure(liste.symb[[6]],borderwidth=2)}
  if (symbol2==17) {tkconfigure(liste.symb[[7]],borderwidth=2)}
  if (symbol2==4) {tkconfigure(liste.symb[[8]],borderwidth=2)}
  symbol.choix=function(num,symb) {
    for (i in 1:8) {tkconfigure(liste.symb[[i]],borderwidth=0)}
    tkconfigure(liste.symb[[num]],borderwidth=2)
    symbol2<<-symb
  }
  tkbind(liste.symb[[1]],"<ButtonRelease-1>",function() {symbol.choix(num=1,symb=1)})
  tkbind(liste.symb[[2]],"<ButtonRelease-1>",function() {symbol.choix(num=2,symb=22)})
  tkbind(liste.symb[[3]],"<ButtonRelease-1>",function() {symbol.choix(num=3,symb=2)})
  tkbind(liste.symb[[4]],"<ButtonRelease-1>",function() {symbol.choix(num=4,symb=3)})
  tkbind(liste.symb[[5]],"<ButtonRelease-1>",function() {symbol.choix(num=5,symb=16)})
  tkbind(liste.symb[[6]],"<ButtonRelease-1>",function() {symbol.choix(num=6,symb=15)})
  tkbind(liste.symb[[7]],"<ButtonRelease-1>",function() {symbol.choix(num=7,symb=17)})
  tkbind(liste.symb[[8]],"<ButtonRelease-1>",function() {symbol.choix(num=8,symb=4)})
  ok=tkbutton(Fen31,text=Env$vocab[1,1],font=Env$police,width=16,command=function() {
    assign("symb",symbol2,pos=Env)
    assign("col.str1",tclvalue(col2),pos=Env)
    tkgrab.release(Fen31)
    tkdestroy(Fen31)
  })
  annuler=tkbutton(Fen31,text=Env$vocab[2,1],font=Env$police,width=16,command=function() {tkgrab.release(Fen31);tkdestroy(Fen31)})
  tkgrid(tklabel(Fen31,text="",font=Env$police))
  tkgrid(tklabel(Fen31,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Fen31,text="     ",font=Env$police),row=1,column=5)
  tkgrid(liste.symb[[1]],row=1,column=1)
  tkgrid(liste.symb[[2]],row=1,column=2)
  tkgrid(liste.symb[[3]],row=1,column=3)
  tkgrid(liste.symb[[4]],row=1,column=4)
  tkgrid(liste.symb[[5]],row=2,column=1)
  tkgrid(liste.symb[[6]],row=2,column=2)
  tkgrid(liste.symb[[7]],row=2,column=3)
  tkgrid(liste.symb[[8]],row=2,column=4)
  tkgrid(tklabel(Fen31,text=" ",font=tkfont.create(family="Arial",size=2)))
  tkgrid(tklabel(Fen31,text=Env$vocab[56,1],font=Env$police),row=4,column=1,columnspan=2,sticky="e")
  tkgrid(col.choose,row=4,column=3,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen31,text="",font=Env$police))
  tkgrid(ok,column=1,columnspan=4)
  tkgrid(tklabel(Fen31,text="",font=tkfont.create(family="Arial",size=4)))
  tkgrid(annuler,column=1,columnspan=4)
  tkgrid(tklabel(Fen31,text="",font=tkfont.create(family="Arial",size=4)))
}


plusieurssymboles <-
function(nb.niv,noms,symbol,col) {
  Fen32=tktoplevel()
  tkwm.resizable(Fen32,FALSE,FALSE)
  tktitle(Fen32)="GrapheR"
  tkgrab.set(Fen32)
  tkfocus(Fen32)
  symbol2=symbol
  col2=col
  liste.symb=list()
  for (i in 1:8) {liste.symb[[i]]=tklabel(Fen32,height=35,width=35,font=Env$police,relief="groove",borderwidth=0,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Symbole",i,".gif",sep=""),fsep=.Platform$file.sep)))}
  niv.liste=tklistbox(Fen32,height=7,selectmode="single",yscrollcommand=function(...) tkset(niv.liste.scroll,...))
  niv.liste.scroll=tkscrollbar(Fen32,repeatinterval=5,command=function(...) tkyview(niv.liste,...))
  for (i in 1:nb.niv) {tkinsert(niv.liste,"end",noms[i])}
  tkselection.set(niv.liste,0)
  if (symbol2[1]==1) {tkconfigure(liste.symb[[1]],borderwidth=2)}
  if (symbol2[1]==22) {tkconfigure(liste.symb[[2]],borderwidth=2)}
  if (symbol2[1]==2) {tkconfigure(liste.symb[[3]],borderwidth=2)}
  if (symbol2[1]==3) {tkconfigure(liste.symb[[4]],borderwidth=2)}
  if (symbol2[1]==16) {tkconfigure(liste.symb[[5]],borderwidth=2)}
  if (symbol2[1]==15) {tkconfigure(liste.symb[[6]],borderwidth=2)}
  if (symbol2[1]==17) {tkconfigure(liste.symb[[7]],borderwidth=2)}
  if (symbol2[1]==4) {tkconfigure(liste.symb[[8]],borderwidth=2)}
  col.choose=tkcanvas(Fen32,width="40",height="25",bg=tclvalue(tclVar(col2[as.numeric(tkcurselection(niv.liste))+1])))
  tkbind(niv.liste,"<ButtonRelease-1>",function() {
    for (i in 1:8) {tkconfigure(liste.symb[[i]],borderwidth=0)}
    if (symbol2[as.numeric(tkcurselection(niv.liste))+1]==1) {tkconfigure(liste.symb[[1]],borderwidth=2)}
    if (symbol2[as.numeric(tkcurselection(niv.liste))+1]==22) {tkconfigure(liste.symb[[2]],borderwidth=2)}
    if (symbol2[as.numeric(tkcurselection(niv.liste))+1]==2) {tkconfigure(liste.symb[[3]],borderwidth=2)}
    if (symbol2[as.numeric(tkcurselection(niv.liste))+1]==3) {tkconfigure(liste.symb[[4]],borderwidth=2)}
    if (symbol2[as.numeric(tkcurselection(niv.liste))+1]==16) {tkconfigure(liste.symb[[5]],borderwidth=2)}
    if (symbol2[as.numeric(tkcurselection(niv.liste))+1]==15) {tkconfigure(liste.symb[[6]],borderwidth=2)}
    if (symbol2[as.numeric(tkcurselection(niv.liste))+1]==17) {tkconfigure(liste.symb[[7]],borderwidth=2)}
    if (symbol2[as.numeric(tkcurselection(niv.liste))+1]==4) {tkconfigure(liste.symb[[8]],borderwidth=2)}
    tkconfigure(col.choose,bg=tclvalue(tclVar(col2[as.numeric(tkcurselection(niv.liste))+1])))
  })
  tkbind(col.choose,"<ButtonRelease-1>",function() {col2[as.numeric(tkcurselection(niv.liste))+1]<<-couleur(fen=Fen32,titre=229,var=col2[as.numeric(tkcurselection(niv.liste))+1],widg=col.choose,type="can",plusieurs=TRUE)})
  symbol.choix=function(num,symb) {
    for (i in 1:8) {tkconfigure(liste.symb[[i]],borderwidth=0)}
    tkconfigure(liste.symb[[num]],borderwidth=2)
    symbol2[as.numeric(tkcurselection(niv.liste))+1]<<-symb
  }
  tkbind(liste.symb[[1]],"<ButtonRelease-1>",function() {symbol.choix(num=1,symb=1)})
  tkbind(liste.symb[[2]],"<ButtonRelease-1>",function() {symbol.choix(num=2,symb=22)})
  tkbind(liste.symb[[3]],"<ButtonRelease-1>",function() {symbol.choix(num=3,symb=2)})
  tkbind(liste.symb[[4]],"<ButtonRelease-1>",function() {symbol.choix(num=4,symb=3)})
  tkbind(liste.symb[[5]],"<ButtonRelease-1>",function() {symbol.choix(num=5,symb=16)})
  tkbind(liste.symb[[6]],"<ButtonRelease-1>",function() {symbol.choix(num=6,symb=15)})
  tkbind(liste.symb[[7]],"<ButtonRelease-1>",function() {symbol.choix(num=7,symb=17)})
  tkbind(liste.symb[[8]],"<ButtonRelease-1>",function() {symbol.choix(num=8,symb=4)})
  ok=tkbutton(Fen32,text=Env$vocab[1,1],font=Env$police,width=16,command=function() {
    assign("symb",symbol2,pos=Env)
    assign("col.str1",col2,pos=Env)
    tkgrab.release(Fen32)
    tkdestroy(Fen32)
  })
  annuler=tkbutton(Fen32,text=Env$vocab[2,1],font=Env$police,width=16,command=function() {tkgrab.release(Fen32);tkdestroy(Fen32)})
  tkgrid(tklabel(Fen32,text="",font=Env$police))
  tkgrid(tklabel(Fen32,text="     ",font=Env$police),row=1,column=0)
  tkgrid(niv.liste,niv.liste.scroll,row=1,column=1,rowspan=4,sticky="n")
  tkgrid.configure(niv.liste.scroll,sticky="ens")
  tkgrid(tklabel(Fen32,text="     ",font=Env$police),row=1,column=2)
  tkgrid(liste.symb[[1]],row=1,column=3)
  tkgrid(liste.symb[[2]],row=1,column=4)
  tkgrid(liste.symb[[3]],row=1,column=5)
  tkgrid(liste.symb[[4]],row=1,column=6)
  tkgrid(tklabel(Fen32,text="   ",font=Env$police),row=1,column=7)
  tkgrid(liste.symb[[5]],row=2,column=3)
  tkgrid(liste.symb[[6]],row=2,column=4)
  tkgrid(liste.symb[[7]],row=2,column=5)
  tkgrid(liste.symb[[8]],row=2,column=6)
  tkgrid(tklabel(Fen32,text=" ",font=tkfont.create(family="Arial",size=2)),row=3,column=3)
  tkgrid(tklabel(Fen32,text=Env$vocab[56,1],font=Env$police),row=4,column=3,columnspan=2,sticky="e")
  tkgrid(col.choose,row=4,column=5,columnspan=2,sticky="w")
  tkgrid(tklabel(Fen32,text="",font=Env$police))
  tkgrid(ok,row=6,column=1)
  tkgrid(annuler,row=6,column=3,columnspan=4)
  tkgrid(tklabel(Fen32,text=" ",font=tkfont.create(family="Arial",size=4)))
}


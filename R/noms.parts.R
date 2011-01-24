noms.parts <-
function(var,nb.niv,noms) {
  if (nchar(var)>0) {
    Fen20=tktoplevel()
    tkwm.resizable(Fen20,FALSE,FALSE)
    tktitle(Fen20)="GrapheR"
    tkgrab.set(Fen20)
    tkfocus(Fen20)
    ancien.nom=tklistbox(Fen20,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(ancien.nom.scroll,...))
    ancien.nom.scroll=tkscrollbar(Fen20,repeatinterval=5,command=function(...) tkyview(ancien.nom,...))
    noms2=noms
    for (i in 1:nb.niv) tkinsert(ancien.nom,"end",noms2[i])
    nouveau.nom=tkentry(Fen20,width=20,font=Env$police)
    tkbind(ancien.nom,"<ButtonRelease-1>",function() {
	tkdelete(nouveau.nom,0,"end")
	tkinsert(nouveau.nom,"end",noms2[as.numeric(tclvalue(tkcurselection(ancien.nom)))+1])
    })
    tkbind(nouveau.nom,"<ButtonRelease-1>",function() {tkdelete(nouveau.nom,0,"end")})
    rename=tkbutton(Fen20,text=Env$vocab[38,1],font=Env$police,command=function() {
      noms2[as.numeric(tclvalue(tkcurselection(ancien.nom)))+1]<<-as.character(tclvalue(tkget(nouveau.nom)))
      tkdelete(ancien.nom,0,"end")
      for (i in 1:length(noms2)) tkinsert(ancien.nom,"end",noms2[i])
	tkdelete(nouveau.nom,0,"end")
    })
    ok=tkbutton(Fen20,text=Env$vocab[1,1],font=Env$police,command=function() {
	assign("noms1",noms2,pos=Env)
      tkgrab.release(Fen20)
      tkdestroy(Fen20)
    })
    annuler=tkbutton(Fen20,text=Env$vocab[2,1],font=Env$police,command=function() {tkgrab.release(Fen20);tkdestroy(Fen20)})
    tkgrid(tklabel(Fen20,text="",font=Env$police))
    tkgrid(tklabel(Fen20,text="          ",font=Env$police),row=1,column=0)
    tkgrid(tklabel(Fen20,text=Env$vocab[162,1],font=Env$police),row=1,column=1)
    tkgrid(tklabel(Fen20,text=Env$vocab[37,1],font=Env$police),row=1,column=3)
    tkgrid(tklabel(Fen20,text="          ",font=Env$police),row=1,column=4)
    tkgrid(ancien.nom,ancien.nom.scroll,row=2,column=1,rowspan=3);tkgrid.configure(ancien.nom.scroll,sticky="ens")
    tkgrid(tklabel(Fen20,text="               ",font=Env$police),row=2,column=2)
    tkgrid(nouveau.nom,row=2,column=3,sticky="n")
    tkgrid(rename,row=3,column=3,sticky="we")
    tkgrid(tklabel(Fen20,text="",font=Env$police))
    tkgrid(ok,row=6,column=1,sticky="we")
    tkgrid(annuler,row=6,column=3,sticky="we")
    tkgrid(tklabel(Fen20,text=" ",font=tkfont.create(family="Arial",size=4)))
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[6,1],icon="error",type="ok")}
}


legende <-
function(type,nb.niv,noms,titre) {
  Fen10=tktoplevel()
  tkwm.geometry(Fen10,paste("200x",145+nb.niv*21,sep=""))
  tkwm.resizable(Fen10,FALSE,FALSE)
  tktitle(Fen10)="GrapheR"
  tkgrab.set(Fen10)
  tkfocus(Fen10)
  titre2=tclVar(titre)
  legende.titre=tkentry(Fen10,width=15,font=Env$police,textvariable=titre2)
  ok=tkbutton(Fen10,text=Env$vocab[1,1],font=Env$police,width=16,command=function() {
    for (i in 1:nb.niv) {noms[i]=tclvalue(tkget(noms.liste[[i]]))}
    if (type=="bar") {
      assign("noms2",noms,pos=Env)
    } else if (type=="courbe" | type=="nuage") {
      assign("noms1",noms,pos=Env)
    }
    assign("legende.titre",tclvalue(titre2),pos=Env)
    tkgrab.release(Fen10)
    tkdestroy(Fen10)
  })
  annuler=tkbutton(Fen10,text=Env$vocab[2,1],font=Env$police,width=16,command=function() {tkgrab.release(Fen10);tkdestroy(Fen10)})
  tkgrid(tklabel(Fen10,text="",font=Env$police))
  tkgrid(tklabel(Fen10,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Fen10,text=Env$vocab[54,1],font=Env$police),row=1,column=1,sticky="e")
  tkgrid(legende.titre,row=1,column=2,sticky="w")
  tkgrid(tklabel(Fen10,text=" ",font=tkfont.create(family="Arial",size=3)))
  noms.liste=list()
  for (i in 1:nb.niv) {
    noms.liste[[i]]=tkentry(Fen10,width=15,font=Env$police,textvariable=tclVar(noms[i]))
    tkgrid(tklabel(Fen10,text=paste(Env$vocab[161,1],i,Env$vocab[228,1],sep=""),font=Env$police),row=i+2,column=1,sticky="e")
    tkgrid(noms.liste[[i]],row=i+2,column=2,sticky="w")
  }
  tkgrid(tklabel(Fen10,text="",font=Env$police))
  tkgrid(ok,column=1,columnspan=2)
  tkgrid(tklabel(Fen10,text="",font=tkfont.create(family="Arial",size=4)))
  tkgrid(annuler,column=1,columnspan=2)
}


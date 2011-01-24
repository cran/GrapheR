noms1 <-
function(type,fact,noms) {
  if (nchar(fact)>0) {
    nb.niv=nlevels(Env$datas.GrapheR[,fact])
    Fen7=tktoplevel()
    tkwm.geometry(Fen7,paste("173x",110+nb.niv*21,sep=""))
    tkwm.resizable(Fen7,FALSE,FALSE)
    tktitle(Fen7)="GrapheR"
    tkgrab.set(Fen7)
    tkfocus(Fen7)
    ok=tkbutton(Fen7,text=Env$vocab[1,1],font=Env$police,width=16,command=function() {
      for (i in 1:nb.niv) {noms[i]=tclvalue(tkget(noms.liste[[i]]))}
      assign("noms1",noms,pos=Env)
      tkgrab.release(Fen7)
      tkdestroy(Fen7)
    })
    annuler=tkbutton(Fen7,text=Env$vocab[2,1],font=Env$police,width=16,command=function() {tkgrab.release(Fen7);tkdestroy(Fen7)})
    tkgrid(tklabel(Fen7,text="",font=Env$police))
    tkgrid(tklabel(Fen7,text="      ",font=Env$police),row=1,column=0)
    noms.liste=list()
    lab=if (type=="box") {Env$vocab[158,1]} else if (type=="bar1") {Env$vocab[159,1]} else if (type=="bar2") {Env$vocab[160,1]}
    for (i in 1:nb.niv) {
      noms.liste[[i]]=tkentry(Fen7,width=10,font=Env$police,textvariable=tclVar(noms[i]))
      tkgrid(tklabel(Fen7,text=paste(lab,i,Env$vocab[228,1],sep=""),font=Env$police),row=i,column=1,sticky="e")
      tkgrid(noms.liste[[i]],row=i,column=2,sticky="w")
    }
    tkgrid(tklabel(Fen7,text="",font=Env$police))
    tkgrid(ok,column=1,columnspan=2)
    tkgrid(tklabel(Fen7,text="",font=tkfont.create(family="Arial",size=3)))
    tkgrid(annuler,column=1,columnspan=2)
  } else {
    msg=if (type=="box") {Env$vocab[7,1]} else {Env$vocab[8,1]}
    tkmessageBox(title=Env$vocab[3,1],message=msg,icon="error",type="ok")
  }
}


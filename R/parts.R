parts <-
function(var,nb.niv,noms,prop) {
  if (nchar(var)>0) {
    Fen19=tktoplevel()
    tkwm.resizable(Fen19,FALSE,FALSE)
    tktitle(Fen19)="GrapheR"
    tkgrab.set(Fen19)
    tkfocus(Fen19)
    niv.liste=tklistbox(Fen19,height=5,font=Env$police,selectmode="multiple",yscrollcommand=function(...) tkset(niv.liste.scroll,...))
    niv.liste.scroll=tkscrollbar(Fen19,repeatinterval=5,command=function(...) tkyview(niv.liste,...))
    for (i in 1:nb.niv) {tkinsert(niv.liste,"end",noms[i])}
    for (i in 1:length(strsplit(prop,split=" ")[[1]])) {tkselection.set(niv.liste,as.numeric(strsplit(prop,split=" ")[[1]][i]))}
    ok=tkbutton(Fen19,text=Env$vocab[1,1],font=Env$police,command=function() {
	assign("niv.prop",tclvalue(tkcurselection(niv.liste)),pos=Env)
	assign("nb.niv1",length(strsplit(Env$niv.prop,split=" ")[[1]]),pos=Env)
	assign("noms1",noms[as.numeric(strsplit(Env$niv.prop,split=" ")[[1]])+1],pos=Env)
	assign("col.str1",gray.colors(Env$nb.niv1),pos=Env)
	assign("dens.motif",rep(0,Env$nb.niv1),pos=Env)
	assign("ang.motif",rep(0,Env$nb.niv1),pos=Env)
	assign("col.motif",rep("black",Env$nb.niv1),pos=Env)
	tkgrab.release(Fen19)
	tkdestroy(Fen19)
    })
    annuler=tkbutton(Fen19,text=Env$vocab[2,1],font=Env$police,command=function() {tkgrab.release(Fen19);tkdestroy(Fen19)})
    tkgrid(tklabel(Fen19,text="",font=Env$police))
    tkgrid(tklabel(Fen19,text="     ",font=Env$police),row=1,column=0)
    tkgrid(tklabel(Fen19,text=Env$vocab[136,1],font=Env$police),row=1,column=1)
    tkgrid(tklabel(Fen19,text="     ",font=Env$police),row=1,column=2)
    tkgrid(tklabel(Fen19,text=" ",font=tkfont.create(family="Arial",size=1)))
    tkgrid(niv.liste,niv.liste.scroll,row=3,column=1)
    tkgrid.configure(niv.liste.scroll,sticky="ens")
    tkgrid(tklabel(Fen19,text="",font=Env$police))
    tkgrid(ok,column=1,sticky="we")
    tkgrid(tklabel(Fen19,text="",font=tkfont.create(family="Arial",size=4)))
    tkgrid(annuler,column=1,sticky="we")
    tkgrid(tklabel(Fen19,text="",font=tkfont.create(family="Arial",size=4)))
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[6,1],icon="error",type="ok")}
}


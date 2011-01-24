niv <-
function(fact,niv) {
  Fen30=tktoplevel()
  tkwm.resizable(Fen30,FALSE,FALSE)
  tktitle(Fen30)="GrapheR"
  tkgrab.set(Fen30)
  tkfocus(Fen30)
  niv.liste=tklistbox(Fen30,height=5,font=Env$police,selectmode="multiple",yscrollcommand=function(...) tkset(niv.liste.scroll,...))
  niv.liste.scroll=tkscrollbar(Fen30,repeatinterval=5,command=function(...) tkyview(niv.liste,...))
  for (i in 1:nlevels(Env$datas.GrapheR[,fact])) {tkinsert(niv.liste,"end",levels(Env$datas.GrapheR[,fact])[i])}
  for (i in 1:length(strsplit(niv,split=" ")[[1]])) {tkselection.set(niv.liste,as.numeric(strsplit(niv,split=" ")[[1]][i]))}
  ok=tkbutton(Fen30,text=Env$vocab[1,1],font=Env$police,command=function() {
    assign("niv.fact",tclvalue(tkcurselection(niv.liste)),pos=Env)
    assign("nb.niv1",length(strsplit(Env$niv.fact,split=" ")[[1]]),pos=Env)
    assign("noms1",levels(Env$datas.GrapheR[,fact])[as.numeric(strsplit(Env$niv.fact,split=" ")[[1]])+1],pos=Env)
    assign("symb",rep(1,Env$nb.niv1),pos=Env)
    assign("col.str1",rep("black",Env$nb.niv1),pos=Env)
    assign("lignes",rep(Env$vocab[166,1],Env$nb.niv1),pos=Env)
    assign("traits",rep(Env$vocab[75,1],Env$nb.niv1),pos=Env)
    assign("ep.lignes",rep(1,Env$nb.niv1),pos=Env)
    assign("droite",rep("aucune",Env$nb.niv1),pos=Env)
    assign("regression",rep("mc",Env$nb.niv1),pos=Env)
    assign("hor",rep("",Env$nb.niv1),pos=Env)
    assign("ver",rep("",Env$nb.niv1),pos=Env)
    assign("coeffa",rep("",Env$nb.niv1),pos=Env)
    assign("coeffb",rep("",Env$nb.niv1),pos=Env)
    tkgrab.release(Fen30)
    tkdestroy(Fen30)
  })
  annuler=tkbutton(Fen30,text=Env$vocab[2,1],font=Env$police,command=function() {tkgrab.release(Fen30);tkdestroy(Fen30)})
  tkgrid(tklabel(Fen30,text=" ",font=tkfont.create(family="Arial",size=3)))
  tkgrid(tklabel(Fen30,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Fen30,text=Env$vocab[155,1],font=Env$police),row=1,column=1)
  tkgrid(tklabel(Fen30,text="     ",font=Env$police),row=1,column=2)
  tkgrid(tklabel(Fen30,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(niv.liste,niv.liste.scroll,column=1)
  tkgrid.configure(niv.liste.scroll,sticky="ens")
  tkgrid(tklabel(Fen30,text="",font=Env$police))
  tkgrid(ok,column=1,sticky="we")
  tkgrid(tklabel(Fen30,text="",font=tkfont.create(family="Arial",size=4)))
  tkgrid(annuler,column=1,sticky="we")
  tkgrid(tklabel(Fen30,text=" ",font=tkfont.create(family="Arial",size=4)))
}


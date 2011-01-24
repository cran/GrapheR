renommer <-
function() {
  if (length(names(Env$datas.GrapheR)>=1)) {
    Fen2=tktoplevel()
    tkwm.resizable(Fen2,FALSE,FALSE)
    tktitle(Fen2)="GrapheR"
    tkgrab.set(Fen2)
    tkfocus(Fen2)
    ancien.nom=tklistbox(Fen2,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(ancien.nom.scroll,...))
    ancien.nom.scroll=tkscrollbar(Fen2,repeatinterval=5,command=function(...) tkyview(ancien.nom,...))
    names2=names(Env$datas.GrapheR)
    for (i in 1:length(names2)) tkinsert(ancien.nom,"end",names2[i])
    nouveau.nom=tkentry(Fen2,width=20,font=Env$police)
    tkbind(ancien.nom,"<ButtonRelease-1>",function() {tkdelete(nouveau.nom,0,"end");tkinsert(nouveau.nom,"end",names2[as.numeric(tclvalue(tkcurselection(ancien.nom)))+1])})
    tkbind(nouveau.nom,"<ButtonRelease-1>",function() {tkdelete(nouveau.nom,0,"end")})
    rename.fct=function(old,new) {
      names2[as.numeric(tclvalue(tkcurselection(old)))+1]=as.character(tclvalue(tkget(new)))
      assign("names2",names2,pos=parent.frame(n=1),inherits=TRUE)
      tkdelete(old,0,"end")
      for (i in 1:length(names2)) {tkinsert(old,"end",names2[i])}
	tkdelete(new,0,"end")
    }
    rename=tkbutton(Fen2,text=Env$vocab[38,1],font=Env$police,command=function() {rename.fct(old=ancien.nom,new=nouveau.nom)})
    ok.fct=function() {
      names(Env$datas.GrapheR)=names2
      tkdelete(Env$var.num.list,0,"end")
      tkdelete(Env$fact.list,0,"end")
      for (i in 1:length(Env$variables)) {
        if (Env$variables[i]=="N") {tkinsert(Env$var.num.list,"end",names(Env$datas.GrapheR)[i])}
        if (Env$variables[i]=="F") {tkinsert(Env$fact.list,"end",names(Env$datas.GrapheR)[i])}
      }
      tkgrab.release(Fen2)
      tkdestroy(Fen2)
    }
    ok=tkbutton(Fen2,text=Env$vocab[1,1],font=Env$police,command=function() {ok.fct()})
    annuler=tkbutton(Fen2,text=Env$vocab[2,1],font=Env$police,command=function() {tkgrab.release(Fen2);tkdestroy(Fen2)})
    tkgrid(tklabel(Fen2,text="",font=Env$police))
    tkgrid(tklabel(Fen2,text="          ",font=Env$police),row=1,column=0)
    tkgrid(tklabel(Fen2,text=Env$vocab[36,1],font=Env$police),row=1,column=1)
    tkgrid(tklabel(Fen2,text=Env$vocab[37,1],font=Env$police),row=1,column=3)
    tkgrid(ancien.nom,ancien.nom.scroll,row=2,column=1,rowspan=3);tkgrid.configure(ancien.nom.scroll,sticky="ens")
    tkgrid(tklabel(Fen2,text="               ",font=Env$police),row=2,column=2)
    tkgrid(nouveau.nom,row=2,column=3,sticky="n")
    tkgrid(tklabel(Fen2,text="          ",font=Env$police),row=2,column=4)
    tkgrid(rename,row=3,column=3,sticky="we")
    tkgrid(tklabel(Fen2,text="",font=Env$police))
    tkgrid(ok,row=6,column=1,sticky="we")
    tkgrid(annuler,row=6,column=3,sticky="we")
    tkgrid(tklabel(Fen2,text="",font=tkfont.create(family="Arial",size=4)))
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[5,1],icon="error",type="ok")}
}


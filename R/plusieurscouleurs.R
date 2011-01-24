plusieurscouleurs <-
function(nb.niv,noms,type,str,col) {
  Fen12=tktoplevel()
  tkwm.resizable(Fen12,FALSE,FALSE)
  tktitle(Fen12)="GrapheR"
  tkgrab.set(Fen12)
  tkfocus(Fen12)
  col2=col
  niv.liste=tklistbox(Fen12,height=5,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(niv.liste.scroll,...))
  niv.liste.scroll=tkscrollbar(Fen12,repeatinterval=5,command=function(...) tkyview(niv.liste,...))
  for (i in 1:nb.niv) {tkinsert(niv.liste,"end",noms[i])}
  tkselection.set(niv.liste,0)
  col.choose=tkcanvas(Fen12,width="40",height="25",bg=tclvalue(tclVar(col2[as.numeric(tkcurselection(niv.liste))+1])))
  tkbind(niv.liste,"<ButtonRelease-1>",function() {tkconfigure(col.choose,bg=tclvalue(tclVar(col2[as.numeric(tkcurselection(niv.liste))+1])))})
  titre=if (type=="bar") {
    if (str=="str1") {113} else if (str=="str2") {95}
  } else if (type=="cam") {143}
  tkbind(col.choose,"<ButtonRelease-1>",function() {col2[as.numeric(tkcurselection(niv.liste))+1]<<-couleur(fen=Fen12,titre=titre,var=col2[as.numeric(tkcurselection(niv.liste))+1],widg=col.choose,type="can",plusieurs=TRUE)})
  ok=tkbutton(Fen12,text=Env$vocab[1,1],font=Env$police,width=16,command=function() {
    if (str=="str1") {
      assign("col.str1",col2,pos=Env)
    } else if (str=="str2") {
      assign("col.str2",col2,pos=Env)
    }
    tkgrab.release(Fen12)
    tkdestroy(Fen12)
  })
  annuler=tkbutton(Fen12,text=Env$vocab[2,1],font=Env$police,width=16,command=function() {tkgrab.release(Fen12);tkdestroy(Fen12)})
  tkgrid(tklabel(Fen12,text="",font=Env$police))
  tkgrid(tklabel(Fen12,text="    ",font=Env$police),row=1,column=0)
  tkgrid(niv.liste,niv.liste.scroll,row=1,column=1)
  tkgrid.configure(niv.liste.scroll,sticky="ens")
  tkgrid(tklabel(Fen12,text="   ",font=Env$police),row=1,column=2)
  tkgrid(tklabel(Fen12,text=Env$vocab[56,1],font=Env$police),row=1,column=3)
  tkgrid(col.choose,row=1,column=4)
  tkgrid(tklabel(Fen12,text="    ",font=Env$police),row=1,column=5)
  tkgrid(tklabel(Fen12,text="",font=Env$police))
  tkgrid(ok,row=3,column=1)
  tkgrid(annuler,row=3,column=3,columnspan=2)
  tkgrid(tklabel(Fen12,text="",font=tkfont.create(family="Arial",size=4)))	
}


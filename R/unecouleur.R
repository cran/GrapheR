unecouleur <-
function(str,col) {
  Fen11=tktoplevel()
  tkwm.resizable(Fen11,FALSE,FALSE)
  tktitle(Fen11)="GrapheR"
  tkgrab.set(Fen11)
  tkfocus(Fen11)
  col2=tclVar("")
  tclvalue(col2)=col
  col.choose=tkcanvas(Fen11,width="40",height="25",bg=tclvalue(col2))
  titre=if (str=="str1") {113} else if (str=="str2") {95}
  tkbind(col.choose,"<ButtonRelease-1>",function() {couleur(fen=Fen11,titre=titre,var=col2,widg=col.choose,type="can",plusieurs=FALSE)})
  ok=tkbutton(Fen11,text=Env$vocab[1,1],font=Env$police,width=16,command=function() {
    if (str=="str1") {assign("col.str1",tclvalue(col2),pos=Env)} else if (str=="str2") {assign("col.str2",tclvalue(col2),pos=Env)}
    tkgrab.release(Fen11)
    tkdestroy(Fen11)
  })
  annuler=tkbutton(Fen11,text=Env$vocab[2,1],font=Env$police,width=16,command=function() {tkgrab.release(Fen11);tkdestroy(Fen11)})
  tkgrid(tklabel(Fen11,text="",font=Env$police))
  tkgrid(tklabel(Fen11,text="   ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Fen11,text=paste(Env$vocab[titre,1],":   ",sep=""),font=Env$police),row=1,column=1,sticky="e")
  tkgrid(col.choose,row=1,column=2,sticky="w")
  tkgrid(tklabel(Fen11,text="   ",font=Env$police),row=1,column=3)
  tkgrid(tklabel(Fen11,text="",font=Env$police))
  tkgrid(ok,column=1,columnspan=2)
  tkgrid(tklabel(Fen11,text="",font=tkfont.create(family="Arial",size=4)))
  tkgrid(annuler,column=1,columnspan=2)
  tkgrid(tklabel(Fen11,text="",font=tkfont.create(family="Arial",size=4)))
}


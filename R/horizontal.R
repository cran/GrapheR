horizontal <-
function() {
  Hor=tktoplevel()
  tkwm.resizable(Hor,FALSE,FALSE)
  tktitle(Hor)="GrapheR"
  tkgrab.set(Hor)
  ordValue=tclVar("")
  ord=tkentry(Hor,width=8,textvariable=ordValue,font=Env$police)
  ord.but=tkbutton(Hor,text=Env$vocab[180,1],width=20,font=Env$police,command=function() {
    ord.val=round(locator(n=1)$y[1],4)
    tclvalue(ordValue)=ord.val
    tkdelete(ord,0,"end")
    tkinsert(ord,"end",ord.val)
    tkfocus(Hor)
  })
  typeValue=tclVar(Env$vocab[75,1])
  type.choose=ttkcombobox(Hor,font=Env$police,values=c(Env$vocab[75,1],Env$vocab[76,1],Env$vocab[77,1]),textvariable=typeValue,state="readonly")
  epValue=tclVar("1")
  ep.choose=tkscale(Hor,from=1,to=5,showvalue=TRUE,font=Env$police,variable=epValue,resolution=1,orient="horizontal")
  colValue=tclVar("black")
  col.choose=tkcanvas(Hor,width="40",height="25",bg=tclvalue(colValue))
  tkbind(col.choose,"<ButtonRelease-1>",function() {couleur(fen=Hor,titre=231,var=colValue,widg=col.choose,type="can",plusieurs=FALSE)})
  tracer=tkbutton(Hor,text=Env$vocab[181,1],font=Env$police,font=Env$police,width=16,command=function() {
    if (dev.cur()>1) {
	type=if (tclvalue(typeValue)==Env$vocab[75,1]) {1} else if (tclvalue(typeValue)==Env$vocab[76,1]) {2} else {3}
      abline(h=as.numeric(tclvalue(ordValue)),lty=type,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))
    } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  fermer=tkbutton(Hor,text=Env$vocab[182,1],font=Env$police,font=Env$police,width=16,command=function() {tkgrab.release(Hor);tkdestroy(Hor);tkfocus(Env$Toolbar)})
  tkgrid(tklabel(Hor,text=" ",font=Env$police))
  tkgrid(tklabel(Hor,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Hor,text=Env$vocab[183,1],font=Env$police),row=1,column=1,sticky="e")
  tkgrid(ord,row=1,column=2)
  tkgrid(tklabel(Hor,text="     ",font=Env$police),row=1,column=3)
  tkgrid(ord.but,row=1,column=4)
  tkgrid(tklabel(Hor,text="     ",font=Env$police),row=1,column=5)
  tkgrid(tklabel(Hor,text=" ",font=tkfont.create(family="Arial",size=5)))
  tkgrid(tklabel(Hor,text=Env$vocab[74,1],font=Env$police),row=3,column=1,sticky="e")
  tkgrid(type.choose,row=3,column=2,columnspan=3,sticky="w")
  tkgrid(tklabel(Hor,text=Env$vocab[78,1],font=Env$police),row=4,column=1,sticky="e")
  tkgrid(ep.choose,row=4,column=2,columnspan=3,sticky="w")
  tkgrid(tklabel(Hor,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(tklabel(Hor,text=Env$vocab[56,1],font=Env$police),row=6,column=1,sticky="e")
  tkgrid(col.choose,row=6,column=2,columnspan=3,sticky="w")
  tkgrid(tklabel(Hor,text=" ",font=Env$police))
  tkgrid(tracer,row=8,column=1,columnspan=2,sticky="e")
  tkgrid(fermer,row=8,column=3,columnspan=2)
  tkgrid(tklabel(Hor,text=" ",font=tkfont.create(family="Arial",size=4)))
}


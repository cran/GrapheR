vertical <-
function() {
  Ver=tktoplevel()
  tkwm.resizable(Ver,FALSE,FALSE)
  tktitle(Ver)="GrapheR"
  tkgrab.set(Ver)
  absValue=tclVar("")
  abs=tkentry(Ver,width=8,textvariable=absValue,font=Env$police)
  abs.but=tkbutton(Ver,text=Env$vocab[180,1],width=20,font=Env$police,command=function() {
    abs.val=round(locator(n=1)$x[1],4)
    tclvalue(absValue)=abs.val
    tkdelete(abs,0,"end")
    tkinsert(abs,"end",abs.val)
    tkfocus(Ver)
  })
  typeValue=tclVar(Env$vocab[75,1])
  type.choose=ttkcombobox(Ver,font=Env$police,values=c(Env$vocab[75,1],Env$vocab[76,1],Env$vocab[77,1]),textvariable=typeValue,state="readonly")
  epValue=tclVar("1")
  ep.choose=tkscale(Ver,from=1,to=5,showvalue=TRUE,font=Env$police,variable=epValue,resolution=1,orient="horizontal")
  colValue=tclVar("black")
  col.choose=tkcanvas(Ver,width="40",height="25",bg=tclvalue(colValue))
  tkbind(col.choose,"<ButtonRelease-1>",function() {couleur(fen=Ver,titre=231,var=colValue,widg=col.choose,type="can",plusieurs=FALSE)})
  tracer=tkbutton(Ver,text=Env$vocab[181,1],font=Env$police,font=Env$police,width=16,command=function() {
    if (dev.cur()>1) {
	type=if (tclvalue(typeValue)==Env$vocab[75,1]) {1} else if (tclvalue(typeValue)==Env$vocab[76,1]) {2} else {3}
      abline(v=as.numeric(tclvalue(absValue)),lty=type,lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue))
    } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  fermer=tkbutton(Ver,text=Env$vocab[182,1],font=Env$police,font=Env$police,width=16,command=function() {tkgrab.release(Ver);tkdestroy(Ver);tkfocus(Env$Toolbar)})
  tkgrid(tklabel(Ver,text=" ",font=Env$police))
  tkgrid(tklabel(Ver,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Ver,text=Env$vocab[179,1],font=Env$police),row=1,column=1,sticky="e")
  tkgrid(abs,row=1,column=2)
  tkgrid(tklabel(Ver,text="     ",font=Env$police),row=1,column=3)
  tkgrid(abs.but,row=1,column=4)
  tkgrid(tklabel(Ver,text="     ",font=Env$police),row=1,column=5)
  tkgrid(tklabel(Ver,text=" ",font=tkfont.create(family="Arial",size=5)))
  tkgrid(tklabel(Ver,text=Env$vocab[74,1],font=Env$police),row=3,column=1,sticky="e")
  tkgrid(type.choose,row=3,column=2,columnspan=3,sticky="w")
  tkgrid(tklabel(Ver,text=Env$vocab[78,1],font=Env$police),row=4,column=1,sticky="e")
  tkgrid(ep.choose,row=4,column=2,columnspan=3,sticky="w")
  tkgrid(tklabel(Ver,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(tklabel(Ver,text=Env$vocab[56,1],font=Env$police),row=6,column=1,sticky="e")
  tkgrid(col.choose,row=6,column=2,columnspan=3,sticky="w")
  tkgrid(tklabel(Ver,text=" ",font=Env$police))
  tkgrid(tracer,row=8,column=1,columnspan=2,sticky="e")
  tkgrid(fermer,row=8,column=3,columnspan=2)
  tkgrid(tklabel(Ver,text=" ",font=tkfont.create(family="Arial",size=4)))
}


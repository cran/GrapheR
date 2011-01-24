affine <-
function(log.axes) {
  Aff=tktoplevel()
  tkwm.resizable(Aff,FALSE,FALSE)
  tktitle(Aff)="GrapheR"
  tkgrab.set(Aff)
  coeffaValue=tclVar("")
  coeffa=tkentry(Aff,width=8,textvariable=coeffaValue,font=Env$police)
  coeffbValue=tclVar("")
  coeffb=tkentry(Aff,width=8,textvariable=coeffbValue,font=Env$police)
  coeffs.but=tkbutton(Aff,text=Env$vocab[185,1],width=30,font=Env$police,command=function() {
    coord=locator(n=2)
    pt1x=coord$x[1]
    pt1y=coord$y[1]
    pt2x=coord$x[2]
    pt2y=coord$y[2]
    a=round(((pt2y-pt1y)/(pt2x-pt1x)),4)
    b=round(pt1y-a*pt1x,4)
    tclvalue(coeffaValue)=a
    tclvalue(coeffbValue)=b
    tkdelete(coeffa,0,"end")
    tkdelete(coeffb,0,"end")
    tkinsert(coeffa,"end",a)
    tkinsert(coeffb,"end",b)
    tkfocus(Aff)
  })
  typeValue=tclVar(Env$vocab[75,1])
  type.choose=ttkcombobox(Aff,font=Env$police,values=c(Env$vocab[75,1],Env$vocab[76,1],Env$vocab[77,1]),textvariable=typeValue,state="readonly")
  epValue=tclVar("1")
  ep.choose=tkscale(Aff,from=1,to=5,showvalue=TRUE,font=Env$police,variable=epValue,resolution=1,orient="horizontal")
  colValue=tclVar("black")
  col.choose=tkcanvas(Aff,width="40",height="25",bg=tclvalue(colValue))
  tkbind(col.choose,"<ButtonRelease-1>",function() {couleur(fen=Aff,titre=231,var=colValue,widg=col.choose,type="can",plusieurs=FALSE)})
  tracer=tkbutton(Aff,text=Env$vocab[181,1],font=Env$police,font=Env$police,width=16,command=function() {
    if (dev.cur()>1) {
	type=if (tclvalue(typeValue)==Env$vocab[75,1]) {1} else if (tclvalue(typeValue)==Env$vocab[76,1]) {2} else {3}
      abline(as.numeric(tclvalue(coeffbValue)),as.numeric(tclvalue(coeffaValue)),lty=type,
        lwd=as.numeric(tclvalue(epValue)),col=tclvalue(colValue),untf=if (log.axes=="") {FALSE} else {TRUE})
    } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  fermer=tkbutton(Aff,text=Env$vocab[182,1],font=Env$police,font=Env$police,width=16,command=function() {tkgrab.release(Aff);tkdestroy(Aff);tkfocus(Env$Toolbar)})
  tkgrid(tklabel(Aff,text=" ",font=Env$police))
  tkgrid(tklabel(Aff,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Aff,text=Env$vocab[184,1],font=Env$police),row=1,column=1,sticky="e")
  tkgrid(tklabel(Aff,text="y  =  ",font=Env$police),row=1,column=2)
  tkgrid(coeffa,row=1,column=3)
  tkgrid(tklabel(Aff,text=" x   +  ",font=Env$police),row=1,column=4)
  tkgrid(coeffb,row=1,column=5)
  tkgrid(tklabel(Aff,text="        ",font=Env$police),row=1,column=6)
  tkgrid(tklabel(Aff,text=" ",font=tkfont.create(family="Arial",size=5)))
  tkgrid(coeffs.but,row=3,column=2,columnspan=4)
  tkgrid(tklabel(Aff,text=" ",font=tkfont.create(family="Arial",size=5)))
  tkgrid(tklabel(Aff,text=Env$vocab[74,1],font=Env$police),row=5,column=1,sticky="e")
  tkgrid(type.choose,row=5,column=2,columnspan=4,sticky="w")
  tkgrid(tklabel(Aff,text=Env$vocab[78,1],font=Env$police),row=6,column=1,sticky="e")
  tkgrid(ep.choose,row=6,column=2,columnspan=4,sticky="w")
  tkgrid(tklabel(Aff,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(tklabel(Aff,text=Env$vocab[56,1],font=Env$police),row=8,column=1,sticky="e")
  tkgrid(col.choose,row=8,column=2,columnspan=4,sticky="w")
  tkgrid(tklabel(Aff,text=" ",font=Env$police))
  tkgrid(tracer,row=10,column=1,columnspan=3)
  tkgrid(fermer,row=10,column=3,columnspan=3,sticky="e")
  tkgrid(tklabel(Aff,text=" ",font=tkfont.create(family="Arial",size=4)))
}


texte <-
function() {
  Text=tktoplevel()
  tkwm.resizable(Text,FALSE,FALSE)
  tktitle(Text)="GrapheR"
  tkgrab.set(Text)
  txtValue=tclVar("")
  txt=tkentry(Text,width=33,font=Env$police,textvariable=txtValue)
  xValue=tclVar("")
  yValue=tclVar("")
  coordx=tkentry(Text,width=8,font=Env$police,textvariable=xValue)
  coordy=tkentry(Text,width=8,font=Env$police,textvariable=yValue)
  coord.but=tkbutton(Text,text=Env$vocab[180,1],width=20,font=Env$police,command=function() {
    coord=locator(n=1)
    x=round(coord$x[1],4)
    y=round(coord$y[1],4)
    tclvalue(xValue)=x
    tclvalue(yValue)=y
    tkdelete(coordx,0,"end")
    tkdelete(coordy,0,"end")
    tkinsert(coordx,"end",x)
    tkinsert(coordy,"end",y)
    tkfocus(Text)
  })
  tailleValue=tclVar("1")
  taille.choose=tkscale(Text,from=0.5,to=5,showvalue=TRUE,font=Env$police,variable=tailleValue,resolution=0.25,orient="horizontal")
  colValue=tclVar("black")
  col.choose=tkbutton(Text,text="Aa",font=tkfont.create(family="Arial",size=10,weight="bold"),command=function() {couleur(fen=Text,titre=232,var=colValue,widg=col.choose,type="but",plusieurs=FALSE)})
  tracer=tkbutton(Text,text=Env$vocab[181,1],font=Env$police,font=Env$police,width=16,command=function() {
    if (dev.cur()>1) {
    text(as.numeric(tclvalue(xValue)),as.numeric(tclvalue(yValue)),labels=tclvalue(txtValue),cex=as.numeric(tclvalue(tailleValue)),
      col=tclvalue(colValue))
    } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  fermer=tkbutton(Text,text=Env$vocab[182,1],font=Env$police,font=Env$police,width=16,command=function() {tkgrab.release(Text);tkdestroy(Text);tkfocus(Env$Toolbar)})
  tkgrid(tklabel(Text,text=" ",font=Env$police))
  tkgrid(tklabel(Text,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Text,text=Env$vocab[186,1],font=Env$police),row=1,column=1,sticky="e")
  tkgrid(txt,row=1,column=2,columnspan=3,sticky="w")
  tkgrid(tklabel(Text,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(tklabel(Text,text=Env$vocab[187,1],font=Env$police),row=3,column=1,sticky="e")
  tkgrid(tklabel(Text,text=Env$vocab[237,1],font=Env$police),row=4,column=1,sticky="e")
  tkgrid(coordx,row=4,column=2)
  tkgrid(tklabel(Text,text=Env$vocab[238,1],font=Env$police),row=5,column=1,sticky="e")
  tkgrid(coordy,row=5,column=2)
  tkgrid(tklabel(Text,text="   ",font=Env$police),row=4,column=3)
  tkgrid(coord.but,row=4,column=4,rowspan=2)
  tkgrid(tklabel(Text,text="       ",font=Env$police),row=4,column=5)
  tkgrid(tklabel(Text,text=Env$vocab[57,1],font=Env$police),row=6,column=1,sticky="e")
  tkgrid(taille.choose,row=6,column=2,columnspan=3,sticky="w")
  tkgrid(tklabel(Text,text=" ",font=tkfont.create(family="Arial",size=1)))
  tkgrid(tklabel(Text,text=Env$vocab[56,1],font=Env$police),row=8,column=1,sticky="e")
  tkgrid(col.choose,row=8,column=2,columnspan=3,sticky="w")
  tkgrid(tklabel(Text,text=" ",font=Env$police))
  tkgrid(tracer,row=10,column=1,columnspan=2,sticky="e")
  tkgrid(fermer,row=10,column=4)
  tkgrid(tklabel(Text,text=" ",font=tkfont.create(family="Arial",size=4)))
}


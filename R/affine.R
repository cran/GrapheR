affine <-
function() {
  fr7.close()
  for (i in 1:length(Env$l.fr7)) {tkdestroy(Env$l.fr7[[i]])}
  Env$l.fr7<-list()
  tkconfigure(Env$l.frames$Fr7,borderwidth=3)
  Env$l.fr7$equation.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[165,1],font=Env$police)
  Env$l.fr7$coeffa.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[166,1],font=Env$police)
  Env$l.fr7$coeffa.wdg<-tkentry(Env$l.frames$Fr7,width=8,textvariable=Env$l.var$add.param1,font=Env$police)
  tkbind(Env$l.fr7$coeffa.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr7$coeffa.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr7$coeffb.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[167,1],font=Env$police)
  Env$l.fr7$coeffb.wdg<-tkentry(Env$l.frames$Fr7,width=8,textvariable=Env$l.var$add.param2,font=Env$police)
  tkbind(Env$l.fr7$coeffb.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr7$coeffb.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr7$coeffs.but<-tkbutton(Env$l.frames$Fr7,text=Env$voc[168,1],width=30,font=Env$police,command=function() {
    if (dev.cur()>1) {
	coord<-locator(n=2)
	pt1x<-coord$x[1]
	pt1y<-coord$y[1]
	pt2x<-coord$x[2]
	pt2y<-coord$y[2]
	a<-round(((pt2y-pt1y)/(pt2x-pt1x)),4)
	b<-round(pt1y-a*pt1x,4)
	tclvalue(Env$l.var$add.param1)<-a
	tclvalue(Env$l.var$add.param2)<-b
    } else {
	msg(text=Env$voc[163,1],type="error")
    }
  })
  Env$l.fr7$trait.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[59,1],font=Env$police)
  Env$l.fr7$trait.wdg<-ttkcombobox(Env$l.frames$Fr7,font=Env$police,values=c(Env$voc[60:62,1]),textvariable=Env$l.var$add.trait,state="readonly")
  Env$l.fr7$epaisseur.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[63,1],font=Env$police)
  Env$l.fr7$epaisseur.wdg<-tkscale(Env$l.frames$Fr7,from=1,to=5,showvalue=TRUE,font=Env$police,variable=Env$l.var$add.epaisseur1,resolution=1,orient="horizontal")
  Env$l.fr7$col.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[45,1],font=Env$police)
  Env$l.fr7$col.wdg<-tkcanvas(Env$l.frames$Fr7,width="40",height="25",bg=tclvalue(Env$l.var$add.col1))
  tkbind(Env$l.fr7$col.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$add.col1),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$add.col1)<-temp
	tkconfigure(Env$l.fr7$col.wdg,bg=tclvalue(Env$l.var$add.col1))
    }
  })
  Env$l.fr7$tracer<-tkbutton(Env$l.frames$Fr7,text=Env$voc[72,1],font=Env$police,font=Env$police,width=16,command=function() {
    if (dev.cur()>1) {
      abline(as.numeric(tclvalue(Env$l.var$add.param2)),as.numeric(tclvalue(Env$l.var$add.param1)),
	  lty=type.trait(type=tclvalue(Env$l.var$add.trait)),lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	  col=tclvalue(Env$l.var$add.col1),untf=ifelse(graphe.log()=="",FALSE,TRUE))
    } else {
	msg(text=Env$voc[163,1],type="error")
    }
  })
  Env$l.fr7$fermer<-tkbutton(Env$l.frames$Fr7,width=16,text=Env$voc[152,1],font=Env$police,command=fr7.close)
  Env$l.fr7$espace.ver1<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver2<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver3<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.hor1<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  Env$l.fr7$espace.hor2<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  tkgrid(Env$l.fr7$espace.ver1)
  tkgrid(Env$l.fr7$espace.hor1,row=1,column=0)
  tkgrid(Env$l.fr7$equation.lab,row=1,column=1,sticky="e")
  tkgrid(Env$l.fr7$coeffa.lab,row=1,column=2)
  tkgrid(Env$l.fr7$coeffa.wdg,row=1,column=3)
  tkgrid(Env$l.fr7$coeffb.lab,row=1,column=4)
  tkgrid(Env$l.fr7$coeffb.wdg,row=1,column=5)
  tkgrid(Env$l.fr7$espace.hor2,row=1,column=6)
  tkgrid(Env$l.fr7$coeffs.but,row=2,column=2,columnspan=4)
  tkgrid(Env$l.fr7$trait.lab,row=3,column=1,sticky="e")
  tkgrid(Env$l.fr7$trait.wdg,row=3,column=2,columnspan=4,sticky="w")
  tkgrid(Env$l.fr7$epaisseur.lab,row=4,column=1,sticky="e")
  tkgrid(Env$l.fr7$epaisseur.wdg,row=4,column=2,columnspan=4,sticky="w")
  tkgrid(Env$l.fr7$col.lab,row=5,column=1,sticky="e")
  tkgrid(Env$l.fr7$col.wdg,row=5,column=2,columnspan=4,sticky="w")
  tkgrid(Env$l.fr7$espace.ver2)
  tkgrid(Env$l.fr7$tracer,row=7,column=1,columnspan=3)
  tkgrid(Env$l.fr7$fermer,row=7,column=3,columnspan=3,sticky="e")
  tkgrid(Env$l.fr7$espace.ver3)
}


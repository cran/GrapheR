horizontal <-
function() {
  fr7.close()
  for (i in 1:length(Env$l.fr7)) {tkdestroy(Env$l.fr7[[i]])}
  Env$l.fr7<-list()
  tkconfigure(Env$l.frames$Fr7,borderwidth=3)
  Env$l.fr7$ord.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[164,1],font=Env$police)
  Env$l.fr7$ord.wdg<-tkentry(Env$l.frames$Fr7,width=8,textvariable=Env$l.var$add.param1,font=Env$police)
  tkbind(Env$l.fr7$ord.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr7$ord.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr7$ord.but<-tkbutton(Env$l.frames$Fr7,text=Env$voc[162,1],width=20,font=Env$police,command=function() {
    if (dev.cur()>1) {
	ord.val<-round(locator(n=1)$y[1],4)
	tclvalue(Env$l.var$add.param1)<-ord.val
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
      abline(h=as.numeric(tclvalue(Env$l.var$add.param1)),lty=type.trait(type=tclvalue(Env$l.var$add.trait)),
	  lwd=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),col=tclvalue(Env$l.var$add.col1))
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
  Env$l.fr7$espace.hor3<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  tkgrid(Env$l.fr7$espace.ver1)
  tkgrid(Env$l.fr7$espace.hor1,row=1,column=0)
  tkgrid(Env$l.fr7$ord.lab,row=1,column=1,sticky="e")
  tkgrid(Env$l.fr7$ord.wdg,row=1,column=2)
  tkgrid(Env$l.fr7$espace.hor2,row=1,column=3)
  tkgrid(Env$l.fr7$ord.but,row=1,column=4)
  tkgrid(Env$l.fr7$espace.hor3,row=1,column=5)
  tkgrid(Env$l.fr7$trait.lab,row=2,column=1,sticky="e")
  tkgrid(Env$l.fr7$trait.wdg,row=2,column=2,columnspan=3,sticky="w")
  tkgrid(Env$l.fr7$epaisseur.lab,row=3,column=1,sticky="e")
  tkgrid(Env$l.fr7$epaisseur.wdg,row=3,column=2,columnspan=3,sticky="w")
  tkgrid(Env$l.fr7$col.lab,row=4,column=1,sticky="e")
  tkgrid(Env$l.fr7$col.wdg,row=4,column=2,columnspan=3,sticky="w")
  tkgrid(Env$l.fr7$espace.ver2)
  tkgrid(Env$l.fr7$tracer,row=6,column=1,columnspan=2,sticky="e")
  tkgrid(Env$l.fr7$fermer,row=6,column=3,columnspan=2)
  tkgrid(Env$l.fr7$espace.ver3)
}


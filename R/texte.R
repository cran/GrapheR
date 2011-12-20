texte <-
function() {
  fr7.close()
  for (i in 1:length(Env$l.fr7)) {tkdestroy(Env$l.fr7[[i]])}
  Env$l.fr7<-list()
  tkconfigure(Env$l.frames$Fr7,borderwidth=3)
  Env$l.fr7$txt.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[202,1],font=Env$police)
  Env$l.fr7$txt.wdg<-tkentry(Env$l.frames$Fr7,width=33,font=Env$police,textvariable=Env$l.var$add.txt)
  Env$l.fr7$coords.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[203,1],font=Env$police)
  Env$l.fr7$x.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[204,1],font=Env$police)
  Env$l.fr7$x.wdg<-tkentry(Env$l.frames$Fr7,width=8,font=Env$police,textvariable=Env$l.var$add.param1)
  tkbind(Env$l.fr7$x.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr7$x.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr7$y.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[205,1],font=Env$police)
  Env$l.fr7$y.wdg<-tkentry(Env$l.frames$Fr7,width=8,font=Env$police,textvariable=Env$l.var$add.param2)
  tkbind(Env$l.fr7$y.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr7$y.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr7$coords.but<-tkbutton(Env$l.frames$Fr7,text=Env$voc[162,1],width=20,font=Env$police,command=function() {
    if (dev.cur()>1) {
	coords<-locator(n=1)
	tclvalue(Env$l.var$add.param1)<-round(coords$x[1],4)
	tclvalue(Env$l.var$add.param2)<-round(coords$y[1],4)
    } else {
	msg(text=Env$voc[163,1],type="error")
    }
  })
  Env$l.fr7$taille.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[46,1],font=Env$police)
  Env$l.fr7$taille.wdg<-tkscale(Env$l.frames$Fr7,from=0.5,to=5,showvalue=TRUE,font=Env$police,variable=Env$l.var$add.epaisseur1,resolution=0.25,orient="horizontal")
  Env$l.fr7$col.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[45,1],font=Env$police)
  Env$l.fr7$col.wdg<-tkbutton(Env$l.frames$Fr7,text="Aa",font=Env$police3,foreground=tclvalue(Env$l.var$add.col1),activeforeground=tclvalue(Env$l.var$add.col1),command=function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$add.col1),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$add.col1)<-temp
	tkconfigure(Env$l.fr7$col.wdg,foreground=temp,activeforeground=temp)
    }
  })
  Env$l.fr7$tracer<-tkbutton(Env$l.frames$Fr7,text=Env$voc[72,1],font=Env$police,width=16,command=function() {
    if (dev.cur()>1) {
	text(as.numeric(tclvalue(Env$l.var$add.param1)),as.numeric(tclvalue(Env$l.var$add.param2)),
	  labels=tclvalue(Env$l.var$add.txt),cex=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),
	  col=tclvalue(Env$l.var$add.col1))
	if (Env$l.code$save==TRUE) {
	  sink(file=file.path(Env$l.code$folder,paste(paste("GrapheR",paste(strsplit(as.character(Sys.Date()),split="-")[[1]],collapse="."),
	    sep="-"),".R",sep=""),fsep=.Platform$file.sep),append=TRUE)
	  cat("# Added: text\n\n")
	  texte<-paste("text(",tclvalue(Env$l.var$add.param1),", ",tclvalue(Env$l.var$add.param2),sep="")
	  texte<-paste(texte,", labels=\"",tclvalue(Env$l.var$add.txt),"\"",sep="")
	  if (tclvalue(Env$l.var$add.col1)!="black" & tclvalue(Env$l.var$add.col1)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$add.col1),"\"",sep="")}
	  texte<-paste(texte,", cex=",tclvalue(Env$l.var$add.epaisseur1),")\n\n",sep="")
	  cat(texte)
	  sink(NULL)
	}
    } else {
	msg(text=Env$voc[163,1],type="error")
    }
  })
  Env$l.fr7$fermer<-tkbutton(Env$l.frames$Fr7,text=Env$voc[152,1],font=Env$police,width=16,command=fr7.close)
  Env$l.fr7$espace.ver1<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver2<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver3<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.hor1<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  Env$l.fr7$espace.hor2<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  Env$l.fr7$espace.hor3<-tklabel(Env$l.frames$Fr7,text="   ",font=Env$police3)
  tkgrid(Env$l.fr7$espace.ver1)
  tkgrid(Env$l.fr7$espace.hor1,row=1,column=0)
  tkgrid(Env$l.fr7$txt.lab,row=1,column=1,sticky="e")
  tkgrid(Env$l.fr7$txt.wdg,row=1,column=2,columnspan=3,sticky="w")
  tkgrid(Env$l.fr7$espace.hor2,row=1,column=5)
  tkgrid(Env$l.fr7$coords.lab,row=2,column=1,sticky="e")
  tkgrid(Env$l.fr7$x.lab,row=3,column=1,sticky="e")
  tkgrid(Env$l.fr7$x.wdg,row=3,column=2)
  tkgrid(Env$l.fr7$y.lab,row=4,column=1,sticky="e")
  tkgrid(Env$l.fr7$y.wdg,row=4,column=2)
  tkgrid(Env$l.fr7$espace.hor3,row=3,column=3)
  tkgrid(Env$l.fr7$coords.but,row=3,column=4,rowspan=2)
  tkgrid(Env$l.fr7$taille.lab,row=5,column=1,sticky="e")
  tkgrid(Env$l.fr7$taille.wdg,row=5,column=2,columnspan=3,sticky="w")
  tkgrid(Env$l.fr7$col.lab,row=6,column=1,sticky="e")
  tkgrid(Env$l.fr7$col.wdg,row=6,column=2,columnspan=3,sticky="w")
  tkgrid(Env$l.fr7$espace.ver2)
  tkgrid(Env$l.fr7$tracer,row=8,column=1,columnspan=2,sticky="e")
  tkgrid(Env$l.fr7$fermer,row=8,column=4)
  tkgrid(Env$l.fr7$espace.ver3)
}


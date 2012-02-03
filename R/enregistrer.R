enregistrer <-
function() {
  fr7.close()
  for (i in 1:length(Env$l.fr7)) {tkdestroy(Env$l.fr7[[i]])}
  Env$l.fr7<-list()
  tkconfigure(Env$l.frames$Fr7,borderwidth=3)
  Env$l.fr7$fenetre.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[211,1],font=Env$police)
  fenetres<-character(length(dev.list()))
  for (i in 1:length(dev.list())) {
    fenetres[i]<-paste(Env$voc[212,1],dev.list()[i],sep="_")
  }
  Env$l.fr7$fenetre.wdg<-ttkcombobox(Env$l.frames$Fr7,font=Env$police,values=fenetres,textvariable=Env$l.var$fen.num,state="readonly")
  Env$l.fr7$fichier.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[213,1],font=Env$police)
  Env$l.fr7$fichier.wdg<-ttkcombobox(Env$l.frames$Fr7,width=8,font=Env$police,values=c("jpg","png","tiff"),textvariable=Env$l.var$fen.type,state="readonly")
  Env$l.fr7$largeur.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[214,1],font=Env$police)
  Env$l.fr7$largeur.wdg<-tkscale(Env$l.frames$Fr7,from=400,to=5000,showvalue=TRUE,font=Env$police,variable=Env$l.var$fen.larg,resolution=50,orient="horizontal")
  Env$l.fr7$res.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[265,1],font=Env$police)
  Env$l.fr7$res.wdg<-ttkcombobox(Env$l.frames$Fr7,width=8,font=Env$police,values=c("72","150","300","600"),textvariable=Env$l.var$fen.res,state="readonly")
  tkbind(Env$l.fr7$res.wdg,"<<ComboboxSelected>>",function() {
    if (tclvalue(Env$l.var$fen.res)=="72") {
	tclvalue(Env$l.var$fen.larg)=="600"
	tkset(Env$l.fr7$largeur.wdg,"600")
    }
    if (tclvalue(Env$l.var$fen.res)=="150") {
	tclvalue(Env$l.var$fen.larg)=="1250"
	tkset(Env$l.fr7$largeur.wdg,"1250")
    }
    if (tclvalue(Env$l.var$fen.res)=="300") {
	tclvalue(Env$l.var$fen.larg)=="2500"
	tkset(Env$l.fr7$largeur.wdg,"2500")
    }
    if (tclvalue(Env$l.var$fen.res)=="600") {
	tclvalue(Env$l.var$fen.larg)=="5000"
	tkset(Env$l.fr7$largeur.wdg,"5000")
    }
  })
  Env$l.fr7$sauver<-tkbutton(Env$l.frames$Fr7,text=Env$voc[215,1],font=Env$police,width=16,command=function() {
    if (dev.cur()>1) {
	if (nchar(tclvalue(Env$l.var$fen.num))>0) {
	  dev.set(as.numeric(strsplit(tclvalue(Env$l.var$fen.num),split="_")[[1]][2]))
	} else {
	  dev.set(dev.list()[1])
	}
	if (tclvalue(Env$l.var$fen.type)=="jpg") {
	  file<-tclvalue(tkgetSaveFile(filetypes="{Jpg {.jpg}}"))
	  if (nchar(file)>0) {
	    dev.print(jpeg,filename=paste(strsplit(file,".jpg"),".jpg",sep=""),quality=100,units="px",
		width=as.numeric(tclvalue(Env$l.var$fen.larg)),res=as.numeric(tclvalue(Env$l.var$fen.res)))
	  }
	}
	if (tclvalue(Env$l.var$fen.type)=="png") {
	  file<-tclvalue(tkgetSaveFile(filetypes="{Png {.png}}"))
	  if (nchar(file)>0) {
	    dev.print(png,filename=paste(strsplit(file,".png"),".png",sep=""),units="px",
		width=as.numeric(tclvalue(Env$l.var$fen.larg)),res=as.numeric(tclvalue(Env$l.var$fen.res)))
	  }
	}
	if (tclvalue(Env$l.var$fen.type)=="tiff") {
	  file<-tclvalue(tkgetSaveFile(filetypes="{Tiff {.tiff}}"))
	  if (nchar(file)>0) {
	    dev.print(tiff,filename=paste(strsplit(file,".tiff"),".tiff",sep=""),units="px",
		width=as.numeric(tclvalue(Env$l.var$fen.larg)),compression="none",res=as.numeric(tclvalue(Env$l.var$fen.res)))
	  }
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
  tkgrid(Env$l.fr7$espace.ver1)
  tkgrid(Env$l.fr7$espace.hor1,row=1,column=0)
  tkgrid(Env$l.fr7$fenetre.lab,row=1,column=1,sticky="e")
  tkgrid(Env$l.fr7$fenetre.wdg,row=1,column=2,sticky="w")
  tkgrid(Env$l.fr7$espace.hor2,row=1,column=3)
  tkgrid(Env$l.fr7$fichier.lab,row=2,column=1,sticky="e")
  tkgrid(Env$l.fr7$fichier.wdg,row=2,column=2,sticky="w")
  tkgrid(Env$l.fr7$largeur.lab,row=3,column=1,sticky="se")
  tkgrid(Env$l.fr7$largeur.wdg,row=3,column=2,sticky="w")
  tkgrid(Env$l.fr7$res.lab,row=4,column=1,sticky="se")
  tkgrid(Env$l.fr7$res.wdg,row=4,column=2,sticky="w")
  tkgrid(Env$l.fr7$espace.ver2)
  tkgrid(Env$l.fr7$sauver,row=6,column=1)
  tkgrid(Env$l.fr7$fermer,row=6,column=2)
  tkgrid(Env$l.fr7$espace.ver3)
}

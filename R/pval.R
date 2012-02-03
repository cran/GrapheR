pval <-
function() {
  fr7.close()
  for (i in 1:length(Env$l.fr7)) {tkdestroy(Env$l.fr7[[i]])}
  Env$l.fr7<-list()
  tkconfigure(Env$l.frames$Fr7,borderwidth=3)
  Env$l.fr7$titre.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[210,1],font=Env$police3)
  Env$l.fr7$img1<-tklabel(Env$l.frames$Fr7,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Pvalue1.gif",fsep=.Platform$file.sep)))
  Env$l.fr7$img2<-tklabel(Env$l.frames$Fr7,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Pvalue2.gif",fsep=.Platform$file.sep)))
  Env$l.fr7$txt1.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[202,1],font=Env$police)
  Env$l.fr7$txt1.wdg<-tkentry(Env$l.frames$Fr7,width=10,textvariable=Env$l.var$add.param1,font=Env$police)
  Env$l.fr7$txt2.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[202,1],font=Env$police)
  Env$l.fr7$txt2.wdg<-tkentry(Env$l.frames$Fr7,width=10,textvariable=Env$l.var$add.param2,font=Env$police)
  Env$l.fr7$taille1.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[46,1],font=Env$police)
  Env$l.fr7$taille1.wdg<-tkscale(Env$l.frames$Fr7,from=0.5,to=3,showvalue=TRUE,font=Env$police,variable=Env$l.var$add.epaisseur1,resolution=0.25,orient="horizontal")
  Env$l.fr7$taille2.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[46,1],font=Env$police)
  Env$l.fr7$taille2.wdg<-tkscale(Env$l.frames$Fr7,from=0.5,to=3,showvalue=TRUE,font=Env$police,variable=Env$l.var$add.epaisseur2,resolution=0.25,orient="horizontal")
  Env$l.fr7$col1.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[45,1],font=Env$police)
  Env$l.fr7$col1.wdg<-tkbutton(Env$l.frames$Fr7,text="Aa",font=Env$police3,command=function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$add.col1),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$add.col1)<-temp
	tkconfigure(Env$l.fr7$col1.wdg,foreground=temp,activeforeground=temp)
    }
  })
  Env$l.fr7$col2.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[45,1],font=Env$police)
  Env$l.fr7$col2.wdg<-tkbutton(Env$l.frames$Fr7,text="Aa",font=Env$police3,command=function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$add.col2),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$add.col2)<-temp
	tkconfigure(Env$l.fr7$col2.wdg,foreground=temp,activeforeground=temp)
    }
  })
  Env$l.fr7$expl1.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[206,1],font=Env$police)
  Env$l.fr7$expl2.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[207,1],font=Env$police)
  if (!is.null(Env$l.var$add.hauteurs)) {
    ecart1<-0.15*max(Env$l.var$add.hauteurs)
    ecart2<-0.08*max(Env$l.var$add.hauteurs)
  }
  Env$l.fr7$but1<-tkbutton(Env$l.frames$Fr7,text=Env$voc[208,1],font=Env$police,width=16,command=function(...) {
    if (dev.cur()>1) {
	if (!is.null(Env$l.var$add.abscisses)) {
	  coords<-locator(n=2)
	  diff1<-numeric(length(Env$l.var$add.abscisses))
	  diff2<-numeric(length(Env$l.var$add.abscisses))
	  for (i in 1:length(Env$l.var$add.abscisses)) {
	    diff1[i]<-abs(coords$x[1]-Env$l.var$add.abscisses[i])
	    diff2[i]<-abs(coords$x[2]-Env$l.var$add.abscisses[i])
	  }
	  x1<-which(diff1==min(diff1))
	  x2<-which(diff2==min(diff2))
	  segments(min(Env$l.var$add.abscisses[x1],Env$l.var$add.abscisses[x2]),Env$l.var$add.matrice[x2,x1]+ecart1,
	    max(Env$l.var$add.abscisses[x1],Env$l.var$add.abscisses[x2]),Env$l.var$add.matrice[x2,x1]+ecart1)
	  segments(Env$l.var$add.abscisses[x1],Env$l.var$add.hauteurs[x1]+ecart2,Env$l.var$add.abscisses[x1],
	    Env$l.var$add.matrice[x2,x1]+ecart1)
	  segments(Env$l.var$add.abscisses[x2],Env$l.var$add.hauteurs[x2]+ecart2,Env$l.var$add.abscisses[x2],
	    Env$l.var$add.matrice[x2,x1]+ecart1)
	  text(Env$l.var$add.abscisses[x1]+(Env$l.var$add.abscisses[x2]-Env$l.var$add.abscisses[x1])/2,
	    Env$l.var$add.matrice[x2,x1]+ecart1+ecart2,tclvalue(Env$l.var$add.param1),
	    cex=as.numeric(tclvalue(Env$l.var$add.epaisseur1)),col=tclvalue(Env$l.var$add.col1))
	  if (Env$l.code$save==TRUE) {
	    sink(file=file.path(Env$l.code$folder,paste(paste("GrapheR",paste(strsplit(as.character(Sys.Date()),split="-")[[1]],collapse="."),
		sep="-"),".R",sep=""),fsep=.Platform$file.sep),append=TRUE)
	    cat("# Added: p-value\n\n")
	    cat(paste("segments(",round(min(Env$l.var$add.abscisses[x1],Env$l.var$add.abscisses[x2]),2),", ",round(Env$l.var$add.matrice[x2,x1]+ecart1,2),
		", ",round(max(Env$l.var$add.abscisses[x1],Env$l.var$add.abscisses[x2]),2),", ",round(Env$l.var$add.matrice[x2,x1]+ecart1,2),")\n",sep=""))
	    cat(paste("segments(",round(Env$l.var$add.abscisses[x1],2),", ",round(Env$l.var$add.hauteurs[x1]+ecart2,2),", ",round(Env$l.var$add.abscisses[x1],2),", ",round(Env$l.var$add.matrice[x2,x1]+ecart1,2),")\n",sep=""))
	    cat(paste("segments(",round(Env$l.var$add.abscisses[x2],2),", ",round(Env$l.var$add.hauteurs[x2]+ecart2,2),", ",round(Env$l.var$add.abscisses[x2],2),", ",round(Env$l.var$add.matrice[x2,x1]+ecart1,2),")\n",sep=""))
	    texte<-paste("text(",round(Env$l.var$add.abscisses[x1]+(Env$l.var$add.abscisses[x2]-Env$l.var$add.abscisses[x1])/2,2),", ",round(Env$l.var$add.matrice[x2,x1]+ecart1+ecart2,2),sep="")
	    texte<-paste(texte,", labels=\"",tclvalue(Env$l.var$add.param1),"\"",sep="")
	    if (tclvalue(Env$l.var$add.col1)!="black" & tclvalue(Env$l.var$add.col1)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$add.col1),"\"",sep="")}
	    texte<-paste(texte,", cex=",tclvalue(Env$l.var$add.epaisseur1),")\n\n",sep="")
	    cat(texte)
	    sink(NULL)
	  }
	  Env$l.var$add.hauteurs[x1:x2]<-Env$l.var$add.matrice[x2,x1]+ecart1+ecart2
	  for (i in 1:length(Env$l.var$add.abscisses)) {
	    for (j in 1:length(Env$l.var$add.abscisses)) {
		Env$l.var$add.matrice[i,j]<-max(Env$l.var$add.hauteurs[i:j])
	    }
	  }
	} else {
	  msg(text=Env$voc[209,1],type="error")
	}
    } else {
	msg(text=Env$voc[163,1],type="error")
    }
  })
  Env$l.fr7$but2<-tkbutton(Env$l.frames$Fr7,text=Env$voc[208,1],font=Env$police,width=16,command=function(...) {
    if (dev.cur()>1) {
	if (!is.null(Env$l.var$add.abscisses)) {
	  coords<-locator(n=2)
	  diff1<-numeric(length(Env$l.var$add.abscisses))
	  diff2<-numeric(length(Env$l.var$add.abscisses))
	  for (i in 1:length(Env$l.var$add.abscisses)) {
	    diff1[i]<-abs(coords$x[1]-Env$l.var$add.abscisses[i])
	    diff2[i]<-abs(coords$x[2]-Env$l.var$add.abscisses[i])
	  }
	  x1<-which(diff1==min(diff1))
	  x2<-which(diff2==min(diff2))
	  segments(min(Env$l.var$add.abscisses[x1],Env$l.var$add.abscisses[x2])-0.5,Env$l.var$add.matrice[x2,x1]+ecart1,
	    max(Env$l.var$add.abscisses[x1],Env$l.var$add.abscisses[x2])+0.5,Env$l.var$add.matrice[x2,x1]+ecart1)
	  text(Env$l.var$add.abscisses[x1]+(Env$l.var$add.abscisses[x2]-Env$l.var$add.abscisses[x1])/2,
	    Env$l.var$add.matrice[x2,x1]+ecart1+ecart2,tclvalue(Env$l.var$add.param2),
	    cex=as.numeric(tclvalue(Env$l.var$add.epaisseur2)),col=tclvalue(Env$l.var$add.col2))
	  if (Env$l.code$save==TRUE) {
	    sink(file=file.path(Env$l.code$folder,paste(paste("GrapheR",paste(strsplit(as.character(Sys.Date()),split="-")[[1]],collapse="."),
		sep="-"),".R",sep=""),fsep=.Platform$file.sep),append=TRUE)
	    cat("# Added: p-value\n\n")
	    cat(paste("segments(",round(min(Env$l.var$add.abscisses[x1],Env$l.var$add.abscisses[x2])-0.5,2),", ",round(Env$l.var$add.matrice[x2,x1]+ecart1,2),
		", ",round(max(Env$l.var$add.abscisses[x1],Env$l.var$add.abscisses[x2])+0.5,2),", ",round(Env$l.var$add.matrice[x2,x1]+ecart1,2),")\n",sep=""))
	    texte<-paste("text(",round(Env$l.var$add.abscisses[x1]+(Env$l.var$add.abscisses[x2]-Env$l.var$add.abscisses[x1])/2,2),", ",round(Env$l.var$add.matrice[x2,x1]+ecart1+ecart2,2),sep="")
	    texte<-paste(texte,", labels=\"",tclvalue(Env$l.var$add.param2),"\"",sep="")
	    if (tclvalue(Env$l.var$add.col2)!="black" & tclvalue(Env$l.var$add.col2)!="#000000") {texte<-paste(texte,", col=\"",tclvalue(Env$l.var$add.col2),"\"",sep="")}
	    texte<-paste(texte,", cex=",tclvalue(Env$l.var$add.epaisseur2),")\n\n",sep="")
	    cat(texte)
	    sink(NULL)
	  }
	  Env$l.var$add.hauteurs[x1:x2]<-Env$l.var$add.matrice[x2,x1]+ecart1+ecart2
	  for (i in 1:length(Env$l.var$add.abscisses)) {
	    for (j in 1:length(Env$l.var$add.abscisses)) {
		Env$l.var$add.matrice[i,j]<-max(Env$l.var$add.hauteurs[i:j])
	    }
	  }
	} else {
	  msg(text=Env$voc[209,1],type="error")
	}
    } else {
	msg(text=Env$voc[163,1],type="error")
    }
  })
  Env$l.fr7$fermer<-tkbutton(Env$l.frames$Fr7,text=Env$voc[152,1],font=Env$police,width=16,command=fr7.close)
  Env$l.fr7$espace.ver1<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver2<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver3<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver4<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver5<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.hor1<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  Env$l.fr7$espace.hor2<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  Env$l.fr7$espace.hor3<-tklabel(Env$l.frames$Fr7,text="               ",font=Env$police3)
  tkgrid(Env$l.fr7$espace.hor1,row=0,column=0)
  tkgrid(Env$l.fr7$titre.lab,row=0,column=1,columnspan=5)
  tkgrid(Env$l.fr7$espace.hor2,row=0,column=6)
  tkgrid(Env$l.fr7$espace.ver1)
  tkgrid(Env$l.fr7$img1,row=2,column=1,columnspan=2)
  tkgrid(Env$l.fr7$espace.hor3,row=2,column=3)
  tkgrid(Env$l.fr7$img2,row=2,column=4,columnspan=2)
  tkgrid(Env$l.fr7$espace.ver2)
  tkgrid(Env$l.fr7$txt1.lab,row=4,column=1,sticky="e")
  tkgrid(Env$l.fr7$txt1.wdg,row=4,column=2,sticky="w")
  tkgrid(Env$l.fr7$txt2.lab,row=4,column=4,sticky="e")
  tkgrid(Env$l.fr7$txt2.wdg,row=4,column=5,sticky="w")
  tkgrid(Env$l.fr7$taille1.lab,row=5,column=1,sticky="e")
  tkgrid(Env$l.fr7$taille1.wdg,row=5,column=2,sticky="w")
  tkgrid(Env$l.fr7$taille2.lab,row=5,column=4,sticky="e")
  tkgrid(Env$l.fr7$taille2.wdg,row=5,column=5,sticky="w")
  tkgrid(Env$l.fr7$col1.lab,row=6,column=1,sticky="e")
  tkgrid(Env$l.fr7$col1.wdg,row=6,column=2,sticky="w")
  tkgrid(Env$l.fr7$col2.lab,row=6,column=4,sticky="e")
  tkgrid(Env$l.fr7$col2.wdg,row=6,column=5,sticky="w")
  tkgrid(Env$l.fr7$espace.ver3)
  tkgrid(Env$l.fr7$expl1,row=8,column=1,columnspan=2)
  tkgrid(Env$l.fr7$expl2,row=8,column=4,columnspan=2)
  tkgrid(Env$l.fr7$but1,row=9,column=1,columnspan=2)
  tkgrid(Env$l.fr7$but2,row=9,column=4,columnspan=2)
  tkgrid(Env$l.fr7$espace.ver4)
  tkgrid(Env$l.fr7$fermer,column=1,columnspan=5)
  tkgrid(Env$l.fr7$espace.ver5)
}

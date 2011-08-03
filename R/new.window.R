new.window <-
function() {
  fr7.close()
  for (i in 1:length(Env$l.fr7)) {tkdestroy(Env$l.fr7[[i]])}
  Env$l.fr7<-list()
  tkconfigure(Env$l.frames$Fr7,borderwidth=3)
  Env$l.fr7$titre<-tklabel(Env$l.frames$Fr7,text=Env$voc[149,1],font=Env$police3)
  Env$l.fr7$col.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[150,1],font=Env$police)
  Env$l.fr7$col.wdg<-tkcanvas(Env$l.frames$Fr7,width="40",height="25",bg=tclvalue(Env$l.var$nw.col))
  tkbind(Env$l.fr7$col.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$nw.col),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$nw.col)<-temp
	tkconfigure(Env$l.fr7$col.wdg,bg=tclvalue(Env$l.var$nw.col))
    }
  })
  Env$l.fr7$lignes.wdg<-tkscale(Env$l.frames$Fr7,from=1,to=4,showvalue=TRUE,font=Env$police5,variable=Env$l.var$nw.lignes,length=200,resolution=1,orient="vertical",command=function(...) {
    tkconfigure(Env$l.fr7$fenetre,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Fenetre",tclvalue(Env$l.var$nw.lignes),"-",tclvalue(Env$l.var$nw.colonnes),".gif",sep=""),fsep=.Platform$file.sep)))
  })
  Env$l.fr7$colonnes.wdg<-tkscale(Env$l.frames$Fr7,from=1,to=4,showvalue=TRUE,font=Env$police5,variable=Env$l.var$nw.colonnes,length=200,resolution=1,orient="horizontal",command=function(...) {
    tkconfigure(Env$l.fr7$fenetre,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Fenetre",tclvalue(Env$l.var$nw.lignes),"-",tclvalue(Env$l.var$nw.colonnes),".gif",sep=""),fsep=.Platform$file.sep)))
  })
  Env$l.fr7$fenetre<-tklabel(Env$l.frames$Fr7,height=200,width=200,font=Env$police,borderwidth=0,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fenetre1-1.gif",fsep=.Platform$file.sep)))
  Env$l.fr7$ok<-tkbutton(Env$l.frames$Fr7,width=16,text=Env$voc[151,1],font=Env$police,command=function() {
    dimensions<-if (tclvalue(Env$l.var$nw.lignes)=="1" & tclvalue(Env$l.var$nw.colonnes)=="1") {c(7,7)} else
      if (tclvalue(Env$l.var$nw.lignes)=="2" & tclvalue(Env$l.var$nw.colonnes)=="1") {c(6,12)} else
      if (tclvalue(Env$l.var$nw.lignes)=="3" & tclvalue(Env$l.var$nw.colonnes)=="1") {c(5,15)} else
      if (tclvalue(Env$l.var$nw.lignes)=="4" & tclvalue(Env$l.var$nw.colonnes)=="1") {c(4,16)} else
      if (tclvalue(Env$l.var$nw.lignes)=="1" & tclvalue(Env$l.var$nw.colonnes)=="2") {c(12,6)} else
      if (tclvalue(Env$l.var$nw.lignes)=="2" & tclvalue(Env$l.var$nw.colonnes)=="2") {c(10,10)} else
      if (tclvalue(Env$l.var$nw.lignes)=="3" & tclvalue(Env$l.var$nw.colonnes)=="2") {c(10,15)} else
      if (tclvalue(Env$l.var$nw.lignes)=="4" & tclvalue(Env$l.var$nw.colonnes)=="2") {c(8,16)} else
      if (tclvalue(Env$l.var$nw.lignes)=="1" & tclvalue(Env$l.var$nw.colonnes)=="3") {c(15,5)} else
      if (tclvalue(Env$l.var$nw.lignes)=="2" & tclvalue(Env$l.var$nw.colonnes)=="3") {c(15,10)} else
      if (tclvalue(Env$l.var$nw.lignes)=="3" & tclvalue(Env$l.var$nw.colonnes)=="3") {c(12,12)} else
      if (tclvalue(Env$l.var$nw.lignes)=="4" & tclvalue(Env$l.var$nw.colonnes)=="3") {c(12,16)} else
      if (tclvalue(Env$l.var$nw.lignes)=="1" & tclvalue(Env$l.var$nw.colonnes)=="4") {c(16,4)} else
      if (tclvalue(Env$l.var$nw.lignes)=="2" & tclvalue(Env$l.var$nw.colonnes)=="4") {c(16,8)} else
      if (tclvalue(Env$l.var$nw.lignes)=="3" & tclvalue(Env$l.var$nw.colonnes)=="4") {c(16,12)} else
      if (tclvalue(Env$l.var$nw.lignes)=="4" & tclvalue(Env$l.var$nw.colonnes)=="4") {c(16,16)}
    dev.new(width=dimensions[1],height=dimensions[2])
    par(bg=tclvalue(Env$l.var$nw.col),mfrow=c(as.numeric(tclvalue(Env$l.var$nw.lignes)),as.numeric(tclvalue(Env$l.var$nw.colonnes))),mar=c(5,6,4,2))
  })
  Env$l.fr7$fermer<-tkbutton(Env$l.frames$Fr7,width=16,text=Env$voc[152,1],font=Env$police,command=fr7.close)
  Env$l.fr7$espace.ver1<-tklabel(Env$l.frames$Fr7,text="",font=Env$police5)
  Env$l.fr7$espace.ver2<-tklabel(Env$l.frames$Fr7,text="",font=Env$police5)
  Env$l.fr7$espace.ver3<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver4<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver5<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.hor1<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  Env$l.fr7$espace.hor2<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  tkgrid(Env$l.fr7$espace.hor1,row=0,column=0)
  tkgrid(Env$l.fr7$titre,row=0,column=1,columnspan=2)
  tkgrid(Env$l.fr7$espace.hor2,row=0,column=3)
  tkgrid(Env$l.fr7$colonnes.wdg,column=2)
  tkgrid(Env$l.fr7$espace.ver1)
  tkgrid(Env$l.fr7$lignes.wdg,row=3,column=1)
  tkgrid(Env$l.fr7$fenetre,row=3,column=2)
  tkgrid(Env$l.fr7$espace.ver2)
  tkgrid(Env$l.fr7$col.lab,row=5,column=1,sticky="e")
  tkgrid(Env$l.fr7$col.wdg,row=5,column=2,sticky="w")
  tkgrid(Env$l.fr7$espace.ver3)
  tkgrid(Env$l.fr7$ok,column=1,columnspan=2)
  tkgrid(Env$l.fr7$espace.ver4)
  tkgrid(Env$l.fr7$fermer,column=1,columnspan=2)
  tkgrid(Env$l.fr7$espace.ver5)
}


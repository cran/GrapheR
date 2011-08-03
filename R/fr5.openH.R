fr5.openH <-
function() {
  Env$l.frames$Fr5.status<-1
  tkconfigure(Env$l.wdg$but.lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr5)) {tkdestroy(Env$l.fr5[[i]])}
  Env$l.fr5<-list()
  Env$l.fr5$tracer.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[58,1],font=Env$police,foreground=ifelse(tclvalue(Env$l.var$hist.type)==Env$voc[42,1],"black","grey"))
  Env$l.fr5$tracer.wdg<-tkcheckbutton(Env$l.frames$Fr5,variable=Env$l.var$hist.dens,state=ifelse(tclvalue(Env$l.var$hist.type)==Env$voc[42,1],"normal","disabled"))
  Env$l.fr5$col.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[45,1],font=Env$police,foreground=ifelse(tclvalue(Env$l.var$hist.type)==Env$voc[42,1],"black","grey"))
  Env$l.fr5$col.wdg<-tkcanvas(Env$l.frames$Fr5,width="25",height="20",bg=ifelse(tclvalue(Env$l.var$hist.type)==Env$voc[42,1],tclvalue(Env$l.var$couleur2A),"grey"))
  tkbind(Env$l.fr5$col.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$couleur2A),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$couleur2A)<-temp
	tkconfigure(Env$l.fr5$col.wdg,bg=temp)
    }
  })
  Env$l.fr5$trait.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[59,1],font=Env$police,foreground=ifelse(tclvalue(Env$l.var$hist.type)==Env$voc[42,1],"black","grey"))
  Env$l.fr5$trait.wdg<-ttkcombobox(Env$l.frames$Fr5,font=Env$police,values=Env$voc[60:62,1],textvariable=Env$l.var$trait1,state=ifelse(tclvalue(Env$l.var$hist.type)==Env$voc[42,1],"readonly","disabled"),width=15)
  Env$l.fr5$epaisseur.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[63,1],font=Env$police,foreground=ifelse(tclvalue(Env$l.var$hist.type)==Env$voc[42,1],"black","grey"))
  Env$l.fr5$epaisseur.wdg<-tkscale(Env$l.frames$Fr5,from=1,to=5,showvalue=TRUE,font=Env$police,variable=Env$l.var$epaisseur1,resolution=1,orient="horizontal",state=ifelse(tclvalue(Env$l.var$hist.type)==Env$voc[42,1],"normal","disabled"))
  Env$l.fr5$espace.hor1<-tklabel(Env$l.frames$Fr5,text="               ",font=Env$police)
  Env$l.fr5$espace.hor2<-tklabel(Env$l.frames$Fr5,text="               ",font=Env$police)
  Env$l.fr5$espace.hor3<-tklabel(Env$l.frames$Fr5,text="               ",font=Env$police)
  tkgrid(Env$l.fr5$tracer.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr5$tracer.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr5$espace.hor1,row=0,column=2)
  tkgrid(Env$l.fr5$col.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr5$col.wdg,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr5$espace.hor2,row=0,column=5)
  tkgrid(Env$l.fr5$trait.lab,row=0,column=6,sticky="e")
  tkgrid(Env$l.fr5$trait.wdg,row=0,column=7,sticky="w")
  tkgrid(Env$l.fr5$espace.hor3,row=0,column=8)
  tkgrid(Env$l.fr5$epaisseur.lab,row=0,column=9,sticky="e")
  tkgrid(Env$l.fr5$epaisseur.wdg,row=0,column=10,sticky="w")
}


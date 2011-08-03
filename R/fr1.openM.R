fr1.openM <-
function() {
  Env$l.frames$Fr1.status<-1
  tkconfigure(Env$l.wdg$but.lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr1)) {tkdestroy(Env$l.fr1[[i]])}
  Env$l.fr1<-list()
  Env$l.fr1$variable.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[36,1],font=Env$police)
  Env$l.fr1$variable.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$l.var$var.num,textvariable=Env$l.var$variable,font=Env$police,state="readonly")
  Env$l.fr1$facteur.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[65,1],font=Env$police)
  Env$l.fr1$facteur.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$l.var$var.fact,textvariable=Env$l.var$facteur1,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$facteur.wdg,"<<ComboboxSelected>>",function() {
    Env$l.var$noms1<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])
    if (exists("noms.list",where=Env$l.fr3)) {
	tkdelete(Env$l.fr3$noms.list,0,"end")
	for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$noms1[i])}
    }
  })
  Env$l.fr1$orientation.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[66,1],font=Env$police)
  Env$l.fr1$orientation.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$voc[67:68,1],textvariable=Env$l.var$box.orient,font=Env$police,state="readonly")
  Env$l.fr1$encadre.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[43,1],font=Env$police)
  Env$l.fr1$encadre.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$encadre)
  Env$l.fr1$espace.hor<-tklabel(Env$l.frames$Fr1,text="                                   ",font=Env$police)
  tkgrid(Env$l.fr1$variable.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr1$variable.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr1$facteur.lab,row=1,column=0,sticky="e")
  tkgrid(Env$l.fr1$facteur.wdg,row=1,column=1,sticky="w")
  tkgrid(Env$l.fr1$espace.hor,row=0,column=2)
  tkgrid(Env$l.fr1$orientation.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr1$orientation.wdg,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr1$encadre.lab,row=1,column=3,sticky="e")
  tkgrid(Env$l.fr1$encadre.wdg,row=1,column=4,sticky="w")
}


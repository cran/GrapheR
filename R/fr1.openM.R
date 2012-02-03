fr1.openM <-
function() {
  Env$l.frames$Fr1.status<-1
  tkconfigure(Env$l.wdg$but.lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr1)) {tkdestroy(Env$l.fr1[[i]])}
  Env$l.fr1<-list()
  Env$l.fr1$variable.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[36,1],font=Env$police)
  Env$l.fr1$variable.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$l.var$var.num,textvariable=Env$l.var$variable,font=Env$police,state="readonly")
  Env$l.fr1$facteur1.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[86,1],font=Env$police)
  Env$l.fr1$facteur1.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$l.var$var.fact,textvariable=Env$l.var$facteur1,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$facteur1.wdg,"<<ComboboxSelected>>",function() {
    if (nchar(tclvalue(Env$l.var$facteur2))>0 & tclvalue(Env$l.var$facteur2)!=Env$voc[82,1]) {
	Env$l.var$facteur.interaction<-factor(paste(Env$dataset[,tclvalue(Env$l.var$facteur1)],Env$dataset[,tclvalue(Env$l.var$facteur2)],sep=":"))
	Env$l.var$noms1<-levels(Env$l.var$facteur.interaction)
	Env$l.var$couleur1B<-rep("white",nlevels(Env$l.var$facteur.interaction))
	Env$l.var$col.borduresB<-rep("black",nlevels(Env$l.var$facteur.interaction))
	if (exists("noms.list",where=Env$l.fr3)) {
	  tkdelete(Env$l.fr3$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$noms1[i])}
	}
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms1[i])}
	  tkconfigure(Env$l.fr4$colboites.wdg,bg=Env$l.var$couleur1B[1])
	  tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[1])
	}
    } else {
	Env$l.var$facteur.interaction<-""
	Env$l.var$noms1<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])
	Env$l.var$couleur1B<-rep("white",nlevels(Env$dataset[,tclvalue(Env$l.var$facteur1)]))
	Env$l.var$col.borduresB<-rep("black",nlevels(Env$dataset[,tclvalue(Env$l.var$facteur1)]))
	if (exists("noms.list",where=Env$l.fr3)) {
	  tkdelete(Env$l.fr3$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$noms1[i])}
	}
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms1[i])}
	  tkconfigure(Env$l.fr4$colboites.wdg,bg=Env$l.var$couleur1B[1])
	  tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[1])
	}
    }
  })
  Env$l.fr1$facteur2.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[87,1],font=Env$police)
  Env$l.fr1$facteur2.wdg<-ttkcombobox(Env$l.frames$Fr1,values=c(Env$voc[82,1],Env$l.var$var.fact),textvariable=Env$l.var$facteur2,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$facteur2.wdg,"<<ComboboxSelected>>",function() {
    if (nchar(tclvalue(Env$l.var$facteur2))>0 & tclvalue(Env$l.var$facteur2)!=Env$voc[82,1]) {
	Env$l.var$facteur.interaction<-factor(paste(Env$dataset[,tclvalue(Env$l.var$facteur1)],Env$dataset[,tclvalue(Env$l.var$facteur2)],sep=":"))
	Env$l.var$noms1<-levels(Env$l.var$facteur.interaction)
	Env$l.var$couleur1B<-rep("white",nlevels(Env$l.var$facteur.interaction))
	Env$l.var$col.borduresB<-rep("black",nlevels(Env$l.var$facteur.interaction))
	if (exists("noms.list",where=Env$l.fr3)) {
	  tkdelete(Env$l.fr3$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$noms1[i])}
	}
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms1[i])}
	  tkconfigure(Env$l.fr4$colboites.wdg,bg=Env$l.var$couleur1B[1])
	  tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[1])
	}
    } else {
	Env$l.var$facteur.interaction<-""
	Env$l.var$noms1<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])
	Env$l.var$couleur1B<-rep("white",nlevels(Env$dataset[,tclvalue(Env$l.var$facteur1)]))
	Env$l.var$col.borduresB<-rep("black",nlevels(Env$dataset[,tclvalue(Env$l.var$facteur1)]))
	if (exists("noms.list",where=Env$l.fr3)) {
	  tkdelete(Env$l.fr3$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$noms1[i])}
	}
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms1[i])}
	  tkconfigure(Env$l.fr4$colboites.wdg,bg=Env$l.var$couleur1B[1])
	  tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[1])
	}
    }
  })
  Env$l.fr1$sysinfo.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[248,1],font=Env$police)
  Env$l.fr1$sysinfo.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$sysinfo)
  Env$l.fr1$orientation.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[66,1],font=Env$police)
  Env$l.fr1$orientation.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$voc[67:68,1],textvariable=Env$l.var$box.orient,font=Env$police,state="readonly")
  Env$l.fr1$encadre.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[43,1],font=Env$police)
  Env$l.fr1$encadre.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$encadre)
  Env$l.fr1$espace.hor<-tklabel(Env$l.frames$Fr1,text="                                   ",font=Env$police)
  tkgrid(Env$l.fr1$variable.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr1$variable.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr1$facteur1.lab,row=1,column=0,sticky="e")
  tkgrid(Env$l.fr1$facteur1.wdg,row=1,column=1,sticky="w")
  tkgrid(Env$l.fr1$facteur2.lab,row=2,column=0,sticky="e")
  tkgrid(Env$l.fr1$facteur2.wdg,row=2,column=1,sticky="w")
  tkgrid(Env$l.fr1$espace.hor,row=0,column=2)
  tkgrid(Env$l.fr1$orientation.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr1$orientation.wdg,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr1$encadre.lab,row=1,column=3,sticky="e")
  tkgrid(Env$l.fr1$encadre.wdg,row=1,column=4,sticky="w")
  tkgrid(Env$l.fr1$sysinfo.lab,row=2,column=3,sticky="e")
  tkgrid(Env$l.fr1$sysinfo.wdg,row=2,column=4,sticky="w")
}

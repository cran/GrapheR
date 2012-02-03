fr1.openB <-
function() {
  Env$l.frames$Fr1.status<-1
  tkconfigure(Env$l.wdg$but.lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr1)) {tkdestroy(Env$l.fr1[[i]])}
  Env$l.fr1<-list()
  Env$l.fr1$titre1<-tklabel(Env$l.frames$Fr1,text=Env$voc[84,1],font=Env$police3)
  Env$l.fr1$rb1<-tkradiobutton(Env$l.frames$Fr1,variable=Env$l.var$moyprop,value="moy",command=barres.moy)
  Env$l.fr1$moyvar.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[36,1],font=Env$police)
  Env$l.fr1$moyvar.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$l.var$var.num,textvariable=Env$l.var$variable,font=Env$police,state="readonly")
  Env$l.fr1$moyfac1.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[86,1],font=Env$police)
  Env$l.fr1$moyfac1.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$l.var$var.fact,textvariable=Env$l.var$facteur1,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$moyfac1.wdg,"<<ComboboxSelected>>",function() {
    if (nchar(tclvalue(Env$l.var$facteur1))>0) {
	Env$l.var$noms1<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])
	if (exists("noms.list",where=Env$l.fr3)) {
	  tkdelete(Env$l.fr3$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$noms1[i])}
	}
    }
  })
  Env$l.fr1$moyfac2.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[87,1],font=Env$police)
  Env$l.fr1$moyfac2.wdg<-ttkcombobox(Env$l.frames$Fr1,values=c(Env$voc[82,1],Env$l.var$var.fact),textvariable=Env$l.var$facteur2,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$moyfac2.wdg,"<<ComboboxSelected>>",function() {
    tclvalue(Env$l.var$stack)<-0
    if (nchar(tclvalue(Env$l.var$facteur2))>0 & tclvalue(Env$l.var$facteur2)!=Env$voc[82,1]) {
	Env$l.var$noms2<-levels(Env$dataset[,tclvalue(Env$l.var$facteur2)])
	tclvalue(Env$l.var$plusieurs)<-1
	Env$l.var$couleur1B<-grey.colors(nlevels(Env$dataset[,tclvalue(Env$l.var$facteur2)]))
	Env$l.var$col.borduresB<-rep("black",nlevels(Env$dataset[,tclvalue(Env$l.var$facteur2)]))
	Env$l.var$hachuresB<-rep(1,nlevels(Env$dataset[,tclvalue(Env$l.var$facteur2)]))
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkconfigure(Env$l.fr4$noms.list,state="normal")
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms2)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms2[i])}
	  tkconfigure(Env$l.fr4$colbarres.wdg,bg=Env$l.var$couleur1B[1])
	  tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[1])
	  for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
	  tkconfigure(Env$l.fr4$l.hachures[[Env$l.var$hachuresB[1]]],borderwidth=2)
	  tkconfigure(Env$l.fr4$stack.lab,foreground="black")
	  tkconfigure(Env$l.fr4$stack.wdg,state="normal")
	  tkdeselect(Env$l.fr4$stack.wdg)
	}
	active.legende()
	if (exists("noms.list",where=Env$l.fr6)) {
	  tkdelete(Env$l.fr6$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms2)) {tkinsert(Env$l.fr6$noms.list,"end",Env$l.var$noms2[i])}
	}
    } else {
	Env$l.var$noms2<-""
	tclvalue(Env$l.var$plusieurs)<-0
	tclvalue(Env$l.var$couleur1A)<-"grey"
	tclvalue(Env$l.var$col.borduresA)<-"black"
	tclvalue(Env$l.var$hachuresA)<-"1"
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkconfigure(Env$l.fr4$noms.list,state="normal")
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  tkconfigure(Env$l.fr4$noms.list,state="disabled")
	  tkconfigure(Env$l.fr4$colbarres.wdg,bg=tclvalue(Env$l.var$couleur1A))
	  tkconfigure(Env$l.fr4$colbordures.wdg,bg=tclvalue(Env$l.var$col.borduresA))
	  for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
	  tkconfigure(Env$l.fr4$l.hachures[[as.numeric(tclvalue(Env$l.var$hachuresA))]],borderwidth=2)
	  tkconfigure(Env$l.fr4$stack.lab,foreground="grey")
	  tkdeselect(Env$l.fr4$stack.wdg)
	  tkconfigure(Env$l.fr4$stack.wdg,state="disabled")
	}
	active.legende()
	if (exists("noms.list",where=Env$l.fr6)) {
	  tkdelete(Env$l.fr6$noms.list,0,"end")
	}
    }
    active.erreur()
  })
  Env$l.fr1$nobar.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[264,1],font=Env$police)
  Env$l.fr1$nobar.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$nobar)
  Env$l.fr1$encadre.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[43,1],font=Env$police)
  Env$l.fr1$encadre.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$encadre)
  Env$l.fr1$sysinfo.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[248,1],font=Env$police)
  Env$l.fr1$sysinfo.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$sysinfo)
  Env$l.fr1$titre2<-tklabel(Env$l.frames$Fr1,text=Env$voc[85,1],font=Env$police3)
  Env$l.fr1$rb2<-tkradiobutton(Env$l.frames$Fr1,variable=Env$l.var$moyprop,value="prop",command=barres.prop)
  Env$l.fr1$propvar.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[36,1],font=Env$police)
  Env$l.fr1$propvar.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$l.var$var.fact,textvariable=Env$l.var$proportions,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$propvar.wdg,"<<ComboboxSelected>>",function() {
    if (nchar(tclvalue(Env$l.var$proportions))>0) {
	tclvalue(Env$l.var$plusieurs)<-0
	tclvalue(Env$l.var$prop.niveaux)<-"0"
	Env$l.var$nomsprop<-""
	tkdelete(Env$l.fr1$propnivx.list,0,"end")
	for (i in 1:nlevels(Env$dataset[,tclvalue(Env$l.var$proportions)])) {tkinsert(Env$l.fr1$propnivx.list,"end",levels(Env$dataset[,tclvalue(Env$l.var$proportions)])[i])}
	tkselection.set(Env$l.fr1$propnivx.list,tclvalue(Env$l.var$prop.niveaux))
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  tkconfigure(Env$l.fr4$noms.list,state="disabled")
	}
	active.legende()
    }
  })
  Env$l.fr1$propnivx.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[88,1],font=Env$police)
  Env$l.fr1$propnivx.list<-tklistbox(Env$l.frames$Fr1,height=6,font=Env$police,selectmode="multiple",yscrollcommand=function(...) tkset(Env$l.fr1$propnivx.scroll,...))
  Env$l.fr1$propnivx.scroll<-tkscrollbar(Env$l.frames$Fr1,repeatinterval=5,command=function(...) tkyview(Env$l.fr1$propnivx.list,...))
  tkbind(Env$l.fr1$propnivx.list,"<Enter>",function() {if(tclvalue(Env$l.var$moyprop)=="prop") {msg(text=Env$voc[142,1],type="info")}})
  tkbind(Env$l.fr1$propnivx.list,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr1$propnivx.list,"<ButtonRelease-1>",function() {
    tclvalue(Env$l.var$stack)<-0
    if (nchar(tclvalue(tkcurselection(Env$l.fr1$propnivx.list)))>2) {
	tclvalue(Env$l.var$plusieurs)<-1
	tclvalue(Env$l.var$prop.niveaux)<-tclvalue(tkcurselection(Env$l.fr1$propnivx.list))
	num<-as.numeric(strsplit(tclvalue(tkcurselection(Env$l.fr1$propnivx.list)),split=" ")[[1]])+1
	Env$l.var$couleur1B<-grey.colors(length(num))
	Env$l.var$col.borduresB<-rep("black",length(num))
	Env$l.var$hachuresB<-rep(1,length(num))
	Env$l.var$nomsprop<-levels(Env$dataset[,tclvalue(Env$l.var$proportions)])[num]
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkconfigure(Env$l.fr4$noms.list,state="normal")
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$nomsprop)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$nomsprop[i])}
	  tkconfigure(Env$l.fr4$colbarres.wdg,bg=Env$l.var$couleur1B[1])
	  tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[1])
	  for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
	  tkconfigure(Env$l.fr4$l.hachures[[Env$l.var$hachuresB[1]]],borderwidth=2)
	  tkconfigure(Env$l.fr4$stack.lab,foreground="black")
	  tkconfigure(Env$l.fr4$stack.wdg,state="normal")
	  tkdeselect(Env$l.fr4$stack.wdg)
	}
	active.legende()
	if (exists("noms.list",where=Env$l.fr6)) {
	  tkconfigure(Env$l.fr6$noms.list,state="normal")
	  tkdelete(Env$l.fr6$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$nomsprop)) {tkinsert(Env$l.fr6$noms.list,"end",Env$l.var$nomsprop[i])}
	}
    } else {
	tclvalue(Env$l.var$plusieurs)<-0
	tclvalue(Env$l.var$prop.niveaux)<-tclvalue(tkcurselection(Env$l.fr1$propnivx.list))
	tclvalue(Env$l.var$couleur1A)<-"grey"
	tclvalue(Env$l.var$col.borduresA)<-"black"
	tclvalue(Env$l.var$hachuresA)<-"1"
	Env$l.var$nomsprop<-""
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkconfigure(Env$l.fr4$noms.list,state="normal")
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  tkconfigure(Env$l.fr4$noms.list,state="disabled")
	  tkconfigure(Env$l.fr4$colbarres.wdg,bg=tclvalue(Env$l.var$couleur1A))
	  tkconfigure(Env$l.fr4$colbordures.wdg,bg=tclvalue(Env$l.var$col.borduresA))
	  for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
	  tkconfigure(Env$l.fr4$l.hachures[[as.numeric(tclvalue(Env$l.var$hachuresA))]],borderwidth=2)
	  tkconfigure(Env$l.fr4$stack.lab,foreground="grey")
	  tkdeselect(Env$l.fr4$stack.wdg)
	  tkconfigure(Env$l.fr4$stack.wdg,state="disabled")
	}
	active.legende()
	if (exists("noms.list",where=Env$l.fr6)) {
	  tkconfigure(Env$l.fr6$noms.list,state="normal")
	  tkdelete(Env$l.fr6$noms.list,0,"end")
	  tkconfigure(Env$l.fr6$noms.list,state="disabled")
	}
    }
    active.erreur()
  })
  Env$l.fr1$propfac.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[65,1],font=Env$police)
  Env$l.fr1$propfac.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$l.var$var.fact,textvariable=Env$l.var$facteurprop,font=Env$police,state="readonly")
  if (tclvalue(Env$l.var$moyprop)=="moy") {
    barres.moy()
  } else {
    barres.prop()
  }
  tkbind(Env$l.fr1$propfac.wdg,"<<ComboboxSelected>>",function() {
    if (nchar(tclvalue(Env$l.var$facteurprop))>0) {
	Env$l.var$nomsprop.fac<-levels(Env$dataset[,tclvalue(Env$l.var$facteurprop)])
	if (exists("noms.list",where=Env$l.fr3)) {
	  tkdelete(Env$l.fr3$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$nomsprop.fac)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$nomsprop.fac[i])}
	}
    } else {
	if (exists("noms.list",where=Env$l.fr3)) {
	  tkdelete(Env$l.fr3$noms.list,0,"end")
	}
    }
    if (exists("noms.wdg",where=Env$l.fr3)) {tkdelete(Env$l.fr3$noms.wdg,0,"end")}
  })
  Env$l.fr1$espace.ver<-tklabel(Env$l.frames$Fr1,text="",font=Env$police2)
  Env$l.fr1$espace.hor<-tklabel(Env$l.frames$Fr1,text="                    ",font=Env$police)
  tkgrid(Env$l.fr1$titre1,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr1$rb1,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr1$espace.ver,row=1,column=0)
  tkgrid(Env$l.fr1$moyvar.lab,row=2,column=0,sticky="e")
  tkgrid(Env$l.fr1$moyvar.wdg,row=2,column=1,sticky="w")
  tkgrid(Env$l.fr1$moyfac1.lab,row=3,column=0,sticky="e")
  tkgrid(Env$l.fr1$moyfac1.wdg,row=3,column=1,sticky="w")
  tkgrid(Env$l.fr1$moyfac2.lab,row=4,column=0,sticky="e")
  tkgrid(Env$l.fr1$moyfac2.wdg,row=4,column=1,sticky="w")
  tkgrid(Env$l.fr1$nobar.lab,row=5,column=0,sticky="e")
  tkgrid(Env$l.fr1$nobar.wdg,row=5,column=1,sticky="w")
  tkgrid(Env$l.fr1$encadre.lab,row=6,column=0,sticky="e")
  tkgrid(Env$l.fr1$encadre.wdg,row=6,column=1,sticky="w")
  tkgrid(Env$l.fr1$sysinfo.lab,row=7,column=0,sticky="e")
  tkgrid(Env$l.fr1$sysinfo.wdg,row=7,column=1,sticky="w")
  tkgrid(Env$l.fr1$espace.hor,row=0,column=2)
  tkgrid(Env$l.fr1$rb2,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr1$titre2,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr1$propvar.lab,row=2,column=3,sticky="e")
  tkgrid(Env$l.fr1$propvar.wdg,row=2,column=4,sticky="w")
  tkgrid(Env$l.fr1$propnivx.lab,row=3,column=3,sticky="e")
  tkgrid(Env$l.fr1$propnivx.list,Env$l.fr1$propnivx.scroll,row=3,column=4,rowspan=4,sticky="w");tkgrid.configure(Env$l.fr1$propnivx.scroll,sticky="ens")
  tkgrid(Env$l.fr1$propfac.lab,row=7,column=3,sticky="e")
  tkgrid(Env$l.fr1$propfac.wdg,row=7,column=4,sticky="w")
}

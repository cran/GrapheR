fr1.openCo <-
function() {
  Env$l.frames$Fr1.status<-1
  tkconfigure(Env$l.wdg$but.lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr1)) {tkdestroy(Env$l.fr1[[i]])}
  Env$l.fr1<-list()
  Env$l.fr1$titre1<-tklabel(Env$l.frames$Fr1,text=Env$voc[84,1],font=Env$police3)
  Env$l.fr1$rb1<-tkradiobutton(Env$l.frames$Fr1,variable=Env$l.var$moyprop,value="moy",command=courbe.moy)
  Env$l.fr1$moyvarX.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[127,1],font=Env$police)
  Env$l.fr1$moyvarX.wdg<-ttkcombobox(Env$l.frames$Fr1,width=15,values=Env$l.var$var.num,textvariable=Env$l.var$varX,font=Env$police,state="readonly")
  Env$l.fr1$moyvarY.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[128,1],font=Env$police)
  Env$l.fr1$moyvarY.wdg<-ttkcombobox(Env$l.frames$Fr1,width=15,values=Env$l.var$var.num,textvariable=Env$l.var$varY,font=Env$police,state="readonly")
  Env$l.fr1$sysinfo.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[248,1],font=Env$police)
  Env$l.fr1$sysinfo.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$sysinfo)
  Env$l.fr1$fact.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[37,1],font=Env$police)
  Env$l.fr1$fact.wdg<-ttkcombobox(Env$l.frames$Fr1,width=15,values=c(Env$voc[82,1],Env$l.var$var.fact),textvariable=Env$l.var$facteur1,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$fact.wdg,"<<ComboboxSelected>>",function() {
    if (nchar(tclvalue(Env$l.var$facteur1))>0) {
	tclvalue(Env$l.var$plusieurs)<-0
	tclvalue(Env$l.var$symboleA)<-"1"
	tclvalue(Env$l.var$couleur2A)<-"black"
	tclvalue(Env$l.var$taille.ptsA)<-"1"
	tclvalue(Env$l.var$type.courbeA)<-""
	tclvalue(Env$l.var$trait1)<-""
	tclvalue(Env$l.var$epaisseur1)<-"1"
	tkdelete(Env$l.fr1$fact.list,0,"end")
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  tkconfigure(Env$l.fr4$noms.list,state="disabled")
	  for (i in 1:8) {tkconfigure(Env$l.fr4$l.symboles[[i]],borderwidth=0)}
	  tkconfigure(Env$l.fr4$l.symboles[[as.numeric(tclvalue(Env$l.var$symboleA))]],borderwidth=2)
	  tkconfigure(Env$l.fr4$col.wdg,bg=tclvalue(Env$l.var$couleur2A))
	}
	active.legende()
	if (tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
	  for (i in 1:nlevels(Env$dataset[,tclvalue(Env$l.var$facteur1)])) {tkinsert(Env$l.fr1$fact.list,"end",levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[i])}
	  tclvalue(Env$l.var$niveau)<-"0"
	  tkselection.set(Env$l.fr1$fact.list,tclvalue(Env$l.var$niveau))
	}
    }
  })
  Env$l.fr1$fact.lab2<-tklabel(Env$l.frames$Fr1,text=Env$voc[130,1],font=Env$police)
  Env$l.fr1$fact.list<-tklistbox(Env$l.frames$Fr1,height=4,width=15,font=Env$police,selectmode="multiple",yscrollcommand=function(...) tkset(Env$l.fr1$fact.scroll,...))
  Env$l.fr1$fact.scroll<-tkscrollbar(Env$l.frames$Fr1,repeatinterval=4,command=function(...) tkyview(Env$l.fr1$fact.list,...))
  tkbind(Env$l.fr1$fact.list,"<Enter>",function() {msg(text=Env$voc[139,1],type="info")})
  tkbind(Env$l.fr1$fact.list,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr1$fact.list,"<ButtonRelease-1>",function() {
    tclvalue(Env$l.var$niveau)<-tclvalue(tkcurselection(Env$l.fr1$fact.list))
    if (length(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])>1) {
	tclvalue(Env$l.var$plusieurs)<-1
	Env$l.var$symboleB<-rep(1,length(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]]))
	Env$l.var$couleur2B<-grey.colors(length(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]]))
	Env$l.var$taille.ptsB<-rep(1,length(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]]))
	tclvalue(Env$l.var$taille.ptsA)<-as.character(Env$l.var$taille.ptsB[1])
	Env$l.var$type.courbeB<-rep("",length(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]]))
	tclvalue(Env$l.var$type.courbeA)<-Env$l.var$type.courbeB[1]
	Env$l.var$trait2<-rep("",length(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]]))
	tclvalue(Env$l.var$trait1)<-Env$l.var$trait2[1]
	Env$l.var$epaisseur2<-rep(1,length(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]]))
	tclvalue(Env$l.var$epaisseur1)<-as.character(Env$l.var$epaisseur2[1])
	Env$l.var$noms1<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[as.numeric(strsplit(tclvalue(Env$l.var$niveau),split=" ")[[1]])+1]
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkconfigure(Env$l.fr4$noms.list,state="normal")
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms1[i])}
	  for (i in 1:8) {tkconfigure(Env$l.fr4$l.symboles[[i]],borderwidth=0)}
	  tkconfigure(Env$l.fr4$l.symboles[[Env$l.var$symboleB[1]]],borderwidth=2)
	  tkconfigure(Env$l.fr4$col.wdg,bg=Env$l.var$couleur2B[1])
	}
	active.legende()
	if (exists("noms.list",where=Env$l.fr6)) {
	  tkconfigure(Env$l.fr6$noms.list,state="normal")
	  tkdelete(Env$l.fr6$noms.list,0,"end")
	  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr6$noms.list,"end",Env$l.var$noms1[i])}
	  tkdelete(Env$l.fr6$noms.wdg,0,"end")
	}
    } else {
	tclvalue(Env$l.var$plusieurs)<-0
	tclvalue(Env$l.var$symboleA)<-"1"
	tclvalue(Env$l.var$couleur2A)<-"black"
	tclvalue(Env$l.var$taille.ptsA)<-"1"
	tclvalue(Env$l.var$type.courbeA)<-""
	tclvalue(Env$l.var$trait1)<-""
	tclvalue(Env$l.var$epaisseur1)<-"1"
	if (exists("noms.list",where=Env$l.fr4)) {
	  tkdelete(Env$l.fr4$noms.list,0,"end")
	  tkconfigure(Env$l.fr4$noms.list,state="disabled")
	  for (i in 1:8) {tkconfigure(Env$l.fr4$l.symboles[[i]],borderwidth=0)}
	  tkconfigure(Env$l.fr4$l.symboles[[as.numeric(tclvalue(Env$l.var$symboleA))]],borderwidth=2)
	  tkconfigure(Env$l.fr4$col.wdg,bg=tclvalue(Env$l.var$couleur2A))
	}
	if (exists("noms.list",where=Env$l.fr6)) {
	  tkdelete(Env$l.fr6$noms.list,0,"end")
	  tkdelete(Env$l.fr6$noms.wdg,0,"end")
	}
	active.legende()
    }
  })
  Env$l.fr1$encadre.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[43,1],font=Env$police)
  Env$l.fr1$encadre.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$encadre)
  Env$l.fr1$titre2<-tklabel(Env$l.frames$Fr1,text=Env$voc[85,1],font=Env$police3)
  Env$l.fr1$rb2<-tkradiobutton(Env$l.frames$Fr1,variable=Env$l.var$moyprop,value="prop",command=courbe.prop)
  Env$l.fr1$propvarX.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[127,1],font=Env$police)
  Env$l.fr1$propvarX.wdg<-ttkcombobox(Env$l.frames$Fr1,width=15,values=Env$l.var$var.num,textvariable=Env$l.var$varX.prop,font=Env$police,state="readonly")
  Env$l.fr1$propvarY.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[128,1],font=Env$police)
  Env$l.fr1$propvarY.wdg<-ttkcombobox(Env$l.frames$Fr1,width=15,values=Env$l.var$var.fact,textvariable=Env$l.var$proportions,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$propvarY.wdg,"<<ComboboxSelected>>",function() {
    tclvalue(Env$l.var$prop.niveaux)<-""
    tkconfigure(Env$l.fr1$propvarY.niv.wdg,values=levels(Env$dataset[,tclvalue(Env$l.var$proportions)]))
  })
  Env$l.fr1$propvarY.niv.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[129,1],font=Env$police)
  Env$l.fr1$propvarY.niv.wdg<-ttkcombobox(Env$l.frames$Fr1,width=15,values="",textvariable=Env$l.var$prop.niveaux,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$propvarY.niv.wdg,"<Enter>",function() {if(tclvalue(Env$l.var$moyprop)=="prop") {msg(text=Env$voc[140,1],type="warning")}})
  tkbind(Env$l.fr1$propvarY.niv.wdg,"<Leave>",function() {msg(text="",type="info")})
  if (tclvalue(Env$l.var$moyprop)=="moy") {courbe.moy()} else {courbe.prop()}
  Env$l.fr1$espace.ver<-tklabel(Env$l.frames$Fr1,text="",font=Env$police2)
  Env$l.fr1$espace.hor1<-tklabel(Env$l.frames$Fr1,text="                    ",font=Env$police)
  Env$l.fr1$espace.hor2<-tklabel(Env$l.frames$Fr1,text="                    ",font=Env$police)
  tkgrid(Env$l.fr1$titre1,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr1$rb1,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr1$espace.ver,row=1,column=0)
  tkgrid(Env$l.fr1$moyvarX.lab,row=2,column=0,sticky="e")
  tkgrid(Env$l.fr1$moyvarX.wdg,row=2,column=1,sticky="w")
  tkgrid(Env$l.fr1$moyvarY.lab,row=3,column=0,sticky="e")
  tkgrid(Env$l.fr1$moyvarY.wdg,row=3,column=1,sticky="w")
  tkgrid(Env$l.fr1$espace.hor1,row=0,column=2)
  tkgrid(Env$l.fr1$fact.lab,row=2,column=3,sticky="e")
  tkgrid(Env$l.fr1$fact.wdg,row=2,column=4,sticky="w")
  tkgrid(Env$l.fr1$fact.lab2,row=3,column=3,sticky="e")
  tkgrid(Env$l.fr1$fact.list,Env$l.fr1$fact.scroll,row=3,column=4,rowspan=4,sticky="w");tkgrid.configure(Env$l.fr1$fact.scroll,sticky="ens")
  tkgrid(Env$l.fr1$encadre.lab,row=7,column=3,sticky="e")
  tkgrid(Env$l.fr1$encadre.wdg,row=7,column=4,sticky="w")
  tkgrid(Env$l.fr1$sysinfo.lab,row=8,column=3,sticky="e")
  tkgrid(Env$l.fr1$sysinfo.wdg,row=8,column=4,sticky="w")
  tkgrid(Env$l.fr1$espace.hor2,row=0,column=5)
  tkgrid(Env$l.fr1$rb2,row=0,column=6,sticky="e")
  tkgrid(Env$l.fr1$titre2,row=0,column=7,sticky="w")
  tkgrid(Env$l.fr1$propvarX.lab,row=2,column=6,sticky="e")
  tkgrid(Env$l.fr1$propvarX.wdg,row=2,column=7,sticky="w")
  tkgrid(Env$l.fr1$propvarY.lab,row=3,column=6,sticky="e")
  tkgrid(Env$l.fr1$propvarY.wdg,row=3,column=7,sticky="w")
  tkgrid(Env$l.fr1$propvarY.niv.lab,row=4,column=6,sticky="e")
  tkgrid(Env$l.fr1$propvarY.niv.wdg,row=4,column=7,sticky="w")
}

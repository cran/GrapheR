fr5.openN <-
function() {
  Env$l.frames$Fr5.status<-1
  tkconfigure(Env$l.wdg$but.lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr5)) {tkdestroy(Env$l.fr5[[i]])}
  Env$l.fr5<-list()
  Env$l.fr5$noms.list<-tklistbox(Env$l.frames$Fr5,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr5$noms.scroll,...))
  Env$l.fr5$noms.scroll<-tkscrollbar(Env$l.frames$Fr5,repeatinterval=5,command=function(...) tkyview(Env$l.fr5$noms.list,...))
  tkbind(Env$l.fr5$noms.list,"<Enter>",function() {if (tclvalue(Env$l.var$plusieurs)==1) {msg(text=Env$voc[141,1],type="info")}})
  tkbind(Env$l.fr5$noms.list,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr5$noms.list,"<ButtonRelease-1>",function() {
    if (tclvalue(Env$l.var$plusieurs)==1) {
	Env$l.var$select<-as.numeric(tclvalue(tkcurselection(Env$l.fr5$noms.list)))+1
	tclvalue(Env$l.var$droiteA)<-as.character(Env$l.var$droiteB[Env$l.var$select])
	tclvalue(Env$l.var$intervalA)<-as.character(Env$l.var$intervalB[Env$l.var$select])
	tclvalue(Env$l.var$trait1)<-as.character(Env$l.var$trait2[Env$l.var$select])
	tclvalue(Env$l.var$epaisseur1)<-as.character(Env$l.var$epaisseur2[Env$l.var$select])
    }
  })
  Env$l.fr5$droite.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[144,1],font=Env$police)
  Env$l.fr5$droite.wdg<-ttkcombobox(Env$l.frames$Fr5,width=43,values=Env$voc[c(95,145:148),1],textvariable=Env$l.var$droiteA,state="readonly",font=Env$police)
  tkbind(Env$l.fr5$droite.wdg,"<<ComboboxSelected>>",function() {
    if (tclvalue(Env$l.var$plusieurs)==1) {
	Env$l.var$droiteB[Env$l.var$select]<-tclvalue(Env$l.var$droiteA)
	tkselection.set(Env$l.fr5$noms.list,as.character(Env$l.var$select-1))
    }
  })
  Env$l.fr5$interval.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[259,1],font=Env$police)
  Env$l.fr5$interval.wdg<-ttkcombobox(Env$l.frames$Fr5,width=43,values=Env$voc[260:263,1],textvariable=Env$l.var$intervalA,state="readonly",font=Env$police)
  tkbind(Env$l.fr5$interval.wdg,"<<ComboboxSelected>>",function() {
    if (tclvalue(Env$l.var$plusieurs)==1) {
	Env$l.var$intervalB[Env$l.var$select]<-tclvalue(Env$l.var$intervalA)
	tkselection.set(Env$l.fr5$noms.list,as.character(Env$l.var$select-1))
    }
  })
  Env$l.fr5$type.trait.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[59,1],font=Env$police)
  Env$l.fr5$type.trait.wdg<-ttkcombobox(Env$l.frames$Fr5,values=Env$voc[60:62,1],textvariable=Env$l.var$trait1,state="readonly",font=Env$police)
  tkbind(Env$l.fr5$type.trait.wdg,"<<ComboboxSelected>>",function() {
    if (tclvalue(Env$l.var$plusieurs)==1) {
	Env$l.var$trait2[Env$l.var$select]<-tclvalue(Env$l.var$trait1)
	tkselection.set(Env$l.fr5$noms.list,as.character(Env$l.var$select-1))
    }
  })
  Env$l.fr5$epaisseur.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[63,1],font=Env$police)
  Env$l.fr5$epaisseur.wdg<-tkscale(Env$l.frames$Fr5,showvalue=TRUE,from=1,to=4,resolution=1,font=Env$police,variable=Env$l.var$epaisseur1,orient="horizontal",command=function(...) {
    if (tclvalue(Env$l.var$plusieurs)==1) {
	Env$l.var$epaisseur2[Env$l.var$select]<-as.numeric(tclvalue(Env$l.var$epaisseur1))
	tkselection.set(Env$l.fr5$noms.list,as.character(Env$l.var$select-1))
    }
  })
  if (tclvalue(Env$l.var$plusieurs)==1) {
    Env$l.var$select<-1
    for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr5$noms.list,"end",Env$l.var$noms1[i])}
    tkselection.set(Env$l.fr5$noms.list,"0")
    tclvalue(Env$l.var$droiteA)<-as.character(Env$l.var$droiteB[1])
    tclvalue(Env$l.var$trait1)<-as.character(Env$l.var$trait2[1])
    tclvalue(Env$l.var$epaisseur1)<-as.character(Env$l.var$epaisseur2[1])
  } else {
    tkconfigure(Env$l.fr5$noms.list,state="disabled")
  }
  Env$l.fr5$espace.hor<-tklabel(Env$l.frames$Fr5,text="                    ",font=Env$police)
  tkgrid(Env$l.fr5$noms.list,Env$l.fr5$noms.scroll,row=0,column=0,rowspan=4,sticky="w");tkgrid.configure(Env$l.fr5$noms.scroll,sticky="ens")
  tkgrid(Env$l.fr5$espace.hor,row=0,column=1)
  tkgrid(Env$l.fr5$droite.lab,row=0,column=2,sticky="e")
  tkgrid(Env$l.fr5$droite.wdg,row=0,column=3,sticky="w")
  tkgrid(Env$l.fr5$interval.lab,row=1,column=2,sticky="e")
  tkgrid(Env$l.fr5$interval.wdg,row=1,column=3,sticky="w")
  tkgrid(Env$l.fr5$type.trait.lab,row=2,column=2,sticky="e")
  tkgrid(Env$l.fr5$type.trait.wdg,row=2,column=3,sticky="w")
  tkgrid(Env$l.fr5$epaisseur.lab,row=3,column=2,sticky="e")
  tkgrid(Env$l.fr5$epaisseur.wdg,row=3,column=3,sticky="w")
}

fr5.openN <-
function() {
  Env$l.frames$Fr5.status<-1
  tkconfigure(Env$l.wdg$but.lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr5)) {tkdestroy(Env$l.fr5[[i]])}
  Env$l.fr5<-list()
  Env$l.fr5$legende.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[99,1],font=Env$police)
  Env$l.fr5$legende.wdg<-tkcheckbutton(Env$l.frames$Fr5,variable=Env$l.var$legende)
  Env$l.fr5$titre.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[44,1],font=Env$police)
  Env$l.fr5$titre.wdg<-tkentry(Env$l.frames$Fr5,textvariable=Env$l.var$legende.titre,font=Env$police,width=40)
  Env$l.fr5$position.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[100,1],font=Env$police)
  Env$l.fr5$position.wdg<-ttkcombobox(Env$l.frames$Fr5,font=Env$police,values=Env$voc[101:109,1],textvariable=Env$l.var$legende.pos,state="readonly",width=15)
  Env$l.fr5$noms.lab1<-tklabel(Env$l.frames$Fr5,text=Env$voc[110,1],font=Env$police)
  Env$l.fr5$noms.list<-tklistbox(Env$l.frames$Fr5,height=4,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr5$noms.scroll,...))
  Env$l.fr5$noms.scroll<-tkscrollbar(Env$l.frames$Fr5,repeatinterval=4,command=function(...) tkyview(Env$l.fr5$noms.list,...))
  if (tclvalue(Env$l.var$plusieurs)==1) {
	for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr5$noms.list,"end",Env$l.var$noms1[i])}
  }
  tkbind(Env$l.fr5$noms.list,"<ButtonRelease-1>",function() {
    tkdelete(Env$l.fr5$noms.wdg,0,"end")
    tkinsert(Env$l.fr5$noms.wdg,"end",Env$l.var$noms1[as.numeric(tclvalue(tkcurselection(Env$l.fr5$noms.list)))+1])
  })
  Env$l.fr5$noms.lab2<-tklabel(Env$l.frames$Fr5,text=Env$voc[111,1],font=Env$police)
  Env$l.fr5$noms.wdg<-tkentry(Env$l.frames$Fr5,width=20,font=Env$police)
  tkbind(Env$l.fr5$noms.wdg,"<Enter>",function() {msg(text=Env$voc[26,1],type="info")})
  tkbind(Env$l.fr5$noms.wdg,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr5$noms.wdg,"<ButtonRelease-1>",function() {tkdelete(Env$l.fr5$noms.wdg,0,"end")})
  tkbind(Env$l.fr5$noms.wdg,"<Return>",function() {
    rename.legende2(value.list=tclvalue(tkcurselection(Env$l.fr5$noms.list)),value.nom=tclvalue(tkget(Env$l.fr5$noms.wdg)))
  })
  active.legende()
  Env$l.fr5$espace.hor<-tklabel(Env$l.frames$Fr5,text="                    ",font=Env$police)
  tkgrid(Env$l.fr5$legende.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr5$legende.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr5$titre.lab,row=1,column=0,sticky="e")
  tkgrid(Env$l.fr5$titre.wdg,row=1,column=1,sticky="w")
  tkgrid(Env$l.fr5$position.lab,row=2,column=0,sticky="e")
  tkgrid(Env$l.fr5$position.wdg,row=2,column=1,sticky="w")
  tkgrid(Env$l.fr5$espace.hor,row=0,column=2)
  tkgrid(Env$l.fr5$noms.lab1,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr5$noms.list,Env$l.fr5$noms.scroll,row=0,column=4,rowspan=4,sticky="w");tkgrid.configure(Env$l.fr5$noms.scroll,sticky="ens")
  tkgrid(Env$l.fr5$noms.lab2,row=4,column=3,sticky="e")
  tkgrid(Env$l.fr5$noms.wdg,row=4,column=4,sticky="w")
}


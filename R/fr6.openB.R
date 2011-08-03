fr6.openB <-
function() {
  Env$l.frames$Fr6.status<-1
  tkconfigure(Env$l.wdg$but.lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr6)) {tkdestroy(Env$l.fr6[[i]])}
  Env$l.fr6<-list()
  Env$l.fr6$legende.lab<-tklabel(Env$l.frames$Fr6,text=Env$voc[99,1],font=Env$police)
  Env$l.fr6$legende.wdg<-tkcheckbutton(Env$l.frames$Fr6,variable=Env$l.var$legende)
  Env$l.fr6$titre.lab<-tklabel(Env$l.frames$Fr6,text=Env$voc[44,1],font=Env$police)
  Env$l.fr6$titre.wdg<-tkentry(Env$l.frames$Fr6,textvariable=Env$l.var$legende.titre,font=Env$police,width=40)
  Env$l.fr6$position.lab<-tklabel(Env$l.frames$Fr6,text=Env$voc[100,1],font=Env$police)
  Env$l.fr6$position.wdg<-ttkcombobox(Env$l.frames$Fr6,font=Env$police,values=Env$voc[101:109,1],textvariable=Env$l.var$legende.pos,state="readonly",width=15)
  Env$l.fr6$noms.lab1<-tklabel(Env$l.frames$Fr6,text=Env$voc[110,1],font=Env$police)
  Env$l.fr6$noms.list<-tklistbox(Env$l.frames$Fr6,height=4,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr6$noms.scroll,...))
  Env$l.fr6$noms.scroll<-tkscrollbar(Env$l.frames$Fr6,repeatinterval=4,command=function(...) tkyview(Env$l.fr6$noms.list,...))
  tkbind(Env$l.fr6$noms.list,"<ButtonRelease-1>",function() {
    tkdelete(Env$l.fr6$noms.wdg,0,"end")
    if (tclvalue(Env$l.var$moyprop)=="moy") {
	tkinsert(Env$l.fr6$noms.wdg,"end",Env$l.var$noms2[as.numeric(tclvalue(tkcurselection(Env$l.fr6$noms.list)))+1])
    } else {
	tkinsert(Env$l.fr6$noms.wdg,"end",Env$l.var$nomsprop[as.numeric(tclvalue(tkcurselection(Env$l.fr6$noms.list)))+1])
    }
  })
  Env$l.fr6$noms.lab2<-tklabel(Env$l.frames$Fr6,text=Env$voc[111,1],font=Env$police)
  Env$l.fr6$noms.wdg<-tkentry(Env$l.frames$Fr6,width=20,font=Env$police)
  tkbind(Env$l.fr6$noms.wdg,"<Enter>",function() {msg(text=Env$voc[26,1],type="info")})
  tkbind(Env$l.fr6$noms.wdg,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr6$noms.wdg,"<ButtonRelease-1>",function() {tkdelete(Env$l.fr6$noms.wdg,0,"end")})
  tkbind(Env$l.fr6$noms.wdg,"<Return>",function() {
    rename.legende(value.list=tclvalue(tkcurselection(Env$l.fr6$noms.list)),value.nom=tclvalue(tkget(Env$l.fr6$noms.wdg)))
  })
  active.legende()
  if (tclvalue(Env$l.var$moyprop)=="moy") {
    for (i in 1:length(Env$l.var$noms2)) tkinsert(Env$l.fr6$noms.list,"end",Env$l.var$noms2[i])
  } else {
    for (i in 1:length(Env$l.var$nomsprop)) tkinsert(Env$l.fr6$noms.list,"end",Env$l.var$nomsprop[i])
  }
  Env$l.fr6$espace.hor<-tklabel(Env$l.frames$Fr6,text="                    ",font=Env$police)
  tkgrid(Env$l.fr6$legende.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr6$legende.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr6$titre.lab,row=1,column=0,sticky="e")
  tkgrid(Env$l.fr6$titre.wdg,row=1,column=1,sticky="w")
  tkgrid(Env$l.fr6$position.lab,row=2,column=0,sticky="e")
  tkgrid(Env$l.fr6$position.wdg,row=2,column=1,sticky="w")
  tkgrid(Env$l.fr6$espace.hor,row=0,column=2)
  tkgrid(Env$l.fr6$noms.lab1,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr6$noms.list,Env$l.fr6$noms.scroll,row=0,column=4,rowspan=4,sticky="w");tkgrid.configure(Env$l.fr6$noms.scroll,sticky="ens")
  tkgrid(Env$l.fr6$noms.lab2,row=4,column=3,sticky="e")
  tkgrid(Env$l.fr6$noms.wdg,row=4,column=4,sticky="w")
}


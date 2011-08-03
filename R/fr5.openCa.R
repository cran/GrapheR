fr5.openCa <-
function() {
  Env$l.frames$Fr5.status<-1
  tkconfigure(Env$l.wdg$but.lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr5)) {tkdestroy(Env$l.fr5[[i]])}
  Env$l.fr5<-list()
  Env$l.fr5$legende.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[99,1],font=Env$police)
  Env$l.fr5$legende.wdg<-tkcheckbutton(Env$l.frames$Fr5,variable=Env$l.var$legende)
  Env$l.fr5$position.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[100,1],font=Env$police)
  Env$l.fr5$position.wdg<-ttkcombobox(Env$l.frames$Fr5,font=Env$police,values=Env$voc[101:109,1],textvariable=Env$l.var$legende.pos,state="readonly",width=15)
  Env$l.fr5$titre.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[44,1],font=Env$police)
  Env$l.fr5$titre.wdg<-tkentry(Env$l.frames$Fr5,width=40,font=Env$police,textvariable=Env$l.var$legende.titre)
  active.legende2()
  Env$l.fr5$espace.hor1<-tklabel(Env$l.frames$Fr5,text="               ",font=Env$police)
  Env$l.fr5$espace.hor2<-tklabel(Env$l.frames$Fr5,text="               ",font=Env$police)
  tkgrid(Env$l.fr5$legende.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr5$legende.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr5$espace.hor1,row=0,column=2)
  tkgrid(Env$l.fr5$titre.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr5$titre.wdg,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr5$espace.hor2,row=0,column=5)
  tkgrid(Env$l.fr5$position.lab,row=0,column=6,sticky="e")
  tkgrid(Env$l.fr5$position.wdg,row=0,column=7,sticky="w")
}


fr5.openM <-
function() {
  Env$l.frames$Fr5.status<-1
  tkconfigure(Env$l.wdg$but.lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr5)) {tkdestroy(Env$l.fr5[[i]])}
  Env$l.fr5<-list()
  Env$l.fr5$taille.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[46,1],font=Env$police)
  Env$l.fr5$taille.wdg<-tkscale(Env$l.frames$Fr5,from=0.1,to=3,showvalue=TRUE,font=Env$police,variable=Env$l.var$lg.moustaches,resolution=0.1,orient="horizontal")
  tkbind(Env$l.fr5$taille.wdg,"<Enter>",function() {msg(text=Env$voc[77,1],type="info")})
  tkbind(Env$l.fr5$taille.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr5$trait.lab<-tklabel(Env$l.frames$Fr5,text=Env$voc[59,1],font=Env$police)
  Env$l.fr5$trait.wdg<-ttkcombobox(Env$l.frames$Fr5,font=Env$police,values=Env$voc[60:62,1],textvariable=Env$l.var$trait1,state="readonly",width=15)
  Env$l.fr5$espace.hor<-tklabel(Env$l.frames$Fr5,text="                                   ",font=Env$police)
  tkgrid(Env$l.fr5$taille.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr5$taille.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr5$espace.hor,row=0,column=2)
  tkgrid(Env$l.fr5$trait.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr5$trait.wdg,row=0,column=4,sticky="w")
}

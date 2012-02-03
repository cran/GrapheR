fr2.opengraphe <-
function() {
  Env$l.frames$Fr2.status<-1
  tkconfigure(Env$l.wdg$but.lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr2)) {tkdestroy(Env$l.fr2[[i]])}
  Env$l.fr2<-list()
  Env$l.fr2$titre.lab<-tklabel(Env$l.frames$Fr2,text=Env$voc[44,1],font=Env$police)
  Env$l.fr2$titre.wdg<-tkentry(Env$l.frames$Fr2,width=30,font=Env$police,textvariable=Env$l.var$titre)
  Env$l.fr2$col.lab<-tklabel(Env$l.frames$Fr2,text=Env$voc[45,1],font=Env$police)
  Env$l.fr2$col.wdg<-tkbutton(Env$l.frames$Fr2,text="Aa",font=Env$police3,foreground=tclvalue(Env$l.var$titre.col),activeforeground=tclvalue(Env$l.var$titre.col),command=function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$titre.col),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$titre.col)<-temp
	tkconfigure(Env$l.fr2$col.wdg,foreground=temp,activeforeground=temp)
    }
  })
  Env$l.fr2$taille.lab<-tklabel(Env$l.frames$Fr2,text=Env$voc[46,1],font=Env$police)
  Env$l.fr2$taille.wdg<-tkscale(Env$l.frames$Fr2,from=0.5,to=4,showvalue=TRUE,font=Env$police,variable=Env$l.var$titre.taille,resolution=0.1,orient="horizontal")
  Env$l.fr2$soustitre.lab<-tklabel(Env$l.frames$Fr2,text=Env$voc[247,1],font=Env$police)
  Env$l.fr2$soustitre.wdg<-tkentry(Env$l.frames$Fr2,width=30,font=Env$police,textvariable=Env$l.var$soustitre)
  Env$l.fr2$espace.hor1<-tklabel(Env$l.frames$Fr2,text="                              ",font=Env$police)
  Env$l.fr2$espace.hor2<-tklabel(Env$l.frames$Fr2,text="                                        ",font=Env$police)
  tkgrid(Env$l.fr2$titre.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr2$titre.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr2$espace.hor1,row=0,column=2)
  tkgrid(Env$l.fr2$col.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr2$col.wdg,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr2$espace.hor2,row=0,column=5)
  tkgrid(Env$l.fr2$taille.lab,row=0,column=6,sticky="e")
  tkgrid(Env$l.fr2$taille.wdg,row=0,column=7,sticky="w")
  tkgrid(Env$l.fr2$soustitre.lab,row=1,column=0,sticky="e")
  tkgrid(Env$l.fr2$soustitre.wdg,row=1,column=1,sticky="w")
}

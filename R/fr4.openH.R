fr4.openH <-
function() {
  Env$l.frames$Fr4.status<-1
  tkconfigure(Env$l.wdg$but.lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr4)) {tkdestroy(Env$l.fr4[[i]])}
  Env$l.fr4<-list()
  Env$l.fr4$nombre.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[55,1],font=Env$police)
  Env$l.fr4$nombre.wdg<-tkentry(Env$l.frames$Fr4,width=5,font=Env$police,textvariable=Env$l.var$hist.barres)
  tkbind(Env$l.fr4$nombre.wdg,"<Enter>",function() {msg(text=Env$voc[156,1],type="info")})
  tkbind(Env$l.fr4$nombre.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr4$colbarres.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[56,1],font=Env$police)
  Env$l.fr4$colbarres.wdg<-tkcanvas(Env$l.frames$Fr4,width="25",height="20",bg=tclvalue(Env$l.var$couleur1A))
  tkbind(Env$l.fr4$colbarres.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$couleur1A),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$couleur1A)<-temp
	tkconfigure(Env$l.fr4$colbarres.wdg,bg=temp)
    }
  })
  Env$l.fr4$colbordures.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[57,1],font=Env$police)
  Env$l.fr4$colbordures.wdg<-tkcanvas(Env$l.frames$Fr4,width="25",height="20",bg=tclvalue(Env$l.var$col.borduresA))
  tkbind(Env$l.fr4$colbordures.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$col.borduresA),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$col.borduresA)<-temp
	tkconfigure(Env$l.fr4$colbordures.wdg,bg=temp)
    }
  })
  Env$l.fr4$espace.hor1<-tklabel(Env$l.frames$Fr4,text="                              ",font=Env$police)
  Env$l.fr4$espace.hor2<-tklabel(Env$l.frames$Fr4,text="                              ",font=Env$police)
  tkgrid(Env$l.fr4$nombre.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr4$nombre.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr4$espace.hor1,row=0,column=2)
  tkgrid(Env$l.fr4$colbarres.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr4$colbarres.wdg,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr4$espace.hor2,row=0,column=5)
  tkgrid(Env$l.fr4$colbordures.lab,row=0,column=6,sticky="e")
  tkgrid(Env$l.fr4$colbordures.wdg,row=0,column=7,sticky="w")
}


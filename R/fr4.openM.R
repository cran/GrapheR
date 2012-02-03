fr4.openM <-
function() {
  Env$l.frames$Fr4.status<-1
  tkconfigure(Env$l.wdg$but.lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr4)) {tkdestroy(Env$l.fr4[[i]])}
  Env$l.fr4<-list()
  Env$l.fr4$noms.list<-tklistbox(Env$l.frames$Fr4,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr4$noms.scroll,...))
  Env$l.fr4$noms.scroll<-tkscrollbar(Env$l.frames$Fr4,repeatinterval=5,command=function(...) tkyview(Env$l.fr4$noms.list,...))
  for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms1[i])}
  tkbind(Env$l.fr4$noms.list,"<Enter>",function() {msg(text=Env$voc[243,1],type="info")})
  tkbind(Env$l.fr4$noms.list,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr4$noms.list,"<ButtonRelease-1>",function() {
    tkconfigure(Env$l.fr4$colboites.wdg,bg=Env$l.var$couleur1B[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1])
    tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1])
  })
  Env$l.fr4$colboites.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[74,1],font=Env$police)
  Env$l.fr4$colboites.wdg<-tkcanvas(Env$l.frames$Fr4,width="25",height="20",bg=Env$l.var$couleur1B[1])
  tkbind(Env$l.fr4$colboites.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=Env$l.var$couleur1B[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1],title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	Env$l.var$couleur1B[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1]<-temp
	tkconfigure(Env$l.fr4$colboites.wdg,bg=temp)
    }
  })
  Env$l.fr4$colbordures.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[57,1],font=Env$police)
  Env$l.fr4$colbordures.wdg<-tkcanvas(Env$l.frames$Fr4,width="25",height="20",bg=Env$l.var$col.borduresB[1])
  tkbind(Env$l.fr4$colbordures.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=Env$l.var$col.borduresB[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1],title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	Env$l.var$col.borduresB[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1]<-temp
	tkconfigure(Env$l.fr4$colbordures.wdg,bg=temp)
    }
  })
  Env$l.fr4$IC.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[75,1],font=Env$police)
  Env$l.fr4$IC.wdg<-tkcheckbutton(Env$l.frames$Fr4,variable=Env$l.var$ICmediane)
  tkbind(Env$l.fr4$IC.wdg,"<Enter>",function() {msg(text=Env$voc[76,1],type="info")})
  tkbind(Env$l.fr4$IC.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr4$varwidth.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[257,1],font=Env$police)
  Env$l.fr4$varwidth.wdg<-tkcheckbutton(Env$l.frames$Fr4,variable=Env$l.var$varwidth)
  tkbind(Env$l.fr4$varwidth.wdg,"<Enter>",function() {msg(text=Env$voc[258,1],type="info")})
  tkbind(Env$l.fr4$varwidth.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr4$moy.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[255,1],font=Env$police)
  Env$l.fr4$moy.wdg<-tkcheckbutton(Env$l.frames$Fr4,variable=Env$l.var$boxmoy)
  tkbind(Env$l.fr4$moy.wdg,"<Enter>",function() {msg(text=Env$voc[256,1],type="info")})
  tkbind(Env$l.fr4$moy.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr4$espace.hor1<-tklabel(Env$l.frames$Fr4,text="                    ",font=Env$police)
  Env$l.fr4$espace.hor2<-tklabel(Env$l.frames$Fr4,text="                    ",font=Env$police)
  tkgrid(Env$l.fr4$noms.list,Env$l.fr4$noms.scroll,row=0,column=0,rowspan=4,sticky="e");tkgrid.configure(Env$l.fr4$noms.scroll,sticky="ens")
  tkgrid(Env$l.fr4$espace.hor1,row=0,column=1)
  tkgrid(Env$l.fr4$colboites.lab,row=0,column=2,sticky="e")
  tkgrid(Env$l.fr4$colboites.wdg,row=0,column=3,sticky="w")
  tkgrid(Env$l.fr4$colbordures.lab,row=1,column=2,sticky="e")
  tkgrid(Env$l.fr4$colbordures.wdg,row=1,column=3,sticky="w")
  tkgrid(Env$l.fr4$espace.hor2,row=0,column=4)
  tkgrid(Env$l.fr4$IC.lab,row=0,column=5,sticky="e")
  tkgrid(Env$l.fr4$IC.wdg,row=0,column=6,sticky="w")
  tkgrid(Env$l.fr4$varwidth.lab,row=1,column=5,sticky="e")
  tkgrid(Env$l.fr4$varwidth.wdg,row=1,column=6,sticky="w")
  tkgrid(Env$l.fr4$moy.lab,row=2,column=5,sticky="e")
  tkgrid(Env$l.fr4$moy.wdg,row=2,column=6,sticky="w")
}

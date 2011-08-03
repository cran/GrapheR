fr4.openB <-
function() {
  Env$l.frames$Fr4.status<-1
  tkconfigure(Env$l.wdg$but.lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr4)) {tkdestroy(Env$l.fr4[[i]])}
  Env$l.fr4<-list()
  Env$l.fr4$noms.list<-tklistbox(Env$l.frames$Fr4,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr4$noms.scroll,...))
  Env$l.fr4$noms.scroll<-tkscrollbar(Env$l.frames$Fr4,repeatinterval=5,command=function(...) tkyview(Env$l.fr4$noms.list,...))
  if (tclvalue(Env$l.var$moyprop)=="moy") {
    for (i in 1:length(Env$l.var$noms2)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms2[i])}
  } else {
    for (i in 1:length(Env$l.var$nomsprop)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$nomsprop[i])}
  }
  tkbind(Env$l.fr4$noms.list,"<Enter>",function() {if (tclvalue(Env$l.var$plusieurs)==1) {msg(text=Env$voc[112,1],type="info")}})
  tkbind(Env$l.fr4$noms.list,"<Leave>",function() {if (tclvalue(Env$l.var$plusieurs)==1) {msg(text="",type="info")}})
  tkbind(Env$l.fr4$noms.list,"<ButtonRelease-1>",function() {
    if (tclvalue(Env$l.var$plusieurs)==1) {
	tkconfigure(Env$l.fr4$colbarres.wdg,bg=Env$l.var$couleur1B[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1])
	tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1])
	for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
	tkconfigure(Env$l.fr4$l.hachures[[Env$l.var$hachuresB[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1]]],borderwidth=2)
    }
  })
  Env$l.fr4$colbarres.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[91,1],font=Env$police)
  Env$l.fr4$colbarres.wdg<-tkcanvas(Env$l.frames$Fr4,width="25",height="20",bg=tclvalue(Env$l.var$couleur1A))
  tkbind(Env$l.fr4$colbarres.wdg,"<ButtonRelease-1>",col.barres)
  Env$l.fr4$colbordures.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[57,1],font=Env$police)
  Env$l.fr4$colbordures.wdg<-tkcanvas(Env$l.frames$Fr4,width="25",height="20",bg=tclvalue(Env$l.var$col.borduresA))
  tkbind(Env$l.fr4$colbordures.wdg,"<ButtonRelease-1>",col.bordures)
  Env$l.fr4$stack.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[93,1],font=Env$police)
  Env$l.fr4$stack.wdg<-tkcheckbutton(Env$l.frames$Fr4,variable=Env$l.var$stack,command=function() {if (tclvalue(Env$l.var$plusieurs)==1) {active.erreur()}})
  tkbind(Env$l.fr4$stack.wdg,"<Enter>",function() {msg(text=Env$voc[113,1],type="warning")})
  tkbind(Env$l.fr4$stack.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr4$hachures.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[92,1],font=Env$police)
  Env$l.fr4$l.hachures<-list()
  for (i in 1:9) {
    Env$l.fr4$l.hachures[[i]]<-tklabel(Env$l.frames$Fr4,height=35,width=35,font=Env$police,relief="groove",borderwidth=0,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Hachures",i,".gif",sep=""),fsep=.Platform$file.sep)))
  }
  if (tclvalue(Env$l.var$plusieurs)==0) {
    tkconfigure(Env$l.fr4$noms.list,state="disabled")
    tkconfigure(Env$l.fr4$colbarres.wdg,bg=tclvalue(Env$l.var$couleur1A))
    tkconfigure(Env$l.fr4$colbordures.wdg,bg=tclvalue(Env$l.var$col.borduresA))
    tkconfigure(Env$l.fr4$stack.lab,foreground="grey")
    tkconfigure(Env$l.fr4$stack.wdg,state="disabled")
    tkconfigure(Env$l.fr4$l.hachures[[as.numeric(tclvalue(Env$l.var$hachuresA))]],borderwidth=2)
  } else {
    tkconfigure(Env$l.fr4$noms.list,state="normal")
    tkconfigure(Env$l.fr4$colbarres.wdg,bg=Env$l.var$couleur1B[1])
    tkconfigure(Env$l.fr4$colbordures.wdg,bg=Env$l.var$col.borduresB[1])
    tkconfigure(Env$l.fr4$stack.lab,foreground="black")
    tkconfigure(Env$l.fr4$stack.wdg,state="normal")
    tkconfigure(Env$l.fr4$l.hachures[[Env$l.var$hachuresB[1]]],borderwidth=2)
    tkselection.set(Env$l.fr4$noms.list,"0")
  }
  tkbind(Env$l.fr4$l.hachures[[1]],"<ButtonRelease-1>",function() {hachures(num=1)})
  tkbind(Env$l.fr4$l.hachures[[2]],"<ButtonRelease-1>",function() {hachures(num=2)})
  tkbind(Env$l.fr4$l.hachures[[3]],"<ButtonRelease-1>",function() {hachures(num=3)})
  tkbind(Env$l.fr4$l.hachures[[4]],"<ButtonRelease-1>",function() {hachures(num=4)})
  tkbind(Env$l.fr4$l.hachures[[5]],"<ButtonRelease-1>",function() {hachures(num=5)})
  tkbind(Env$l.fr4$l.hachures[[6]],"<ButtonRelease-1>",function() {hachures(num=6)})
  tkbind(Env$l.fr4$l.hachures[[7]],"<ButtonRelease-1>",function() {hachures(num=7)})
  tkbind(Env$l.fr4$l.hachures[[8]],"<ButtonRelease-1>",function() {hachures(num=8)})
  tkbind(Env$l.fr4$l.hachures[[9]],"<ButtonRelease-1>",function() {hachures(num=9)})
  Env$l.fr4$espace.hor1<-tklabel(Env$l.frames$Fr4,text="                         ",font=Env$police)
  Env$l.fr4$espace.hor2<-tklabel(Env$l.frames$Fr4,text="                              ",font=Env$police)
  tkgrid(Env$l.fr4$noms.list,Env$l.fr4$noms.scroll,row=0,column=0,rowspan=4,sticky="e");tkgrid.configure(Env$l.fr4$noms.scroll,sticky="ens")
  tkgrid(Env$l.fr4$espace.hor1,row=0,column=1)
  tkgrid(Env$l.fr4$colbarres.lab,row=0,column=2,sticky="e")
  tkgrid(Env$l.fr4$colbarres.wdg,row=0,column=3,sticky="w")
  tkgrid(Env$l.fr4$colbordures.lab,row=1,column=2,sticky="e")
  tkgrid(Env$l.fr4$colbordures.wdg,row=1,column=3,sticky="w")
  tkgrid(Env$l.fr4$stack.lab,row=2,column=2,sticky="e")
  tkgrid(Env$l.fr4$stack.wdg,row=2,column=3,sticky="w")
  tkgrid(Env$l.fr4$espace.hor2,row=0,column=4)
  tkgrid(Env$l.fr4$hachures.lab,row=0,column=5,sticky="e")
  tkgrid(Env$l.fr4$l.hachures[[1]],row=0,column=6)
  tkgrid(Env$l.fr4$l.hachures[[2]],row=0,column=7)
  tkgrid(Env$l.fr4$l.hachures[[3]],row=0,column=8)
  tkgrid(Env$l.fr4$l.hachures[[4]],row=1,column=6)
  tkgrid(Env$l.fr4$l.hachures[[5]],row=1,column=7)
  tkgrid(Env$l.fr4$l.hachures[[6]],row=1,column=8)
  tkgrid(Env$l.fr4$l.hachures[[7]],row=2,column=6)
  tkgrid(Env$l.fr4$l.hachures[[8]],row=2,column=7)
  tkgrid(Env$l.fr4$l.hachures[[9]],row=2,column=8)
}


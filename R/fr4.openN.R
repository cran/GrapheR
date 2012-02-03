fr4.openN <-
function() {
  Env$l.frames$Fr4.status<-1
  tkconfigure(Env$l.wdg$but.lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr4)) {tkdestroy(Env$l.fr4[[i]])}
  Env$l.fr4<-list()
  Env$l.fr4$noms.list<-tklistbox(Env$l.frames$Fr4,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr4$noms.scroll,...))
  Env$l.fr4$noms.scroll<-tkscrollbar(Env$l.frames$Fr4,repeatinterval=5,command=function(...) tkyview(Env$l.fr4$noms.list,...))
  tkbind(Env$l.fr4$noms.list,"<Enter>",function() {if (tclvalue(Env$l.var$plusieurs)==1) {msg(text=Env$voc[141,1],type="info")}})
  tkbind(Env$l.fr4$noms.list,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr4$noms.list,"<ButtonRelease-1>",function() {
    if (tclvalue(Env$l.var$plusieurs)==1) {
	Env$l.var$select<-as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1
	for (i in 1:8) {tkconfigure(Env$l.fr4$l.symboles[[i]],borderwidth=0)}
	tkconfigure(Env$l.fr4$l.symboles[[Env$l.var$symboleB[Env$l.var$select]]],borderwidth=2)
	tkconfigure(Env$l.fr4$col.wdg,bg=Env$l.var$couleur2B[Env$l.var$select])
	tclvalue(Env$l.var$taille.ptsA)<-as.character(Env$l.var$taille.ptsB[Env$l.var$select])
    }
  })
  Env$l.fr4$symboles.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[138,1],font=Env$police)
  Env$l.fr4$l.symboles<-list()
  for (i in 1:8) {
    Env$l.fr4$l.symboles[[i]]<-tklabel(Env$l.frames$Fr4,height=35,width=35,font=Env$police,relief="groove",borderwidth=0,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Symbole",i,".gif",sep=""),fsep=.Platform$file.sep)))
  }
  tkbind(Env$l.fr4$l.symboles[[1]],"<ButtonRelease-1>",function() {symboles(num=1)})
  tkbind(Env$l.fr4$l.symboles[[2]],"<ButtonRelease-1>",function() {symboles(num=2)})
  tkbind(Env$l.fr4$l.symboles[[3]],"<ButtonRelease-1>",function() {symboles(num=3)})
  tkbind(Env$l.fr4$l.symboles[[4]],"<ButtonRelease-1>",function() {symboles(num=4)})
  tkbind(Env$l.fr4$l.symboles[[5]],"<ButtonRelease-1>",function() {symboles(num=5)})
  tkbind(Env$l.fr4$l.symboles[[6]],"<ButtonRelease-1>",function() {symboles(num=6)})
  tkbind(Env$l.fr4$l.symboles[[7]],"<ButtonRelease-1>",function() {symboles(num=7)})
  tkbind(Env$l.fr4$l.symboles[[8]],"<ButtonRelease-1>",function() {symboles(num=8)})
  Env$l.fr4$col.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[45,1],font=Env$police)
  Env$l.fr4$col.wdg<-tkcanvas(Env$l.frames$Fr4,width="25",height="20")
  tkbind(Env$l.fr4$col.wdg,"<ButtonRelease-1>",col.symboles)
  Env$l.fr4$taille.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[131,1],font=Env$police)
  Env$l.fr4$taille.wdg<-tkscale(Env$l.frames$Fr4,showvalue=TRUE,from=0.5,to=3,resolution=0.1,font=Env$police,variable=Env$l.var$taille.ptsA,orient="horizontal",command=function(...) {
    if (tclvalue(Env$l.var$plusieurs)==1) {
	Env$l.var$taille.ptsB[Env$l.var$select]<-as.numeric(tclvalue(Env$l.var$taille.ptsA))
	tkselection.set(Env$l.fr4$noms.list,as.character(Env$l.var$select-1))
    }
  })
  if (tclvalue(Env$l.var$plusieurs)==1) {
    Env$l.var$select<-1
    for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$noms1[i])}
    tkselection.set(Env$l.fr4$noms.list,"0")
    tkconfigure(Env$l.fr4$l.symboles[[Env$l.var$symboleB[1]]],borderwidth=2)
    tkconfigure(Env$l.fr4$col.wdg,bg=Env$l.var$couleur2B[1])
    tclvalue(Env$l.var$taille.ptsA)<-as.character(Env$l.var$taille.ptsB[1])
  } else {
    tkconfigure(Env$l.fr4$noms.list,state="disabled")
    tkconfigure(Env$l.fr4$l.symboles[[as.numeric(tclvalue(Env$l.var$symboleA))]],borderwidth=2)
    tkconfigure(Env$l.fr4$col.wdg,bg=tclvalue(Env$l.var$couleur2A))
  }
  Env$l.fr4$ptlab.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[242,1],font=Env$police)
  Env$l.fr4$ptlab.wdg<-tkcheckbutton(Env$l.frames$Fr4,variable=Env$l.var$ptlab)
  Env$l.fr4$espace.hor1<-tklabel(Env$l.frames$Fr4,text="                    ",font=Env$police)
  Env$l.fr4$espace.hor2<-tklabel(Env$l.frames$Fr4,text="                    ",font=Env$police)
  tkgrid(Env$l.fr4$noms.list,Env$l.fr4$noms.scroll,row=0,column=0,rowspan=3,sticky="w");tkgrid.configure(Env$l.fr4$noms.scroll,sticky="ens")
  tkgrid(Env$l.fr4$espace.hor1,row=0,column=1)
  tkgrid(Env$l.fr4$symboles.lab,row=0,column=2,sticky="e")
  tkgrid(Env$l.fr4$l.symboles[[1]],row=0,column=3)
  tkgrid(Env$l.fr4$l.symboles[[2]],row=0,column=4)
  tkgrid(Env$l.fr4$l.symboles[[3]],row=0,column=5)
  tkgrid(Env$l.fr4$l.symboles[[4]],row=0,column=6)
  tkgrid(Env$l.fr4$l.symboles[[5]],row=1,column=3)
  tkgrid(Env$l.fr4$l.symboles[[6]],row=1,column=4)
  tkgrid(Env$l.fr4$l.symboles[[7]],row=1,column=5)
  tkgrid(Env$l.fr4$l.symboles[[8]],row=1,column=6)
  tkgrid(Env$l.fr4$col.lab,row=2,column=2,sticky="e")
  tkgrid(Env$l.fr4$col.wdg,row=2,column=3,columnspan=4,sticky="w")
  tkgrid(Env$l.fr4$espace.hor2,row=0,column=7)
  tkgrid(Env$l.fr4$taille.lab,row=0,column=8,sticky="e")
  tkgrid(Env$l.fr4$taille.wdg,row=0,column=9,sticky="w")
  tkgrid(Env$l.fr4$ptlab.lab,row=1,column=8,sticky="e")
  tkgrid(Env$l.fr4$ptlab.wdg,row=1,column=9,sticky="w")
}

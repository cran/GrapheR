fr4.openCa <-
function() {
  Env$l.frames$Fr4.status<-1
  tkconfigure(Env$l.wdg$but.lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr4)) {tkdestroy(Env$l.fr4[[i]])}
  Env$l.fr4<-list()
  Env$l.fr4$noms.list<-tklistbox(Env$l.frames$Fr4,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr4$noms.scroll,...))
  Env$l.fr4$noms.scroll<-tkscrollbar(Env$l.frames$Fr4,repeatinterval=5,command=function(...) tkyview(Env$l.fr4$noms.list,...))
  for (i in 1:length(Env$l.var$nomsparts)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$nomsparts[i])}
  tkselection.set(Env$l.fr4$noms.list,"0") 
  tkbind(Env$l.fr4$noms.list,"<Enter>",function() {msg(text=Env$voc[123,1],type="info")})
  tkbind(Env$l.fr4$noms.list,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr4$noms.list,"<ButtonRelease-1>",function() {
    tkconfigure(Env$l.fr4$colparts.wdg,bg=Env$l.var$couleur1B[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1])
    for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
    tkconfigure(Env$l.fr4$l.hachures[[Env$l.var$hachuresB[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1]]],borderwidth=2)
  })
  Env$l.fr4$colparts.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[124,1],font=Env$police)
  Env$l.fr4$colparts.wdg<-tkcanvas(Env$l.frames$Fr4,width="25",height="20",bg=Env$l.var$couleur1B[1])
  tkbind(Env$l.fr4$colparts.wdg,"<ButtonRelease-1>",col.parts)
  Env$l.fr4$colbordures.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[57,1],font=Env$police)
  Env$l.fr4$colbordures.wdg<-tkcanvas(Env$l.frames$Fr4,width="25",height="20",bg=tclvalue(Env$l.var$col.borduresA))
  tkbind(Env$l.fr4$colbordures.wdg,"<Enter>",function() {msg(text=Env$voc[126,1],type="info")})
  tkbind(Env$l.fr4$colbordures.wdg,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr4$colbordures.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$col.borduresA),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$col.borduresA)<-temp
	tkconfigure(Env$l.fr4$colbordures.wdg,bg=tclvalue(Env$l.var$col.borduresA))
    }
  })
  Env$l.fr4$hachures.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[92,1],font=Env$police)
  Env$l.fr4$l.hachures<-list()
  for (i in 1:9) {
    Env$l.fr4$l.hachures[[i]]<-tklabel(Env$l.frames$Fr4,height=35,width=35,font=Env$police,relief="groove",borderwidth=0,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Hachures",i,".gif",sep=""),fsep=.Platform$file.sep)))
  }
  tkconfigure(Env$l.fr4$l.hachures[[Env$l.var$hachuresB[1]]],borderwidth=2)
  tkbind(Env$l.fr4$l.hachures[[1]],"<ButtonRelease-1>",function() {hachures2(num=1)})
  tkbind(Env$l.fr4$l.hachures[[2]],"<ButtonRelease-1>",function() {hachures2(num=2)})
  tkbind(Env$l.fr4$l.hachures[[3]],"<ButtonRelease-1>",function() {hachures2(num=3)})
  tkbind(Env$l.fr4$l.hachures[[4]],"<ButtonRelease-1>",function() {hachures2(num=4)})
  tkbind(Env$l.fr4$l.hachures[[5]],"<ButtonRelease-1>",function() {hachures2(num=5)})
  tkbind(Env$l.fr4$l.hachures[[6]],"<ButtonRelease-1>",function() {hachures2(num=6)})
  tkbind(Env$l.fr4$l.hachures[[7]],"<ButtonRelease-1>",function() {hachures2(num=7)})
  tkbind(Env$l.fr4$l.hachures[[8]],"<ButtonRelease-1>",function() {hachures2(num=8)})
  tkbind(Env$l.fr4$l.hachures[[9]],"<ButtonRelease-1>",function() {hachures2(num=9)})
  Env$l.fr4$espace.hor1<-tklabel(Env$l.frames$Fr4,text="                         ",font=Env$police)
  Env$l.fr4$espace.hor2<-tklabel(Env$l.frames$Fr4,text="                              ",font=Env$police)
  tkgrid(Env$l.fr4$noms.list,Env$l.fr4$noms.scroll,row=0,column=0,rowspan=7,sticky="e");tkgrid.configure(Env$l.fr4$noms.scroll,sticky="ens")
  tkgrid(Env$l.fr4$espace.hor1,row=0,column=1)
  tkgrid(Env$l.fr4$colparts.lab,row=0,column=2,sticky="e")
  tkgrid(Env$l.fr4$colparts.wdg,row=0,column=3,sticky="w")
  tkgrid(Env$l.fr4$colbordures.lab,row=1,column=2,sticky="e")
  tkgrid(Env$l.fr4$colbordures.wdg,row=1,column=3,sticky="w")
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

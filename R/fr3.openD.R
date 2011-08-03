fr3.openD <-
function() {
  Env$l.frames$Fr3.status<-1
  tkconfigure(Env$l.wdg$but.lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr3)) {tkdestroy(Env$l.fr3[[i]])}
  Env$l.fr3<-list()
  Env$l.fr3$titre1<-tklabel(Env$l.frames$Fr3,text=Env$voc[15,1],font=Env$police3)
  Env$l.fr3$var.list<-tklistbox(Env$l.frames$Fr3,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr3$var.scroll,...))
  Env$l.fr3$var.scroll<-tkscrollbar(Env$l.frames$Fr3,repeatinterval=5,command=function(...) tkyview(Env$l.fr3$var.list,...))
  tkbind(Env$l.fr3$var.list,"<ButtonRelease-1>",function() {
    if(!is.null(Env$dataset)) {
	tkdelete(Env$l.fr3$nom.wdg,0,"end")
	tkinsert(Env$l.fr3$nom.wdg,"end",names(Env$dataset[as.numeric(tclvalue(tkcurselection(Env$l.fr3$var.list)))+1]))
    }
  })
  Env$l.fr3$titre2<-tklabel(Env$l.frames$Fr3,text=Env$voc[22,1],font=Env$police3)
  Env$l.fr3$nom.wdg<-tkentry(Env$l.frames$Fr3,width=20,font=Env$police)
  tkbind(Env$l.fr3$nom.wdg,"<Enter>",function() {msg(text=Env$voc[26,1],type="info")})
  tkbind(Env$l.fr3$nom.wdg,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr3$nom.wdg,"<ButtonRelease-1>",function() {
    if(!is.null(Env$dataset)) {
	tkdelete(Env$l.fr3$nom.wdg,0,"end")
    }
  })
  tkbind(Env$l.fr3$nom.wdg,"<Return>",rename.variable)
  if(!is.null(Env$dataset)) {
    for (i in 1:ncol(Env$dataset)) {tkinsert(Env$l.fr3$var.list,"end",colnames(Env$dataset)[i])}
  }
  Env$l.fr3$espace.ver<-tklabel(Env$l.frames$Fr3,text="",font=Env$police2)
  Env$l.fr3$espace.hor<-tklabel(Env$l.frames$Fr3,text="                                                       ",font=Env$police)
  tkgrid(Env$l.fr3$titre1,row=0,column=0)
  tkgrid(Env$l.fr3$espace.ver)
  tkgrid(Env$l.fr3$var.list,Env$l.fr3$var.scroll,row=2,column=0,rowspan=5);tkgrid.configure(Env$l.fr3$var.scroll,sticky="ens")
  tkgrid(Env$l.fr3$espace.hor,row=2,column=1)
  tkgrid(Env$l.fr3$titre2,row=0,column=2)
  tkgrid(Env$l.fr3$nom.wdg,row=2,column=2)
}


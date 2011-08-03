fr2.openD <-
function() {
  Env$l.frames$Fr2.status<-1
  tkconfigure(Env$l.wdg$but.lab2,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr2)) {tkdestroy(Env$l.fr2[[i]])}
  Env$l.fr2<-list()
  Env$l.fr2$titre1<-tklabel(Env$l.frames$Fr2,text=Env$voc[15,1],font=Env$police3)
  Env$l.fr2$var.list<-tklistbox(Env$l.frames$Fr2,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr2$var.scroll,...))
  Env$l.fr2$var.scroll<-tkscrollbar(Env$l.frames$Fr2,repeatinterval=5,command=function(...) tkyview(Env$l.fr2$var.list,...))
  tkbind(Env$l.fr2$var.list,"<ButtonRelease-1>",function() {
    if(!is.null(Env$dataset)) {
	tkconfigure(Env$l.fr2$type.wdg,text=class(Env$dataset[,as.numeric(tclvalue(tkcurselection(Env$l.fr2$var.list)))+1]))
	tkconfigure(Env$l.fr2$resume.wdg,state="normal")
	tkdelete(Env$l.fr2$resume.wdg,"0.0","end")
	resume<-cbind(summary(Env$dataset)[,as.numeric(tclvalue(tkcurselection(Env$l.fr2$var.list)))+1])
	for (i in 1:nrow(resume)) {
	  if(!is.na(resume[i])) {tkinsert(Env$l.fr2$resume.wdg,"end",paste(resume[i],"\n",sep=""))}
	}
	tkconfigure(Env$l.fr2$resume.wdg,state="disabled")
    }
  })
  Env$l.fr2$type.lab<-tklabel(Env$l.frames$Fr2,text=Env$voc[16,1],font=Env$police)
  Env$l.fr2$type.wdg<-tklabel(Env$l.frames$Fr2,text="",font=Env$police)
  Env$l.fr2$titre2<-tklabel(Env$l.frames$Fr2,text=Env$voc[17,1],font=Env$police3)
  Env$l.fr2$resume.wdg<-tktext(Env$l.frames$Fr2,width=16,height=8,font=Env$police4,state="disabled")
  tkdelete(Env$l.fr2$var.list,0,"end")
  if(!is.null(Env$dataset)) {
    for (i in 1:ncol(Env$dataset)) {tkinsert(Env$l.fr2$var.list,"end",colnames(Env$dataset)[i])}
  }
  tkconfigure(Env$l.fr2$type.wdg,text="")
  tkconfigure(Env$l.fr2$resume.wdg,state="normal")
  tkdelete(Env$l.fr2$resume.wdg,"0.0","end")
  tkconfigure(Env$l.fr2$resume.wdg,state="disabled")
  Env$l.fr2$espace.ver<-tklabel(Env$l.frames$Fr2,text="",font=Env$police2)
  Env$l.fr2$espace.hor1<-tklabel(Env$l.frames$Fr2,text="                                                  ",font=Env$police2)
  Env$l.fr2$espace.hor2<-tklabel(Env$l.frames$Fr2,text="                                                  ",font=Env$police2)
  tkgrid(Env$l.fr2$titre1,row=0,column=0)
  tkgrid(Env$l.fr2$espace.ver,row=1,column=0)
  tkgrid(Env$l.fr2$var.list,Env$l.fr2$var.scroll,row=2,column=0,rowspan=5);tkgrid.configure(Env$l.fr2$var.scroll,sticky="ens")
  tkgrid(Env$l.fr2$espace.hor1,row=2,column=2)
  tkgrid(Env$l.fr2$type.lab,row=3,column=3,sticky="e")
  tkgrid(Env$l.fr2$type.wdg,row=3,column=4,sticky="w")
  tkgrid(Env$l.fr2$espace.hor2,row=2,column=5)
  tkgrid(Env$l.fr2$titre2,row=0,column=6)
  tkgrid(Env$l.fr2$resume.wdg,row=2,column=6,rowspan=5)
}


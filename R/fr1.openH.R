fr1.openH <-
function() {
  Env$l.frames$Fr1.status<-1
  tkconfigure(Env$l.wdg$but.lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr1)) {tkdestroy(Env$l.fr1[[i]])}
  Env$l.fr1<-list()
  Env$l.fr1$variable.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[36,1],font=Env$police)
  Env$l.fr1$variable.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$l.var$var.num,textvariable=Env$l.var$variable,font=Env$police,state="readonly")
  Env$l.fr1$facteur.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[37,1],font=Env$police)
  Env$l.fr1$facteur.wdg<-ttkcombobox(Env$l.frames$Fr1,values=c(Env$voc[82,1],Env$l.var$var.fact),textvariable=Env$l.var$facteur1,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$facteur.wdg,"<<ComboboxSelected>>",function() {
    if (nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
	tkconfigure(Env$l.fr1$niveau.lab,foreground="black")
	tkconfigure(Env$l.fr1$niveau.wdg,state="readonly")
	tkdelete(Env$l.fr1$niveau.wdg,0,"end")
	tkconfigure(Env$l.fr1$niveau.wdg,values=levels(Env$dataset[,tclvalue(Env$l.var$facteur1)]))
	tclvalue(Env$l.var$niveau)<-levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[1]
    } else {
	tkconfigure(Env$l.fr1$niveau.lab,foreground="grey")
	tkdelete(Env$l.fr1$niveau.wdg,0,"end")
	tclvalue(Env$l.var$niveau)<-""
	tkconfigure(Env$l.fr1$niveau.wdg,state="disabled")
    }
  })
  Env$l.fr1$niveau.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[38,1],font=Env$police,foreground=ifelse(nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1],"black","grey"))
  Env$l.fr1$niveau.wdg<-ttkcombobox(Env$l.frames$Fr1,values="",textvariable=Env$l.var$niveau,font=Env$police,state=ifelse(nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1],"readonly","disabled"))
  Env$l.fr1$type.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[39,1],font=Env$police)
  Env$l.fr1$type.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$voc[40:42,1],textvariable=Env$l.var$hist.type,font=Env$police,state="readonly")
  if (nchar(tclvalue(Env$l.var$facteur1))>0 & tclvalue(Env$l.var$facteur1)!=Env$voc[82,1]) {
    tkconfigure(Env$l.fr1$niveau.wdg,values=levels(Env$dataset[,tclvalue(Env$l.var$facteur1)]))
  }
  tkbind(Env$l.fr1$type.wdg,"<Enter>",function() {msg(text=Env$voc[114,1],type="warning")})
  tkbind(Env$l.fr1$type.wdg,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr1$type.wdg,"<<ComboboxSelected>>",function() {
    if (exists("hor.liminf.wdg",where=Env$l.fr3)) {
	if (tclvalue(Env$l.var$hist.type)==Env$voc[40,1]) {
	  tkconfigure(Env$l.fr3$hor.liminf.wdg,state="disabled")
	  tkconfigure(Env$l.fr3$hor.limsup.wdg,state="disabled")
	} else {
	  tkconfigure(Env$l.fr3$hor.liminf.wdg,state="normal")
	  tkconfigure(Env$l.fr3$hor.limsup.wdg,state="normal")
	}
    }
    if (exists("tracer.lab",where=Env$l.fr5)) {
	if (!tclvalue(Env$l.var$hist.type)==Env$voc[42,1]) {
	  tkconfigure(Env$l.fr5$tracer.lab,foreground="grey")
	  tkconfigure(Env$l.fr5$tracer.wdg,state="disabled")
	  tkconfigure(Env$l.fr5$col.lab,foreground="grey")
	  tkconfigure(Env$l.fr5$col.wdg,bg="grey")
	  tkconfigure(Env$l.fr5$trait.lab,foreground="grey")
	  tkconfigure(Env$l.fr5$trait.wdg,state="disabled")
	  tkconfigure(Env$l.fr5$epaisseur.lab,foreground="grey")
	  tkconfigure(Env$l.fr5$epaisseur.wdg,state="disabled")
	} else {
	  tkconfigure(Env$l.fr5$tracer.lab,foreground="black")
	  tkconfigure(Env$l.fr5$tracer.wdg,state="normal")
	  tkconfigure(Env$l.fr5$col.lab,foreground="black")
	  tkconfigure(Env$l.fr5$col.wdg,bg=tclvalue(Env$l.var$couleur2A))
	  tkconfigure(Env$l.fr5$trait.lab,foreground="black")
	  tkconfigure(Env$l.fr5$trait.wdg,state="readonly")
	  tkconfigure(Env$l.fr5$epaisseur.lab,foreground="black")
	  tkconfigure(Env$l.fr5$epaisseur.wdg,state="normal")
	}
    }
  })
  Env$l.fr1$encadre.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[43,1],font=Env$police)
  Env$l.fr1$encadre.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$encadre)
  Env$l.fr1$espace.hor<-tklabel(Env$l.frames$Fr1,text="                                   ",font=Env$police)
  tkgrid(Env$l.fr1$variable.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr1$variable.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr1$facteur.lab,row=1,column=0,sticky="e")
  tkgrid(Env$l.fr1$facteur.wdg,row=1,column=1,sticky="w")
  tkgrid(Env$l.fr1$niveau.lab,row=2,column=0,sticky="e")
  tkgrid(Env$l.fr1$niveau.wdg,row=2,column=1,sticky="w")
  tkgrid(Env$l.fr1$espace.hor,row=0,column=2)
  tkgrid(Env$l.fr1$type.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr1$type.wdg,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr1$encadre.lab,row=1,column=3,sticky="e")
  tkgrid(Env$l.fr1$encadre.wdg,row=1,column=4,sticky="w")
}


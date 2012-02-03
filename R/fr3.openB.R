fr3.openB <-
function() {
  Env$l.frames$Fr3.status<-1
  tkconfigure(Env$l.wdg$but.lab3,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr3)) {tkdestroy(Env$l.fr3[[i]])}
  Env$l.fr3<-list()
  Env$l.fr3$grad.lab1<-tklabel(Env$l.frames$Fr3,text=Env$voc[47,1],font=Env$police)
  Env$l.fr3$grad.col<-tkbutton(Env$l.frames$Fr3,text="Aa",font=Env$police3,foreground=tclvalue(Env$l.var$graduations.col),activeforeground=tclvalue(Env$l.var$graduations.col),command=function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$graduations.col),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$graduations.col)<-temp
	tkconfigure(Env$l.fr3$grad.col,foreground=temp,activeforeground=temp)
    }
  })
  Env$l.fr3$grad.lab2<-tklabel(Env$l.frames$Fr3,text=Env$voc[48,1],font=Env$police)
  Env$l.fr3$grad.taille<-tkscale(Env$l.frames$Fr3,from=0.5,to=4,showvalue=TRUE,font=Env$police,variable=Env$l.var$graduations.taille,resolution=0.1,orient="horizontal")
  Env$l.fr3$grad.lab3<-tklabel(Env$l.frames$Fr3,text=Env$voc[245,1],font=Env$police)
  Env$l.fr3$grad.orient<-ttkcombobox(Env$l.frames$Fr3,width=16,values=c(Env$voc[246,1],Env$voc[67,1]),textvariable=Env$l.var$graduations.orient,font=Env$police,state="readonly")
  Env$l.fr3$leg.lab1<-tklabel(Env$l.frames$Fr3,text=Env$voc[49,1],font=Env$police)
  Env$l.fr3$leg.col<-tkbutton(Env$l.frames$Fr3,text="Aa",font=Env$police3,foreground=tclvalue(Env$l.var$legendes.col),activeforeground=tclvalue(Env$l.var$legendes.col),command=function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$legendes.col),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$legendes.col)<-temp
	tkconfigure(Env$l.fr3$leg.col,foreground=temp,activeforeground=temp)
    }
  })
  Env$l.fr3$leg.lab2<-tklabel(Env$l.frames$Fr3,text=Env$voc[50,1],font=Env$police)
  Env$l.fr3$leg.taille<-tkscale(Env$l.frames$Fr3,from=0.5,to=4,showvalue=TRUE,font=Env$police,variable=Env$l.var$legendes.taille,resolution=0.1,orient="horizontal")
  Env$l.fr3$titre1<-tklabel(Env$l.frames$Fr3,text=Env$voc[51,1],font=Env$police3)
  Env$l.fr3$noms.titre.lab<-tklabel(Env$l.frames$Fr3,text=Env$voc[44,1],font=Env$police)
  Env$l.fr3$noms.titre.wdg<-tkentry(Env$l.frames$Fr3,width=20,font=Env$police,textvariable=Env$l.var$titre.axehor)
  Env$l.fr3$noms.lab1<-tklabel(Env$l.frames$Fr3,text=Env$voc[89,1],font=Env$police)
  Env$l.fr3$noms.list<-tklistbox(Env$l.frames$Fr3,height=4,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr3$noms.scroll,...))
  Env$l.fr3$noms.scroll<-tkscrollbar(Env$l.frames$Fr3,repeatinterval=4,command=function(...) tkyview(Env$l.fr3$noms.list,...))
  if (tclvalue(Env$l.var$moyprop)=="moy") {
    for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$noms1[i])}
  } else {
    for (i in 1:length(Env$l.var$nomsprop.fac)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$nomsprop.fac[i])}
  }
  tkbind(Env$l.fr3$noms.list,"<ButtonRelease-1>",function() {
    tkdelete(Env$l.fr3$noms.wdg,0,"end")
    if (tclvalue(Env$l.var$moyprop)=="moy") {
	tkinsert(Env$l.fr3$noms.wdg,"end",Env$l.var$noms1[as.numeric(tclvalue(tkcurselection(Env$l.fr3$noms.list)))+1])
    } else {
	tkinsert(Env$l.fr3$noms.wdg,"end",Env$l.var$nomsprop.fac[as.numeric(tclvalue(tkcurselection(Env$l.fr3$noms.list)))+1])
    }
  })
  Env$l.fr3$noms.lab2<-tklabel(Env$l.frames$Fr3,text=Env$voc[90,1],font=Env$police)
  Env$l.fr3$noms.wdg<-tkentry(Env$l.frames$Fr3,width=20,font=Env$police)
  tkbind(Env$l.fr3$noms.wdg,"<Enter>",function() {msg(text=Env$voc[26,1],type="info")})
  tkbind(Env$l.fr3$noms.wdg,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr3$noms.wdg,"<ButtonRelease-1>",function() {tkdelete(Env$l.fr3$noms.wdg,0,"end")})
  tkbind(Env$l.fr3$noms.wdg,"<Return>",function() {
    if (tclvalue(Env$l.var$moyprop)=="moy") {
	rename.noms1(value.list=tclvalue(tkcurselection(Env$l.fr3$noms.list)),value.nom=tclvalue(tkget(Env$l.fr3$noms.wdg)))
    } else {
	rename.nomsprop.fac(value.list=tclvalue(tkcurselection(Env$l.fr3$noms.list)),value.nom=tclvalue(tkget(Env$l.fr3$noms.wdg)))
    }
  })
  Env$l.fr3$titre2<-tklabel(Env$l.frames$Fr3,text=Env$voc[52,1],font=Env$police3)
  Env$l.fr3$valeurs.titre.lab<-tklabel(Env$l.frames$Fr3,text=Env$voc[44,1],font=Env$police)
  Env$l.fr3$valeurs.titre.wdg<-tkentry(Env$l.frames$Fr3,width=20,font=Env$police,textvariable=Env$l.var$titre.axever)
  Env$l.fr3$valeurs.liminf.lab<-tklabel(Env$l.frames$Fr3,text=Env$voc[53,1],font=Env$police)
  Env$l.fr3$valeurs.liminf.wdg<-tkentry(Env$l.frames$Fr3,width=5,font=Env$police,textvariable=Env$l.var$liminf.axever)
  tkbind(Env$l.fr3$valeurs.liminf.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr3$valeurs.liminf.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr3$valeurs.limsup.lab<-tklabel(Env$l.frames$Fr3,text=Env$voc[54,1],font=Env$police)
  Env$l.fr3$valeurs.limsup.wdg<-tkentry(Env$l.frames$Fr3,width=5,font=Env$police,textvariable=Env$l.var$limsup.axever)
  tkbind(Env$l.fr3$valeurs.limsup.wdg,"<Enter>",function() {msg(text=Env$voc[155,1],type="warning")})
  tkbind(Env$l.fr3$valeurs.limsup.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr3$valeurs.log.lab<-tklabel(Env$l.frames$Fr3,text=Env$voc[73,1],font=Env$police)
  Env$l.fr3$valeurs.log.wdg<-tkcheckbutton(Env$l.frames$Fr3,variable=Env$l.var$log.axever)
  tkbind(Env$l.fr3$valeurs.log.wdg,"<Enter>",function() {msg(text=Env$voc[158,1],type="warning")})
  tkbind(Env$l.fr3$valeurs.log.wdg,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr3$espace.ver1<-tklabel(Env$l.frames$Fr3,text="",font=Env$police2)
  Env$l.fr3$espace.ver2<-tklabel(Env$l.frames$Fr3,text="",font=Env$police2)
  Env$l.fr3$espace.hor<-tklabel(Env$l.frames$Fr3,text="                                   ",font=Env$police)
  tkgrid(Env$l.fr3$grad.lab1,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr3$grad.col,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr3$espace.hor,row=0,column=2)
  tkgrid(Env$l.fr3$grad.lab2,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr3$grad.taille,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr3$grad.lab3,row=1,column=0,sticky="e")
  tkgrid(Env$l.fr3$grad.orient,row=1,column=1,sticky="w")
  tkgrid(Env$l.fr3$leg.lab1,row=2,column=0,sticky="e")
  tkgrid(Env$l.fr3$leg.col,row=2,column=1,sticky="w")
  tkgrid(Env$l.fr3$leg.lab2,row=2,column=3,sticky="e")
  tkgrid(Env$l.fr3$leg.taille,row=2,column=4,sticky="w")
  tkgrid(Env$l.fr3$espace.ver1)
  tkgrid(Env$l.fr3$titre1,row=4,column=0,columnspan=2)
  tkgrid(Env$l.fr3$espace.ver2)
  tkgrid(Env$l.fr3$noms.titre.lab,row=6,column=0,sticky="e")
  tkgrid(Env$l.fr3$noms.titre.wdg,row=6,column=1,sticky="w")
  tkgrid(Env$l.fr3$noms.lab1,row=7,column=0,sticky="e")
  tkgrid(Env$l.fr3$noms.list,Env$l.fr3$noms.scroll,row=7,column=1,rowspan=4);tkgrid.configure(Env$l.fr3$noms.scroll,sticky="ens")
  tkgrid(Env$l.fr3$noms.lab2,row=11,column=0,sticky="e")
  tkgrid(Env$l.fr3$noms.wdg,row=11,column=1,sticky="w")
  tkgrid(Env$l.fr3$titre2,row=4,column=3,columnspan=2)
  tkgrid(Env$l.fr3$valeurs.titre.lab,row=6,column=3,sticky="e")
  tkgrid(Env$l.fr3$valeurs.titre.wdg,row=6,column=4,sticky="w")
  tkgrid(Env$l.fr3$valeurs.liminf.lab,row=7,column=3,sticky="e")
  tkgrid(Env$l.fr3$valeurs.liminf.wdg,row=7,column=4,sticky="w")
  tkgrid(Env$l.fr3$valeurs.limsup.lab,row=8,column=3,sticky="e")
  tkgrid(Env$l.fr3$valeurs.limsup.wdg,row=8,column=4,sticky="w")
  tkgrid(Env$l.fr3$valeurs.log.lab,row=9,column=3,sticky="e")
  tkgrid(Env$l.fr3$valeurs.log.wdg,row=9,column=4,sticky="w")
}

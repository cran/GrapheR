fr4.openD <-
function() {
  Env$l.frames$Fr4.status<-1
  tkconfigure(Env$l.wdg$but.lab4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr4)) {tkdestroy(Env$l.fr4[[i]])}
  Env$l.fr4<-list()
  type.var<-"unknown"
  Env$l.fr4$titre1<-tklabel(Env$l.frames$Fr4,text=Env$voc[15,1],font=Env$police3)
  Env$l.fr4$var.list<-tklistbox(Env$l.frames$Fr4,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr3$var.scroll,...))
  Env$l.fr4$var.scroll<-tkscrollbar(Env$l.frames$Fr4,repeatinterval=5,command=function(...) tkyview(Env$l.fr4$var.list,...))
  tkbind(Env$l.fr4$var.list,"<Enter>",function() {msg(text=Env$voc[34,1],type="warning")})
  tkbind(Env$l.fr4$var.list,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr4$var.list,"<ButtonRelease-1>",function() {
    type.var<-class(Env$dataset[,as.numeric(tclvalue(tkcurselection(Env$l.fr4$var.list)))+1])
    if (type.var=="numeric" | type.var=="integer") {
	tkconfigure(Env$l.fr4$rb.noregroup,state="normal",text=paste(Env$voc[27,1],nlevels(as.factor(Env$dataset[,as.numeric(tclvalue(tkcurselection(Env$l.fr4$var.list)))+1])),Env$voc[28,1],sep=""))
	tkconfigure(Env$l.fr4$rb.regroup1,state="normal")
	tkconfigure(Env$l.fr4$but,state="normal")
    } else if (type.var=="character") {
	tkconfigure(Env$l.fr4$rb.noregroup,state="disabled")
	tkconfigure(Env$l.fr4$rb.regroup1,state="disabled")
	tkconfigure(Env$l.fr4$rb.regroup2,state="disabled")
	tkconfigure(Env$l.fr4$rb.regroup3,state="disabled")
	tkconfigure(Env$l.fr4$curs.wdg,state="disabled",foreground="grey")
	tkconfigure(Env$l.fr4$curs.lab,foreground="grey")
	tkconfigure(Env$l.fr4$but,state="normal")
    } else {
	tkconfigure(Env$l.fr4$rb.noregroup,state="disabled")
	tkconfigure(Env$l.fr4$rb.regroup1,state="disabled")
	tkconfigure(Env$l.fr4$rb.regroup2,state="disabled")
	tkconfigure(Env$l.fr4$rb.regroup3,state="disabled")
	tkconfigure(Env$l.fr4$curs.wdg,state="disabled",foreground="grey")
	tkconfigure(Env$l.fr4$curs.lab,foreground="grey")
	tkconfigure(Env$l.fr4$but,state="disabled")
    }
  })
  Env$l.fr4$rb.noregroup<-tkradiobutton(Env$l.frames$Fr4,font=Env$police,variable=Env$l.var$regroup1,value=0,text=paste(Env$voc[27,1],0,Env$voc[28,1],sep=""),command=function() {
    tkconfigure(Env$l.fr4$rb.regroup2,state="disabled")
    tkconfigure(Env$l.fr4$rb.regroup3,state="disabled")
    tkconfigure(Env$l.fr4$curs.wdg,state="disabled",foreground="grey")
    tkconfigure(Env$l.fr4$curs.lab,foreground="grey")
  },state="disabled")
  Env$l.fr4$rb.regroup1<-tkradiobutton(Env$l.frames$Fr4,font=Env$police,variable=Env$l.var$regroup1,value=1,text=Env$voc[29,1],command=function() {
    tkconfigure(Env$l.fr4$rb.regroup2,state="normal")
    tkconfigure(Env$l.fr4$rb.regroup3,state="normal")
    tkconfigure(Env$l.fr4$curs.wdg,state="normal",foreground="black")
    tkconfigure(Env$l.fr4$curs.lab,foreground="black")
  },state="disabled")
  Env$l.fr4$rb.regroup2<-tkradiobutton(Env$l.frames$Fr4,font=Env$police,variable=Env$l.var$regroup2,value="long",text=Env$voc[30,1],state="disabled")
  Env$l.fr4$rb.regroup3<-tkradiobutton(Env$l.frames$Fr4,font=Env$police,variable=Env$l.var$regroup2,value="eff",text=Env$voc[31,1],state="disabled")
  Env$l.fr4$curs.wdg<-tkscale(Env$l.frames$Fr4,from=2,to=20,showvalue=TRUE,font=Env$police,variable=Env$l.var$regroup3,resolution=1,orient="horizontal",state="disabled",foreground="grey")
  Env$l.fr4$curs.lab<-tklabel(Env$l.frames$Fr4,text=Env$voc[32,1],foreground="grey")
  Env$l.fr4$but<-tkbutton(Env$l.frames$Fr4,text=Env$voc[33,1],width=16,state="disabled",command=function() {convert.variable(type.var)})
  if(!is.null(Env$dataset)) {
    for (i in 1:ncol(Env$dataset)) {tkinsert(Env$l.fr4$var.list,"end",colnames(Env$dataset)[i])}
  }
  Env$l.fr4$espace.ver<-tklabel(Env$l.frames$Fr4,text="",font=Env$police2)
  Env$l.fr4$espace.hor1<-tklabel(Env$l.frames$Fr4,text="                    ",font=Env$police)
  Env$l.fr4$espace.hor2<-tklabel(Env$l.frames$Fr4,text="               ",font=Env$police)
  tkgrid(Env$l.fr4$titre1,row=0,column=0)
  tkgrid(Env$l.fr4$espace.ver)
  tkgrid(Env$l.fr4$var.list,Env$l.fr4$var.scroll,row=2,column=0,rowspan=5);tkgrid.configure(Env$l.fr4$var.scroll,sticky="ens")
  tkgrid(Env$l.fr4$espace.hor1,row=2,column=1)
  tkgrid(Env$l.fr4$rb.noregroup,row=2,column=2,columnspan=2,sticky="w")
  tkgrid(Env$l.fr4$rb.regroup1,row=3,column=2,columnspan=2,sticky="w")
  tkgrid(Env$l.fr4$rb.regroup2,row=4,column=3,sticky="w")
  tkgrid(Env$l.fr4$rb.regroup3,row=5,column=3,sticky="w")
  tkgrid(Env$l.fr4$curs.wdg,row=4,column=4)
  tkgrid(Env$l.fr4$curs.lab,row=5,column=4)
  tkgrid(Env$l.fr4$espace.hor2,row=2,column=5)
  tkgrid(Env$l.fr4$but,row=2,column=6,rowspan=2)
}


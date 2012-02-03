fr1.openD <-
function() {
  Env$l.frames$Fr1.status<-1
  tkconfigure(Env$l.wdg$but.lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr1)) {tkdestroy(Env$l.fr1[[i]])}
  Env$l.fr1<-list()
  Env$l.fr1$titre1<-tklabel(Env$l.frames$Fr1,text=Env$voc[2,1],font=Env$police3)
  Env$l.fr1$ext.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[3,1],font=Env$police)
  Env$l.fr1$ext.wdg<-ttkcombobox(Env$l.frames$Fr1,values=c("txt","csv"),textvariable=Env$l.var$extension,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$ext.wdg,"<<ComboboxSelected>>",function() {
    if (tclvalue(Env$l.var$extension)=="txt") {tclvalue(Env$l.var$sepcol)<-Env$voc[5,1]} else
	if (tclvalue(Env$l.var$extension)=="csv") {tclvalue(Env$l.var$sepcol)<-Env$voc[6,1]}
  })
  Env$l.fr1$col.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[4,1],font=Env$police)
  Env$l.fr1$col.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$voc[5:7,1],textvariable=Env$l.var$sepcol,font=Env$police,state="readonly")
  Env$l.fr1$dec.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[8,1],font=Env$police)
  Env$l.fr1$dec.wdg<-ttkcombobox(Env$l.frames$Fr1,values=Env$voc[c(9,6),1],textvariable=Env$l.var$sepdec,font=Env$police,state="readonly")
  Env$l.fr1$na.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[10,1],font=Env$police)
  Env$l.fr1$na.wdg<-tkentry(Env$l.frames$Fr1,width=4,textvariable=Env$l.var$na,font=Env$police)
  Env$l.fr1$hdr.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[11,1],font=Env$police)
  Env$l.fr1$hdr.wdg<-tkcheckbutton(Env$l.frames$Fr1,variable=Env$l.var$header)
  Env$l.fr1$but1<-tkbutton(Env$l.frames$Fr1,text=Env$voc[13,1],font=Env$police,width=16,command=data.load1)
  Env$l.fr1$titre2<-tklabel(Env$l.frames$Fr1,text=Env$voc[12,1],font=Env$police3)
  Env$l.fr1$obj.list<-tklistbox(Env$l.frames$Fr1,height=7,font=Env$police,selectmode="single",yscrollcommand=function(...) tkset(Env$l.fr1$obj.scroll,...))
  Env$l.fr1$obj.scroll<-tkscrollbar(Env$l.frames$Fr1,repeatinterval=5,command=function(...) tkyview(Env$l.fr1$obj.list,...))
  tables<-NULL
  if (length(ls(.GlobalEnv))>0) {
    for (i in 1:length(ls(.GlobalEnv))) {
	if (is.data.frame(get(ls(.GlobalEnv)[i]))) {tables<-c(tables,ls(.GlobalEnv)[i])}
    }
  }
  tkdelete(Env$l.fr1$obj.list,0,"end")
  if (!is.null(tables)) {
    for (i in 1:length(tables)) {tkinsert(Env$l.fr1$obj.list,"end",tables[i])}
  } else {}
  tkbind(Env$l.fr1$obj.list,"<Enter>",function() {msg(text=Env$voc[14,1],type="info")})
  tkbind(Env$l.fr1$obj.list,"<Leave>",function() {msg(text="",type="info")})
  Env$l.fr1$but2<-tkbutton(Env$l.frames$Fr1,text=Env$voc[13,1],font=Env$police,width=16,command=data.load2)
  Env$l.fr1$espace.ver1<-tklabel(Env$l.frames$Fr1,text="",font=Env$police2)
  Env$l.fr1$espace.ver2<-tklabel(Env$l.frames$Fr1,text="",font=Env$police2)
  Env$l.fr1$espace.hor<-tklabel(Env$l.frames$Fr1,text="                                                  ",font=Env$police)
  tkgrid(Env$l.fr1$titre1,row=0,column=0,columnspan=2)
  tkgrid(Env$l.fr1$espace.ver1,row=1,column=0)
  tkgrid(Env$l.fr1$ext.lab,row=2,column=0,sticky="e")
  tkgrid(Env$l.fr1$ext.wdg,row=2,column=1,sticky="w")
  tkgrid(Env$l.fr1$col.lab,row=3,column=0,sticky="e")
  tkgrid(Env$l.fr1$col.wdg,row=3,column=1,sticky="w")
  tkgrid(Env$l.fr1$dec.lab,row=4,column=0,sticky="e")
  tkgrid(Env$l.fr1$dec.wdg,row=4,column=1,sticky="w")
  tkgrid(Env$l.fr1$na.lab,row=5,column=0,sticky="e")
  tkgrid(Env$l.fr1$na.wdg,row=5,column=1,sticky="w")
  tkgrid(Env$l.fr1$hdr.lab,row=6,column=0,sticky="e")
  tkgrid(Env$l.fr1$hdr.wdg,row=6,column=1,sticky="w")
  tkgrid(Env$l.fr1$espace.ver2,row=7,column=0)
  tkgrid(Env$l.fr1$but1,row=8,column=0,columnspan=2)
  tkgrid(Env$l.fr1$espace.hor,row=0,column=3)
  tkgrid(Env$l.fr1$titre2,row=0,column=4)
  tkgrid(Env$l.fr1$obj.list,Env$l.fr1$obj.scroll,row=2,column=4,rowspan=5);tkgrid.configure(Env$l.fr1$obj.scroll,sticky="ens")
  tkgrid(Env$l.fr1$but2,row=8,column=4)
}

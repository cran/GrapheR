fr1.openCa <-
function() {
  Env$l.frames$Fr1.status<-1
  tkconfigure(Env$l.wdg$but.lab1,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr1)) {tkdestroy(Env$l.fr1[[i]])}
  Env$l.fr1<-list()
  Env$l.fr1$var.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[36,1],font=Env$police)
  Env$l.fr1$var.wdg<-ttkcombobox(Env$l.frames$Fr1,values=c(Env$l.var$var.num,Env$l.var$var.fact),textvariable=Env$l.var$variable,font=Env$police,state="readonly")
  tkbind(Env$l.fr1$var.wdg,"<<ComboboxSelected>>",function() {
    if (is.factor(Env$dataset[,tclvalue(Env$l.var$variable)])) {
	Env$l.var$noms1<-levels(Env$dataset[,tclvalue(Env$l.var$variable)])
    } else {
	Env$l.var$noms1<-levels(factor(Env$dataset[,tclvalue(Env$l.var$variable)]))
    }
    tclvalue(Env$l.var$plusieurs)<-1
    Env$l.var$nomsparts<-Env$l.var$noms1
    Env$l.var$couleur1B<-grey.colors(length(Env$l.var$nomsparts))
    Env$l.var$hachuresB<-rep(1,length(Env$l.var$nomsparts))
    tclvalue(Env$l.var$parts.niveaux)<-paste(0:(length(Env$l.var$noms1)-1),collapse=" ")
    tkdelete(Env$l.fr1$parts.list,0,"end")
    for (i in 1:length(Env$l.var$noms1)) {tkinsert(Env$l.fr1$parts.list,"end",Env$l.var$noms1[i])}
    if (exists("noms.list",where=Env$l.fr3)) {
	tkdelete(Env$l.fr3$noms.list,0,"end")
	for (i in 1:length(Env$l.var$nomsparts)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$nomsparts[i])}
	tkdelete(Env$l.fr3$noms.wdg,0,"end")
    }
    if (exists("noms.list",where=Env$l.fr4)) {
	tkdelete(Env$l.fr4$noms.list,0,"end")
	for (i in 1:length(Env$l.var$nomsparts)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$nomsparts[i])}
	tkconfigure(Env$l.fr4$colparts.wdg,bg=Env$l.var$couleur1B[1])
	for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
	tkconfigure(Env$l.fr4$l.hachures[[Env$l.var$hachuresB[1]]],borderwidth=2)
    }
  })
  Env$l.fr1$parts.lab<-tklabel(Env$l.frames$Fr1,text=Env$voc[115,1],font=Env$police)
  Env$l.fr1$parts.list<-tklistbox(Env$l.frames$Fr1,height=7,font=Env$police,selectmode="multiple",yscrollcommand=function(...) tkset(Env$l.fr1$parts.scroll,...))
  Env$l.fr1$parts.scroll<-tkscrollbar(Env$l.frames$Fr1,repeatinterval=5,command=function(...) tkyview(Env$l.fr1$parts.list,...))
  tkbind(Env$l.fr1$parts.list,"<Enter>",function() {msg(text=Env$voc[159,1],type="info")})
  tkbind(Env$l.fr1$parts.list,"<Leave>",function() {msg(text="",type="info")})
  tkbind(Env$l.fr1$parts.list,"<ButtonRelease-1>",function() {
    tclvalue(Env$l.var$parts.niveaux)<-tclvalue(tkcurselection(Env$l.fr1$parts.list))
    if (nchar(tclvalue(tkcurselection(Env$l.fr1$parts.list)))>2) {
	tclvalue(Env$l.var$plusieurs)<-1
    } else {
	tclvalue(Env$l.var$plusieurs)<-0
    }
    Env$l.var$nomsparts<-Env$l.var$noms1[as.numeric(strsplit(tclvalue(Env$l.var$parts.niveaux),split=" ")[[1]])+1]
    Env$l.var$couleur1B<-grey.colors(length(Env$l.var$nomsparts))
    Env$l.var$hachuresB<-rep(1,length(Env$l.var$nomsparts))
    if (exists("noms.list",where=Env$l.fr3)) {
	tkdelete(Env$l.fr3$noms.list,0,"end")
	for (i in 1:length(Env$l.var$nomsparts)) {tkinsert(Env$l.fr3$noms.list,"end",Env$l.var$nomsparts[i])}
	tkdelete(Env$l.fr3$noms.wdg,0,"end")
    }
    if (exists("noms.list",where=Env$l.fr4)) {
	tkdelete(Env$l.fr4$noms.list,0,"end")
	for (i in 1:length(Env$l.var$nomsparts)) {tkinsert(Env$l.fr4$noms.list,"end",Env$l.var$nomsparts[i])}
	tkconfigure(Env$l.fr4$colparts.wdg,bg=Env$l.var$couleur1B[1])
	for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
	tkconfigure(Env$l.fr4$l.hachures[[Env$l.var$hachuresB[1]]],borderwidth=2)
    }
  })
  Env$l.fr1$espace.hor<-tklabel(Env$l.frames$Fr1,text="                                        ",font=Env$police)
  tkgrid(Env$l.fr1$var.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr1$var.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr1$espace.hor,row=0,column=2)
  tkgrid(Env$l.fr1$parts.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr1$parts.list,Env$l.fr1$parts.scroll,row=0,column=4,rowspan=7,sticky="w");tkgrid.configure(Env$l.fr1$parts.scroll,sticky="ens")
}


fr5.openD <-
function() {
  Env$l.frames$Fr5.status<-1
  tkconfigure(Env$l.wdg$but.lab5,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr5)) {tkdestroy(Env$l.fr5[[i]])}
  Env$l.fr5<-list()
  Env$l.var$levels.temp <- NULL
  Env$l.fr5$fact.lab <- tklabel(Env$l.frames$Fr5,text=Env$voc[249,1],font=Env$police)
  Env$l.fr5$fact.wdg <- ttkcombobox(Env$l.frames$Fr5,values=Env$l.var$var.fact,textvariable=Env$l.var$facteur1,state="readonly")
  tkbind(Env$l.fr5$fact.wdg,"<<ComboboxSelected>>",function() {
    tkdelete(Env$l.fr5$liste.actual,0,"end")
    for (i in 1:nlevels(Env$dataset[,tclvalue(Env$l.var$facteur1)])) {
	tkinsert(Env$l.fr5$liste.actual,"end",levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[i])
    }
    tkdelete(Env$l.fr5$liste.new,0,"end")
  })
  Env$l.fr5$titre1 <- tklabel(Env$l.frames$Fr5,text=Env$voc[250,1],font=Env$police3)
  Env$l.fr5$liste.actual <- tklistbox(Env$l.frames$Fr5,selectmode="single",height=5)
  Env$l.fr5$titre2 <- tklabel(Env$l.frames$Fr5,text=Env$voc[251,1],font=Env$police3)
  Env$l.fr5$liste.new <- tklistbox(Env$l.frames$Fr5,selectmode="single",height=5)
  Env$l.fr5$but1 <- tkbutton(Env$l.frames$Fr5,text=">",width=5,command=function() {
    if (tclvalue(tkcurselection(Env$l.fr5$liste.actual))!="") {
	selection <- as.numeric(tclvalue(tkcurselection(Env$l.fr5$liste.actual)))+1
	tkinsert(Env$l.fr5$liste.new,"end",levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[selection])
	Env$l.var$levels.temp <- c(Env$l.var$levels.temp,levels(Env$dataset[,tclvalue(Env$l.var$facteur1)])[selection])
    }
  })
  Env$l.fr5$but2 <- tkbutton(Env$l.frames$Fr5,text="<",width=5,command=function() {
    if (tclvalue(tkcurselection(Env$l.fr5$liste.new))!="") {
	selection <- tclvalue(tkcurselection(Env$l.fr5$liste.new))
	tkdelete(Env$l.fr5$liste.new,selection)
	Env$l.var$levels.temp <- Env$l.var$levels.temp[-(as.numeric(selection)+1)]
    }
  })
  Env$l.fr5$but3 <- tkbutton(Env$l.frames$Fr5,text=Env$voc[252,1],width=16,command=function() {
    if (length(Env$l.var$levels.temp)==nlevels(Env$dataset[,tclvalue(Env$l.var$facteur1)])) {
	Env$dataset[,tclvalue(Env$l.var$facteur1)] <- factor(Env$dataset[,tclvalue(Env$l.var$facteur1)],levels=Env$l.var$levels.temp)
	msg(text=Env$voc[254,1],type="info")
    } else {
	msg(text=Env$voc[253,1],type="error")
    }
  })
  Env$l.fr5$espace.hor1 <- tklabel(Env$l.frames$Fr5,text="          ",font=Env$police)
  Env$l.fr5$espace.hor2 <- tklabel(Env$l.frames$Fr5,text="          ",font=Env$police)
  tkgrid(Env$l.fr5$fact.lab,row=1,column=0,sticky="ne")
  tkgrid(Env$l.fr5$fact.wdg,row=1,column=1,sticky="nw")
  tkgrid(Env$l.fr5$espace.hor1,row=1,column=2)
  tkgrid(Env$l.fr5$titre1,row=0,column=3)
  tkgrid(Env$l.fr5$liste.actual,row=1,column=3,rowspan=2)
  tkgrid(Env$l.fr5$but1,row=1,column=4)
  tkgrid(Env$l.fr5$but2,row=2,column=4)
  tkgrid(Env$l.fr5$titre2,row=0,column=5)
  tkgrid(Env$l.fr5$liste.new,row=1,column=5,rowspan=2)
  tkgrid(Env$l.fr5$espace.hor2,row=1,column=6)
  tkgrid(Env$l.fr5$but3,row=1,column=7,sticky="n")
}

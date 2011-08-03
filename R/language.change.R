language.change <-
function() {
  fr7.close()
  for (i in 1:length(Env$l.fr7)) {tkdestroy(Env$l.fr7[[i]])}
  Env$l.fr7<-list()
  tkconfigure(Env$l.frames$Fr7,borderwidth=3)
  lang.var<-tclVar("")
  save.var<-tclVar("1")
  Env$l.fr7$lang.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[216,1])
  Env$l.fr7$lang.wdg<-ttkcombobox(Env$l.frames$Fr7,width=10,font=Env$police,values=Env$voc[217:218,1],textvariable=lang.var,state="readonly")
  Env$l.fr7$save.lab<-tklabel(Env$l.frames$Fr7,text=Env$voc[219,1])
  Env$l.fr7$save.wdg<-tkcheckbutton(Env$l.frames$Fr7,variable=save.var)
  Env$l.fr7$ok<-tkbutton(Env$l.frames$Fr7,text=Env$voc[151,1],font=Env$police,width=16,command=function() {
    if (nchar(tclvalue(lang.var))>0) {
	langue<-NULL
	if (tclvalue(lang.var)==Env$voc[217,1]) {langue<-"en"} else
	if (tclvalue(lang.var)==Env$voc[218,1]) {langue<-"fr"}
	if (tclvalue(save.var)==1) {
	  write(langue,file=file.path(.path.package("GrapheR"),"lang","Language.txt",fsep=.Platform$file.sep))
	}
	fermer.GrapheR()
	run.GrapheR(lang=langue)
    } else {
	msg(text=Env$voc[220,1],type="error")
    }
  })
  Env$l.fr7$fermer<-tkbutton(Env$l.frames$Fr7,text=Env$voc[152,1],font=Env$police,width=16,command=fr7.close)
  Env$l.fr7$espace.ver1<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver2<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.ver3<-tklabel(Env$l.frames$Fr7,text="",font=Env$police2)
  Env$l.fr7$espace.hor1<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  Env$l.fr7$espace.hor2<-tklabel(Env$l.frames$Fr7,text="     ",font=Env$police3)
  tkgrid(Env$l.fr7$espace.ver1)
  tkgrid(Env$l.fr7$espace.hor1,row=1,column=0)
  tkgrid(Env$l.fr7$lang.lab,row=1,column=1,sticky="e")
  tkgrid(Env$l.fr7$lang.wdg,row=1,column=2,sticky="w")
  tkgrid(Env$l.fr7$espace.hor2,row=1,column=3)
  tkgrid(Env$l.fr7$save.lab,row=2,column=1,sticky="e")
  tkgrid(Env$l.fr7$save.wdg,row=2,column=2,sticky="w")
  tkgrid(Env$l.fr7$espace.ver2)
  tkgrid(Env$l.fr7$ok,row=4,column=1)
  tkgrid(Env$l.fr7$fermer,row=4,column=2)
  tkgrid(Env$l.fr7$espace.ver3)
}


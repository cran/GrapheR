fr6.openM <-
function() {
  Env$l.frames$Fr6.status<-1
  tkconfigure(Env$l.wdg$but.lab6,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fleche_haut.gif",fsep=.Platform$file.sep)))
  for (i in 1:length(Env$l.fr6)) {tkdestroy(Env$l.fr6[[i]])}
  Env$l.fr6<-list()
  Env$l.fr6$outliers.lab<-tklabel(Env$l.frames$Fr6,text=Env$voc[78,1],font=Env$police)
  Env$l.fr6$outliers.wdg<-tkcheckbutton(Env$l.frames$Fr6,variable=Env$l.var$outliers)
  Env$l.fr6$col.lab<-tklabel(Env$l.frames$Fr6,text=Env$voc[45,1],font=Env$police)
  Env$l.fr6$col.wdg<-tkcanvas(Env$l.frames$Fr6,width="25",height="20",bg=tclvalue(Env$l.var$couleur3))
  tkbind(Env$l.fr6$col.wdg,"<ButtonRelease-1>",function() {
    temp<-tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(Env$l.var$couleur3),title=Env$voc[64,1]))
    if (nchar(temp)>0) {
	tclvalue(Env$l.var$couleur3)<-temp
	tkconfigure(Env$l.fr6$col.wdg,bg=temp)
    }
  })
  Env$l.fr6$symbole.lab<-tklabel(Env$l.frames$Fr6,text=Env$voc[79,1],font=Env$police)
  Env$l.fr6$symbole.wdg<-ttkcombobox(Env$l.frames$Fr6,font=Env$police,values=Env$voc[80:81,1],textvariable=Env$l.var$box.symbol,state="readonly",width=15)
  Env$l.fr6$espace.hor1<-tklabel(Env$l.frames$Fr6,text="                                   ",font=Env$police)
  Env$l.fr6$espace.hor2<-tklabel(Env$l.frames$Fr6,text="                                        ",font=Env$police)
  tkgrid(Env$l.fr6$outliers.lab,row=0,column=0,sticky="e")
  tkgrid(Env$l.fr6$outliers.wdg,row=0,column=1,sticky="w")
  tkgrid(Env$l.fr6$espace.hor1,row=0,column=2)
  tkgrid(Env$l.fr6$col.lab,row=0,column=3,sticky="e")
  tkgrid(Env$l.fr6$col.wdg,row=0,column=4,sticky="w")
  tkgrid(Env$l.fr6$espace.hor2,row=0,column=5)
  tkgrid(Env$l.fr6$symbole.lab,row=0,column=6,sticky="e")
  tkgrid(Env$l.fr6$symbole.wdg,row=0,column=7,sticky="w")
}


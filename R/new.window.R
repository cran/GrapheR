new.window <-
function() {
  New=tktoplevel()
  tkwm.resizable(New,FALSE,FALSE)
  tktitle(New)="GrapheR"
  tkgrab.set(New)
  tkfocus(New)
  col.fond=tclVar("white")
  col.fond.choose=tkcanvas(New,width="40",height="25",bg=tclvalue(col.fond))
  tkbind(col.fond.choose,"<ButtonRelease-1>",function() {couleur(fen=New,titre=230,var=col.fond,widg=col.fond.choose,type="can",plusieurs=FALSE)})
  lignes=tclVar("1")
  lignes.choose=tkscale(New,from=1,to=4,showvalue=TRUE,font=tkfont.create(family="Arial",size=12),variable=lignes,length=200,resolution=1,orient="vertical",command=function(...) {
    tkconfigure(fenetre,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Fenetre",tclvalue(lignes),"-",tclvalue(colonnes),".gif",sep=""),fsep=.Platform$file.sep)))
  })
  colonnes=tclVar("1")
  colonnes.choose=tkscale(New,from=1,to=4,showvalue=TRUE,font=tkfont.create(family="Arial",size=12),variable=colonnes,length=200,resolution=1,orient="horizontal",command=function(...) {
    tkconfigure(fenetre,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",paste("Fenetre",tclvalue(lignes),"-",tclvalue(colonnes),".gif",sep=""),fsep=.Platform$file.sep)))
  })
  fenetre=tklabel(New,height=200,width=200,font=Env$police,borderwidth=0,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Fenetre1-1.gif",fsep=.Platform$file.sep)))
  ok=tkbutton(New,width=16,text=Env$vocab[1,1],font=Env$police,command=function() {
    dimensions=if (tclvalue(lignes)=="1" & tclvalue(colonnes)=="1") {c(7,7)} else
      if (tclvalue(lignes)=="2" & tclvalue(colonnes)=="1") {c(6,12)} else
      if (tclvalue(lignes)=="3" & tclvalue(colonnes)=="1") {c(5,15)} else
      if (tclvalue(lignes)=="4" & tclvalue(colonnes)=="1") {c(4,16)} else
      if (tclvalue(lignes)=="1" & tclvalue(colonnes)=="2") {c(12,6)} else
      if (tclvalue(lignes)=="2" & tclvalue(colonnes)=="2") {c(10,10)} else
      if (tclvalue(lignes)=="3" & tclvalue(colonnes)=="2") {c(10,15)} else
      if (tclvalue(lignes)=="4" & tclvalue(colonnes)=="2") {c(8,16)} else
      if (tclvalue(lignes)=="1" & tclvalue(colonnes)=="3") {c(15,5)} else
      if (tclvalue(lignes)=="2" & tclvalue(colonnes)=="3") {c(15,10)} else
      if (tclvalue(lignes)=="3" & tclvalue(colonnes)=="3") {c(12,12)} else
      if (tclvalue(lignes)=="4" & tclvalue(colonnes)=="3") {c(12,16)} else
      if (tclvalue(lignes)=="1" & tclvalue(colonnes)=="4") {c(16,4)} else
      if (tclvalue(lignes)=="2" & tclvalue(colonnes)=="4") {c(16,8)} else
      if (tclvalue(lignes)=="3" & tclvalue(colonnes)=="4") {c(16,12)} else
      if (tclvalue(lignes)=="4" & tclvalue(colonnes)=="4") {c(16,16)}
    if (.Platform$OS.type=="windows") {windows(width=dimensions[1],height=dimensions[2])} else {x11(width=dimensions[1],height=dimensions[2])}
    par(bg=tclvalue(col.fond),mfrow=c(as.numeric(tclvalue(lignes)),as.numeric(tclvalue(colonnes))),mar=c(5,6,4,2))
    tkgrab.release(New)
    tkdestroy(New)
  })
  annuler=tkbutton(New,width=16,text=Env$vocab[2,1],font=Env$police,command=function() {tkgrab.release(New);tkdestroy(New)})
  tkgrid(tklabel(New,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(New,text="     ",font=Env$police),row=1,column=3)
  tkgrid(tklabel(New,text=Env$vocab[177,1],font=tkfont.create(family="Arial",size=9,weight="bold")),column=1,columnspan=2)
  tkgrid(colonnes.choose,column=2)
  tkgrid(tklabel(New,text=" ",font=tkfont.create(family="Arial",size=4)))
  tkgrid(lignes.choose,row=4,column=1)
  tkgrid(fenetre,row=4,column=2)
  tkgrid(tklabel(New,text=" ",font=tkfont.create(family="Arial",size=15)))
  tkgrid(tklabel(New,text=Env$vocab[178,1],font=Env$police),row=6,column=1,sticky="e")
  tkgrid(col.fond.choose,row=6,column=2,sticky="w")
  tkgrid(tklabel(New,text="",font=Env$police))
  tkgrid(ok,column=1,columnspan=2)
  tkgrid(tklabel(New,text=" ",font=tkfont.create(family="Arial",size=4)))
  tkgrid(annuler,column=1,columnspan=2)
  tkgrid(tklabel(New,text=" ",font=tkfont.create(family="Arial",size=4)))
}


graphe.choix <-
function() {
  if (length(names(Env$datas.GrapheR)>=1)) {
    tkwm.withdraw(Env$Fen1)
    assign("Fen4",tktoplevel(),pos=Env)
    tktitle(Env$Fen4)="GrapheR"
    tkwm.resizable(Env$Fen4,FALSE,FALSE)
    tcl("wm","protocol",Env$Fen4,"WM_DELETE_WINDOW",function() {tkdestroy(Env$Fen4);tkdestroy(Env$Fen1)})
    but1=tkbutton(Env$Fen4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$images[1,1],fsep=.Platform$file.sep)),command=function() {histogram()})
    but2=tkbutton(Env$Fen4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$images[2,1],fsep=.Platform$file.sep)),command=function() {moustaches()})
    but3=tkbutton(Env$Fen4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$images[3,1],fsep=.Platform$file.sep)),command=function() {barres()})
    but4=tkbutton(Env$Fen4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$images[4,1],fsep=.Platform$file.sep)),command=function() {camembert()})
    but5=tkbutton(Env$Fen4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$images[5,1],fsep=.Platform$file.sep)),command=function() {courbe()})
    but6=tkbutton(Env$Fen4,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images",Env$images[6,1],fsep=.Platform$file.sep)),command=function() {nuage()})
    retour=tkbutton(Env$Fen4,text=Env$vocab[226,1],font=Env$police,command=function() {tkdestroy(Env$Fen4);tkwm.deiconify(Env$Fen1);tkfocus(Env$Fen1)})
    tkgrid(tklabel(Env$Fen4,text="",font=tkfont.create(family="Arial",size=4)))
    tkgrid(tklabel(Env$Fen4,text=Env$vocab[225,1],font=tkfont.create(family="Arial",size=11,weight="bold")),columnspan=7)
    tkgrid(tklabel(Env$Fen4,text="",font=Env$police))
    tkgrid(tklabel(Env$Fen4,text="               ",font=Env$police),row=1,column=0)
    tkgrid(but1,row=5,column=1)
    tkgrid(tklabel(Env$Fen4,text="                    ",font=Env$police),row=1,column=2)
    tkgrid(but2,row=5,column=3)
    tkgrid(tklabel(Env$Fen4,text="                    ",font=Env$police),row=1,column=4)
    tkgrid(but3,row=5,column=5)
    tkgrid(tklabel(Env$Fen4,text="               ",font=Env$police),row=1,column=6)
    tkgrid(tklabel(Env$Fen4,text="",font=Env$police))
    tkgrid(but4,row=8,column=1)
    tkgrid(but5,row=8,column=3)
    tkgrid(but6,row=8,column=5)
    tkgrid(tklabel(Env$Fen4,text="",font=tkfont.create(family="Arial",size=10)))
    tkgrid(retour,column=2,columnspan=3,sticky="we")
    tkgrid(tklabel(Env$Fen4,text="",font=tkfont.create(family="Arial",size=4)))
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[5,1],icon="error",type="ok")}
}


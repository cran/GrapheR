toolbar <-
function(type,type.hist,sequence,log.ax,ht,abs) {
  assign("toolbar.GrapheR",1,pos=Env)
  assign("Toolbar",tktoplevel(),pos=Env)
  tkwm.resizable(Env$Toolbar,FALSE,FALSE)
  tktitle(Env$Toolbar)="GrapheR"
  tkfocus(Env$Toolbar)
  tcl("wm","protocol",Env$Toolbar,"WM_DELETE_WINDOW",function() {assign("toolbar.GrapheR",0,pos=Env);tkdestroy(Env$Toolbar)})
  but1=tkbutton(Env$Toolbar,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Icone1.gif",fsep=.Platform$file.sep)),state=if (type=="cam") {"disabled"} else {"normal"},command=function() {
    if (dev.cur()>1) {vertical()} else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  but2=tkbutton(Env$Toolbar,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Icone2.gif",fsep=.Platform$file.sep)),state=if (type=="cam") {"disabled"} else {"normal"},command=function() {
    if (dev.cur()>1) {horizontal()} else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  but3=tkbutton(Env$Toolbar,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Icone3.gif",fsep=.Platform$file.sep)),state=if (type=="courbe" | type=="nuage") {"normal"} else {"disabled"},command=function() {
    if (dev.cur()>1) {affine(log.axes=log.ax)} else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  but4=tkbutton(Env$Toolbar,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Icone4.gif",fsep=.Platform$file.sep)),state=if (type=="hist" & type.hist=="dens") {"normal"} else {"disabled"},command=function() {
    if (dev.cur()>1) {distrib(sequence=sequence)} else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  but5=tkbutton(Env$Toolbar,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Icone5.gif",fsep=.Platform$file.sep)),command=function() {
    if (dev.cur()>1) {texte()} else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  but6=tkbutton(Env$Toolbar,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Icone6.gif",fsep=.Platform$file.sep)),state=if (type=="barres") {"normal"} else {"disabled"},command=function() {
    if (dev.cur()>1) {pvalue(abscisses=abs,hauteurs=ht)} else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  but7=tkbutton(Env$Toolbar,image=tkimage.create("photo",file=file.path(.path.package("GrapheR"),"images","Icone7.gif",fsep=.Platform$file.sep)),command=function() {
    if (dev.cur()>1) {sauver()} else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[9,1],icon="error",type="ok")}
  })
  tkgrid(but1,but2,but3,but4,but5,but6,but7)
}


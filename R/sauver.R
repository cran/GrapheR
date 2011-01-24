sauver <-
function() {
  Sauv=tktoplevel()
  tkwm.resizable(Sauv,FALSE,FALSE)
  tktitle(Sauv)="GrapheR"
  tkgrab.set(Sauv)
  tkfocus(Sauv)
  fenetres=integer(length(dev.list()))
  for (i in 1:length(dev.list())) {
    fenetres[i]=paste(Env$vocab[192,1],dev.list()[i],sep="")
  }
  fenValue=tclVar(fenetres[1])
  fen.choose=ttkcombobox(Sauv,font=Env$police,values=fenetres,textvariable=fenValue,state="readonly")
  largValue=tclVar("600")
  larg=tkscale(Sauv,from=400,to=1200,showvalue=TRUE,font=Env$police,variable=largValue,resolution=10,orient="horizontal")
  rb1=tkradiobutton(Sauv)
  rb2=tkradiobutton(Sauv)
  rb3=tkradiobutton(Sauv)
  fichier=tclVar("jpg")
  tkconfigure(rb1,font=Env$police,variable=fichier,value="jpg",text="Jpg     ")
  tkconfigure(rb2,font=Env$police,variable=fichier,value="png",text="Png     ")
  tkconfigure(rb3,font=Env$police,variable=fichier,value="pdf",text="Pdf  ")
  enregistrer=tkbutton(Sauv,text=Env$vocab[234,1],font=Env$police,width=16,command=function() {
    dev.set(as.numeric(strsplit(tclvalue(fenValue),split="_")[[1]][2]))
    if (tclvalue(fichier)=="jpg") {
      file=tclvalue(tkgetSaveFile(filetypes="{Jpg {.jpg}}"))
	dev.print(jpeg,filename=paste(strsplit(file,".jpg"),".jpg",sep=""),quality=100,width=as.numeric(tclvalue(largValue)))
    }
    if (tclvalue(fichier)=="png") {
	file=tclvalue(tkgetSaveFile(filetypes="{Png {.png}}"))
	dev.print(png,filename=paste(strsplit(file,".png"),".png",sep=""),width=as.numeric(tclvalue(largValue)))
    }
    if (tclvalue(fichier)=="pdf") {
	file=tclvalue(tkgetSaveFile(filetypes="{Pdf {.pdf}}"))
	dev.print(pdf,paste(strsplit(file,".pdf"),".pdf",sep=""),pointsize=10,width=as.numeric(tclvalue(largValue))/100)
    }
    tkfocus(Sauv)
  })
  fermer=tkbutton(Sauv,text=Env$vocab[182,1],font=Env$police,width=16,command=function() {tkgrab.release(Sauv);tkdestroy(Sauv);tkfocus(Env$Toolbar)})
  tkgrid(tklabel(Sauv,text="",font=Env$police))
  tkgrid(tklabel(Sauv,text="        ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Sauv,text=Env$vocab[191,1],font=Env$police),row=1,column=1,sticky="e")
  tkgrid(fen.choose,row=1,column=3,columnspan=3,sticky="w")
  tkgrid(tklabel(Sauv,text="",font=Env$police))
  tkgrid(tklabel(Sauv,text=Env$vocab[14,1],font=Env$police),row=3,column=1,sticky="e")
  tkgrid(tklabel(Sauv,text="   ",font=Env$police),row=3,column=2)
  tkgrid(rb1,row=3,column=3)
  tkgrid(rb2,row=3,column=4)
  tkgrid(rb3,row=3,column=5)
  tkgrid(tklabel(Sauv,text="      ",font=Env$police),row=3,column=6)
  tkgrid(tklabel(Sauv,text="",font=tkfont.create(family="Arial",size=4)))
  tkgrid(tklabel(Sauv,text=Env$vocab[193,1],font=Env$police),row=5,column=1,sticky="se")
  tkgrid(larg,row=5,column=3,columnspan=3)
  tkgrid(tklabel(Sauv,text="",font=Env$police))
  tkgrid(enregistrer,row=7,column=1,sticky="e")
  tkgrid(fermer,row=7,column=3,columnspan=3)
  tkgrid(tklabel(Sauv,text="",font=tkfont.create(family="Arial",size=4)))
}


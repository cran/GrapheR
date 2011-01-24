GrapheR.begin <-
function() {
  tkbind("all","<F1>",function() {
    if (Env$lang=="en") {browseURL(file.path(.path.package("GrapheR"),"doc","manual_en.pdf",fsep=.Platform$file.sep))}
    if (Env$lang=="fr") {browseURL(file.path(.path.package("GrapheR"),"doc","manual_fr.pdf",fsep=.Platform$file.sep))}
  })
  Fen0=tktoplevel()
  tktitle(Fen0)="GrapheR"
  tkwm.geometry(Fen0,"225x150")
  tkwm.resizable(Fen0,FALSE,FALSE)
  tkfocus(Fen0)
  assign("lang","en",pos=Env)
  language=tclVar("English")
  language.choose=ttkcombobox(Fen0,values=c("English",paste("Fran","\U00E7","ais",sep="")),textvariable=language,font=Env$police,width=12,state="readonly")
  tkbind(language.choose,"<<ComboboxSelected>>",function() {
    assign("lang",if (tclvalue(language)==paste("Fran","\U00E7","ais",sep="")) {"fr"} else if (tclvalue(language)=="English") {"en"},pos=Env)
  })
  ok.but=tkbutton(Fen0,text="Ok",font=Env$police,width=12,command=function() {
    assign("vocab",if (Env$lang=="en") {
	read.csv(file.path(.path.package("GrapheR"),"lang","Language_en.csv",fsep=.Platform$file.sep),sep=";",as.is=1,header=FALSE,encoding="UTF-8")
    } else if (Env$lang=="fr") {
	read.csv(file.path(.path.package("GrapheR"),"lang","Language_fr.csv",fsep=.Platform$file.sep),sep=";",as.is=1,header=FALSE,encoding="UTF-8")
    },pos=Env)
    assign("images",if (Env$lang=="en") {
	read.csv(file.path(.path.package("GrapheR"),"lang","Images_en.csv",fsep=.Platform$file.sep),sep=";",as.is=1,header=FALSE,encoding="UTF-8")
    } else if (Env$lang=="fr") {
	read.csv(file.path(.path.package("GrapheR"),"lang","Images_fr.csv",fsep=.Platform$file.sep),sep=";",as.is=1,header=FALSE,encoding="UTF-8")
    },pos=Env)
    tkdestroy(Fen0)
    commencer()
  })
  tkgrid(tklabel(Fen0,text=" ",font=tkfont.create(family="Arial",size=2)))
  tkgrid(tklabel(Fen0,text="     ",font=Env$police),row=1,column=0)
  tkgrid(tklabel(Fen0,text="At any moment, press F1 to call help",font=tkfont.create(family="Arial",slant="italic",size=8)),row=1,column=1)
  tkgrid(tklabel(Fen0,text=" ",font=tkfont.create(family="Arial",size=2)))
  tkgrid(tklabel(Fen0,text="Select your language :",font=Env$police),column=1)
  tkgrid(tklabel(Fen0,text=" ",font=tkfont.create(family="Arial",size=2)),column=1)
  tkgrid(language.choose,column=1)
  tkgrid(tklabel(Fen0,text=" ",font=Env$police))
  tkgrid(ok.but,column=1)
}


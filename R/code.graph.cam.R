code.graph.cam <-
function() {
  texte<-"pie(variable"
  texte<-paste(texte,", col=c(\"",paste(Env$l.var$couleur1B,collapse="\",\""),"\")",sep="")
  if (tclvalue(Env$l.var$col.borduresA)!="black" & tclvalue(Env$l.var$col.borduresA)!="#000000") {texte<-paste(texte,", border=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")}
  if (tclvalue(Env$l.var$cam.lien)==1) {
    texte<-paste(texte,",\n  labels=c(\"",paste(Env$l.var$nomsparts,collapse="\",\""),"\")",sep="")
  } else {
    texte<-paste(texte,",\n  labels=\"\"",sep="")
  }
  texte<-paste(texte,", clockwise=",ifelse(tclvalue(Env$l.var$cam.orient)==Env$voc[120,1],"TRUE","FALSE"),sep="")
  texte<-paste(texte,", init.angle=",ifelse(tclvalue(Env$l.var$cam.orient)==Env$voc[120,1],as.numeric(tclvalue(Env$l.var$cam.start))+90,
    -1*as.numeric(tclvalue(Env$l.var$cam.start))+90),")\n",sep="")
  if (any(Env$l.var$hachuresB!=1)) {
    hachures<-graphe.hachures(num=Env$l.var$hachuresB)
    texte<-paste(texte,"par(new=TRUE)\n",sep="")
    texte<-paste(texte,"pie(variable",sep="")
    if (tclvalue(Env$l.var$col.borduresA)!="black" & tclvalue(Env$l.var$col.borduresA)!="#000000") {
	texte<-paste(texte,", col=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")
	texte<-paste(texte,", border=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")
    }
    paste(texte,", labels=\"\"",sep="")
    texte<-paste(texte,", clockwise=",ifelse(tclvalue(Env$l.var$cam.orient)==Env$voc[120,1],"TRUE","FALSE"),sep="")
    texte<-paste(texte,", init.angle=",ifelse(tclvalue(Env$l.var$cam.orient)==Env$voc[120,1],as.numeric(tclvalue(Env$l.var$cam.start))+90,
	-1*as.numeric(tclvalue(Env$l.var$cam.start))+90),sep="")
    texte<-paste(texte,",\n  density=c(",paste(hachures$densite,collapse=","),")",sep="")
    texte<-paste(texte,", angle=c(",paste(hachures$angle,collapse=","),"))\n\n",sep="")
  }
  cat(texte)
  code.graph.titre()
  if (tclvalue(Env$l.var$cam.lien)==0 & tclvalue(Env$l.var$legende)==1) {
    cat("# Legend\n\n")
    cat("par(xpd=TRUE)\n")
    texte<-paste("legend(\"",code.graph.posleg(),"\"",sep="")
    texte<-paste(texte,", legend=c(\"",paste(Env$l.var$nomsparts,collapse="\",\""),"\")",sep="")
    texte<-paste(texte,",\n  fill=c(\"",paste(Env$l.var$couleur1B,collapse="\",\""),"\")",sep="")
    if (nchar(tclvalue(Env$l.var$legende.titre))>0) {texte<-paste(texte,", title=\"",tclvalue(Env$l.var$legende.titre),"\"",sep="")}
    paste(texte,", labels=\"\")\n",sep="")
    cat(texte)
    cat("par(xpd=FALSE)\n\n")
  }
}

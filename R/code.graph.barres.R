code.graph.barres <-
function() {
  texte<-""
  if (tclvalue(Env$l.var$moyprop)=="moy") {
    if (tclvalue(Env$l.var$plusieurs)==0) {
	texte<-"graph <- barplot(means, axes=FALSE, ann=FALSE"
	texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur1A),"\"",sep="")
	if (tclvalue(Env$l.var$col.borduresA)!="black" & tclvalue(Env$l.var$col.borduresA)!="#000000") {texte<-paste(texte,", border=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")}
	if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
	texte<-paste(texte,",\n  ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	texte<-paste(texte,", names=c(\"",paste(Env$l.var$noms1,collapse="\",\""),"\"))\n\n",sep="")
	if (graphe.log()=="" & tclvalue(Env$l.var$hachuresA)!="1") {
	  hachures<-graphe.hachures(num=as.numeric(tclvalue(Env$l.var$hachuresA)))
	  texte<-paste(texte,"barplot(means, axes=FALSE, ann=FALSE",sep="")
	  if (tclvalue(Env$l.var$col.borduresA)!="black" & tclvalue(Env$l.var$col.borduresA)!="#000000") {
	    texte<-paste(texte,", col=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")
	    texte<-paste(texte,", border=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")
	  }
	  texte<-paste(texte,", ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	  texte<-paste(texte,",\n  density=",hachures$densite,sep="")
	  texte<-paste(texte,", angle=",hachures$angle,sep="")
	  texte<-paste(texte,", names.arg=\"\"",sep="")
	  texte<-paste(texte,", add=TRUE)\n\n",sep="")
	}
	cat(texte)
	code.graph.axes()
	code.graph.titre()
	if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
	code.graph.barres.erreurs(type="means")
    } else {
	texte<-"graph <- barplot(means, axes=FALSE, ann=FALSE"
	texte<-paste(texte,", col=c(\"",paste(Env$l.var$couleur1B,collapse="\",\""),"\")",sep="")
	if (any(!Env$l.var$col.borduresB%in%c("black","#000000"))) {texte<-paste(texte,", border=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")}
	if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
	texte<-paste(texte,",\n  ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	texte<-paste(texte,", beside=",ifelse(tclvalue(Env$l.var$stack)==1,"FALSE","TRUE"),sep="")
	texte<-paste(texte,", names=c(\"",paste(Env$l.var$noms1,collapse="\",\""),"\"))\n\n",sep="")
	if (graphe.log()=="" & any(Env$l.var$hachuresB!=1)) {
	  hachures<-graphe.hachures(num=Env$l.var$hachuresB)
	  texte<-paste(texte,"barplot(means, axes=FALSE, ann=FALSE",sep="")
	  if (any(!Env$l.var$col.borduresB%in%c("black","#000000"))) {
	    texte<-paste(texte,", col=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")
	    texte<-paste(texte,", border=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")
	  }
	  texte<-paste(texte,",\n  ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	  texte<-paste(texte,", beside=",ifelse(tclvalue(Env$l.var$stack)==1,"FALSE","TRUE"),sep="")
	  texte<-paste(texte,", density=c(",paste(hachures$densite,collapse=","),")",sep="")
	  texte<-paste(texte,", angle=c(",paste(hachures$angle,collapse=","),")",sep="")
	  texte<-paste(texte,",\n  names.arg=\"\"",sep="")
	  texte<-paste(texte,", add=TRUE)\n\n",sep="")
	}
	cat(texte)
	code.graph.axes()
	code.graph.titre()
	if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
	if (tclvalue(Env$l.var$stack)==0) {code.graph.barres.erreurs(type="means")}
	if (tclvalue(Env$l.var$legende)==1) {
	  cat("# Legend\n\n")
	  cat("par(xpd=TRUE)\n")
	  texte<-paste("legend(\"",code.graph.posleg(),"\"",sep="")
	  texte<-paste(texte,", legend=c(\"",paste(Env$l.var$noms2,collapse="\",\""),"\")",sep="")
	  texte<-paste(texte,",\n  fill=c(\"",paste(Env$l.var$couleur1B,collapse="\",\""),"\")",sep="")
	  if (nchar(tclvalue(Env$l.var$legende.titre))>0) {texte<-paste(texte,", title=\"",tclvalue(Env$l.var$legende.titre),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  cat(texte)
	  cat("par(xpd=FALSE)\n\n")
	}
    }
  } else {
    if (tclvalue(Env$l.var$plusieurs)==0) {
	texte<-"graph <- barplot(proportions, axes=FALSE, ann=FALSE"
	texte<-paste(texte,", col=\"",tclvalue(Env$l.var$couleur1A),"\"",sep="")
	if (tclvalue(Env$l.var$col.borduresA)!="black" & tclvalue(Env$l.var$col.borduresA)!="#000000") {texte<-paste(texte,", border=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")}
	if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
	texte<-paste(texte,",\n  ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	texte<-paste(texte,", names=c(\"",paste(Env$l.var$nomsprop.fac,collapse="\",\""),"\"))\n\n",sep="")
	if (graphe.log()=="" & tclvalue(Env$l.var$hachuresA)!="1") {
	  hachures<-graphe.hachures(num=as.numeric(tclvalue(Env$l.var$hachuresA)))
	  texte<-paste(texte,"barplot(proportions, axes=FALSE, ann=FALSE",sep="")
	  if (tclvalue(Env$l.var$col.borduresA)!="black" & tclvalue(Env$l.var$col.borduresA)!="#000000") {
	    texte<-paste(texte,", col=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")
	    texte<-paste(texte,", border=\"",tclvalue(Env$l.var$col.borduresA),"\"",sep="")
	  }
	  texte<-paste(texte,", ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	  texte<-paste(texte,",\n  density=",hachures$densite,sep="")
	  texte<-paste(texte,", angle=",hachures$angle,sep="")
	  texte<-paste(texte,", names.arg=\"\"",sep="")
	  texte<-paste(texte,", add=TRUE)\n\n",sep="")
	}
	cat(texte)
	code.graph.axes()
	code.graph.titre()
	if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
	code.graph.barres.erreurs(type="proportions")
    } else {
	texte<-"graph <- barplot(proportions, axes=FALSE, ann=FALSE"
	texte<-paste(texte,", col=c(\"",paste(Env$l.var$couleur1B,collapse="\",\""),"\")",sep="")
	if (any(!Env$l.var$col.borduresB%in%c("black","#000000"))) {texte<-paste(texte,", border=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")}
	if (graphe.log()!="") {texte<-paste(texte,", log=\"",graphe.log(),"\"",sep="")}
	texte<-paste(texte,",\n  ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	texte<-paste(texte,", beside=",ifelse(tclvalue(Env$l.var$stack)==1,"FALSE","TRUE"),sep="")
	texte<-paste(texte,", names=c(\"",paste(Env$l.var$nomsprop.fac,collapse="\",\""),"\"))\n\n",sep="")
	if (graphe.log()=="" & any(Env$l.var$hachuresB!=1)) {
	  hachures<-graphe.hachures(num=Env$l.var$hachuresB)
	  texte<-paste(texte,"barplot(proportions, axes=FALSE, ann=FALSE",sep="")
	  if (any(!Env$l.var$col.borduresB%in%c("black","#000000"))) {
	    texte<-paste(texte,", col=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")
	    texte<-paste(texte,", border=c(\"",paste(Env$l.var$col.borduresB,collapse="\",\""),"\")",sep="")
	  }
	  texte<-paste(texte,",\n  ylim=c(",round(Env$l.code$y.inf,2),",",round(Env$l.code$y.sup,2),")",sep="")
	  texte<-paste(texte,", beside=",ifelse(tclvalue(Env$l.var$stack)==1,"FALSE","TRUE"),sep="")
	  texte<-paste(texte,", density=c(",paste(hachures$densite,collapse=","),")",sep="")
	  texte<-paste(texte,", angle=c(",paste(hachures$angle,collapse=","),")",sep="")
	  texte<-paste(texte,",\n  names.arg=\"\"",sep="")
	  texte<-paste(texte,", add=TRUE)\n\n",sep="")
	}
	cat(texte)
	code.graph.axes()
	code.graph.titre()
	if (tclvalue(Env$l.var$encadre)==1) {cat("box()\n\n")}
	if (tclvalue(Env$l.var$stack)==0) {code.graph.barres.erreurs(type="proportions")}
	if (tclvalue(Env$l.var$legende)==1) {
	  cat("# Legend\n\n")
	  cat("par(xpd=TRUE)\n")
	  texte<-paste("legend(\"",code.graph.posleg(),"\"",sep="")
	  texte<-paste(texte,", legend=c(\"",paste(Env$l.var$nomsprop,collapse="\",\""),"\")",sep="")
	  texte<-paste(texte,",\n  fill=c(\"",paste(Env$l.var$couleur1B,collapse="\",\""),"\")",sep="")
	  if (nchar(tclvalue(Env$l.var$legende.titre))>0) {texte<-paste(texte,", title=\"",tclvalue(Env$l.var$legende.titre),"\"",sep="")}
	  texte<-paste(texte,")\n",sep="")
	  cat(texte)
	  cat("par(xpd=FALSE)\n\n")
	}
    }
  }
}

code.graph.titre <-
function() {
  titre.x<-""
  titre.y<-""
  if (Env$l.var$ecran=="M") {
    titre.x<-if (tclvalue(Env$l.var$box.orient)==Env$voc[68,1]) {
	tclvalue(Env$l.var$titre.axenoms)
    } else {
	tclvalue(Env$l.var$titre.axevaleurs)
    }
    titre.y<-if (tclvalue(Env$l.var$box.orient)==Env$voc[68,1]) {
	tclvalue(Env$l.var$titre.axevaleurs)
    } else {
	tclvalue(Env$l.var$titre.axenoms)
    }
  } else {
    titre.x<-tclvalue(Env$l.var$titre.axehor)
    titre.y<-tclvalue(Env$l.var$titre.axever)
  }
  if (tclvalue(Env$l.var$titre)!="" | titre.x!="" | titre.y!="") {
    texte<-"title("
    if (tclvalue(Env$l.var$titre)!="") {
	texte<-paste(texte,"main=\"",tclvalue(Env$l.var$titre),"\"",sep="")
	if (tclvalue(Env$l.var$titre.col)!="black" & tclvalue(Env$l.var$titre.col)!="#000000") {texte<-paste(texte,", col.main=\"",tclvalue(Env$l.var$titre.col),"\"",sep="")}
	if (tclvalue(Env$l.var$titre.taille)!="1.5") {texte<-paste(texte,", cex.main=",tclvalue(Env$l.var$titre.taille),sep="")}
    }
    if (titre.x!="") {
	if (tclvalue(Env$l.var$titre)!="") {
	  texte<-paste(texte,",\n  xlab=\"",titre.x,"\"",sep="")
	} else {
	  texte<-paste(texte,"xlab=\"",titre.x,"\"",sep="")
	}
    }
    if (titre.y!="") {
	if (tclvalue(Env$l.var$titre)!="" | titre.x!="") {
	  if (tclvalue(Env$l.var$titre)=="") {
	    texte<-paste(texte,", ylab=\"",titre.y,"\"",sep="")
	  } else {
	    if (titre.x=="") {
		texte<-paste(texte,",\n  ylab=\"",titre.y,"\"",sep="")
	    } else {
		texte<-paste(texte,", ylab=\"",titre.y,"\"",sep="")
	    }
	  }
	} else {
	  texte<-paste(texte,"ylab=\"",titre.y,"\"",sep="")
	}
    }
    if (titre.x!="" | titre.y!="") {
	if (tclvalue(Env$l.var$legendes.col)!="black" & tclvalue(Env$l.var$legendes.col)!="#000000") {texte<-paste(texte,", col.lab=\"",tclvalue(Env$l.var$legendes.col),"\"",sep="")}
	if (tclvalue(Env$l.var$legendes.taille)!="1") {texte<-paste(texte,", cex.lab=",tclvalue(Env$l.var$legendes.taille),sep="")}
    }
    cat(paste(texte,")\n\n",sep=""))
  }
}

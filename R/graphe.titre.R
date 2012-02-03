graphe.titre <-
function(type="",orient=NULL) {
  if (type=="moust") {
    titre.x<-if (orient=="ver") {
	tclvalue(Env$l.var$titre.axenoms)
    } else {
	tclvalue(Env$l.var$titre.axevaleurs)
    }
    titre.y<-if (orient=="ver") {
	tclvalue(Env$l.var$titre.axevaleurs)
    } else {
	tclvalue(Env$l.var$titre.axenoms)
    }
    if (nchar(tclvalue(Env$l.var$soustitre))!=0) {
	title(main=tclvalue(Env$l.var$titre),col.main=tclvalue(Env$l.var$titre.col),cex.main=as.numeric(tclvalue(Env$l.var$titre.taille)),line=2.2)
	title(main=tclvalue(Env$l.var$soustitre),col.main=tclvalue(Env$l.var$titre.col),cex.main=0.7*as.numeric(tclvalue(Env$l.var$titre.taille)),line=0.9)
	title(xlab=titre.x,ylab=titre.y,cex.lab=as.numeric(tclvalue(Env$l.var$legendes.taille)),col.lab=tclvalue(Env$l.var$legendes.col))
    } else {
	title(main=tclvalue(Env$l.var$titre),col.main=tclvalue(Env$l.var$titre.col),cex.main=as.numeric(tclvalue(Env$l.var$titre.taille)),
	  xlab=titre.x,ylab=titre.y,cex.lab=as.numeric(tclvalue(Env$l.var$legendes.taille)),col.lab=tclvalue(Env$l.var$legendes.col))
    }
  } else {
    if (nchar(tclvalue(Env$l.var$soustitre))!=0) {
	title(main=tclvalue(Env$l.var$titre),col.main=tclvalue(Env$l.var$titre.col),cex.main=as.numeric(tclvalue(Env$l.var$titre.taille)),line=2.2)
	title(main=tclvalue(Env$l.var$soustitre),col.main=tclvalue(Env$l.var$titre.col),cex.main=0.7*as.numeric(tclvalue(Env$l.var$titre.taille)),line=0.9)
	title(xlab=tclvalue(Env$l.var$titre.axehor),ylab=tclvalue(Env$l.var$titre.axever),cex.lab=as.numeric(tclvalue(Env$l.var$legendes.taille)),
	  col.lab=tclvalue(Env$l.var$legendes.col))
    } else {
	title(main=tclvalue(Env$l.var$titre),col.main=tclvalue(Env$l.var$titre.col),cex.main=as.numeric(tclvalue(Env$l.var$titre.taille)),
	  xlab=tclvalue(Env$l.var$titre.axehor),ylab=tclvalue(Env$l.var$titre.axever),cex.lab=as.numeric(tclvalue(Env$l.var$legendes.taille)),
	  col.lab=tclvalue(Env$l.var$legendes.col))
    }
  }
  if (tclvalue(Env$l.var$sysinfo)==1) {
    mtext(paste("R version: ",getRversion()," - GrapheR version: ",installed.packages()["GrapheR","Version"]," - Date: ",paste(strsplit(as.character(Sys.Date()),split="-")[[1]],
	collapse="."),sep=""),side=4,cex=0.6)
  }
}

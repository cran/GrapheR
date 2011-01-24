barres.stack <-
function(varValue,fact2Value,besideValue,erreur.lab,erreur.choose,col.erreur.lab,col.erreur,col.erreur.choose,segment.lab,segment.check,typeValue) {
  if (nchar(tclvalue(varValue))>0) {
    if (as.numeric(tclvalue(besideValue))==0) {
      if (is.factor(Env$datas.GrapheR[,tclvalue(varValue)])==TRUE) {
	  if (nchar(Env$niv.prop)<=1) {
	    barres.erreur.act(act=TRUE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
	  } else {
	    barres.erreur.act(act=FALSE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
	  }
      } else {
	  if (tclvalue(typeValue)=="som") {
	    barres.erreur.act(act=FALSE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
	  } else {
	    if (nchar(tclvalue(fact2Value))>0 & tclvalue(fact2Value)!=Env$vocab[48,1]) {
		barres.erreur.act(act=FALSE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
	    } else {
		barres.erreur.act(act=TRUE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
	    }
	  }
      }
    } else {
      if (tclvalue(typeValue)!="som") {
	  barres.erreur.act(act=TRUE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
      } else {
	  barres.erreur.act(act=FALSE,erreur.lab=erreur.lab,erreur.choose=erreur.choose,col.erreur.lab=col.erreur.lab,col.erreur=col.erreur,col.erreur.choose=col.erreur.choose,segment.lab=segment.lab,segment.check=segment.check)
      }
    }
  }
}


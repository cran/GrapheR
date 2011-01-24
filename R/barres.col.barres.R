barres.col.barres <-
function(varValue,fact2Value) {
  if (nchar(tclvalue(varValue))>0) {
    if (is.factor(Env$datas.GrapheR[,tclvalue(varValue)])==TRUE) {
      if (nchar(Env$niv.prop)==1) {
        unecouleur(str="str1",col=Env$col.str1)
	} else {
	  plusieurscouleurs(nb.niv=Env$nb.niv2,noms=Env$noms2,type="bar",str="str1",col=Env$col.str1)
	}
    } else {
      if (nchar(tclvalue(fact2Value))==0 | tclvalue(fact2Value)==Env$vocab[48,1]) {
	  unecouleur(str="str1",col=Env$col.str1)
	} else {
	  plusieurscouleurs(nb.niv=Env$nb.niv2,noms=Env$noms2,type="bar",str="str1",col=Env$col.str1)
	}
    }
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[6,1],icon="error",type="ok")}
}


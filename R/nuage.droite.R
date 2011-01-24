nuage.droite <-
function(var1Value,var2Value,factValue) {
  if (nchar(tclvalue(var1Value))>0 & nchar(tclvalue(var2Value))>0) {
    if (nchar(tclvalue(factValue))==0 | tclvalue(factValue)==Env$vocab[48,1]) {
      unedroite(var1=tclvalue(var1Value),var2=tclvalue(var2Value),drt=Env$droite,reg=Env$regression,horiz=Env$hor,vertic=Env$ver,coefa=Env$coeffa,coefb=Env$coeffb,tra=Env$traits,ep.lig=Env$ep.lignes)
    } else {
	plusieursdroites(var1=tclvalue(var1Value),var2=tclvalue(var2Value),nb.niv=Env$nb.niv1,noms=Env$noms1,drt=Env$droite,reg=Env$regression,horiz=Env$hor,vertic=Env$ver,coefa=Env$coeffa,coefb=Env$coeffb,tra=Env$traits,ep.lig=Env$ep.lignes)
    }
  } else {tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[157,1],icon="error",type="ok")}
}


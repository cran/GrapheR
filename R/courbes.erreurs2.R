courbes.erreurs2 <-
function(liste.val,liste.barres.inf,liste.barres.sup,col.symb,trait,xsup,xinf,err.seg) {
  for (i in 1:length(liste.val)) {
    for (j in 1:length(liste.val[[i]])) {
	if (liste.barres.inf[[i]][j]!=0 | liste.barres.sup[[i]][j]!=0) {segments(as.numeric(names(liste.val[[i]])[j]),liste.val[[i]][j]-liste.barres.inf[[i]][j],as.numeric(names(liste.val[[i]])[j]),liste.val[[i]][j]+liste.barres.sup[[i]][j],col=col.symb[i],lty=trait[i])}
	if (err.seg==1) {
	  if (liste.barres.inf[[i]][j]!=0) {segments(as.numeric(names(liste.val[[i]])[j])-0.02*(xsup-xinf),liste.val[[i]][j]-liste.barres.inf[[i]][j],as.numeric(names(liste.val[[i]])[j])+0.02*(xsup-xinf),liste.val[[i]][j]-liste.barres.inf[[i]][j],col=col.symb[i])}
	  if (liste.barres.sup[[i]][j]!=0) {segments(as.numeric(names(liste.val[[i]])[j])-0.02*(xsup-xinf),liste.val[[i]][j]+liste.barres.sup[[i]][j],as.numeric(names(liste.val[[i]])[j])+0.02*(xsup-xinf),liste.val[[i]][j]+liste.barres.sup[[i]][j],col=col.symb[i])}
	}
    }
  }
}


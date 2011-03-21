courbes.erreurs1 <-
function(val,barres.inf,barres.sup,col.symb,trait,err.seg,xsup,xinf) {
  for (i in 1:length(val)) {
    if (barres.inf[i]!=0 | barres.sup[i]!=0) {segments(as.numeric(names(val)[i]),val[i]-barres.inf[i],as.numeric(names(val)[i]),val[i]+barres.sup[i],col=col.symb,lty=trait)}
    if (err.seg==1) {
	if (barres.inf[i]!=0) {segments(as.numeric(names(val)[i])-0.02*(xsup-xinf),val[i]-barres.inf[i],as.numeric(names(val)[i])+0.02*(xsup-xinf),val[i]-barres.inf[i],col=col.symb)}
	if (barres.sup[i]!=0) {segments(as.numeric(names(val)[i])-0.02*(xsup-xinf),val[i]+barres.sup[i],as.numeric(names(val)[i])+0.02*(xsup-xinf),val[i]+barres.sup[i],col=col.symb)}
    }
  }
}


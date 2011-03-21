barres.yinf <-
function(y.inf,val,err,beside,log.ax) {
  value=if(y.inf=="Auto") {
    if (any(val<0)==TRUE) {
	if (class(val)=="matrix") {
	  if (beside==1) {1.2*min(colSums(val))} else {1.2*min(val-err)}
	} else {1.2*min(val)}
    } else {
	if (log.ax==1) {0.01} else {0}
    }
  } else {as.numeric(y.inf)}
  return(value)
}


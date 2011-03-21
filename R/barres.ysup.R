barres.ysup <-
function(y.sup,val,err,beside,log.ax) {
  value=if (y.sup=="Auto") {
    if (any(val>0)==TRUE) {
	if (class(val)=="matrix") {
  	  if (beside==1) {1.2*max(colSums(val))} else {1.2*max(val+err)}
	} else {1.2*max(val)}
    } else {
	if (log.ax==1) {0.01} else {0}
    }
  } else {as.numeric(y.sup)}
  return(value)
}


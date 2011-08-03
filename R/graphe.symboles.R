graphe.symboles <-
function(num) {
  symboles<-NULL
  if (tclvalue(Env$l.var$plusieurs)==0) {
    if (num==1) {symboles<-1} else
    if (num==2) {symboles<-0} else
    if (num==3) {symboles<-2} else
    if (num==4) {symboles<-3} else
    if (num==5) {symboles<-16} else
    if (num==6) {symboles<-15} else
    if (num==7) {symboles<-17} else
    if (num==8) {symboles<-4}
  } else {
    for (i in 1:length(num)) {
	if (num[i]==1) {symboles<-c(symboles,1)} else
	if (num[i]==2) {symboles<-c(symboles,0)} else
	if (num[i]==3) {symboles<-c(symboles,2)} else
	if (num[i]==4) {symboles<-c(symboles,3)} else
	if (num[i]==5) {symboles<-c(symboles,16)} else
	if (num[i]==6) {symboles<-c(symboles,15)} else
	if (num[i]==7) {symboles<-c(symboles,17)} else
	if (num[i]==8) {symboles<-c(symboles,4)}
    }
  }
  return(symboles)
}


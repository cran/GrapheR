graphe.hachures <-
function(num) {
  densite<-NULL
  angle<-NULL
  if (tclvalue(Env$l.var$plusieurs)==0) {
    if (num==1) {densite<-0;angle<-0} else
    if (num==2) {densite<-4;angle<-135} else
    if (num==3) {densite<-4;angle<-90} else
    if (num==4) {densite<-4;angle<-45} else
    if (num==5) {densite<-4;angle<-0} else
    if (num==6) {densite<-15;angle<-135} else
    if (num==7) {densite<-15;angle<-90} else
    if (num==8) {densite<-15;angle<-45} else
    if (num==9) {densite<-15;angle<-0}
  } else {
    for (i in 1:length(num)) {
	if (num[i]==1) {densite<-c(densite,0);angle<-c(angle,0)} else
	if (num[i]==2) {densite<-c(densite,4);angle<-c(angle,135)} else
	if (num[i]==3) {densite<-c(densite,4);angle<-c(angle,90)} else
	if (num[i]==4) {densite<-c(densite,4);angle<-c(angle,45)} else
	if (num[i]==5) {densite<-c(densite,4);angle<-c(angle,0)} else
	if (num[i]==6) {densite<-c(densite,15);angle<-c(angle,135)} else
	if (num[i]==7) {densite<-c(densite,15);angle<-c(angle,90)} else
	if (num[i]==8) {densite<-c(densite,15);angle<-c(angle,45)} else
	if (num[i]==9) {densite<-c(densite,15);angle<-c(angle,0)}
    }
  }
  return(list(densite=densite,angle=angle))
}


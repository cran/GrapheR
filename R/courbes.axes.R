courbes.axes <-
function(datas.GrapheR2,x.inf,var1,x.sup,y.inf,val,err.inf,y.sup,err.sup) {
  value=list(xinf=NULL,xsup=NULL,yinf=NULL,ysup=NULL)
  value$xinf=if (x.inf=="Auto") {0.8*min(datas.GrapheR2[,var1])} else {as.numeric(x.inf)}
  value$xsup=if (x.sup=="Auto") {1.1*max(datas.GrapheR2[,var1])} else {as.numeric(x.sup)}
  value$yinf=if (y.inf=="Auto") {0.8*min(val-err.inf)} else {as.numeric(y.inf)}
  value$ysup=if (y.sup=="Auto") {1.1*max(val+err.sup)} else {as.numeric(y.sup)}
  return(value)
}


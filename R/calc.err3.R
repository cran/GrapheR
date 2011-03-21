calc.err3 <-
function(datas.GrapheR2,err.type,fact1,niv.prop2,proportions.mat,var) {
  value=list(inf=NULL,sup=NULL)
  inf=NULL
  sup=NULL
  if (err.type==Env$vocab[121,1]) {
    for (i in 1:nlevels(datas.GrapheR2[,fact1])) {
	for (j in 1:length(niv.prop2)) {
	  inf=c(inf,proportions.mat[j,i]-binom.test(length(datas.GrapheR2[,var][datas.GrapheR2[,var]==levels(datas.GrapheR2[,var])[niv.prop2[j]+1] & 
	    datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]),length(datas.GrapheR2[,var][datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]))$conf.int[1])
	  sup=c(sup,binom.test(length(datas.GrapheR2[,var][datas.GrapheR2[,var]==levels(datas.GrapheR2[,var])[niv.prop2[j]+1] & 
	    datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]),length(datas.GrapheR2[,var][datas.GrapheR2[,fact1]==levels(datas.GrapheR2[,fact1])[i]]))$conf.int[2]-proportions.mat[j,i])
	}
    }
    value$inf=matrix(inf,nrow=length(niv.prop2),dimnames=list(levels(datas.GrapheR2[,var])[niv.prop2+1],levels(datas.GrapheR2[,fact1])))
    value$sup=matrix(sup,nrow=length(niv.prop2),dimnames=list(levels(datas.GrapheR2[,var])[niv.prop2+1],levels(datas.GrapheR2[,fact1])))
  } else {
    value$inf=matrix(rep(0,length(niv.prop2)*nlevels(datas.GrapheR2[,fact1])),nrow=length(niv.prop2),dimnames=list(levels(datas.GrapheR2[,var])[niv.prop2+1],levels(datas.GrapheR2[,fact1])))
    value$sup=value$inf
  }
  return(value)
}


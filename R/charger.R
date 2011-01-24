charger <-
function(ext,dcm,hdr,na.cod,spr,rsm) {
  fichier=if (ext=="Txt") {
    tclvalue(tkgetOpenFile(filetypes="{{Fichiers txt} {.txt}}"))
  } else if (ext=="Csv") {
    tclvalue(tkgetOpenFile(filetypes="{{Fichiers csv} {.csv}}"))
  }
  if (!nchar(fichier)) tkmessageBox(title=Env$vocab[3,1],message=Env$vocab[4,1],icon="error",type="ok")
  else {
    assign("datas.GrapheR",read.table(fichier,dec=if (dcm==Env$vocab[11,1]) {"."} else {","},header=if (hdr==1) {TRUE} else {FALSE},na.strings=na.cod,
      sep=if (spr==Env$vocab[12,1]) {","} else if (spr==Env$vocab[13,1]) {";"} else {""}),pos=Env)
    if (length(which(is.na(Env$datas.GrapheR)==TRUE))!=0) {na.lign=length(table(which(is.na(Env$datas.GrapheR)==TRUE,arr.ind=TRUE)[,1]))} else {na.lign=0}
    tkconfigure(rsm,state="normal")
    tkdelete(rsm,"0.0","end")
    tkinsert(rsm,"end",paste(Env$vocab[19,1],dim(Env$datas.GrapheR)[2],Env$vocab[20,1],dim(Env$datas.GrapheR)[1],Env$vocab[21,1],length(which(is.na(Env$datas.GrapheR)==TRUE)),Env$vocab[22,1],na.lign,Env$vocab[23,1],sep=""))
    tkconfigure(rsm,state="disabled")
    variables=NULL
    for (i in 1:length(names(Env$datas.GrapheR))) {
      if(is.numeric(Env$datas.GrapheR[,i])==TRUE) {variables=c(variables,"N")}
      if(is.factor(Env$datas.GrapheR[,i])==TRUE) {variables=c(variables,"F")}
    }
    assign("variables",variables,pos=Env)
    tkdelete(Env$var.num.list,0,"end")
    tkdelete(Env$fact.list,0,"end")
    for (i in 1:length(Env$variables)) {
      if (Env$variables[i]=="N") {tkinsert(Env$var.num.list,"end",names(Env$datas.GrapheR)[i])}
      if (Env$variables[i]=="F") {tkinsert(Env$fact.list,"end",names(Env$datas.GrapheR)[i])}
    }
    tkconfigure(Env$info,state="normal")
    tkdelete(Env$info,"0.0","end")
    tkconfigure(Env$info,state="disabled")
  }
}


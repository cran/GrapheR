hachures2 <-
function(num) {
  if (nchar(tclvalue(tkcurselection(Env$l.fr4$noms.list)))>0) {
    Env$l.var$hachuresB[as.numeric(tclvalue(tkcurselection(Env$l.fr4$noms.list)))+1]<-num
    for (i in 1:9) {tkconfigure(Env$l.fr4$l.hachures[[i]],borderwidth=0)}
    tkconfigure(Env$l.fr4$l.hachures[[num]],borderwidth=2)
  } else {
    msg(text=Env$voc[25,1],type="error")
  }
}


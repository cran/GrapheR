couleur <-
function (fen,titre,var,widg,type,plusieurs) {
  temp=NULL
  if (plusieurs==TRUE) {
    temp=tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(tclVar(var)),title=Env$vocab[titre,1]))
  } else {
    temp=tclvalue(tcl("tk_chooseColor",initialcolor=tclvalue(var),title=Env$vocab[titre,1]))
  }
  if (nchar(temp)>0) {
    if (plusieurs==TRUE) {
      if (type=="but") {tkconfigure(widg,foreground=temp,activeforeground=temp)}
      if (type=="can") {tkconfigure(widg,background=temp)}
    } else {
      tclvalue(var)=temp
      if (type=="but") {tkconfigure(widg,foreground=tclvalue(var),activeforeground=tclvalue(var))}
      if (type=="can") {tkconfigure(widg,background=tclvalue(var))}
    }
  }
  if (nchar(temp)>0 & plusieurs==TRUE) {return(temp)}
}


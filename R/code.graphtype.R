code.graphtype <-
function() {
  graph <- if (Env$l.var$ecran=="H") {"Histogram"} else if (Env$l.var$ecran=="M") {"Box plot"} else if (Env$l.var$ecran=="B") {"Bar plot"} else
    if (Env$l.var$ecran=="Ca") {"Pie chart"} else if (Env$l.var$ecran=="Co") {"Curve"} else if (Env$l.var$ecran=="N") {"Scatter plot"}
  cat("\n\n\n#------------------------------\n")
  Env$l.code$graphsnb <- Env$l.code$graphsnb+1
  cat(paste("# GRAPH ",Env$l.code$graphsnb,": ",graph,sep=""))
  cat("\n#------------------------------\n\n")
}

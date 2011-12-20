code.graph.posleg <-
function() {
  position<-if (tclvalue(Env$l.var$legende.pos)==Env$voc[102,1]) {"top"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[103,1]) {"topright"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[104,1]) {"left"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[105,1]) {"center"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[106,1]) {"right"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[107,1]) {"bottomleft"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[108,1]) {"bottom"} else
    if (tclvalue(Env$l.var$legende.pos)==Env$voc[109,1]) {"bottomright"} else
    {"topleft"}
  return(position)
}
barres.type <-
function(type,typeValue,fact2.lab,fact2.choose,rb1,rb2,rb3,prop.but) {
  tclvalue(typeValue)=if(type=="fact") {"prop"} else {"moy"}
  tkconfigure(fact2.lab,foreground=if(type=="fact") {"grey"} else {"black"})
  tkconfigure(fact2.choose,state=if(type=="fact") {"disabled"} else {"readonly"})
  tkconfigure(rb1,state=if(type=="fact") {"disabled"} else {"normal"})
  tkconfigure(rb2,state=if(type=="fact") {"disabled"} else {"normal"})
  tkconfigure(rb3,state=if(type=="fact") {"normal"} else {"disabled"})
  tkconfigure(prop.but,state=if(type=="fact") {"normal"} else {"disabled"})
}


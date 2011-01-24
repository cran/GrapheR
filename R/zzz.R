.First.lib=function(libname, pkgname){
  fullName <- paste("package", pkgname, sep = ":")
  myEnv <- as.environment(match(fullName, search()))
  rm(.First.lib, envir = myEnv)
  if(exists(".required", envir = myEnv, inherits = FALSE)){
    required <- get(".required", envir = myEnv)
    for(pkg in required)
      require(pkg, quietly = TRUE, character.only = TRUE, save = FALSE)
  }
  if(exists(".First.lib", envir = myEnv, inherits = FALSE)){
    f <- get(".First.lib", envir = myEnv, inherits = FALSE)
    if(is.function(f))
    f(libname, pkgname)
  else
    stop(paste("package", sQuote(pkgname), "has a non-function .First.lib"))
  }
  assign("Env",new.env(),pos="package:GrapheR")
  options(locatorBell=FALSE)
  assign("police",tkfont.create(family="Arial",size=8),pos=Env)
  assign("toolbar.GrapheR",0,pos=Env)
  packageStartupMessage("\nWelcome to GrapheR v 1.0\n\nUse GrapheR.begin() to relaunch the interface after closing\n\nHave fun\n")
  GrapheR.begin()
}
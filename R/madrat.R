#' @importFrom madrat vcat

.onLoad <- function(libname, pkgname){
  .Deprecated("mrcommons|mrfeed|mrland|mrmagpie|mrplayground|mrremind|mrvalidation", old="moinput",
              msg="Package 'moinput' is deprecated.\nUse its successors 'mrcommons', 'mrfeed', 'mrland', 'mrmagpie', 'mrplayground', 'mrremind' or 'mrvalidation' instead.")
  madrat::setConfig(packages=c(madrat::getConfig("packages"),pkgname), .cfgchecks=FALSE, .verbose=FALSE)
  madrat::setConfig(nolabels=c(madrat::getConfig("nolabels"),"REMIND"), .cfgchecks=FALSE, .verbose=FALSE)
}

#create an own warning function which redirects calls to vcat (package internal)
warning <- function(...) vcat(0,...)

# create a own stop function which redirects calls to stop (package internal)
stop <- function(...) vcat(-1,...)

# create an own cat function which redirects calls to cat (package internal)
cat <- function(...) vcat(1,...)
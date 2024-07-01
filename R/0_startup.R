.onAttach <- function(libname, pkgname) {
  packageStartupMessage("bayesTPC requires loading the package NIMBLE to function.\nPlease run 'library(nimble)` before using.")
  }

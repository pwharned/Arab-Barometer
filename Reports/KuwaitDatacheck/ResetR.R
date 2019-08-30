ResetR = function() {
  
  # 1) Remove all objects
  rm(list = ls(all=TRUE, envir = .GlobalEnv), envir = .GlobalEnv)
  
  # 2) Unload non-native packages. 
  nat = c(".GlobalEnv", "package:datasets", "package:evd", "package:nortest", "package:MASS", "package:stats", "package:graphics", "package:grDevices", "package:utils", "package:methods", "Autoloads", "package:base")
  
  p = search()
  for (i in p) {
    if (is.na(match(i, nat))) {
      try(eval(parse(text=paste0("detach(", i, ", unload=T, force=T)"))), silent=T) # force=T is need in case package has dependency
    }
  }
  
  # 3) Close all connections
  try(closeAllConnections(), silent=T)
  
  # 4) Restore default options
  try(options(baseenv()$.Options2), silent=T) # Remember to put assign(".Options2", options(), baseenv()) at the bottom of YOUR_R_HOME\etc\Rprofile.site
  
  # 5) Close all graphic devices
  graphics.off()
  
}
ResetR()

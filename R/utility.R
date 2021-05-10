# Get operating system 
# 
# This package returns the type of OS you use.
# 
# This function and derivates have been circling over the web, I could not trace the original source.
getOS <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}



# get directory from a paht
fileFromPath <- function(x){
  all <- unlist(strsplit(x, "/"))
  paste(all[length(all)], collapse="/")
}

# from: https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
randomString <- function(n=1, length=12){
  # initialize vector
  randomString <- c(1:n)                  
  for (i in 1:n){
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    length, replace=TRUE), collapse="")
  }
  return(randomString)
}



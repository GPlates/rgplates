if(rgplates:::getOS()=="linux") wd <- file.path(Sys.getenv("Dropbox"), "Software/rgplates")
if(rgplates:::getOS()=="windows") wd <- file.path("D:/rgplates")
if(rgplates:::getOS()=="osx") wd <- file.path("~/Desktop/rgplates")

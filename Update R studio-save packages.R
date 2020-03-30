
### Save packages to update R without the need to reinstall packages

ls()
rm(list=ls())


tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")
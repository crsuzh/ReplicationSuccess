try(detach("package:ReplicationSuccess", unload=TRUE), silent=TRUE)
system("make -C .. lib")
library("ReplicationSuccess", lib.loc = "../lib/")
library("ReplicationSuccess", lib.loc = "../lib/v_old")
library("ReplicationSuccess")
library(devtools)


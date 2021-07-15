try(detach("package:ReplicationSuccess", unload=TRUE), silent=TRUE)
system("make -C .. lib")
library("ReplicationSuccess", lib.loc = "../lib/")
library(devtools)
check_win_devel("..")



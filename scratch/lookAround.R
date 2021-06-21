detach("package:ReplicationSuccess", unload=TRUE)
system("make -C .. lib")
library("ReplicationSuccess", lib.loc = "../lib/")


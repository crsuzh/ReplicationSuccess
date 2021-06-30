try(detach("package:ReplicationSuccess", unload=TRUE), silent=TRUE)
system("make -C .. lib")
library("ReplicationSuccess", lib.loc = "../lib/")

levelEquivalent(dinf = 0.8, level = 0.025)
levelEquivalent(dinf = 0.8, level = 0.05, alternative="two.sided")

levelEquivalent(dinf = c(0.8, .9), level = 0.025)
levelEquivalent(dinf = 0.8, level = c(0.05, .002), alternative="two.sided")
levelEquivalent(dinf = c(0.8,.9), level = c(0.05, .002), alternative="two.sided")


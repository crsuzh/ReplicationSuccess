
## function pReplicate() computes a generalisation of Killeen's pRep
## probability of an effect in the same direction
## zo: test statistic of original study
## c: variance ratio sigma_o^2/sigma_r^2 = n_r/n_o

pReplicate <- function(po = NULL, 
                       zo = p2z(p = po, alternative = alternative),
                       c = 1,
                       alternative = "two.sided"){
    pRep <- pnorm(q = zo/sqrt(1 + 1/c))
    return(pRep)
}


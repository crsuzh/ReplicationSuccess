# ReplicationSuccess

The R package ReplicationSuccess contains utilities for design and analysis of
replication studies. Traditional methods based on statistical significance, as
well as more recently developed methods such as the sceptical *p*-value are
included. Data sets from four large-scale replication projects are also
provided. The package also includes functions to compute the *p*-value from the
harmonic mean chi-squared test as well as the *p*-value for intrinsic
credibility.

## Installation

ReplicationSuccess will soon be available on CRAN. The development version
can be installed from GitHub
```r
## package "remotes" required for installation from github
remotes::install_github(repo = "florafauna/ReplicationSuccess")
```

## How to use the package

A good start is to read the vignette. It can be accessed with the command
```r
vignette("ReplicationSuccess")
```

For a deeper understanding of the theory underlying ReplicationSuccess we 
recommend to read our papers:

  - Held, L. (2020). A new standard for the analysis and design of replication
  studies (with discussion). *Journal of the Royal Statistical Society: Series A
  (Statistics in Society)*. 183(2):431-448. <https://doi.org/10.1111/rssa.12493>
  
  - Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication
  success based on relative effect size. <https://arxiv.org/abs/2009.07782>
  
  - Held, L. (2020). The harmonic mean chi-squared test to substantiate
    scientific findings. *Journal of the Royal Statistical Society: Series C
    (Applied Statistics)*, 69:697-708. <https://doi.org/10.1111/rssc.12410>
    
  - Micheloud, C., Held, L. (2021). Power Calculations for Replication Studies.
    <https://arxiv.org/abs/2004.10814>
    
  - Pawel, S., Held, L. (2020). Probabilistic forecasting of replication
    studies. PLoS ONE 15(4):e0231416.
    <https://doi.org/10.1371/journal.pone.0231416>
    
  - Held, L. (2019). The assessment of intrinsic credibility and a new argument
    for *p* < 0.005. *Royal Society Open Science*, 6:181534.
    <https://dx.doi.org/10.1098/rsos.181534>

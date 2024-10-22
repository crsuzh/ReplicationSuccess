# ReplicationSuccess 1.3.3

- new functions `pEdgington`, `powerEdgington`, and `sampleSizeEdgington` to
  design and analyze replication studies based on sum of *p*-values
  (Edgington's) method (see <https://doi.org/10.1098/rsos.240149> for details)
- updated some references

# ReplicationSuccess 1.3.2

- new maintainer Samuel Pawel
- Rewrote conversion functions `ci2se`, `ci2estimate`, `ci2z`, `ci2p`, `z2p`,
and `p2z`.
  - Functions now use vapply() instead of Vectorize()
  - Leads to a substantial speedup
  - Might lead to changes in output format of these functions
  - Fixes behaviour in computation of `z <- estimate / se` in case where `se == 0`

# ReplicationSuccess 1.3.1.1
- Rewrote conversion functions `ci2se`, `ci2estimate`, `ci2z`, `ci2p`, `z2p`,
and `p2z`.
  - Functions now use vapply() instead of Vectorize()
  - Leads to a substantial speedup
  - Might lead to changes in output format of these functions
  - Fixes behaviour in computation of `z <- estimate / se` in case where `se == 0`

# ReplicationSuccess 1.3.1

- Removed functions from exports:
  - .PPpSceptical_
  - .T1EpSceptical_
  - .effectSizeReplicationSuccess_
  - .effectSizeSignificance_
  - .levelSceptical_
  - .p2z_
  - .pSceptical_
  - .powerReplicationSuccess_
  - .powerSignificance_
  - .predictionInterval_
  - .sampleSizeReplicationSuccess_
  - .z2p_

# ReplicationSuccess 1.3

- Change of maintainer.
- Transfer ownership of development repository to CRS
- Update links throughout the entire repository following maintainer and ownership change
- Implemented `"controlled"` level
- Added one-sided p-values in `RProjects'
- Removed alternatives `"greater"` and `"less"` in functions  `T1EpSceptical` and 
`PPpSceptical`.
Alternative `"one.sided"` now returns the T1E rate (respectively project power) 
assuming both alternatives are one-sided in the same direction. 
- Changed arguments and output of function `hMeanChiSqCI`:
  - Renamed argument `level` to `conf.level` in the new version
  - Removed argument `wGamma` entirely
  - When called with `alternative = "none"`, the returned list does not contain
  elements `gammaMean` and `gammaHMean` anymore.
- Removed argument `type = "liberal"` in all functions that had this option
- Removed functions `levelEquivalent` and `unirootAll`
- Added systematic tests to all functions in the package
- Updated function documentation
- Updated vignette


# ReplicationSuccess 1.2

- New data set `protzko2020` 
- Fixed a bug in the error message of `effectSizeReplicationSuccess`
- Added license information and improved documentation of data set `RProjects`

# ReplicationSuccess 1.1.1

- Fixed a bug that prevented vignette building on R devel with updated texlive

# ReplicationSuccess 1.1.0

- New maintainer Samuel Pawel
- Bugfixes `hMeanChiSqMu`
- Replace rootSolve::uniroot.all with custom function `unirootAll`

# ReplicationSuccess 1.0

- CRAN release

# ReplicationSuccess 0.2

- `sampleSizeSignificance`, `powerReplicationSuccess`,
  `sampleSizeReplicationSuccess` now use analytic instead of numerical
  implementation

- Development version of the package migrated to GitHub

- Vectorization in functions now with `Vectorize` instead of `mapply`

- Unit tests migrated to testthat

- Documentation migrated to roxygen2


# ReplicationSuccess 0.1-4

- New function `T1EpSceptical`

- New function `PPpSceptical`

- NEWS file added

- New function `Qtest`


# ReplicationSuccess 0.1-3

- Golden threshold implemented (see <https://arxiv.org/abs/2009.07782>)

- Recalibration of `pSceptical` now implemented via type argument

- `powerReplicationSuccess` and `sampleSizeReplicationSuccess` now also take
  type argument

- `pSceptical` returns per default recalibrated sceptical p-value (golden
  recalibration)

- `sampleSizeReplicationSuccess` and `sampleSizeSignificance` can now also be
  computed based on relative effect size (before only based on power)
  
- New function `effectSizeReplicationSuccess`

- New function `effectSizeSignificance`

- Name of `thresholdSceptical` changed to `levelSceptical`

# ReplicationSuccess 0.2

- `sampleSizeSignificance`, `powerReplicationSuccess`,
  `sampleSizeReplicationSuccess` now use analytic instead of numerical
  implementation

- development version of the package migrated to GitHub

- vectorization in functions now with `Vectorize` instead of `mapply`

- unit tests migrated to testthat

- documentation migrated to roxygen2


# ReplicationSuccess 0.1-4

- new function `T1EpSceptical`

- new function `PPpSceptical`

- NEWS file added

- new function `Qtest`


# ReplicationSuccess 0.1-3

- golden threshold implemented (see <https://arxiv.org/abs/2009.07782>)

- recalibration of `pSceptical` now implemented via type argument

- `powerReplicationSuccess` and `sampleSizeReplicationSuccess` now also take
  type argument

- `pSceptical` returns per default recalibrated sceptical p-value (golden
  recalibration)

- `sampleSizeReplicationSuccess` and `sampleSizeSignificance` can now also be
  computed based on relative effect size (before only based on power)
  
- new function `effectSizeReplicationSuccess`

- new function `effectSizeSignificance`

- name of `thresholdSceptical` changed to `levelSceptical`

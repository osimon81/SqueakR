test_that("ethnogram plots", {
  data(deepsqueak_data)
  expect_error(plotEthnogram(deepsqueak_data), NA)
})

test_that("ethnogram split by time plots", {
  data(deepsqueak_data)
  expect_error(plotEthnogramSplitByTonality(deepsqueak_data), NA)
})

test_that("density stacked by frequency plots", {
  data(deepsqueak_data)
  expect_error(plotDensityStackedByFrequency(deepsqueak_data), NA)
})

test_that("density split by frequency plots", {
  data(deepsqueak_data)
  expect_error(plotDensitySplitByFrequency(deepsqueak_data), NA)
})

test_that("density stacked by custom plots", {
  data(deepsqueak_data)
  expect_error(plotDensityStackedByCustom(deepsqueak_data), NA)
})

test_that("density split by custom plots", {
  data(deepsqueak_data)
  expect_error(plotDensitySplitByCustom(deepsqueak_data), NA)
})

test_that("density stacked by density plots", {
  data(deepsqueak_data)
  expect_error(plotDensityStackedByDuration(deepsqueak_data), NA)
})

test_that("density split by density plots", {
  data(deepsqueak_data)
  expect_error(plotDensitySplitByDuration(deepsqueak_data), NA)
})

test_that("histogram plots", {
  data(deepsqueak_data)
  expect_error(plotDeltaHistogram(deepsqueak_data), NA)
})

test_that("box plot plots", {
  data(deepsqueak_data)
  expect_error(plotPrincipalBoxplot(deepsqueak_data), NA)
})

test_that("correlation matrix plots", {
  data(deepsqueak_data)
  expect_error(plotCorrelations(deepsqueak_data), NA)
})

test_that("data loads", {
  data(deepsqueak_data)
  expect_error(SqueakR:::loadSpecData(deepsqueak_data), NA)
})

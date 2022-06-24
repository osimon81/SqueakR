test_that("experiment creates", {
  expect_equal(typeof(create_experiment("my_experiment")), "list")
})

test_that("experiment is described", {
  expt <- create_experiment("my_experiment")
  expect_message(describe_experiment(expt))
})

test_that("experiment is updated", {
  expt <- create_experiment("my_experiment")
  expect_error(describe_experiment(expt), NA)
})

test_that("add timepoint data for experiment", {
  expect_error(add_timepoint_data(data_path = "/fake_path_that_doesn't_lead_to_file"))
})

test_that("score data for experiment", {
  expect_error(score_timepoint_data(deepsqueak_data, id = "Mouse_Data.xlsx", group = "Group1", animal = "2301", experimenter = "Exptr 1"), NA)
})

test_that("experiment saves", {
  expt <- create_experiment("my_experiment")
  expect_error(save_experiment(expt, save_path = tempdir()), NA)
})

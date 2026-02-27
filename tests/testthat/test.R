# global.R ----

## Functions ----

test_that("round_to", {
  expect_equal(round_to(23, 5), 25)
  expect_equal(round_to(23, 2.5), 22.5)
})


## UI builders ----

test_that("build_results_dt", {
  expect_silent({
    test_costs <- setNames(c(37, 28, 34), c("1", "2", "3"))
    calc_metrics(
      programs = PROGRAMS$corn,
      costs = test_costs,
      appl_cost = 10,
      inputs = list(yield = 180, price = 5, disease_severity = 0.05)
    ) |>
      build_results_dt(opts = OPTS$corn)
  })
})

# src/crop_module.R ----

test_that("calc_benefit", {
  expect_silent({
    calc_benefit(
      inputs = list(
        yield = 180,
        price = 5,
        disease_severity = 0.05
      ),
      cost = 37,
      params = list(
        b0 = 0.044,
        b1 = -0.028,
        b0_se = 0.017,
        b1_se = 0.015,
        theta = 0.455
      )
    )
  })
})

test_that("calc_metrics", {
  expect_silent({
    test_costs <- setNames(c(37, 28, 34), c("1", "2", "3"))
    calc_metrics(
      programs = PROGRAMS$corn,
      costs = test_costs,
      appl_cost = 10,
      inputs = list(
        yield = 180,
        price = 5,
        disease_severity = 0.05
      )
    )
  })
})


# src/alfalfa_module.R ----

test_that("calc_benefit_alfalfa", {
  expect_silent({
    calc_benefit_alfalfa(
      inputs = list(
        hay_yield = 2,
        hay_price = 100,
        defoliation = 25,
        rfq = 150,
        duration_dummy = 1
      ),
      cost = 20,
      params = PROGRAMS$alfalfa |> slice(1)
    )
  })
})

test_that("calc_metrics_alfalfa", {
  expect_silent({
    test_costs <- setNames(c(37, 28, 34), c("1", "2", "3"))
    calc_metrics_alfalfa(
      programs = PROGRAMS$alfalfa,
      costs = test_costs,
      appl_cost = 10,
      inputs = list(
        hay_yield = 2,
        hay_price = 100,
        defoliation = 25,
        rfq = 150,
        duration_dummy = 1
      )
    )
  })
})

############################ Significance tests ################################

# Load data --------------------------------------------------------------------
models <- c("base", paste0("cx", c(15, 1, 5, 0)))
results <- lapply(
  setNames(nm = models),
  function(x) readRDS(file.path("results", paste0(x, "_np_r.rds")))
)

# Load default parameters values (for rounds0)
source(file.path("scripts", "default_params.R"))

# T-tests ----------------------------------------------------------------------
# Compare to cx0
t_tests0 <- sapply(
  results[-length(results)],
  function(y) {
    op <- t.test(y$opinion$mean[rounds0, ],
                   results$cx0$opinion$mean[rounds0, ],
                   conf.int = TRUE)
    e0 <- t.test(y$echo$echo0[rounds0, ], results$cx0$echo$echo0[rounds0, ],
                   conf.int = TRUE)
    e <- t.test(
      y$echo$echo0[rounds0, ] + y$echo$echo1[rounds0, ],
      results$cx0$echo$echo0[rounds0, ] + results$cx0$echo$echo1[rounds0, ],
      conf.int = TRUE
    )
    list(op = op, e0 = e0, e = e)
  },
  simplify = FALSE
)
# Compare to cx5
t_tests5 <- sapply(
  results[1:3],
  function(y) {
    op <- t.test(y$opinion$mean[rounds0, ],
                 results$cx5$opinion$mean[rounds0, ],
                 conf.int = TRUE)
    e0 <- t.test(y$echo$echo0[rounds0, ], results$cx5$echo$echo0[rounds0, ],
                 conf.int = TRUE)
    e <- t.test(
      y$echo$echo0[rounds0, ] + y$echo$echo1[rounds0, ],
      results$cx5$echo$echo0[rounds0, ] + results$cx5$echo$echo1[rounds0, ],
      conf.int = TRUE
    )
    list(op = op, e0 = e0, e = e)
  },
  simplify = FALSE
)
# Compare to cx1
t_tests1 <- sapply(
  results[1:2],
  function(y) {
    op <- t.test(y$opinion$mean[rounds0, ],
                 results$cx1$opinion$mean[rounds0, ],
                 conf.int = TRUE)
    e0 <- t.test(y$echo$echo0[rounds0, ], results$cx1$echo$echo0[rounds0, ],
                conf.int = TRUE)
    e <- t.test(
      y$echo$echo0[rounds0, ] + y$echo$echo1[rounds0, ],
      results$cx1$echo$echo0[rounds0, ] + results$cx1$echo$echo1[rounds0, ],
      conf.int = TRUE
    )
    list(op = op, e0 = e0, e = e)
  },
  simplify = FALSE
)
# Compare to cx15
t_tests15 <- list(
  op = t.test(results$base$opinion$mean[rounds0, ],
              results$cx15$opinion$mean[rounds0, ],
              conf.int = TRUE),
  e0 = t.test(results$base$echo$echo0[rounds0, ],
              results$cx15$echo$echo0[rounds0, ],
              conf.int = TRUE),
  e = t.test(
    results$base$echo$echo0[rounds0, ] + results$base$echo$echo1[rounds0, ],
    results$cx15$echo$echo0[rounds0, ] + results$cx15$echo$echo1[rounds0, ],
    conf.int = TRUE
  )
)

# Reported in paper ------------------------------------------------------------
# "However, changes in echo chamber formation were not significant for cx ≥ 1
# (minimum p = .228, using Welch independent samples t-tests)"
e1 <- c(cx1_to_base = t_tests1$base$e$p.value,
        cx1_to_cx15 = t_tests1$cx15$e$p.value,
        cx15_to_base = t_tests15$e$p.value)
e1
min(e1)

# "At cx = 0.5, the average echo chamber membership dropped somewhat (from ~95%
# for cx ≥ 1 to 88.9% for cx = 0.5, p < .001)"
e2 <- list(memberships = t_tests5$cx1$e$estimate,
           p.values = c(p1 = t_tests5$cx1$e$p.value,
                        p15 = t_tests5$cx15$e$p.value,
                        pbase = t_tests5$base$e$p.value))
e2
max(e2$p.values)

# "...before dropping substantially to 79.3% for cx = 0 (p < .001 compared to
# cx = 0.5)"
e3 <- c(membership = t_tests0$cx5$e$estimate[2],
        p = t_tests0$cx5$e$p.value)
e3

# "[For opinion,] the distributions for cx ≥ 1 changed little
# (minimum p = .183)"
op1 <- c(cx1_to_base = t_tests1$base$op$p.value,
        cx1_to_cx15 = t_tests1$cx15$op$p.value,
        cx15_to_base = t_tests15$op$p.value)
op1
min(op1)

# "The mean [of opinion] was significantly lower than that of the simulations for
# cx = 1 (p = .009)"
t_tests5$cx1$op$p.value

# ...but was not significantly different for the two higher values of cx
# (minimum p = .120)
op3 <- c(cx15 = t_tests5$cx15$op$p.value, base = t_tests5$base$op$p.value)
op3
min(op3)

# "the mean opinion was substantially lower for cX = 0, however
# (maximum p = 0.001)."
op4 <- sapply(t_tests0, function(x) x$op$p.value)
op4
max(op4)

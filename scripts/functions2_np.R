############################ Model functions ###################################
#
# The functions that run the simulations.
#
# The first function (fun.a) runs each round of the model. For network updating,
# fun.a only selects the edges to create or sever.

# The second function (fun.b) sets up the run, saves outputs and agent values
# for the next round, and updates the network (based on informaiton returned
# from fun.a).
#
# These functions call parameters from the global environment, loaded in prior
# setup code.
#
# (An alternative would be to package parameters into an object and either
# change parameters to take them from the object or use a for loop and assign()
# to recreate the parameters within the function's environment.)
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Run through each round #######################################################

# # Code to allow fun.a to be run line-by-line if necessary for bug fixes or
# # changes
# dt <- dt1
# da <- da1
# sdi <- sdi1
# sdj <- sdj1
# bap <- bap1
# btp <- btp1
# ccp <- ccp1
# bsp <- bsp1
# bag <- bag1
# btg <- btg1
# ck <- ck1
# cb <- cb1
# cx <- cx1
# nu <- nu1
# mu <- mu1
# p1 <- p11
# agents1 <- agents0
# g1 <- g0

fun.a <- function(
  agents1, g1,
  # Defaults take what's in the global environment (if anything).
  dt = dt1, da = da1, sdi = sdi1, sdj = sdj1,
  bap = bap1, btp = btp1, ccp = ccp1, bsp = bsp1,
  bag = bag1, btg = btg1, ck = ck1, cb = cb1,
  cx = cx1, nu = nu1, mu = mu1, p1 = p11, ths = ths1
) {
  # Step 1: Information creation -----------------------------------------------
  ej1 <- rnorm(1, sd = sdj)
  ej0 <- rnorm(1, sd = sdj)
  see <-
    agents1$id[rbinom(length(agents1$id), 1, p1) |> as.logical() |> which()]
  K <- adjacent_vertices(g1, agents1$id) |> sapply(length)
  if (length(see) > 0) {
    # Step 2: Agents share (or not) --------------------------------------------
    agents1$pt1 <- (agents1$at1 - 1/3) / (agents1$Kt1 - 2/3)
    agents1$pt0 <- (agents1$at0 - 1/3) / (agents1$Kt0 - 2/3)
    ET1 <- K * agents1$pt1
    ET0 <- K * agents1$pt0
    agents1$pa1 <- (agents1$aa1 - 1/3) / (agents1$Ka1 - 2/3)
    agents1$pa0 <- (agents1$aa0 - 1/3) / (agents1$Ka0 - 2/3)
    agents1$ps <- (agents1$as - 1/3) / (agents1$as + agents1$bs - 2/3)
    EA1 <- K * agents1$pa1
    EA0 <- K * agents1$pa0
    lmda <- ifelse(
      (agents1$as + agents1$bs) * (abs(agents1$ps - .5) + .5) <= mu,
      1,
      nu
    )
    agents1$U1 <-
      log(EA1 + 1) * lmda * bap * agents1$ps -
      ET1 * btp -
      ccp +
      ej1 +
      rnorm(nrow(agents1), sd = sdi)
    agents1$U0 <-
      log(EA0 + 1) * lmda * bap * (1 - agents1$ps) -
      ET0 * btp -
      ccp +
      ej0 +
      rnorm(nrow(agents1), sd = sdi)
    pub <- see[agents1$U1[agents1$id %fin% see] > 0 |
                 agents1$U0[agents1$id %fin% see] > 0]
  }
  if (length(pub) != 0) {
    share1 <- agents1$id[agents1$U1 > 0 | agents1$U0 > 0]
    # Of those who will share, who are connected to each other?
    g3 <- delete_vertices(g1, agents1$id[!agents1$id %fin% share1])
    # Of those who will share, who are connected to an original sharer?
    groups <- components(g3)
    share <- groups$membership[
      groups$membership %fin%
        groups$membership[names(groups$membership) %fin% pub]
    ] |> names()
    agents3 <- agents1[agents1$id %fin% share, ]
    # Step 3: Agents choose to share constructively or toxically ---------------
    if (sum(!share %fin% pub) > 0) {  # Sharers other than original posters
      # Does the info favour the pro- or anti-science position?
      styp0 <- (agents3$U1 > agents3$U0) * 1
      # Is the info shared agreeably or toxically?
      # Proportion of connections supporting pro-science
      pp <- sapply(
        adjacent_vertices(g1, agents3$id),
        function(x) {
          tmp <- styp0[agents3$id %fin% x$name]
          tmp1 <- c(s1 = sum(tmp == 1), s0 = sum(tmp == 0))
          tmp1[["s1"]] / sum(tmp1)
        }
      )
      # Share A1 or T0?
      px1 <- mapply(
        function(ps0, pp0) {
          ifelse(is.nan(pp0), 0, 2 * cx * (ps0 - .5) * (1 - pp0))
        },
        pp0 = pp, ps0 = agents3$ps
      )
      px1[px1 < 0] <- 0  # Quicker than mins and maxs.
      px1[px1 > 1] <- 1
      # Share A0 or T1?
      px0 <- mapply(
        function(ps0, pp0) {
          ifelse(is.nan(pp0), 0, 2 * cx * (.5 - ps0) * pp0)
        },
        pp0 = pp, ps0 = agents3$ps
      )
      px0[px0 < 0] <- 0
      px0[px0 > 1] <- 1
      x1 <- rbinom(length(px1), 1, px1)
      x0 <- rbinom(length(px0), 1, px0)
      styp <- ifelse(
        # Original share are constructive
        agents3$id %fin% pub, ifelse(styp0 == 1, "A1", "A0"),
        ifelse(styp0 == 1,
               ifelse(x1 == 0, "A1", "T0"),
               ifelse(x0 == 0, "A0", "T1"))
      )
      names(styp) <- share
    } else {
      styp <- ifelse(agents3$U1 > agents3$U0, "A1", "A0")
      names(styp) <- share
    }
    ashare1 <- agents1$id %fin% share[styp == "A1"] * 1
    ashare0 <- agents1$id %fin% share[styp == "A0"] * 1
    txshare1 <- agents1$id %fin% share[styp == "T1"] * 1
    txshare0 <- agents1$id %fin% share[styp == "T0"] * 1
    
    # Who's connected to anyone who sees the info and shares?
    
    # Note: adjacent_vertices() does not play nicely with returning vertex
    #       names (neither `$name` nor `|> names()` works). The best way to
    #       do this seems to be by selecting the vertex names corresponding
    #       to the vertex numbers.
    
    seen0 <-
      V(g1)$name[adjacent_vertices(g1, share) |> unlist() |> unique()] |>
      c(share) |>
      unique() |>
      sort()
    g5 <- delete_vertices(g1, !V(g1)$name %fin% seen0)
    info3 <- data.frame(styp = styp, share = share)
    info4 <- mapply(
      function(ga, sn) {
        a1_n <- (info3$styp[info3$share %fin% names(ga)] == "A1") |>
          as.numeric()
        a0_n <- (info3$styp[info3$share %fin% names(ga)] == "A0") |>
          as.numeric()
        t1_n <- (info3$styp[info3$share %fin% names(ga)] == "T1") |>
          as.numeric()
        t0_n <- (info3$styp[info3$share %fin% names(ga)] == "T0") |>
          as.numeric()
        c(aa1 = (a1_n |> sum()),
          aa0 = (a0_n |> sum()),
          at1 = t1_n |> sum(),
          at0 = t0_n |> sum())
      },
      ga = adjacent_vertices(g5, seen0), sn = seen0
    ) |> t() |> as.data.frame()
    if (((!agents1$id %fin% rownames(info4)) |> sum()) != 0) {
      info <- rbind.data.frame(
        info4,
        data.frame(aa1 = rep(0, sum(!agents1$id %fin% rownames(info4))),
                   aa0 = rep(0, sum(!agents1$id %fin% rownames(info4))),
                   at1 = rep(0, sum(!agents1$id %fin% rownames(info4))),
                   at0 = rep(0, sum(!agents1$id %fin% rownames(info4))),
                   row.names = agents1$id[!agents1$id %fin% rownames(info4)])
      )
      info <- info[order(rownames(info)), ]
    } else {
      info <- info4
    }
  } else {
    info <- data.frame(aa1 = rep(0, length(agents1$id)),
                       aa0 = rep(0, length(agents1$id)),
                       at1 = rep(0, length(agents1$id)),
                       at0 = rep(0, length(agents1$id)),
                       row.names = agents1$id)
    ashare1 <- rep(0, length(agents1$id))
    ashare0 <- rep(0, length(agents1$id))
    txshare1 <- rep(0, length(agents1$id))
    txshare0 <- rep(0, length(agents1$id))
  }
  
  # Step 4: Agents Update ------------------------------------------------------
  agentsout <- data.frame(id = agents1$id, row.names = agents1$id)
  agentsout$bs <- agents1$bs + info$aa0 * lmda / ths
  agentsout$as <- agents1$as + info$aa1 * lmda * ths
  agentsout$at1 <- agents1$at1 * dt + info$at1
  agentsout$at0 <- agents1$at0 * dt + info$at0
  agentsout$aa1 <- agents1$aa1 * da + info$aa1
  agentsout$aa0 <- agents1$aa0 * da + info$aa0
  agentsout$at1 <- ifelse(agentsout$at1 <= 1, 1, agentsout$at1)
  agentsout$at0 <- ifelse(agentsout$at0 <= 1, 1, agentsout$at0)
  agentsout$aa1 <- ifelse(agentsout$aa1 <= 1, 1, agentsout$aa1)
  agentsout$aa0 <- ifelse(agentsout$aa0 <= 1, 1, agentsout$aa0)
  agentsout$Kt1 <- ifelse(agents1$at1 <= 1,
                          agents1$Kt1 + K,
                          agents1$Kt1 * dt + K)
  agentsout$Kt0 <- ifelse(agents1$at0 <= 1,
                          agents1$Kt0 + K,
                          agents1$Kt0 * dt + K)
  agentsout$Ka1 <- ifelse(agents1$aa1 <= 1,
                          agents1$Ka1 + K,
                          agents1$Ka1 * da + K)
  agentsout$Ka0 <- ifelse(agents1$aa0 <= 1,
                          agents1$Ka0 + K,
                          agents1$Ka0 * da + K)
  agentsout$ps <- (agentsout$as - 1/3) / (agentsout$as + agentsout$bs - 2/3)
  agentsout$lmda <- ifelse(
    (agentsout$as + agentsout$bs) * (abs(agentsout$ps - .5) + .5) <= mu,
    1,
    nu
  )
  
  # Step 5: Agents Disconnect --------------------------------------------------
  pt1 <- (agentsout$at1 - 1/3) / (agentsout$Kt1 - 2/3)
  pt0 <- (agentsout$at0 - 1/3) / (agentsout$Kt0 - 2/3)
  pa1 <- (agentsout$aa1 - 1/3) / (agentsout$Ka1 - 2/3)
  pa0 <- (agentsout$aa0 - 1/3) / (agentsout$Ka0 - 2/3)
  # Calculate optimal number of connections
  Ki1 <- ((bag * lmda * agentsout$ps) / ((btg * pt1) + ck)) - (1 / (pa1 + cb))
  Ki0 <- ((bag * lmda * (1-agentsout$ps)) / ((btg * pt0) + ck)) -
    (1 / (pa0 + cb))
  # Calculate optimal number of connections
  DKi1 <- Ki1 - K
  DKi0 <- Ki0 - K
  
  # Agents disconnect from disliked connections --------------------------------
  if (length(pub) > 1) {  # if pub = 1 then it would be constructive.
    txsh1 <- share[styp == "T1"]
    txsh0 <- share[styp == "T0"]
    # Find disconnections, disconnect from pro-science
    if (sum(info$at1 > 0 & DKi1 <= -1) > 0) {
      tc1 <- agentsout$id[info$at1 > 0 & DKi1 <= -1]
      if (length(tc1) == 1) {
        tmp <- txsh1[txsh1 %fin% neighbors(g1, tc1)$name]
        tc2 <- sample(tmp, 1)
      } else {
        tc2 <- sapply(
          adjacent_vertices(g1, tc1),
          function(x) {
            tmp <- txsh1[txsh1 %fin% x$name]
            sample(tmp, 1)
          }
        )
      }
      txcon1 <- cbind(tc1, tc2)
    } else {
      txcon1 <- character()
    }
    # Find disconnections, disconnect from anti-science
    if (sum(info$at0 > 0 & DKi0 <= -1) > 0) {
      tc1 <- agentsout$id[info$at0 > 0 & DKi0 <= -1]
      if (length(tc1) == 1) {
        tmp <- txsh0[txsh0 %fin% neighbors(g1, tc1)$name]
        tc2 <- sample(tmp, 1)
      } else {
        tc2 <- sapply(
          adjacent_vertices(g1, tc1),
          function(x) {
            tmp <- txsh0[txsh0 %fin% x$name]
            sample(tmp, 1)
          }
        )
      }
      txcon0 <- cbind(tc1, tc2)
    } else {
      txcon0 <- character()
    }
    txcon <- rbind(txcon1, txcon0)
    if (ncol(txcon) != 0) {
      txconc <- apply(txcon, 1, function(x) paste0(x, collapse = "|"))
    } else {
      txconc <- list()
    }
  } else {
    txconc <- list()
    txsh1 <- character()
    txsh0 <- character()
  }
  # Step 6: Agents form new connections ----------------------------------------
  if (length(pub) > 0) {
    ash1 <- styp[share][styp[share] == "A1"] |> names()
    ash0 <- styp[share][styp[share] == "A0"] |> names()
    nccand01 <- agentsout$id[DKi1 >= 1 &
                             !agentsout$id %fin% c(txsh1, txsh0) &
                             agentsout$id %fin% seen0]
    nccand00 <- agentsout$id[DKi0 >= 1 &
                             !agentsout$id %fin% c(txsh1, txsh0) &
                             agentsout$id %fin% seen0]
    g4 <- delete_vertices(g1, !V(g1)$name %fin% share)
    groups1 <- components(g4)
    if (length(nccand01) > 0 & length(ash1) > 0) {
      # Group membership of agents seeking connection
      ncon01 <- sapply(
        nccand01,
        function(x) {
          ifelse(x %fin% names(groups1$membership),
                 groups1$membership[[x]],
                 groups1$membership[names(groups1$membership) %fin%
                                      neighbors(g1, x)$name])
        }
      )
      # Group membership of options
      ncon11 <- groups1$membership[ash1]
      # Options in same group
      ncon21 <- mapply(
        function(x, y) {
          tmp <- names(ncon11[ncon11 == x])
          tmp <- tmp[!tmp %fin% y]
          c(y, if (length(tmp) > 0) sample(tmp, 1) else NA)
        },
        x = ncon01, y = names(ncon01)
      ) |> as.data.frame()
      ncon21 <- ncon21[, !is.na(ncon21[2, ]), drop = FALSE]
      adjv0 <- adjacent_vertices(g5, ash1)
      if (length(ash1) > 1) {
        adjv <- do.call(
          cbind,
          mapply(
            function(x, y) {
              matrix(c(x$name, rep(y, length(x))), nrow = 2, byrow = TRUE)
            },
            x = adjv0, y = names(adjv0),
            SIMPLIFY = FALSE
          )
        )
      } else {
        adjv <- matrix(
          c(adjv0[[1]]$name, rep(names(adjv0), length(adjv0[[1]]))),
          nrow = 2, byrow = TRUE
        )
      }
      # None of the new connections match existing ones
      ncon21 <-
        ncon21[, !as.data.frame(ncon21) %fin% as.data.frame(adjv), drop = FALSE]
    } else {
      ncon21 <- character()
    }
    if (length(nccand00) > 0 & length(ash0) > 0) {
      ncon00 <- sapply(
        nccand00,
        function(x) {
          ifelse(x %fin% names(groups1$membership),
                 groups1$membership[[x]],
                 groups1$membership[names(groups1$membership) %fin%
                                      neighbors(g1, x)$name])
        },
        simplify = FALSE
      )
      ncon10 <- groups1$membership[ash0]
      ncon20 <- mapply(
        function(x, y) {
          tmp <- names(ncon10)[ncon10[x] == ncon10]
          tmp <- tmp[!tmp %fin% y]
          c(y, if (length(tmp) > 0) sample(tmp, 1) else NA)
        },
        x = ncon00, y = names(ncon00)
      ) |> as.data.frame()
      ncon20 <- ncon20[, !is.na(ncon20[2, ]), drop = FALSE]
      adjv0 <- adjacent_vertices(g5, ash0)
      if (length(ash0) > 1) {
        adjv <- do.call(
          cbind,
          mapply(
            function(x, y) {
              matrix(c(x$name, rep(y, length(x))), nrow = 2, byrow = TRUE)
            },
            x = adjv0, y = names(adjv0),
            SIMPLIFY = FALSE
          )
        )
      } else {
        adjv <- matrix(
          c(adjv0[[1]]$name, rep(names(adjv0), length(adjv0[[1]]))),
          nrow = 2, byrow = TRUE
        )
      }
      # None of the new connections match existing ones
      ncon20 <-
        ncon20[, !as.data.frame(ncon20) %fin% as.data.frame(adjv), drop = FALSE]
    } else {
      ncon20 <- character()
    }
    if (length(ncon21) > 0) {
      if (length(ncon20) > 0) {
        ncon2 <- cbind(ncon21, ncon20)
      } else {
        ncon2 <- ncon21
      }
    } else {
      if (length(ncon20) > 0) {
        ncon2 <- ncon20
      } else {
        ncon2 <- character()
      }
    }
    # Lower id always in top row so duplicates can be found.
    if (length(ncon2) > 0) {
      ncon2 <- apply(ncon2, 2, function(x) sort(x))
      ncon2 <- unique(ncon2, MARGIN = 2)
    }
    pub1 <- styp[pub][styp[pub] == "A1"] |> names()
    pub0 <- styp[pub][styp[pub] == "A0"] |> names()
  } else {
    ncon2 <- list()
    pub1 <- character()
    pub0 <- character()
  }
  # Save outputs and agents for next round -------------------------------------
  list(agents = agentsout,
       unfriend = txconc,
       newcon = ncon2,
       ashare1 = ashare1,
       ashare0 = ashare0,
       txshare1 = txshare1,
       txshare0 = txshare0,
       pub1 = length(pub1),
       pub0 = length(pub0),
       K = K)
}

# Run through each iteration (network / personality random) ####################

# # Code to allow fun.b to be run line-by-line if necessary for bug fixes or
# # changes.
# g0 <- gf
# pers0 <- persf
# iter <- iter0
# n_agent <- n_agent0
# group_n <- group_n0
# p_in <- p_in0
# p_out <- p_out0
# rounds <- rounds0
# stcon <- stcon0
# shrpp <- shrpp0
# dt1 <- dt0
# da1 <- da0
# sdi1 <- sdi0
# sdj1 <- sdj0
# bap1 <- bap0
# btp1 <- btp0
# ccp1 <- ccp0
# bsp1 <- bsp0
# bag1 <- bag0
# btg1 <- btg0
# ck1 <- ck0
# cb1 <- cb0
# ths1 <- ths0
# cx1 <- cx0
# nu1 <- nu0
# mu1 <- mu0
# p11 <- p10
# gma <- gma0
# at <- at0
# aa <- aa0

# Run through each iteration (network fixed) #####################
fun.b2 <- function(g0, pers0,
  # In general, set these in the global environment
  iter = iter0, n_agent = n_agent0,
  rounds = rounds0, stcon = stcon0, shrpp = shrpp0,
  dt1 = dt0, da1 = da0, sdi1 = sdi0, sdj1 = sdj0,
  bap1 = bap0, btp1 = btp0, ccp1 = ccp0, bsp1 = bsp0,
  bag1 = bag0, btg1 = btg0, ck1 = ck0, cb1 = cb0,
  ths1 = ths0, cx1 = cx0, nu1 = nu0, mu1 = mu0, p11 = p10,
  gma = gma0, at = at0, aa = aa0
) {
  # Setup ----------------------------------------------------------------------
  agents0 <- data.frame(
    id = formatC(1:n_agent,
                 width = nchar(n_agent),
                 format = "d",
                 flag = "0")
    , bs = (stcon / ths1) * 1.5^-pers0$i
    , as = (stcon * ths1) * 1.5^pers0$i
    , at1 = at
    , at0 = at
    , aa1 = aa
    , aa0 = aa
    , Ka1 = aa / shrpp
    , Ka0 = aa / shrpp
  )
  agents0$Kt1 <- agents0$Ka1
  agents0$Kt0 <- agents0$Ka0
  K <- adjacent_vertices(g0, agents0$id) |> sapply(length)
  # Output objects
  opinion1 <- matrix(NA, n_agent, rounds)
  K1 <- matrix(NA, n_agent, rounds)
  a10 <- matrix(NA, n_agent, rounds)
  a11 <- matrix(NA, n_agent, rounds)
  tx10 <- matrix(NA, n_agent, rounds)
  tx11 <- matrix(NA, n_agent, rounds)
  echo <- matrix(NA, n_agent, rounds)
  pub11 <- c(NA, rounds)
  pub10 <- c(NA, rounds)
  lmda1 <- matrix(NA, n_agent, rounds)
  out <- list()
  out$agents <- agents0
  g <- g0
  # Execute --------------------------------------------------------------------
  for (i in 1:rounds) {
    out <- fun.a(
      out$agents, g,
      dt = dt1, da = da1, sdi = sdi1, sdj = sdj1,
      bap = bap1, btp = btp1, ccp = ccp1, bsp = bsp1,
      bag = bag1, btg = btg1, ck = ck1, cb = cb1,
      cx = cx1, nu = nu1, mu = mu1, p1 = p11, ths = ths1
    )
    opinion1[, i] <- out$agents$ps
    K1[, i] <- out$K
    a11[, i] <- out$ashare1
    a10[, i] <- out$ashare0
    tx11[, i] <- out$txshare1
    tx10[, i] <- out$txshare0
    pub11[i] <- out$pub1
    pub10[i] <- out$pub0
    lmda1[, i] <- out$agents$lmda
    
    # Steps 5 & 6 continued ----------------------------------------------------
    if (length(out$unfriend) != 0 & (!is.na(out$unfriend)) |> sum() > 0) {
      g <- delete_edges(g, out$unfriend)
    }
    if (length(out$newcon) != 0) {
      g <- add_edges(g, out$newcon)
    }
    # Step 7: New connections if < 2 -------------------------------------------
    adv0 <- sapply(adjacent_vertices(g, out$agents$id), length)
    adv <- names(adv0[adv0 <= 1])
    count0 <- 1
    while ((length(adv) > 0) & (count0 <= length(adv))) {
      edgesel0 <- adv[count0]
      edgesel0ps <- out$agents[edgesel0, "ps"]
      psd <- (out$agents$ps - edgesel0ps) |> abs()  # ps difference
      psss <- (out$agents$ps >= .5) == (edgesel0ps >= .5)  # ps same side
      neib <- neighbors(g, edgesel0)$name
      edgeselopt <- out$agents$id[(psd < gma | psss) &
                                    !out$agents$id %fin% c(neib, edgesel0)]
      if (length(edgeselopt) > 0) {
        edgesel1 <- sample(edgeselopt, 1)
        g <- g + edge(c(edgesel0, edgesel1))
      } else {
        count0 <- count0 + 1
      }
      adv0 <- sapply(adjacent_vertices(g, out$agents$id), length)
      adv <- names(adv0[adv0 <= 1])
    }
    # Step 8: New connections if none ------------------------------------------
    compon <- components(g)
    while (1 %fin% compon$csize) {
      comord <- compon$csize |> order()
      edgesel0 <- compon$membership[compon$membership == comord[1]] |> names()
      edgesel1 <- sample(out$agents$id[!out$agents$id == edgesel0], 1)
      g <- g + edge(c(edgesel0, edgesel1))
      compon <- components(g)
    }
    # Echo chamber membership
    op <- ifelse(out$agents$ps >= .6, 1, ifelse(out$agents$ps <= .4, -1, 0))
    echo00 <- mapply(
      function(x, y) {
        # Proportion of connections with pro-science view
        m <- (out$agents[y, "ps"] >= .5) |> mean()
        # In pro (1) or anti (-1) science echo chamber or neither (0)?
        ifelse(x == 1 & m >= .9, 1, ifelse(x == -1 & m <= .1, -1, 0))
      },
      x = op, y = adjacent_vertices(g, out$agents$id)
    )
    echo[, i] <- echo00
  }
  # Output ---------------------------------------------------------------------
  list(opinion1 = opinion1,
       K1 = K1,
       a11 = a11,
       a10 = a10,
       tx11 = tx11,
       tx10 = tx10,
       echo = echo,
       pub11 = pub11,
       pub10 = pub10,
       lmda1 = lmda1,
       g0 = g0,
       g = g,
       agents = out$agents,
       final_round = i)
}

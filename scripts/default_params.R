########################### Default parameter values ###########################
#
# Loads the default parameter values.
#
# Copyright CSIRO 2024 under GPL-3.0-or-later.
#
################################################################################

# Overall model parameters -----------------------------------------------------
iter0 <- 200    # Number of models per parameter set
rounds0 <- 200  # Number of iterations of the model

# Network parameters (used in creating the network graphs) ---------------------
n_agent0 <- 500          # Number of agents
group_n <- n_agent0 / 5  # Number of groups
p_in <- 1                # P of connecting with someone in one's group
p_out <- 1/n_agent0      # P of connecting with someone not in one's group

# Model setup parameters -------------------------------------------------------
stcon0 <- 25    # Starting confidence
stcdv0 <- 1.2   # Starting confidence deviation
shrpp0 <- .4    # Expectation of proportion of contacts who will share at start
aa0 <- 20       # Constructive sharing prior.
at0 <- 1        # Toxic prior

# Other parameters -------------------------------------------------------------
dt0 <- .9      # Discount multiplier for toxic interactions
da0 <- .9      # Discount multiplier for agreeable interactions
sdi0 <- .5     # SD of individual randomness of info posting decision
sdj0 <- 0      # SD of information over different rounds (and polarities)
bap0 <- 1      # Utility of agreeable interactions for posting
btp0 <- 10     # Cost of toxicity for posting
ccp0 <- .5     # Fixed cost of creating info
bsp0 <- 5      # Social benefit utility parameter
u0 <- .2       # Difference between U0 and U1 required to post / share. (~0 - .6 are reasonable)
# 'g variables are as for 'p variables only for connections rather than
# posting/sharing
bag0 <- bap0
btg0 <- 2     # People are less sensitive to toxicity for staying connected than sharing
ck0 <- .04    # Cost per con to maintain the con -- theoretical max cons = 25 (1/ck).
cb0 <- .2     # Benefit per con of having the con -- max deduction from cons = 5 (1/cb).
ths0 <- 1.05  # Pro-science bias
cx0 <- 1      # General propensity to interact toxically
nu0 <- .2     # Lost interest reduction
mu0 <- 1000   # Required value of lost interest function to lose interest
p10 <- .1     # Probability of seeing the original news
gma0 <- .1    # Distance connections are allowed on opposite side (for low connection agents)

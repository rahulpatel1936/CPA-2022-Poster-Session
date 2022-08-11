# Post-Hoc Power Analyses  ------------------------------------------------
library(MBESS)
# Honest IM 
pwr::pwr.t2n.test(n1 = 64,
                  n2 = 69,
                  power = NULL,
                  d = 0.18,
                  sig.level = 0.05,
                  alternative = "greater")

?pwr::pwr.t.test

pwr::pwr.t.test(n = NULL,
                d = 0.18,
                sig.level = 0.05,
                power = .90,
                type = "two.sample",
                alternative = "greater")

# Deceptive IM 
pwr::pwr.t2n.test(n1 = 64,
                  n2 = 69,
                  power = NULL,
                  d = 0.15,
                  sig.level = 0.05,
                  alternative = "two.sided")

pwr::pwr.t.test(n = NULL,
                d = 0.15,
                sig.level = 0.05,
                power = .90,
                type = "two.sample",
                alternative = "two.sided")

library(TOSTER)

# Deceptive IM equivalence test 

power_t_TOST(n = c(64, 69),
             power = NULL,
             low_eqbound = -0.10,
             high_eqbound = 0.10,
             alpha = 0.05,
             type = "two.sample")


power_t_TOST(n = NULL,
             power = .90,
             low_eqbound = -0.10,
             high_eqbound = 0.10,
             alpha = 0.05,
             type = "two.sample")


library(dplyr)
library(tidyr)
library(drake)
library(ascertainr)
source("R/ascertainr_util.R")
source("R/ascertainr_plan.R")
source("R/util.R")
### CFR Distribution
cfr_distr <- rbeta(
    10000, shape1 = 319.4389, shape2 = 22872.75
)
## Mean and standard deviation of reporting to death delay
mu_delta <- 10 ## days
std_delta <- 2  ## days
trunc <- 30
date_week_finishing <-  as.Date('29/03/2020', format = '%d/%m/%Y')
drake::drake_config(ascertainr_plan)

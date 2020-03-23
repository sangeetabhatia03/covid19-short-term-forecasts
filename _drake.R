library(dplyr)
library(tidyr)
library(drake)

source("params.R")
source("R/plan.R")
source("R/util.R")

drake_config(plan)

deaths_threshold <- function(ts, Threshold_criterion_7days = 5, Threshold_criterion_prev7days = 5) {

    th1 <- sum(
        ts$Deaths[ts$DateRep >= max(ts$DateRep) - 7], na.rm = TRUE
    ) >=
        Threshold_criterion_7days

    th2 <- sum(
        ts$Deaths[ts$DateRep >= max(ts$DateRep) - 14 &
                  ts$DateRep <= max(ts$DateRep) - 7]
        ) >=
        Threshold_criterion_prev7days

    th1 & th2

}

infile <- "data/COVID-19-geographic-disbtribution-worldwide-2020-03-22.csv"
date_week_finishing <-  as.Date('22/03/2020', format = '%d/%m/%Y')
shape <- 3.16
scale <- 1.52
                                        # hist(rgamma(1e4,shape = shape, scale = scale))
new_params <- epitrix::gamma_shapescale2mucv(
    shape = shape, scale = scale
)

si_mean <- new_params$mu
si_std <- new_params$mu * new_params$cv
## Neil suggested sensitivity analyses with a shorter and a longer time
## window
si_mean <- c(si_mean, 6.48)
si_std <- c(si_std, 3.83)

Threshold_criterion_4weeks <- 100
Threshold_criterion_7days <- 5 ## at least 2 deaths
Threshold_criterion_prev7days <- 5 ## at least 5 deaths in the week before last
outfile <-  paste0('Team.input/data_', date_week_finishing,'.rds')
outfile <- here::here(outfile)


############### Model Outputs ###############################
output_files <- list.files(
    path = "Team.output", pattern = "*rds"
)
output_files <- paste0(
    "Team.output/", output_files
)
names(output_files) <- gsub(
    x = basename(output_files),
    pattern = ".rds",
    replacement = ""
)

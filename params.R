infile <- "data/COVID-19-geographic-disbtribution-worldwide-2020-03-17.csv"
date_week_finishing <-  as.Date('08/03/2020', format = '%d/%m/%Y')
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
Threshold_criterion_7days <- 2 ## at least 2 deaths

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

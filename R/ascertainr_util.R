## Returns file path
infile_week_finishing <- function(date_week_finishing) {

    here::here(
        "Team.input",
        glue::glue("data_{date_week_finishing}.rds")
    )
}

## normalised
report_to_death_distr <- function(mu, std, trunc) {

    out <- EpiEstim::discr_si(seq(0, trunc), mu, std)
    out / sum(out)

}

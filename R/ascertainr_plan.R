ascertainr_plan <- drake::drake_plan(

    ## Read in the innput data prepared by centralised team
    input_data = readr::read_rds(
        drake::file_in(infile_week_finishing(date_week_finishing))
    ),

    ascertainr_deaths = input_data$D_active_transmission,
    ascertainr_cases = input_data$I_active_transmission ,
    ## create a named list, as we use names everywhere from here on.
    countries = purrr::set_names(
        input_data$Country, input_data$Country
    ),

    report_to_death = report_to_death_distr(
        mu = mu_delta, std = std_delta, trunc = trunc
    ),

    weighted_cases  = purrr::map(
        countries,
        function(country) {
            x <- matrix(ascertainr_cases[[country]], ncol = 1)
            ascertainr::weighted_incid(
                incid = x, weights = report_to_death, trunc = trunc
            )
        }
     ),

    deaths_to_cases = purrr::map(
        countries,
        function(country) {
            message(country)
            x <- weighted_cases[[country]]
            deaths <- matrix(ascertainr_deaths[[country]], ncol = 1)
            ascertainr::ratio_deaths_cases(
                wtd_incid = x, deaths = deaths, nsamples = 10000
            )
        }
     ),

    ascertainment = purrr::map(
        countries,
        function(country) {
            ascertainr::ascertainment(
                cfr_distr = cfr_distr,
                death_to_case = deaths_to_cases[[country]]
           )
        }
     ),

    episize_prev = purrr::map(
        countries,
        function(country) {
            ascertainr::true_episize_past(
                deaths = matrix(ascertainr_deaths[[country]], ncol = 1),
                mu_delta = mu_delta,
                cfr_distr = cfr_distr
            )
        }
     ),

    episize_projected = purrr::map(
        countries,
        function(country) {
            ascertainr::true_episize_future(
                cases = ascertainr_cases[[country]],
                rho = ascertainment[[country]]
            )
        }
     ),

)

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

    deaths_to_cases_qntls = purrr::imap_dfr(
        deaths_to_cases,
        function(rit, country) {
            message(country)
            df <- quantiles_to_df(rit)
            scaled_deaths <- ascertainr_deaths[[country]] /
                max(ascertainr_deaths[[country]], na.rm = TRUE)

            scaled_cases <- ascertainr_cases[[country]] /
                max(ascertainr_cases[[country]], na.rm = TRUE)

            ## Align deaths on day t with cases on day t - 10
            scaled_cases <- stats::lag(scaled_cases, n = round(mu_delta))

            df <- cbind(
                date = ascertainr_deaths[["dates"]],
                scaled_deaths = scaled_deaths,
                scaled_cases = scaled_cases,
                df
            )
            df
        }, .id = "country"
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

    ascertainment_qntls = purrr::map_dfr(
        ascertainment,
        function(rho) {
            df <- quantiles_to_df(rho)
            df <- cbind(
                date = ascertainr_deaths[["dates"]],
                df
            )
            df
        }, .id = "country"
     ),


    episize_prev = purrr::map(
        countries,
        function(country) {
            idx <- seq(
                1,
                length(ascertainr_deaths[[country]]) - round(mu_delta)
            )

            ascertainr::true_episize_past(
                deaths = matrix(
                   ascertainr_deaths[[country]][idx], ncol = 1
                ),
                mu_delta = mu_delta,
                cfr_distr = cfr_distr
            )
        }
     ),

    episize_prev_qntls = purrr::imap_dfr(
        episize_prev,
        function(x, country) {
            idx <- seq(
                1,
                length(ascertainr_deaths[[country]]) - round(mu_delta)
            )
            df <- quantiles_to_df(x)
            df <- cbind(
                date = ascertainr_deaths[["dates"]][idx],
                df
            )
            df
        }, .id = "country"
     ),


    episize_projected = purrr::map(
        countries,
        function(country) {
            idx <- seq(
                from = length(
                    ascertainr_cases[[country]]
                ) - round(mu_delta) + 1,
                to = length(ascertainr_deaths[[country]])
            )

            ascertainr::true_episize_future(
                cases = matrix(ascertainr_cases[[country]][idx], ncol = 1),
                rho = ascertainment[[country]][idx, ]
            )
        }
    ),

    episize_projected_qntls = purrr::imap_dfr(
        episize_projected,
        function(x, country) {
            idx <- seq(
                from = length(
                    ascertainr_deaths[[country]]
                ) - round(mu_delta) + 1,
                to = length(ascertainr_deaths[[country]])
            )
            df <- quantiles_to_df(x)
            df <- cbind(
                date = ascertainr_deaths[["dates"]][idx],
                df
            )
            df
        }, .id = "country"
        ),

    ## Effective reproduction number estimates using I_true estimated
    ## above
    ascertainr_rt = purrr::map(
        countries,
        function(cntry) {
            epi_window <- 7
            obs <- dplyr::filter(
                episize_projected_qntls,
                country == cntry
                ) %>% dplyr::select(date, `50.0%`)
            obs[is.na(obs)] <- 0
            epi_res <- purrr::map2(
                input_data$si_mean,
                input_data$si_std,
                function(mu, sd) {
                    si_distr <- EpiEstim::discr_si(
                        0:30, mu, sd
                    )
                    out <- EpiEstim::estimate_R(
                        obs[,2],
                        method = 'non_parametric_si',
                        config = EpiEstim::make_config(
                            list(
                                mean_prior = 1,
                                si_distr = si_distr,
                                t_start = seq(
                                    2, length(obs$date) -  epi_window + 1, 1),
                                t_end = seq(
                                    epi_window + 1, length(obs$date), 1
                                )
                            )
                        )
                    )
                    out$R$t_end <- obs$date[out$R$t_end]
                    out$R
                }
            )
        }
    ),

    ascertainr_rlast <- purrr::map(
        ascertainr_rt,
        function(cntry_rt) {
            out <- purrr::map(
                cntry_rt,
                function(si_rt) { ## Rt for each SI
                    r_samples(si_rt)
                }
            )
        }
     ),
)

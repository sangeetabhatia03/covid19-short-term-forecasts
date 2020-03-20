plan <- drake_plan(

    raw_data = read.csv(
        file_in(here::here(infile)), stringsAsFactors = FALSE
    ) %>%
        mutate_at(
            vars("DateRep"), ~ as.Date(., format = '%d/%m/%Y')
        ) %>%
        ## Manual fixes.
        ## For 2020-03-17, there are two rows for Somalia
        ## one with 0 Cases and one with 1 Cases, Delete one of them
        filter(
            ! (Countries.and.territories == "Somalia" &
               DateRep == "2020-03-17" &
               Cases == 0)
        ),


     by_country_deaths = dplyr::select(
        raw_data, DateRep, Deaths, Countries.and.territories
        ) %>%
         tidyr::spread(
            key = Countries.and.territories, value = Deaths, fill = 0
            ) %>%
        dplyr::filter(DateRep <= date_week_finishing),


    by_country_cases = dplyr::select(
        raw_data, DateRep, Cases, Countries.and.territories
    ) %>%
        tidyr::spread(
            key = Countries.and.territories, value = Cases, fill = 0
     ) %>% dplyr::filter(DateRep <= date_week_finishing),

    ## Filters for cases
    ## Select countries with at least 100 Cases in the last 4 weeks, and
    ## at least 10 Cases in the last week.

    last_4_weeks = by_country_cases[by_country_cases$DateRep >=
                                 max(by_country_cases$DateRep) - 28, ],

    keep = names(
        which(
            colSums(last_4_weeks[, -1]) >= Threshold_criterion_4weeks
        )
    ),

    last_1_week = by_country_deaths[by_country_deaths$DateRep >=
                                max(by_country_deaths$DateRep) - 7,
                                c("DateRep", keep)],

    keep2 = names(
        which(
            colSums(last_1_week[, -1]) >= Threshold_criterion_7days
        )
    ),

    ## For consistency with Pierre's code, rename DateRep to dates
    cases_to_use = by_country_cases[, c("DateRep", keep2)] %>%
        dplyr::rename(dates = "DateRep"),

    deaths_to_use = by_country_deaths[, c("DateRep", keep2)] %>%
        dplyr::rename(dates = "DateRep"),



    out = saveRDS(
        object = list(
            date_week_finishing = date_week_finishing,
            Threshold_criterion_4weeks = Threshold_criterion_4weeks,
            Threshold_criterion_7days = Threshold_criterion_7days,
            I_active_transmission = cases_to_use,
            D_active_transmission = deaths_to_use,
            Country = keep2,
            si_mean = si_mean,
            si_std = si_std
        ),
        file = file_out(outfile)
    ),

#################################################################
#################################################################
####### Part 2, when we have the model outputs ##################
####### check that model outputs have the expected components ###
###### Only keep the ones that have expected components #########
    model_outputs = purrr::map(output_files, readRDS) %>%
        purrr::keep(
            function(x) {
                all(
                    names(x) %in%
                    c("I_active_transmission",
                      ##"D_active_transmission",
                      "Country",
                      "Rt_last",
                      "Predictions")
                )
         }
    ),

    model_predictions_qntls = purrr::map(
        model_outputs,
        function(x) {
            pred <- x[["Predictions"]]
            purrr::map_dfr(pred, function(x) {
                out <- t(
                    apply(
                        x, 2, quantile, prob = c(0.025, 0.1,
                                                 0.5, 0.6, 0.975)
                    )
                )
                as.data.frame(out)
            }, .id = "country"
           )
        }
    )
    ## The full posterioris going to be too big
    ## when we have more models and many countries.
    ## model_predictions = purrr::map_dfr(
    ##     model_outputs,
    ##     ~ dplyr::bind_rows(.x[["Predictions"]], .id = "country"),
    ##     .id = "model"
    ## )



)

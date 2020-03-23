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

    ## Apply thresholds
    pass = split(raw_data, raw_data$`Countries.and.territories`) %>%
        purrr::keep(deaths_threshold) %>%
        dplyr::bind_rows(),

     by_country_deaths = dplyr::select(
        pass, DateRep, Deaths, Countries.and.territories
        ) %>%
         tidyr::spread(
            key = Countries.and.territories, value = Deaths, fill = 0
            ) %>%
        dplyr::filter(DateRep <= date_week_finishing),


    by_country_cases = dplyr::select(
        pass, DateRep, Cases, Countries.and.territories
    ) %>%
        tidyr::spread(
            key = Countries.and.territories, value = Cases, fill = 0
     ) %>% dplyr::filter(DateRep <= date_week_finishing),


    ## For consistency with Pierre's code, rename DateRep to dates
    cases_to_use = dplyr::rename(by_country_cases, dates = "DateRep"),

    deaths_to_use = dplyr::rename(by_country_deaths, dates = "DateRep"),

    Country = colnames(deaths_to_use)[!colnames(deaths_to_use) == "dates"],


    out = saveRDS(
        object = list(
            date_week_finishing = date_week_finishing,
            Threshold_criterion_4weeks = Threshold_criterion_4weeks,
            Threshold_criterion_7days = Threshold_criterion_7days,
            I_active_transmission = cases_to_use,
            D_active_transmission = deaths_to_use,
            Country = Country,
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
            purrr::map_dfr(pred, function(y) {
                out <- t(
                    apply(y,
                          2,
                          quantile,
                          prob = c(0.025, 0.1, 0.4, 0.5, 0.6, 0.9, 0.975)
                    )
                )
                out <- as.data.frame(out)
                out <- tibble::rownames_to_column(out, var = "date")
                out
            }, .id = "country"
           )
        }
    ),
    ## The full posterioris going to be too big
    ## when we have more models and many countries.
    ## model_predictions = purrr::map_dfr(
    ##     model_outputs,
    ##     ~ dplyr::bind_rows(.x[["Predictions"]], .id = "country"),
    ##     .id = "model"
    ## )

  model_rts_qntls = purrr::map(
        model_outputs,
        function(x) {
            out <- t(
                apply(
                    x[["Rt_last"]],
                    2,
                    quantile,
                    prob = c(0.025, 0.1, 0.4, 0.5, 0.6, 0.9, 0.975)
                )
            )
            as.data.frame(out)

        }
     )
  ## Finally render the report
  ## report = rmarkdown::render(
  ##     input = file_in("standardised_weekly_report.Rmd"),
  ##     output_file = "index.html"
  ## )

)

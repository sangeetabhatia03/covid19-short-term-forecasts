plan <- drake_plan(

    raw_data = read.csv(
        file_in(here::here(infile)), stringsAsFactors = FALSE
    ) %>%
        dplyr::mutate_at(
            vars("DateRep"), ~ as.Date(., format = '%d/%m/%Y')
        ) %>%
        ## Manual fixes.
        ## For 2020-03-17, there are two rows for Somalia
        ## one with 0 Cases and one with 1 Cases, Delete one of them
        dplyr::filter(
            ! (Countries.and.territories == "Somalia" &
               DateRep == "2020-03-17" &
               Cases == 0)
        ) %>%
        dplyr::filter(DateRep <= date_week_finishing),


    ## Apply thresholds
    pass = split(raw_data, raw_data$`Countries.and.territories`) %>%
        purrr::keep(deaths_threshold) %>%
        dplyr::bind_rows(),

     by_country_deaths = dplyr::select(
        pass, DateRep, Deaths, Countries.and.territories
        ) %>%
         tidyr::spread(
            key = Countries.and.territories, value = Deaths, fill = 0
            ),
    ## No lines means no cases for that day. That is why fill is 0.
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

####################################################################
####################################################################
####### Part 2, when we have the model outputs #####################
####### check that model outputs have the expected components ######
####### Only keep the ones that have expected components ###########
    model_outputs = purrr::map(output_files, readRDS),

    model_predictions_qntls = purrr::map(
        model_outputs,
        function(x) {
            pred <- x[["Predictions"]]
            purrr::map_dfr(
                pred, extract_predictions_qntls, .id = "country"
            )
        }
     ),

    weekly_predictions_qntls = purrr::map_dfr(
        model_outputs,
        function(x) {
            pred <- x[["Predictions"]]
            purrr::imap_dfr(pred, function(y, country) {

                dates <- as.Date(colnames(y[[1]]))
                obs_deaths <- dplyr::filter(
                    pass,
                    `Countries.and.territories` == country &
                    DateRep %in% as.Date(dates)
                    )
                if (nrow(obs_deaths) == 0) {
                    message(
                        "No observations for dates ", dates, " in ", country
                    )
                    obs_deaths <- NA
                } else {
                    obs_deaths <- sum(obs_deaths$Deaths)
                }

                weekly_df <- daily_to_weekly(y)

                weekly_df$observed <- obs_deaths
                weekly_df

            }, .id = "country"
           )
        }, .id = "model"
     ),


    model_rt_qntls = purrr::map(
        model_outputs,
        function(x) {
            pred <- x[["R_last"]]
            purrr::map_dfr(pred, function(y) {
                names(y) <- c("si_1", "si_2")
                out <- purrr::map_dfr(
                    y,
                    function(y_si) {
                        out2 <- quantile(
                            y_si,
                            prob = c(0.025, 0.1, 0.4, 0.5, 0.6, 0.9, 0.975)
                        )
                        out2 <- as.data.frame(out2)
                        out2 <- tibble::rownames_to_column(
                            out2,  var = "quantile"
                        )
                    }, .id = "si"
                )
                out
            }, .id = "country"
           )
        }
    ),
    ## Model 1 Rt Estimates
    model_1_rt =  model_rt_qntls[grep(
        pattern = "RtI0", x = names(model_rt_qntls)
    )] %>%
        dplyr::bind_rows(.id = "proj") %>%
    tidyr::spread(key = quantile, value = out2),


    model_2_rt =  model_rt_qntls[grep(
        pattern = "sbkp", x = names(model_rt_qntls)
    )] %>%
        dplyr::bind_rows(.id = "proj") %>%
        tidyr::spread(key = quantile, value = out2),

    formatted_weekly_predictions_qntls = split(
        weekly_predictions_qntls,
        weekly_predictions_qntls$si
    ) %>%
        purrr::imap(
            function(x, si_name) {
                out <- format_weekly_pred(x)
                out$model <- x$model
                out$`Observed Deaths` <- x$observed
                ## Get Rt Estimates for this SI and this country
                rt <- purrr::map_dfr(
                    model_rt_qntls,
                    function(y) y[(y$si == si_name), ],
                    .id = "model"
                )
                rt <- format_last_rt(rt)
                out <- dplyr::left_join(x = out, y = rt)
                out$Country <- snakecase::to_any_case(
                    as.character(out$Country),
                    case = "title"
                )

                out <- dplyr::arrange(out, Country)
                out
            }
    ),

    weeks_ending = list(
        "2020-03-08" = "2020-03-08",
        "2020-03-15" = "2020-03-15",
        "2020-03-22" = "2020-03-22"
    ),
    ## For each, for each country, pool projections from diff models
    ## In the output, the first level is week, 2nd is country and 3rd
    ## is SI.
    ensemble_model_predictions = purrr::map(
        weeks_ending,
        function(week) {
            idx <- grep(x = names(model_outputs), pattern = week)
            outputs <- purrr::map(model_outputs[idx], ~ .[["Predictions"]])
            ## First Level is model, 2nd is country, 3rd is SI.
            countries <- names(outputs[[1]])
            names(countries) <- countries
            purrr::map(
                countries,
                function(country) {
                    ## y is country specific output
                    y <- purrr::map(outputs, ~ .[[country]])
                    ## y has 2 components, one for each SI.
                    y_1 <- purrr::map(y, ~ .[[1]]) ## si_1
                    y_2 <- purrr::map(y, ~ .[[2]]) ## si_1

                    out = list(
                        pool_predictions(y_1),
                        pool_predictions(y_2)
                    )
                }
            )
        }
    ),

    ensemble_model_rt = purrr::map_dfr(
        weeks_ending,
        function(week) {
            idx <- grep(x = names(model_outputs), pattern = week)
            outputs <- purrr::map(model_outputs[idx], ~ .[["R_last"]])
            ## First Level is model, 2nd is country, 3rd is SI.
            countries <- names(outputs[[1]])
            names(countries) <- countries
            purrr::map_dfr(
                countries,
                function(country) {
                    ## y is country specific output
                    y <- purrr::map(outputs, ~ .[[country]])
                    ## y has 2 components, one for each SI.
                    ## Determine quantiles
                    probs <- c(0.025, 0.5, 0.975)
                    y_1 <- purrr::map(y, ~ .[[1]]) ## si_1
                    y_1 <- quantile(
                        unlist(y_1), probs = probs
                    )
                    y_1 <- tibble::rownames_to_column(
                        data.frame(out2 = y_1), var = "quantile"
                    )
                    y_1$si <- "si_1"

                    y_2 <- purrr::map(y, ~ .[[2]]) ## si_1
                    y_2 <- quantile(
                        unlist(y_2), probs = probs
                    )
                    y_2 <- tibble::rownames_to_column(
                        data.frame(out2 = y_2), var = "quantile"
                    )
                    y_2$si <- "si_2"
                    rbind(y_1, y_2)

                }, .id = "country"
            )
        }, .id = "model" ## this is really week ending, but to be able to resue prev code, i am calling it model
    ),

    ensemble_model_qntls = purrr::map_dfr(
        ensemble_model_predictions,
        function(pred) {
            purrr::map_dfr(
                pred, extract_predictions_qntls, .id = "country"
            )
        }, .id = "proj"
    ),

    ensemble_weekly_qntls = purrr::map_dfr(
        ensemble_model_predictions,
        function(pred) {
             purrr::map_dfr(
                pred, daily_to_weekly, .id = "country"
           )
        }, .id = "proj"
    ),

    fmtd_ensemble_weekly_qntls = split(
        ensemble_weekly_qntls,
        ensemble_weekly_qntls$si
    ) %>% purrr::map(function(x) {

        this_si <- x$si[1]
        ## Get observed number of deaths calculated earlier.
        y <- dplyr::select(
            weekly_predictions_qntls,
            week_ending,
            country,
            Observed = observed
        )
        y <- dplyr::distinct(y)
        x <- format_weekly_pred(x)
        x$`Week Ending` <- as.Date(x$`Week Ending`)
        y$week_ending <- as.Date(y$week_ending)
        x <- dplyr::left_join(
            x,
            y,
            by = c("Country" = "country",
                   "Week Ending" = "week_ending")
        )
        x <- dplyr::arrange(x, Country)

        ## Get R_t estimates for this country and this
        ## Week.
        rt <- dplyr::filter(ensemble_model_rt, si == this_si)
        rt <- format_last_rt(rt)
        rt$model <- as.Date(rt$model)
        x <- dplyr::left_join(
            x,
            rt,
            by = c("Week Ending" = "model",
                   "Country" = "Country")
            )
        x$Country <- snakecase::to_any_case(
            as.character(x$Country),
            case = "title"
        )

        x
    }),

    ## Model 1. Name contains RtI0
    model_1 =  model_predictions_qntls[grep(
        pattern = "RtI0", x = names(model_predictions_qntls)
    )] %>% dplyr::bind_rows(.id = "proj") %>%
    dplyr::mutate_at(vars("date"), as.Date),

    ## Model 2. Names contains sbkp
    model_2 =  model_predictions_qntls[grep(
        pattern = "sbkp",
        x = names(model_predictions_qntls)
    )] %>%
        dplyr::bind_rows(.id = "proj") %>%
    dplyr::mutate_at(vars("date"), as.Date),


    obs = dplyr::rename(
        pass, country = "Countries.and.territories"
    ) %>%
        dplyr::mutate_at(vars("DateRep"), as.Date),


 ##    ## The full posterioris going to be too big
 ##    ## when we have more models and many countries.
 ##    ## model_predictions = purrr::map_dfr(
 ##    ##     model_outputs,
 ##    ##     ~ dplyr::bind_rows(.x[["Predictions"]], .id = "country"),
 ##    ##     .id = "model"
 ##    ## )


  ##Finally render the report
  report = rmarkdown::render(
      input = knitr_in("standardised_weekly_report.Rmd"),
      output_file = "index.html",
      output_format = rmarkdown::html_document(css = file_in("style.css"))
  )

)

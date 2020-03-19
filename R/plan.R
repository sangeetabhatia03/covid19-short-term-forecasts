plan <- drake_plan(

    date_week_finishing =  as.Date('08/03/2020',format = '%d/%m/%Y'),

    raw_data = readr::read_csv(
        file_in("data/COVID-19-geographic-disbtribution-worldwide-2020-03-17.csv")
        ) %>%
        janitor::clean_names() %>%
        dplyr::mutate_at(vars("date_rep"), ~ as.Date(., format = '%d/%m/%Y')),

    ## Manual fixes.
    ## For 2020-03-17, there are two rows for Somalia
    ## one with 0 cases and one with 1 cases, Delete one of them

    raw_data = filter(
        raw_data,
        ! (countries_and_territories == "Somalia" &
           date_rep == "2020-03-17" &
           cases == 0)
    ),

    by_country_ts = dplyr::select(
        raw_data, date_rep, cases, countries_and_territories
    ) %>%
        tidyr::spread(
            key = countries_and_territories, value = cases, fill = 0
     ) %>% dplyr::filter(date_rep <= date_week_finishing)

    ## Filters.
    ## Select countries with at least 100 cases in the last 4 weeks, and
    ## at least 10 cases in the last week.


    last_4_weeks = by_country_ts[by_country_ts$date_rep >=
                                 max(by_country_ts$date_rep) - 28, ],

    keep = names(
        which(
            colSums(last_4_weeks[, -1]) >= 100
        )
    ),

    last_1_week = by_country_ts[by_country_ts$date_rep >=
                                max(by_country_ts$date_rep) - 7,
                                c("date_rep", keep)],

    keep = names(
        which(
            colSums(last_1_week[, -1]) >= 10
        )
    ),


    data_to_use = by_country_ts[, c("date_rep", keep)],

)

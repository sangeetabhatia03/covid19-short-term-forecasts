projection_plot <- function(obs, pred) {

    ## Number of projections
    nprojs <- length(unique(pred$proj))

    ## Latest projections get a different color
    palette <- c(
        rep("#98984d", nprojs - 1),
        "#b3669e"
    )
    names(palette) <- unique(pred$proj)

    date_min <- as.Date("2020-03-01")
    date_max <- max(pred$date) + 2
    dates_to_mark <- seq(
        from = date_min,
        to = date_max,
        by = "1 day"
    )
    dates_to_mark <- dates_to_mark[weekdays(dates_to_mark) == "Monday"]
    ## Get dates of adding vlines.
    window_eps <- dplyr::group_by(pred, proj) %>%
        dplyr::summarise(date = min(date)) %>%
        dplyr::ungroup()

    window_eps$xintercepts <- as.numeric(
        window_eps$date - 1
    ) + 0.5
    ## To get nice labels
    ## https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
    integer_breaks <- function(n = 5, ...) {
        fxn <- function(x) {
            breaks <- floor(pretty(x, n, ...))
            names(breaks) <- attr(breaks, "labels")
            breaks
        }
        return(fxn)
    }

    p <- ggplot() +
    geom_point(data = obs, aes(dates, deaths)) +
        geom_line(
            data = pred,
            aes(date, `50%`, col = proj, group = proj)
        ) +
        geom_ribbon(
            data = pred,
            aes(x = date,
                ymin = `2.5%`,
                ymax = `97.5%`,
                fill = proj,
                group = proj),
            alpha = 0.4) +
    scale_color_manual(
        values = palette,
        aesthetics = c("color", "fill")
    ) +
    theme_pubr() +
    theme(legend.position = "none") +
        scale_x_date(breaks = dates_to_mark, limits = c(date_min, date_max)) +
        scale_y_continuous(breaks = integer_breaks()) +
    geom_vline(
        xintercept = c(
           window_eps$xintercepts
        ),
        linetype = "dashed"
    ) + xlab("") +
        ylab("Deaths") +
        theme(
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.text.x =
                element_text(angle = -90, hjust = 0, size = 20),
            strip.text.x = element_text(
                margin = margin(2,0,2,0, "pt"),
                size = 20
            )
          )

    p
}


rt_plot <- function(rt) {


    nice_names <- snakecase::to_any_case(
        rt$country,
        "title"
    )
    names(nice_names) <- rt$country
    rt$country <- reorder(rt$country, -rt$`50%`)
    if (length(unique(rt$model)) == 1) width <- 0.1
    else width <- 0.7

    p <- ggplot() +
        geom_errorbar(
            data = rt,
            aes(x = country, ymin = `2.5%`, ymax = `97.5%`, col = model),
            position = position_dodge(width = width),
            size = 1.1
        ) +
        geom_point(
            data = rt,
            aes(x = country, y = `50%`, col = model),
            position = position_dodge(width = 0.5),
            size = 4
            ) +
        theme_pubr() +
        xlab("") +
        ylab("Effective Reproduction Number") +
        scale_x_discrete(labels = nice_names) +
        theme(
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.text.x =
                element_text(angle = -90, hjust = 0, size = 20),
            legend.position = "none"
        ) +
        geom_hline(
            yintercept = 1,
            linetype = "dashed"
        )

    p
}



######################################################################
## Extract model quantiles
## Each model output is a list with several elements,
## the key one is Predictions.
## This is a list of data.frames with one data.frame for each country
## For each country, we have a list of 2 components, corresponding
## to the 2 serial intervals being considered.
## it is this last list (country, 2 components) that is passed to
## this function.
extract_predictions_qntls <- function(y) {

    names(y) <- paste0("si_", seq_along(y))
    out <- purrr::map_dfr(
        y,
        function(y_si) {
            out2 <- t(
                apply(y_si,
                      2,
                      quantile,
                      prob = c(0.025, 0.1, 0.4, 0.5, 0.6, 0.9, 0.975)
                      )
            )
            out2 <- as.data.frame(out2)
            out2 <- tibble::rownames_to_column(out2, var = "date")
            out2

        }, .id = "si"
    )
    out
}

######################################################################
## Extract model quantiles
## Each model output is a list with several elements,
## the key one is Predictions.
## This is a list of data.frames with one data.frame for each country
## For each country, we have a list of 2 components, corresponding
## to the 2 serial intervals being considered.
## it is this last list (country, 2 components) that is passed to
## this function.
daily_to_weekly <- function(y) {

    names(y) <- paste0("si_", seq_along(y))
    out <- purrr::map_dfr(
        y,
        function(y_si) {
            weekly <- rowSums(y_si)
            weekly <- quantile(
                weekly,
                prob = c(0.025, 0.1, 0.4, 0.5, 0.6, 0.9, 0.975)
            )
            weekly_df <- as.data.frame(weekly)
            ## This is not the last date for which predictions are
            ## available, but the last date for which observations are
            ## available.
            weekly_df$week_ending <- as.Date(colnames(y_si)[1]) - 1

            weekly_df <- tibble::rownames_to_column(
                weekly_df, var = "quantile"
            )

            weekly_df<- tidyr::spread(
                weekly_df, key = quantile, value = weekly
            )
            weekly_df

        }, .id = "si"
    )
    out

}

###
### Pool model outpus together based on some weights
### outputs is a list of model predictions
### No checks are carried out in this function, so make sure
### that predictions being pooled are for the same dates etc when
### this function is called.
pool_predictions <- function(outputs, weights = 1) {

    wtd_outputs <- purrr::map2(outputs, weights, function(x, w) x * w)
    out <- Reduce('rbind', wtd_outputs)
    out

}

## x is a data.frame of weekly predictions
format_weekly_pred <- function(x) {

    x <- dplyr::mutate_if(x, is.numeric, ~ signif(., 3))
    x <- dplyr::mutate_if(x, is.numeric, ~prettyNum(., big.mark = ","))

    out <- data.frame(
        Country = x$country,
        `Week Ending` = x$week_ending,
        `Predicted Deaths` = glue::glue(
             "{x$`50%`} ({x$`2.5%`} - {x$`97.5%`})",
        ),
        check.names = FALSE
    )

    out
}

## rt in tall format
format_last_rt <- function(rt) {

    rt <- dplyr::select(rt, -si)
    rt <- rt[rt$quantile %in% c("2.5%", "50%", "97.5%"), ]
    rt <- tidyr::spread(rt, quantile, out2)
    rt <- dplyr::arrange(rt, `50%`)
    rt <- dplyr::mutate_if(
        rt,
        is.numeric,
        ~ format(round(., 2), nsmall = 1)
    )
    rt$`R_t` <- glue::glue(
        "{rt$`50%`} ({rt$`2.5%`} - {rt$`97.5%`})"
    )
    rt <- dplyr::select(rt, model, Country = country, `R_t`)
    rt

}

## mat is T X N where N is the number of samples
quantiles_to_df <- function(mat, probs = c(0.025, 0.50, 0.975)) {

    qntls <- t(apply(mat, 1, quantile, probs = probs, na.rm = TRUE))
    out <- data.frame(qntls)
    colnames(out) <- scales::percent(probs, accuracy = 0.1)
    out

}

r_samples <- function(res, n = 10000) {

    in_last_window <- tail(res, 1)
    mean_r <- in_last_window$`Mean(R)`
    std_r <- in_last_window$`Std(R)`
    reparam <- epitrix::gamma_mucv2shapescale(
            mu = mean_r,
            cv = std_r / mean_r
    )
    out <- rgamma(
        n = n,
        shape = reparam$shape,
        scale = reparam$scale
    )
    out

}


deaths_threshold <- function(ts,
                             Threshold_criterion_7days = 10,
                             Threshold_criterion_prev7days = 10) {

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

## Paarmeters for data preparation
## week_finishing in the form yyy-mm-dd
parameters <- function(week_finishing) {

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

    params <- list(
        infile = glue::glue(
            "data/COVID-19-geographic-disbtribution-worldwide-",
            "{week_finishing}.csv"
        ),
        si_mean = si_mean,
        si_std = si_std,
        threshold_criterion_7days = 10,
        Threshold_criterion_prev7days = 10,
        outfile = here::here(
            glue::glue("Team.input/data_{week_finishing}.rds")
        )
    )

    params

}

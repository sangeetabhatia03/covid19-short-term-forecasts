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

    ## Get dates of adding vlines.
    window_eps <- dplyr::group_by(pred, proj) %>%
        dplyr::summarise(date = min(date)) %>%
        dplyr::ungroup()

    window_eps$xintercepts <- as.numeric(
        window_eps$date - 1
    ) + 0.5
    ## To get nice labels

    nice_names <- snakecase::to_any_case(
        pred$country,
        "title"
    )
    names(nice_names) <- pred$country

    p <- ggplot() +
    geom_point(data = obs, aes(DateRep, Deaths)) +
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
        facet_wrap(
            ~country,
            scales = "free_y",
            labeller = as_labeller(
                nice_names
            ),
            ncol = 2
        ) +
    scale_color_manual(
        values = palette,
        aesthetics = c("color", "fill")
    ) +
    theme_pubr() +
    theme(legend.position = "none") +
        xlim(date_min, date_max) +
    geom_vline(
        xintercept = c(
           window_eps$xintercepts
        ),
        linetype = "dashed"
    ) + xlab("") +
        theme(
            axis.text.y = element_text(size = 8),
            axis.text.x =
                element_text(angle = -90, hjust = 0),
            strip.text.x = element_text(margin = margin(2,0,2,0, "pt"))

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
    p <- ggplot() +
        geom_errorbar(
            data = rt,
            aes(x = country, ymin = `2.5%`, ymax = `97.5%`, col = model),
            position = position_dodge(width = 0.5)
        ) +
        geom_point(
            data = rt,
            aes(x = country, y = `50%`, col = model),
            position = position_dodge(width = 0.5)
            ) +
        theme_pubr() +
        xlab("") +
        ylab("Effective Reproduction Number") +
        theme(legend.position = "none") +
        scale_x_discrete(labels = nice_names) +
        theme(
            axis.text.x =
                element_text(angle = -90, hjust = 0),
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

    x <- dplyr::mutate_if(x, is.numeric, ~ round(., 2))

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
    rt <- dplyr::mutate_if(
        rt,
        is.numeric,
        ~ round(., 2)
    )
    rt$`R_t` <- glue::glue(
        "{rt$`50%`} ({rt$`2.5%`} - {rt$`97.5%`})"
    )
    rt <- dplyr::select(rt, model, Country = country, `R_t`)
    rt

}

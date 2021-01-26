plot.change <- function(change, method_levels= c("anomaly","trend","observed")){
  change.plot <- change %>% tidyr::pivot_longer(names_to="type", names_prefix = "change_str_", values_to="value",
                                                c(change_str_trend, change_str_anomaly, change_str_observed)) %>%
    mutate(value=readr::parse_number(value, na = c("", "NA", "NANA"))/100)

  change.plot$location_id <- factor(change.plot$location_id,
                                    levels(reorder(change.plot[change.plot$type==method_levels[1],]$location_id,
                                                   -change.plot[change.plot$type==method_levels[1],]$value)))

  change.plot$type <- factor(change.plot$type, method_levels)

  ggplot(change.plot) +
    geom_bar(aes(y=location_id, x=value, fill=type),
             stat="identity",
             position=position_dodge2(reverse = TRUE)
    ) +
    # theme(guide_legend(reverse=T)) +
    rcrea::theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d(name="Method") +
    labs(x=NULL,y=NULL) +
    scale_x_continuous(labels=scales::percent)

  ggsave(file.path("results","plots",
  paste0("change.",method_levels[1],".png")), width=8, height=6)
}

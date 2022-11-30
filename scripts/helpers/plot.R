#
# TV_family <- function(tvd, dataset) {
#
#   df <- data.frame(names(tvd), Reduce(rbind, tvd))
#   names(df) <- c("Measure", "Value", "StdDev")
#
#   rename <- list(
#     TV = TeX("TV_{x_0, x_1}(y)"),
#     TE = TeX("TE_{x_0, x_1}(y)"),
#     ExpSE_x1 = TeX("Exp-SE_{x_1}(y)"),
#     ExpSE_x0 = TeX("Exp-SE_{x_0}(y)"),
#     NDE = TeX("NDE_{x_0, x_1}(y)"),
#     NIE = TeX("NIE_{x_1, x_0}(y)"),
#     ETT = TeX("ETT_{x_0, x_1}(y | x_0)"),
#     DE = TeX("DE_{x_0, x_1}(y | x_0)"),
#     IE = TeX("IE_{x_1, x_0}(y | x_0)"),
#     SE = TeX("SE_{x_1, x_0}(y)")
#   )
#
#   df$Measure <- factor(df$Measure, levels = names(rename))
#
#   ggplot(df, aes(x = Measure, y = Value, fill = Measure)) + geom_col() +
#     theme_minimal() +
#     geom_errorbar(
#       aes(x = Measure, ymin = Value - 1.96*StdDev, ymax = Value + 1.96*StdDev),
#       color = "black", width = 0.5
#     ) +
#     theme(
#       legend.position = "none",
#       axis.text = element_text(size = 16),
#       axis.title.x = element_text(size = 18),
#       axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
#       title = element_text(size = 20)
#     ) + scale_x_discrete(labels = parse(text = unlist(rename))) +
#     xlab("Causal Fairness Measure") +
#     ggtitle(TeX(paste0("Decomposition of total variation TV_{x_0, x_1}(y): ",
#                        str_to_title(dataset), " dataset")))
#
# }

multi_plot <- function(..., labels = NULL, ncol = 1L) {

  plotlist <- list(...)
  y_range <- lapply(plotlist, function(obj) layer_scales(obj)$y$range$range)
  y_range <- Reduce(rbind, y_range)
  ylims <- c(min(y_range[, 1]), max(y_range[, 2]))

  plotlist <- lapply(plotlist, function(obj) obj + ylim(ylims))

  cowplot::plot_grid(plotlist = plotlist, labels = labels, ncol = ncol,
                     label_size = 20)

}


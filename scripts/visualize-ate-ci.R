library(ggplot2)
library(reshape2)
library(gganimate)
library(data.table)

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

grid <- c(100, 200, 300, 400, 500, 750, 1000, 2000, 3000, 5000, 7500, 10000,
          12500, 15000, 20000, 25000, 30000, 50000)

df <- NULL
for(i in seq_along(grid)) {
  
  load(file.path(root, "results", paste0("est_", i, ".RData")))
  est <- cbind(est, nsize = grid[i])
  df <- rbind(df, est)
}

# evolution over sample size
ggplot(melt(df, id.vars = "nsize"), aes(x = value, fill = variable)) +
  geom_density(alpha = 0.4) + theme_bw() +
  xlab("ATE estimate") + xlim(c(1/3, 1)) +
  theme(
    legend.position = c(0.8, 0.8),
    legend.box.background = element_rect()
  ) + geom_vline(xintercept = 2/3, color = "red", linetype = "dashed") +
  transition_time(nsize) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

df <- as.data.table(df)

ggplot(
  melt(df[, lapply(.SD, sd), by = "nsize"], id.vars = "nsize"),
  aes(x = log(nsize), y = value, color = variable)
) + geom_line() + theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.box.background = element_rect()
  ) +
  xlab("log(sample size)") + ylab("Confidence interval width")
library(latex2exp)
library(ggplot2)
library(scales)

root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

dat <- get_data("adult")

a1 <- ggplot(dat, aes(x = age, fill = sex)) +
  geom_density(alpha = 0.4) + theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.box.background = element_rect()
  ) + ggtitle(TeX("Adult: age $\\perp$ sex rejected (p < 0.001)"))

wilcox.test(dat[dat$sex == "Female", ]$age, dat[dat$sex == "Male", ]$age)

a2 <- ggplot(dat, aes(x = sex, fill = race)) +
  geom_bar(position = "fill") +
  geom_text(aes(label = percent(round(..count../tapply(..count.., ..x.., sum)[..x..], 4))),
            stat = "count", position = position_fill(0.5)) +
  scale_y_continuous(labels = percent) + ylab("proportion") +
  theme_minimal() + ggtitle(TeX("Adult: race $\\perp$ sex rejected (p < 0.001)"))

chisq.test(table(dat$race, dat$sex))

dat <- get_data("compas")

c1 <- ggplot(dat, aes(x = age, fill = race)) +
  geom_density(alpha = 0.4) + theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.box.background = element_rect()
  ) + ggtitle(TeX("COMPAS: age $\\perp$ race rejected (p < 0.001)"))

wilcox.test(dat[dat$race == "White", ]$age, dat[dat$race == "Non-White", ]$age)

c2 <- ggplot(dat, aes(x = race, fill = sex)) +
  geom_bar(position = "fill") +
  geom_text(aes(label = percent(round(..count../tapply(..count.., ..x.., sum)[..x..], 4))),
            stat = "count", position = position_fill(0.5)) +
  scale_y_continuous(labels = percent) + ylab("proportion") +
  theme_minimal() + ggtitle(TeX("COMPAS: race $\\perp$ sex rejected (p < 0.001)"))

cowplot::plot_grid(c1, c2, a1, a2, ncol = 2L)
ggsave(file.path(root, "misc", "paper", "figures", "independence-tests.png"),
       height = 8, width = 12, bg = "white")

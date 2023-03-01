
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

n <- 1000000
X <- rbinom(n, 1, 0.1)
Z <- rbinom(n, 1, 0.3)
W <- rnorm(n) - 5.5 * (Z == 0) * X

df <- data.frame(X, Z = factor(ifelse(Z, "Z = 1 (rich)", "Z = 0 (poor)")), W)
head(df)

ggplot(df, aes(x = W, color = factor(X), fill = factor(X))) +
  geom_density(alpha = 0.3) + theme_bw() +
  facet_grid(rows = vars(Z)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16),
        title = element_text(size = 16)) +
  xlab("SAT scores (W)") + ylab("") +
  scale_fill_discrete(name = "Group",
                      labels = c(TeX("Majority ($x_1$)"), TeX("Minority ($x_0$)"))) +
  scale_color_discrete(guide = "none") +
  ggtitle("SAT scores by race and SE status")

ggsave(file.path(root, "misc", "paper", "figures", "who-is-who.png"),
       height = 4, width = 5.33)

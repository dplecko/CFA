
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
        legend.position = "bottom") +
  xlab("SAT scores") + ylab("") + 
  scale_fill_discrete(name = "Group", labels = c("Majority", "Minority")) +
  scale_color_discrete(guide = "none") +
  ggtitle("SAT scores stratified by race and socio-economic status")

ggsave(file.path(root, "misc", "paper", "figures", "who-is-who.png"), 
       height = 6, width = 8)

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

DNase %>% 
ggplot( aes(x=-conc, y=density/3, )) + 
  geom_point( size = 1,alpha=0.2 ) +
  binomial_smooth(se=FALSE, lwd=2,alpha=0.75,fullrange=TRUE)+
  theme_classic()


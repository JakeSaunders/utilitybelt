binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

DNase %>% 
ggplot( aes(x=-conc, y=density/3, )) + 
  geom_point( size = 1,alpha=0.2 ) +
  binomial_smooth(se=FALSE, lwd=2,alpha=0.75,fullrange=TRUE)+
  theme_classic()

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}


binomial_smooth(se=FALSE, lwd=2,alpha=0.75,fullrange=TRUE)



# 1. Linear Curve (Gaussian family with identity link)
set.seed(123)
x_linear <- 1:20
y_linear <- 2 * x_linear + 5 + rnorm(20, 0, 5)
data_linear <- data.frame(x = x_linear, y = y_linear)

plot_linear <- ggplot(data_linear, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "gaussian")) +
  labs(title = "1) Linear Curve (Gaussian Family)")

print(plot_linear)

# 2. S-Shaped Curve (Binomial family with logit link)
set.seed(123)
x_logistic <- seq(-5, 5, length.out = 100)
probability <- 1 / (1 + exp(-x_logistic))
y_logistic <- rbinom(100, 1, probability)
data_logistic <- data.frame(x = x_logistic, y = y_logistic)

plot_logistic <- ggplot(data_logistic, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "2) S-Shaped Curve (Binomial Family)")

print(plot_logistic)

# 3. Logarithmic Curve (Gaussian family with log link)
set.seed(123)
x_log <- seq(0.1, 10, length.out = 100)
y_log <- (2 + 3 * log(x_log) + rnorm(100, 0, 1)) + 10
data_log <- data.frame(x = x_log, y = y_log)

plot_log <- ggplot(data_log, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "gaussian")) +
  labs(title = "3) [Error] Logarithmic Curve (Gaussian Family, Log Link)")

print(plot_log)


# 4. Exponential-like Curve (Gamma family with inverse link)
set.seed(123)
x_exp <- 1:20
y_exp <- 1 / (0.1 + 0.05 * x_exp) + rnorm(20, 0, 0.5)
data_exp <- data.frame(x = x_exp, y = y_exp)

plot_exp <- ggplot(data_exp, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "inverse"))) +
  labs(title = "4) Exponential-like Curve (Gamma Family, Inverse Link)")

print(plot_exp)

# 5. Quadratic Curve (Gaussian family with polynomial formula)
set.seed(123)
x_quad <- -10:10
y_quad <- 2 + 0.5 * x_quad + 0.2 * x_quad^2 + rnorm(21, 0, 5)
data_quad <- data.frame(x = x_quad, y = y_quad)

plot_quad <- ggplot(data_quad, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2), method.args = list(family = "gaussian")) +
  labs(title = "5) Quadratic Curve (Polynomial Formula)")

print(plot_quad)
# 6. Poisson-like Curve (Poisson family with log link)
set.seed(123)
x_poisson <- 1:20
y_poisson <- rpois(20, lambda = exp(0.1 * x_poisson))
data_poisson <- data.frame(x = x_poisson, y = y_poisson)
plot_poisson <- ggplot(data_poisson, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(title = "6) Poisson-like Curve (Poisson Family)")
print(plot_poisson)

# 7. Inverse U-shaped Curve (Gaussian family with polynomial formula)
set.seed(123)
x_inv_u <- seq(-10, 10, length.out = 100)
y_inv_u <- 2 - 0.5 * (x_inv_u^2) + rnorm(100, 0, 5)
data_inv_u <- data.frame(x = x_inv_u, y = y_inv_u)
plot_inv_u <- ggplot(data_inv_u, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2), method.args = list(family = "gaussian")) +
  labs(title = "7) Inverse U-shaped Curve (Polynomial Formula)")
print(plot_inv_u)

# 8. Exponential Growth Curve (Gaussian family with log link)
set.seed(123)
x_exp_growth <- seq(0, 10, length.out = 100)
y_exp_growth <- (2 * exp(0.3 * x_exp_growth) + rnorm(100, 0, 5))+10
data_exp_growth <- data.frame(x = x_exp_growth, y = y_exp_growth)
plot_exp_growth <- ggplot(data_exp_growth, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = "log"))) +
  labs(title = "8) Exponential Growth Curve (Gaussian Family, Log Link)")
print(plot_exp_growth)


library(gridExtra)
all.graphs <- grid.arrange(plot_linear, plot_logistic, plot_log, plot_exp, plot_quad, plot_poisson, plot_inv_u, plot_exp_growth,
           ncol = 2)

ggsave(filename = "glm_curves.jpeg",plot = all.graphs, width = 8, height = 16, dpi = 300)




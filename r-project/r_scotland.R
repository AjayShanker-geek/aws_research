library(sf)
library(spdep)

scotland <- readRDS("Scotland.rds")

center_scotland <- st_centroid(scotland)

knn_scotland1 <- knn2nb(knearneigh(st_coordinates(center_scotland), k = 1))
knn_scotland2 <- knn2nb(knearneigh(st_coordinates(center_scotland), k = 2))
knn_scotland3 <- knn2nb(knearneigh(st_coordinates(center_scotland), k = 3))

par(mfrow = c(1, 3), mar = c(0, 0, 1, 0), oma = c(0, 0, 0, 0))

plot(st_geometry(scotland), border = "lightgray", main = "k = 1", axes = FALSE, bty = "n")
plot.nb(knn_scotland1, st_geometry(center_scotland), col = 'red', add = TRUE)

plot(st_geometry(scotland), border = "lightgray", main = "k = 2", axes = FALSE, bty = "n")
plot.nb(knn_scotland2, st_geometry(center_scotland), col = 'blue', add = TRUE)

plot(st_geometry(scotland), border = "lightgray", main = "k = 3", axes = FALSE, bty = "n")
plot.nb(knn_scotland3, st_geometry(center_scotland), col = 'purple', add = TRUE)

library(ggplot2)
library(patchwork)

queen_neighbors <- poly2nb(scotland, queen = TRUE)
rook_neighbors <- poly2nb(scotland, queen = FALSE)

different_neighbors <- mapply(function(x, y) !identical(x, y), 
                              queen_neighbors, rook_neighbors)

as.character(scotland$name)[different_neighbors]

plot_neighbors <- function(region_name, neighbors, title) {
  target_region <- scotland[scotland$name == region_name, ]
  neighbor_indices <- neighbors[[which(scotland$name == region_name)]]
  neighbor_regions <- scotland[neighbor_indices, ]
  
  scotland$region_type <- "Other"
  scotland$region_type[scotland$name %in% target_region$name] <- "Area"
  scotland$region_type[scotland$name %in% neighbor_regions$name] <- "Neighbor"
  
  scotland$region_type <- factor(scotland$region_type, 
                                 levels = c("Area", "Neighbor", "Other"))
  
  ggplot(data = scotland) +
    geom_sf(aes(fill = region_type), color = "black") +
    scale_fill_manual(values = c("Area" = "orange", "Neighbor" = "skyblue", 
                                 "Other" = "white")) +
    labs(title = paste(title, "\n", region_name), fill = "Region Type") + 
    theme_minimal() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"), 
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 8, lineheight = 1.2),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7)
    )
}

plot_neighbors_for_region <- function(region_name, queen_neighbors, rook_neighbors) {
  queen_plot <- plot_neighbors(region_name, queen_neighbors, "Queen Neighbors for")
  rook_plot <- plot_neighbors(region_name, rook_neighbors, "Rook Neighbors for")
  
  combined_plot <- queen_plot + rook_plot + plot_layout(ncol = 2, widths = c(1, 1))
  
  return(combined_plot)
}

regions <- c("EastLothian", "Falkirk", "Clackmannan", "Edinburgh")
all_plots <- list()

for (region_name in regions) {
  all_plots[[region_name]] <- plot_neighbors_for_region(region_name, 
                                                        queen_neighbors, 
                                                        rook_neighbors)
}

wrap_plots(all_plots, ncol = 2) + plot_layout(guides = 'collect')

rook_weights <- nb2listw(rook_neighbors, style = "B")

# (i) Normal test (without skewness correction)
moran.test(scotland$cancer, rook_weights, randomisation = FALSE, 
           alternative = "greater")

# (ii) Permutation test
set.seed(5226)

moran.mc(scotland$cancer, rook_weights, nsim = 9999)

# (iii) Monte Carlo test by assuming that the count in each region follows a 
# Poisson distribution with expectation given in the variable `expected`
set.seed(5226)

moran.pois <- function(y, exp, listw, nsim) {
  Tstat <- rep(0, nsim)
  Tstat[1] <- moran(y, listw, length(y), Szero(listw))$I
  pmeans <- exp
  for (ii in 2:nsim) {
    tmp <- rpois(length(pmeans), pmeans)
    Tstat[ii] <- moran(tmp, listw, length(y), Szero(listw))$I
  }
  sum(Tstat[-1] > Tstat[1]) / (nsim + 1)
}

moran.pois(scotland$cancer, scotland$expected, rook_weights, nsim = 9999)

# (i) Geary C test under normality
geary.test(scotland$cancer, rook_weights, randomisation = FALSE)

# (ii) Permutation test
set.seed(5226)

geary.mc(scotland$cancer, rook_weights, nsim=9999)

# (iii) Monte Carlo test by assuming that the count in each region follows a 
# Poisson distribution with expectation given in the variable `expected`
set.seed(5226)

geary.pois <- function(y, exp, listw, nsim) {
  Tstat <- rep(0, nsim)
  Tstat[1] <- geary(y, listw, length(y), length(y)-1, Szero(listw))$C
  pmeans <- exp
  for(ii in 2:nsim) {
    tmp <- rpois(rep(1, length(pmeans)), pmeans)
    Tstat[ii] <- geary(tmp, listw, length(y), length(y)-1, Szero(listw))$C
  }
  sum(Tstat[-1] < Tstat[1])/(nsim+1)
}

geary.pois(scotland$cancer, scotland$expected, rook_weights, 9999)

loc.moran <- localmoran(scotland$logratio, rook_weights, alternative = "two.sided")

mp <- moran.plot(scotland$logratio, rook_weights)

scotland$quadrant <- NA
p.2side <- loc.moran[, "Pr(z != E(Ii))"]

# High-High
scotland[(mp$x >= 0 & mp$wx >= 0) & (p.2side <= 0.05), "quadrant"] <- 1

# High-Low
scotland[(mp$x >= 0 & mp$wx <= 0) & (p.2side <= 0.05), "quadrant"] <- 3

# Low-High
scotland[(mp$x <= 0 & mp$wx >= 0) & (p.2side <= 0.05), "quadrant"] <- 4

# Low-Low
scotland[(mp$x <= 0 & mp$wx <= 0) & (p.2side <= 0.05), "quadrant"] <- 2

# Non-significant
scotland[(p.2side > 0.05), "quadrant"] <- 5

ggplot(data = scotland) +
  geom_sf(aes(fill = as.factor(quadrant))) +
  coord_sf(expand = FALSE) +
  scale_fill_manual(breaks = c(1, 2, 3, 4, 5),
                    labels = c("High-High", "Low-Low", "High-Low", "Low-High", 
                               "Non-significant"),
                    values = c("red", "blue", "lightpink", "skyblue2", "white")) +
  theme(legend.title = element_blank())

library(nlme)

linear_model <- gls(logratio ~ percentAFF + eastkm + northkm, 
                    data = scotland, 
                    method = "ML")
summary(linear_model)

linear_model2 <- gls(logratio ~ percentAFF + northkm, 
                     data = scotland, 
                     method = "ML")

summary(linear_model2)

standardized_residuals <- scale(residuals(linear_model2))

fitted_vals <- fitted(linear_model2)
residuals_vals <- residuals(linear_model2)

par(mfrow = c(1, 2))

plot(fitted_vals, residuals_vals,
     main = "Standardized Residuals vs Fitted Values",
     xlab = "Fitted values", ylab = "Residuals",
     pch = 20, col = "blue", cex.main = 0.8)
abline(h = 0, col = "red")

qqnorm(standardized_residuals, main = "Normal Q-Q Plot", cex.main = 0.8)
qqline(standardized_residuals, col = "red")

linear_model_wls <- gls(logratio ~ percentAFF + northkm, 
                        data = scotland, 
                        method = "ML",
                        weights = varFixed(~ 1 / expected))

summary(linear_model_wls)

wts <- nb2listw(rook_neighbors, style = 'B')
residuals_vals <- as.vector(residuals_vals)
moran.plot(residuals_vals, wts, main = "Moran's I for Residuals")

model_exp <- gls(logratio ~ percentAFF + northkm, 
                 data = scotland, 
                 method = "ML",
                 correlation = corExp(form = ~ eastkm + northkm, nugget = TRUE))

summary(model_exp)

library(spatialreg)

scotland_nb <- poly2nb(scotland, queen = FALSE)
wts <- nb2listw(scotland_nb, style = "B")

scotland_sar <- spautolm(logratio ~ percentAFF + northkm, 
                         data = scotland, listw = wts)

summary(scotland_sar)

set.seed(5226)

moran.mc(residuals(scotland_sar), listw = wts, nsim = 9999)

scotland_car <- spautolm(logratio ~ percentAFF + northkm, 
                         data=scotland, listw=wts, family="CAR")

summary(scotland_car)

set.seed(5226)

moran.mc(residuals(scotland_car), listw = wts, nsim = 9999)


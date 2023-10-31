# Figure to explain manual tour
library(tourr)
library(palmerpenguins)
library(dplyr)
library(ggplot2)

penguins <- penguins %>%
  na.omit() 
penguins_sub <- penguins[,c(1, 3:6)] %>% 
  mutate(across(where(is.numeric),  ~ scale(.)[,1])) %>%
  rename(bl = bill_length_mm,
         bd = bill_depth_mm,
         fl = flipper_length_mm,
         bm = body_mass_g)

s <- basis_random(4, 1)
render_gif(penguins_sub[,-1],
           radial_tour(start=s, mvar=2),
           display_dist(),
           "penguins1d.gif",
           width=300,
           height=300)

ggplot(penguins_sub, aes(x=bd, y=bm)) + geom_point()

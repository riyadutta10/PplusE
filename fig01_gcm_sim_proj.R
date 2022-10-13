#Required Libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
#Load Data
global_gimsim <- read_csv('gcm_sim_avg8gcms.csv') %>% as.data.table()
global_proj126 <- read_csv('gcm_proj126_avg8gcms.csv') %>% as.data.table()
global_proj245 <- read_csv('gcm_proj245_avg8gcms.csv') %>% as.data.table()
global_proj585 <- read_csv('gcm_proj585_avg8gcms.csv') %>% as.data.table()

#Plots gimsim
range_PmE_gimsim <- max(global_gimsim$PmE) - min(global_gimsim$PmE)
min_PmE_gimsim <- min(global_gimsim$PmE)
range_PpE_gimsim <- max(global_gimsim$PpE) - min(global_gimsim$PpE)
min_PpE_gimsim <- min(global_gimsim$PpE)
range_T_gimsim <- max(global_gimsim$T) - min(global_gimsim$T)
min_T_gimsim <- min(global_gimsim$T)

p01 <- ggplot(data = global_gimsim, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "#377eb8", size = 1) +
  geom_line(aes(y = (T - min_T_gimsim)*range_PmE_gimsim/range_T_gimsim + min_PmE_gimsim), color = "red") +
  labs(x = NULL, y = NULL, title = "Simulations", tag = "A") +
  theme_bw() +
  #geom_label(aes(x = 1992, y = -90, label = "R-squared = 0.012\np-value = 0.141"), fill = "white", size = 5) +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PmE_gimsim)*range_T_gimsim/range_PmE_gimsim + min_T_gimsim, name = NULL)) +
  scale_x_continuous(limits = c(1980, 2015), expand = c(0, 0), breaks = seq(1980, 2020, 10)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))

p02 <- ggplot(data = global_gimsim, aes(x = Year, y = PpE)) +
  geom_line(show.legend = FALSE, color = "#4daf4a", size = 1) +
  geom_line(aes(y = (T - min_T_gimsim)*range_PpE_gimsim/range_T_gimsim + min_PpE_gimsim), color = "red") +
  labs(x = NULL, y = NULL, title = "Simulations", tag = "B") +
  theme_bw() +
  #geom_label(aes(x = 1992, y = 2250, label = "R-squared = 0.822\np-value < 2e-16"), fill = "white", size = 5) +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PpE_gimsim)*range_T_gimsim/range_PpE_gimsim + min_T_gimsim, name = NULL)) +
  scale_x_continuous(limits = c(1980, 2015), expand = c(0, 0), breaks = seq(1980, 2020, 10)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))

#Plots Proj126
range_PmE_proj126 <- max(global_proj126$PmE) - min(global_proj126$PmE)
min_PmE_proj126 <- min(global_proj126$PmE)
range_PpE_proj126 <- max(global_proj126$PpE) - min(global_proj126$PpE)
min_PpE_proj126 <- min(global_proj126$PpE)
range_T_proj126 <- max(global_proj126$T) - min(global_proj126$T)
min_T_proj126 <- min(global_proj126$T)

p03 <- ggplot(data = global_proj126, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "#377eb8", size = 1) +
  geom_line(aes(y = (T - min_T_proj126)*range_PmE_proj126/range_T_proj126 + min_PmE_proj126), color = "red") +
  labs(x = NULL, y = NULL, title = "Projections (SSP126)", tag = "C") +
  theme_bw() +
  #geom_label(aes(x = 1865, y = -9, label = "R-squared = 0.055\np-value = 0.013"), fill = "white", size = 5) +
  scale_y_continuous(breaks = seq(-2, -10, -2), sec.axis = sec_axis(~(. - min_PmE_proj126)*range_T_proj126/range_PmE_proj126 + min_T_proj126, name = NULL)) +
  scale_x_continuous(limits = c(2010, 2100), expand = c(0, 0), breaks = seq(2010, 2100, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))

p04 <- ggplot(data = global_proj126, aes(x = Year, y = PpE)) +
  geom_line(show.legend = FALSE, color = "#4daf4a", size = 1) +
  geom_line(aes(y = (T - min_T_proj126)*range_PpE_proj126/range_T_proj126 + min_PpE_proj126), color = "red") +
  labs(x = NULL, y = NULL, title = "Projections (SSP126)", tag = "D") +
  theme_bw() +
  #geom_label(aes(x = 1865, y = 2015, label = "R-squared = 0.802\np-value < 2e-16"), fill = "white", size = 5) +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PpE_proj126)*range_T_proj126/range_PpE_proj126 + min_T_proj126, name = NULL)) +
  scale_x_continuous(limits = c(2010, 2100), expand = c(0, 0), breaks = seq(2010, 2100, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))

#Plots Proj245
range_PmE_proj245 <- max(global_proj245$PmE) - min(global_proj245$PmE)
min_PmE_proj245 <- min(global_proj245$PmE)
range_PpE_proj245 <- max(global_proj245$PpE) - min(global_proj245$PpE)
min_PpE_proj245 <- min(global_proj245$PpE)
range_T_proj245 <- max(global_proj245$T) - min(global_proj245$T)
min_T_proj245 <- min(global_proj245$T)

p05 <- ggplot(data = global_proj245, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "#377eb8", size = 1) +
  geom_line(aes(y = (T - min_T_proj245)*range_PmE_proj245/range_T_proj245 + min_PmE_proj245), color = "red") +
  labs(x = NULL, y = NULL, title = "Projections (SSP245)", tag = "E") +
  theme_bw() +
  #geom_label(aes(x = 1865, y = 0, label = "R-squared = 0.395\np-value = 3e-09"), fill = "white", size = 5) +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PmE_proj245)*range_T_proj245/range_PmE_proj245 + min_T_proj245, name = NULL)) +
  scale_x_continuous(limits = c(2010, 2100), expand = c(0, 0), breaks = seq(2010, 2100, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))

p06 <- ggplot(data = global_proj245, aes(x = Year, y = PpE)) +
  geom_line(show.legend = FALSE, color = "#4daf4a", size = 1) +
  geom_line(aes(y = (T - min_T_proj245)*range_PpE_proj245/range_T_proj245 + min_PpE_proj245), color = "red") +
  labs(x = NULL, y = NULL, title = "Projections (SSP245)", tag = "F") +
  theme_bw() +
  #geom_label(aes(x = 1865, y = 2050, label = "R-squared = 0.753\np-value < 2e-16"), fill = "white", size = 5) +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PpE_proj245)*range_T_proj245/range_PpE_proj245 + min_T_proj245, name = NULL)) +
  scale_x_continuous(limits = c(2010, 2100), expand = c(0, 0), breaks = seq(2010, 2100, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))

#Plots Proj585
range_PmE_proj585 <- max(global_proj585$PmE) - min(global_proj585$PmE)
min_PmE_proj585 <- min(global_proj585$PmE)
range_PpE_proj585 <- max(global_proj585$PpE) - min(global_proj585$PpE)
min_PpE_proj585 <- min(global_proj585$PpE)
range_T_proj585 <- max(global_proj585$T) - min(global_proj585$T)
min_T_proj585 <- min(global_proj585$T)

p07 <- ggplot(data = global_proj585, aes(x = Year, y = PmE)) +
  geom_line(show.legend = FALSE, color = "#377eb8", size = 1) +
  geom_line(aes(y = (T - min_T_proj585)*range_PmE_proj585/range_T_proj585 + min_PmE_proj585), color = "red") +
  labs(x = NULL, y = NULL, title = "Projections (SSP585)", tag = "G") +
  theme_bw() +
  #geom_label(aes(x = 1865, y = -40, label = "R-squared = 0.176\np-value = 2e-04"), fill = "white", size = 5) +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PmE_proj585)*range_T_proj585/range_PmE_proj585 + min_T_proj585, name = NULL)) +
  scale_x_continuous(limits = c(2010, 2100), expand = c(0, 0), breaks = seq(2010, 2100, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))

p08 <- ggplot(data = global_proj585, aes(x = Year, y = PpE)) +
  geom_line(show.legend = FALSE, color = "#4daf4a", size = 1) +
  geom_line(aes(y = (T - min_T_proj585)*range_PpE_proj585/range_T_proj585 + min_PpE_proj585), color = "red") +
  labs(x = NULL, y = NULL, title = "Projections (SSP585)", tag = "H") +
  theme_bw() +
  #geom_label(aes(x = 1865, y = 2000, label = "R-squared = 0.122\np-value = 0.002"), fill = "white", size = 5) +
  scale_y_continuous(sec.axis = sec_axis(~(. - min_PpE_proj585)*range_T_proj585/range_PpE_proj585 + min_T_proj585, name = NULL)) +
  scale_x_continuous(limits = c(2010, 2100), expand = c(0, 0), breaks = seq(2010, 2100, 30)) +
  theme(panel.grid.minor = element_blank(), plot.tag = element_text(size = 24), plot.title = element_text(size=28), axis.text = element_text(size = 20), axis.title = element_text(size = 24))

yleft1 = grid::textGrob("Annual Precipitation minus Evaporation [mm]", rot=90, gp = grid::gpar(fontsize = 28))
yleft2 = grid::textGrob("Annual Precipitation plus Evaporation [mm]", rot=90, gp = grid::gpar(fontsize = 28))
yright = grid::textGrob("Mean Annual Temperature [°C]", rot=-90, gp = grid::gpar(fontsize = 28))
p00 <- gridExtra::grid.arrange(p01, p03, p05, p07, ncol = 1,right = yright, left = yleft1, padding = unit(1, "line"))
p09 <- gridExtra::grid.arrange(p02, p04, p06, p08, ncol = 1,right = yright, left = yleft2, padding = unit(1, "line"))
p10 <- gridExtra::grid.arrange(p00, p09, ncol = 2)

ggsave("fig01_gcm.pdf", p10, dpi = 600, width = 8.15*2, height = 4.78*4, units = "in")




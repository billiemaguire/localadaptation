library(tidyverse)
library(glmmTMB)
library(performance)
library(Rmisc)
library(emmeans)

## cleaning the dataset

new_ds <- read.csv("2023FWWLocalAdaptationExperiment.csv") |>
  select(Color_type, Host_Species, Host_Plant, 
         Origin_Site, Experimental_Site, Sex, Pupa_mass, 
         Date_hatch, Pupa_Date, Death_Date, Survived, 
         Dev_Time, Usable_to_pupation_fitness_total,
         Fitness_Score, Mom, Local_Foreign) |>
  filter(!Mom %in% c("HC25", "HC-B01","HC25","HC20","HC16","HC15","HC14",
                     "BC21","BC12","BC07")) |> #removing hybrid experiment
  filter(!Usable_to_pupation_fitness_total %in% c("0","2", "")) |> 
  #removing ones that were missing, crushed or otherwise had some human error
  filter(!Fitness_Score %in% c("","NA", " ")) |>
  filter(!Sex %in% c("M?")) |> 
  dplyr::group_by(Color_type, Experimental_Site) |> 
  mutate(ceMax_fitness = max(Fitness_Score, na.rm = T),
         ceMax_pupal = max(Pupa_mass, na.rm = T),
         ceMax_dt = max(Dev_Time, na.rm = T)) |> 
  ungroup() |> 
  mutate(Relative_fitness = Fitness_Score/ceMax_fitness,
         Relative_pupal = Pupa_mass/ceMax_pupal,
         Relative_dt = Dev_Time/ceMax_dt) |> 
  separate(Mom, 
          into = c("part1", "part2", "part3", "Matline"), 
          sep = "-", 
          fill = "right",
          remove = FALSE) |> 
  mutate(Matline = ifelse(is.na(Matline) == T, part1, Matline)) |> 
  mutate(Matline = as.factor(Matline)) |> 
  filter( Host_Plant %in% c("Box Elder", "Cherry", "Broadleaf","Hickory", "Walnut"))

# building a model for each host plant

## Red Cherry
new_ds |> 
  filter(Host_Plant %in% c("Cherry") & Color_type == "Red") |> 
  glmmTMB(Relative_fitness ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")

## Red Local
new_ds |> 
  filter(Host_Plant %in% c("Walnut", "Hickory", "Broadleaf") & Color_type == "Red") |> 
  glmmTMB(Relative_fitness ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")
  
## Black Cherry
new_ds |> 
  filter(Host_Plant %in% c("Cherry") & Color_type == "Black") |> 
  glmmTMB(Relative_fitness ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")


## Black Boxelder
new_ds |> 
  filter(Host_Plant %in% c("Box Elder") & Color_type == "Black") |> 
  glmmTMB(Relative_fitness ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")


## making figure 1 

s1 <- summarySE(measurevar = "Relative_fitness", data = new_ds, 
                groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
                na.rm = T)

# s_pupal <- summarySE(measurevar = "Relative_pupal", data = new_ds, 
#                     groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
#                     na.rm = T)
# s_dt <- summarySE(measurevar = "Relative_dt", data = new_ds, 
#                  groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
#                  na.rm = T)
# s_s <- summarySE(measurevar = "Survived", data = new_ds, 
#                 groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
#                 na.rm = T)

redCherry <- s1 |> 
  filter(Host_Plant %in% c("Cherry"),
         Color_type == "Red",
         Origin_Site != "MA") |> 
  ggplot() +
  geom_line(aes(x = Experimental_Site, 
                y = Relative_fitness, 
                group = Origin_Site, 
                #colour = Origin_Site,
                linetype = Origin_Site),
            size = 1) +
  geom_pointrange(aes(x = Experimental_Site, 
                      y = Relative_fitness,
                      ymin = Relative_fitness-se,
                      ymax = Relative_fitness+se),
                  size = 4,
                  fatten = .5,
                  color = "#cc3d2d") +
  
  theme_linedraw() +
  #scale_color_manual(values = c("#8487e0", "#b7ed9a")) +
  #theme(legend.position="none") +
  labs(title = "A. Red: Cherry", 
       x = "Experimental Site",
       y = "Relative Fitness")  +
  ylim(0,0.6) #+
#theme(legend.position="none")

redLocal <- s1 |> 
  filter(Host_Plant %in% c("Walnut", "Hickory", "Broadleaf"),
         Color_type == "Red",
         Origin_Site != "MA") |> 
  ggplot() +
  geom_line(aes(x = Experimental_Site, 
                y = Relative_fitness, 
                group = Origin_Site, 
                #colour = Origin_Site,
                linetype = Origin_Site),
            size = 1) +
  geom_pointrange(aes(x = Experimental_Site, 
                      y = Relative_fitness,
                      ymin = Relative_fitness-se,
                      ymax = Relative_fitness+se),
                  size = 4,
                  fatten = .5,
                  color = "#cc3d2d") +
  
  theme_linedraw() +
  #scale_color_manual(values = c("#8487e0", "#b7ed9a")) +
  #theme(legend.position="none") +
  labs(title = "B. Red: Local", 
       x = "Experimental Site",
       y = "Relative Fitness") +
  ylim(0,0.6)

blackCherry <- s1 |> 
  filter(Host_Plant %in% c("Cherry"),
         Color_type == "Black") |> 
  ggplot() +
  geom_line(aes(x = Experimental_Site, 
                y = Relative_fitness, 
                group = Origin_Site, 
                linetype = Origin_Site),
            size = 1) +
  geom_pointrange(aes(x = Experimental_Site, 
                      y = Relative_fitness,
                      ymin = Relative_fitness-se,
                      ymax = Relative_fitness+se),
                  size = 4,
                  fatten = .5,
                  color = "#2d2c2e") +
  
  theme_linedraw() +
  scale_linetype_manual(values = c("dashed", "dotted")) +
  #theme(legend.position="none") +
  labs(title = "C. Black: Cherry", 
       x = "Experimental Site",
       y = "Relative Fitness")  +
  ylim(0,0.6) #+
#theme(legend.position="none")

blackBoxElder <- s1 |> 
  filter(Host_Plant %in% c("Box Elder"),
         Color_type == "Black") |> 
  ggplot() +
  geom_line(aes(x = Experimental_Site, 
                y = Relative_fitness, 
                group = Origin_Site, 
                linetype = Origin_Site),
            size = 1) +
  geom_pointrange(aes(x = Experimental_Site, 
                      y = Relative_fitness,
                      ymin = Relative_fitness-se,
                      ymax = Relative_fitness+se),
                  size = 4,
                  fatten = .5,
                  color = "#2d2c2e") +
  
  theme_linedraw() +
  scale_linetype_manual(values = c("dashed", "dotted")) +
  #theme(legend.position="none") +
  labs(title = "D. Black: Box Elder", 
       x = "Experimental Site",
       y = "Relative Fitness")  +
  ylim(0,0.6)

library(gridExtra)

grid.arrange(redCherry, redLocal, blackCherry, blackBoxElder, ncol = 2)





library(tidyverse)
library(glmmTMB)
library(performance)
library(Rmisc)
library(emmeans)
library(gridExtra)

## cleaning the dataset

  new_ds <- read.csv("/Users/billiemaguire/Documents/local adaptation/FW_summer_24_weintraub/data/2023FWWLocalAdaptationExperiment.csv") |>
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

## making figure 1 

s1 <- summarySE(measurevar = "Relative_fitness", data = new_ds, 
                groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
                na.rm = T)
s_pupal <- summarySE(measurevar = "Pupa_mass", data = new_ds, 
           groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
           na.rm = T)
s_dt <- summarySE(measurevar = "Dev_Time", data = new_ds, 
           groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
           na.rm = T)
s_s <- summarySE(measurevar = "Survived", data = new_ds, 
           groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
           na.rm = T)

## Making Significance test for each supplemental figure
# building a model for each host plant/biotype comparison

### SURVIVAL 
## Red Cherry

new_ds |> 
  filter(Host_Plant %in% c("Cherry") & Color_type == "Red") |> 
  glmmTMB(Survived ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = binomial(link = "logit")) |> 
  summary()
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")

## Red Local
new_ds |> 
  filter(Host_Plant %in% c("Walnut", "Hickory", "Broadleaf") & Color_type == "Red") |> 
  glmer(Survived ~ Origin_Site*Experimental_Site + (1 | Matline), 
        data = _, 
        family = binomial(link = "logit")) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")

new_ds |> 
  filter(Host_Plant %in% c("Walnut", "Hickory", "Broadleaf") & Color_type == "Red") |> 
  group_by(Matline) |> 
  dplyr::mutate(n = n(),
         Living = sum(Survived),
         Neg = n - Living) |> 
  ungroup() |> 
  glm(cbind(Living, Neg) ~ Origin_Site*Experimental_Site, family = binomial, data = _) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")
  
## Black Cherry
new_ds |> 
  filter(Host_Plant %in% c("Cherry") & Color_type == "Black") |> 
  glmer(Survived ~ Origin_Site*Experimental_Site + (1 | Matline), 
        data = _, 
        family = binomial(link = "logit")) |>
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")


## Black Boxelder
new_ds |> 
  filter(Host_Plant %in% c("Box Elder") & Color_type == "Black") |> 
  glmer(Survived ~ Origin_Site*Experimental_Site + (1 | Matline), 
        data = _, 
        family = binomial(link = "logit")) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")


### PUPAL MASS

## Red Cherry
new_ds |> 
  filter(Host_Plant %in% c("Cherry") & Color_type == "Red") |> 
  glmmTMB(Pupa_mass ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  summary()
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")

## Red Local
new_ds |> 
  filter(Host_Plant %in% c("Walnut", "Hickory", "Broadleaf") & Color_type == "Red") |> 
  glmmTMB(Pupa_mass ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  summary()
  emmeans(~Origin_Site*Experimental_Site)
  pairs(simple = "each")

## Black Cherry
new_ds |> 
  filter(Host_Plant %in% c("Cherry") & Color_type == "Black") |> 
  glmmTMB(Pupa_mass ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")


## Black Boxelder
new_ds |> 
  filter(Host_Plant %in% c("Box Elder") & Color_type == "Black") |> 
  glmmTMB(Pupa_mass ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")


### DEVELOPMENT TIME 
new_ds |> 
  filter(Host_Plant %in% c("Cherry") & Color_type == "Red") |> 
  glmmTMB(Dev_Time ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")

## Red Local
new_ds |> 
  filter(Host_Plant %in% c("Walnut", "Hickory", "Broadleaf") & Color_type == "Red") |> 
  glmmTMB(Dev_Time ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")

## Black Cherry
new_ds |> 
  filter(Host_Plant %in% c("Cherry") & Color_type == "Black") |> 
  glmmTMB(Dev_Time ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")


## Black Boxelder
new_ds |> 
  filter(Host_Plant %in% c("Box Elder") & Color_type == "Black") |> 
  glmmTMB(Dev_Time ~ Origin_Site*Experimental_Site + (1 | Matline), 
          data = _, 
          family = gaussian()) |> 
  emmeans(~Origin_Site*Experimental_Site) |> 
  pairs(simple = "each")

### MAKING NEW PLOTS

s_pupal <- summarySE(measurevar = "Pupa_mass", data = new_ds, 
                     groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
                     na.rm = T)
s_dt <- summarySE(measurevar = "Dev_Time", data = new_ds, 
                  groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
                  na.rm = T)
s_s <- summarySE(measurevar = "Survived", data = new_ds, 
                 groupvars = c("Experimental_Site", "Origin_Site", "Host_Plant", "Color_type"),
                 na.rm = T)

make_plots <- function(ds, column, column_name, a, b) {
  
  redCherry <- ds |> 
    filter(Host_Plant %in% c("Cherry"),
           Color_type == "Red",
           Origin_Site != "MA") |> 
    ggplot() +
    geom_line(aes(x = Experimental_Site, 
                  y = {{ column }}, 
                  group = Origin_Site, 
                  #colour = Origin_Site,
                  linetype = Origin_Site),
              size = 1) +
    geom_pointrange(aes(x = Experimental_Site, 
                        y = {{ column }},
                        ymin = {{ column }}-se,
                        ymax = {{ column }}+se),
                    size = 4,
                    fatten = .5,
                    color = "#cc3d2d") +
    
    theme_linedraw() +
    #scale_color_manual(values = c("#8487e0", "#b7ed9a")) +
    #theme(legend.position="none") +
    labs(title = "A. Red: Cherry", 
         x = "Experimental Site",
         y = column_name,
         linetype = "Origin Site")  +
    ylim(a,b) #+
  #theme(legend.position="none")
  
  redLocal <- ds |> 
    filter(Host_Plant %in% c("Walnut", "Hickory", "Broadleaf"),
           Color_type == "Red",
           Origin_Site != "MA") |> 
    ggplot() +
    geom_line(aes(x = Experimental_Site, 
                  y = {{ column }}, 
                  group = Origin_Site, 
                  #colour = Origin_Site,
                  linetype = Origin_Site),
              size = 1) +
    geom_pointrange(aes(x = Experimental_Site, 
                        y = {{ column }},
                        ymin = {{ column }}-se,
                        ymax = {{ column }}+se),
                    size = 4,
                    fatten = .5,
                    color = "#cc3d2d") +
    
    theme_linedraw() +
    #scale_color_manual(values = c("#8487e0", "#b7ed9a")) +
    #theme(legend.position="none") +
    labs(title = "B. Red: Local", 
         x = "Experimental Site",
         y = column_name,
         linetype = "Origin Site") +
    ylim(a,b)
  
  blackCherry <- ds |> 
    filter(Host_Plant %in% c("Cherry"),
           Color_type == "Black") |> 
    ggplot() +
    geom_line(aes(x = Experimental_Site, 
                  y = {{ column }}, 
                  group = Origin_Site, 
                  linetype = Origin_Site),
              size = 1) +
    geom_pointrange(aes(x = Experimental_Site, 
                        y = {{ column }},
                        ymin = {{ column }}-se,
                        ymax = {{ column }}+se),
                    size = 4,
                    fatten = .5,
                    color = "#2d2c2e") +
    
    theme_linedraw() +
    scale_linetype_manual(values = c("dashed", "dotted")) +
    #theme(legend.position="none") +
    labs(title = "C. Black: Cherry", 
         x = "Experimental Site",
         y = column_name,
         linetype = "Origin Site")  +
    ylim(a,b) #+
  #theme(legend.position="none")
  
  blackBoxElder <- ds |> 
    filter(Host_Plant %in% c("Box Elder"),
           Color_type == "Black") |> 
    ggplot() +
    geom_line(aes(x = Experimental_Site, 
                  y = {{ column }}, 
                  group = Origin_Site, 
                  linetype = Origin_Site),
              size = 1) +
    geom_pointrange(aes(x = Experimental_Site, 
                        y = {{ column }},
                        ymin = {{ column }}-se,
                        ymax = {{ column }}+se),
                    size = 4,
                    fatten = .5,
                    color = "#2d2c2e") +
    
    theme_linedraw() +
    scale_linetype_manual(values = c("dashed", "dotted")) +
    #theme(legend.position="none") +
    labs(title = "D. Black: Box Elder", 
         x = "Experimental Site",
         y = column_name,
         linetype = "Origin Site")  +
    ylim(a,b)

make_plots(s_pupal, Pupa_mass, "Pupal Mass", 0, 200)
make_plots(s_dt, Dev_Time, "Development Time", 0, 75)
make_plots(s_s, Survived, "Survival", 0, 1)

## calculating among family variances
## Cherry
new_ds |> 
  filter(Host_Plant %in% c("Cherry") & Color_type == "Red") |> 
  dplyr::group_by(Matline, Experimental_Site,Origin_Site) |> 
  dplyr::summarise(mean = mean(Survived, na.rm = T)) |> 
  ungroup() |> 
  dplyr::group_by(Experimental_Site, Origin_Site) |> 
  dplyr::summarise(var = sd(mean, na.rm = T))

## Local
new_ds |> 
  filter(Host_Plant %in% c("Walnut", "Hickory", "Broadleaf") & Color_type == "Red") |> 
  dplyr::group_by(Matline, Experimental_Site,Origin_Site) |> 
  dplyr::summarise(mean = mean(Survived, na.rm = T)) |> 
  ungroup() |> 
  dplyr::group_by(Experimental_Site, Origin_Site) |> 
  dplyr::summarise(var = sd(mean, na.rm = T))

### calculating survival days
new_ds |> 
  filter(Survived == 0) |> 
  mutate(
    birth_date    = yday(mdy(ifelse(nchar(Date_hatch) > 4, Date_hatch, paste0(Date_hatch, "/2023")))),
    death_date    = yday(mdy(ifelse(nchar(Death_Date) > 4, Death_Date, paste0(Death_Date, "/2023")))),
    lifespan_days = as.integer(death_date - birth_date),
    good_host = ifelse(Host_Plant %in% c("Walnut", "Hickory", "Broadleaf"), "local_host", Host_Plant)) |> 
  dplyr::group_by(Origin_Site, Experimental_Site, Color_type, good_host) |> 
  dplyr::summarise(mean = mean(lifespan_days, na.rm = T)) |> 
  filter(Origin_Site %in% c("CO", "DC") & good_host == "local_host")




    
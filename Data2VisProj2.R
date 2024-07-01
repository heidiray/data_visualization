setwd("/Users/heidiray/Downloads")
getwd()

# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(conflicted)
library(dplyr)

filter(mtcars, cyl == 8)

installed.packages(c('purrr', 'reader'))
library(readstata13)
library(readxl)
library(tidyverse)
library(tidyverse)
library(directlabels)
library(maps)
library(rnaturalearthdata)
library(sf)
library(ggridges)
library(ggthemes)
library(scales)
list.files()

#load data
dat_2012 <- read.csv("2012.csv")
dat_2014 <- read.csv("2014.csv")

#make tibbles
dat_12_tib <- as_tibble(dat_2012)
dat_14_tib <- as_tibble(dat_2014)


#create variables
dat_12_tib$race[dat_12_tib$race == 'A'] <- 'Asian/Pacific Islander'
dat_12_tib$race[dat_12_tib$race == 'B'] <- 'Black'
dat_12_tib$race[dat_12_tib$race == 'I'] <- 'Native American/Alaska Native'
dat_12_tib$race[dat_12_tib$race == 'P'] <- 'Black Hispanic'
dat_12_tib$race[dat_12_tib$race == 'Q'] <- 'White Hispanic'
dat_12_tib$race[dat_12_tib$race == 'W'] <- 'White'
dat_12_tib$race[dat_12_tib$race == 'U'] <- 'Unknown'
dat_12_tib$race[dat_12_tib$race == 'Z'] <- 'Other'

dat_14_tib$race[dat_14_tib$race == 'A'] <- 'Asian/Pacific Islander'
dat_14_tib$race[dat_14_tib$race == 'B'] <- 'Black'
dat_14_tib$race[dat_14_tib$race == 'I'] <- 'Native American/Alaska Native'
dat_14_tib$race[dat_14_tib$race == 'P'] <- 'Black Hispanic'
dat_14_tib$race[dat_14_tib$race == 'Q'] <- 'White Hispanic'
dat_14_tib$race[dat_14_tib$race == 'W'] <- 'White'
dat_14_tib$race[dat_14_tib$race == 'U'] <- 'Unknown'
dat_14_tib$race[dat_14_tib$race == 'Z'] <- 'Other'

#make tables
table(dat_12_tib$race)
table(dat_14_tib$race)


#2014 plot
pdf(file="stopnfrisk14.pdf", height=8, width=20)

ggplot(data = dat_14_tib, aes(x = fct_infreq(race), 
                            fill = fct_infreq(race))) + 
  geom_bar(alpha = .50, lwd = 1.5) +
  theme_bw() +
  labs(title = "NYPD Stop Distribution by Race in 2014",
       subtitle = "Is One Race Stopped Disproportionately?",
       fill = "race",
       x = "Race",
       y = "Number of Stops",
       color = "race") +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 14)) 

#2012 plot
library(ggplot2)
pdf(file="stopnfrisk12.pdf", height=8, width=20)

ggplot(data = dat_12_tib, aes(x = fct_infreq(race), fill = fct_infreq(race))) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_bar(alpha = 0.50, lwd = 1.5) +
  theme_bw() +
  labs(title = "NYPD Stop Distribution by Race in 2012",
       subtitle = "Is One Race Stopped Disproportionately?",
       fill = "Race",
       x = "Race",
       y = "Number of Stops",
       color = "Race") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    legend.title = element_text(hjust = 0.5, size = 20),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  # Added axis.text.x
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 14)
  )


# Creating two lists
White12 <- 43.7
Black12 <- 24.7

# Creating a bar graph
ggplot(data.frame(Race = c("White", "Black"), Percentage = c(White12, Black12)), aes(x = Race, y = Percentage, fill = Race)) +
  geom_bar(stat = "identity", color = "darkturquoise", width = 0.5, alpha = 0.4) +
  theme_minimal() +
  labs(title = "NYC Race Distribution 2012",
       x = "Race",
       y = "Percentage") +
  scale_fill_manual(values = c("White" = "pink", "Black" = "deeppink2")) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.3, size = 6, color = "white") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none")

barplot(c(White12, Black12), 
        names.arg = c("White", "Black"),
        main = "NYC Race Distribution 2012",
        xlab = "Race",
        ylab = "Percentage",
        col = c("blue", "green"),
        ylim = c(0, 100))


White14 <- 42.6
Black14 <- 24.5
ggplot(data.frame(Race = c("White", "Black"), Percentage = c(White14, Black14)), aes(x = Race, y = Percentage, fill = Race)) +
  geom_bar(stat = "identity", color = "darkturquoise", width = 0.5, alpha = 0.4) +
  theme_minimal() +
  labs(title = "NYC Race Distribution 2014",
       x = "Race",
       y = "Percentage") +
  scale_fill_manual(values = c("White" = "pink", "Black" = "deeppink2")) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.3, size = 6, color = "white") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none")

barplot(c(White14, Black14), 
        names.arg = c("White", "Black"),
        main = "NYC Race Distribution 2014",
        xlab = "Race",
        ylab = "Percentage",
        col = c("blue", "green"),
        ylim = c(0, 100))

race12 <- dat_12_tib$race
race14 <- dat_14_tib$race

numstops12 <- length(race12)
numstops14 <- length(race14)

numstops2012 <- 532911
numstops2014 <- 45787

barplot(c(numstops12, numstops14),
        names.arg = c("2012", "2014"),
        main = "Stops Per Year",
        xlab = "Year",
        ylab = "Number of Stops",
        col = c("pink", "magenta"))



# Setting scipen option to prevent scientific notation
options(scipen = 999)

# Creating a bar graph
barplot(c(numstops12, numstops14),
        names.arg = c("2012", "2014"),
        main = "Stops Per Year",
        xlab = "Year",
        ylab = "Number of Stops",
        col = c("pink", "magenta"))

# Resetting scipen to default (optional)
options(scipen = 0)


# install and import needed libraries
install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

# Question 1
# create a function that manually computes standard deviation
x <- c(1,2,3)
st_dv <- function(x) {
  sqrt(sum((x - mean(x))^2/(length(x)- 1)))
}
st_dv(x)
# use assertthat to make sure sd_x and sd return the same values for 3 test vectors
library(assertthat)
a <- 1:25
b <- 20:22
c <- 2.5:45
assert_that(sd(a) == st_dv(a), sd(b) == st_dv(b), sd(c) == st_dv(c))

# Question 2
# create a tibble that contains two columns, 'coral' and  'health status'
coral_pop = c("Montipora capitata",
              "Porites compressa",
              "Porites lobata")
# randomly sample 100 coral species
coral = sample(coral_pop, size=100, replace=TRUE)
# randomly sample health status of coral using specific parameters
# TRUE = disease (prob = 0.3), FALSE = not diseased (prob = 0.8)
status = as.logical(c("TRUE", "FALSE"))
health_status = sample(status, size=100, replace=TRUE, prob = c(0.3, 0.8))
coral_health_tbl <- tibble(data.frame(coral,health_status))
coral_health_tbl

# Question 3
# create column that represents coral cover values
coral_health_tbl$coral_cover[coral_health_tbl$health_status==T]=rnorm(n=length(
  coral_health_tbl$coral[coral_health_tbl$health_status==T]),mean=9,sd=2)
coral_health_tbl$coral_cover[coral_health_tbl$health_status==F]=rnorm(n=length(
  coral_health_tbl$coral[coral_health_tbl$health_status==F]),mean=1,sd=3)
coral_health_tbl
                        
# Question 4
# add column of binary health status (status = FALSE, 0; status = TRUE, 1)
coral_health_tbl$binary_health_status <- as.integer(coral_health_tbl$health_status)
view(coral_health_tbl)

# Question 5
# plot the distribution of coral cover for healthy versus diseased corals
ggplot(data=coral_health_tbl, mapping = aes(x=coral_cover, y=stat(count / sum(count)), fill = health_status)) +
  scale_fill_manual(values = c("grey", "white"), name = "Health Status", labels = c("Diseased", "Healthy")) +
  geom_histogram(col = "black", binwidth=1) +
  labs(x = "% Coral Cover", y = "Normalized Count")

# Question 6  
# Create plot highlighting distribution per species
ggplot(coral_health_tbl, aes(x=coral_cover, fill=coral))+scale_fill_manual(values = c("green", "blue","navy"))+
  geom_density(aes(y=..count../sum(..count..)),adjust=2, colour = "black")+xlim(c(-10,25))  

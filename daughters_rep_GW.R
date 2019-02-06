# You should replicate everything EXCLUDING Column 6 in Table 5 and all of Table 6.
# Need 8 tables (1-9) and 1 figure?

# Load necessary packages.
library(tidyverse)
library(Zelig)
library("ZeligChoice")
library(xtable)		
library(reshape2)
library(apsrtable)
library(stargazer)
# library(rms)

# Load women cases.
women.cases <- read.csv("glynn_sen_daughters_by_case_1.csv", stringsAsFactors = FALSE) %>% 

# Remove male plaintiffs and keep only relevant cases.
  filter(femplaintiff == 1) %>% 
  filter(area == "employment" |
         area == "Title IX" |
         area == "pregnancy" |
         area == "abortion" |
         area == "reproductive rights") %>% 
  mutate(area = factor(area))

# Load judges data.
judge.means <- read.csv("glynn_sen_daughters_by_judge.csv", stringsAsFactors = FALSE) 

# Subset judge data for just
# those with fertility data.
all <- judge.means %>% 
  filter(! is.na(girls))

##############################
### Table 1: Number of children and girls
##############################
aa <- table(all$republican, all$child)
bb <- table(all$republican, all$girls)
xtable(aa)
xtable(bb)

##############################
### Table 2: Judge Demographics
##############################
dems <- filter(all, republican == 0)
reps <- filter(all, republican == 1)
women <- filter(all, woman == 1)
men <- filter(all, woman == 0)

# First row of mean number of kids.
mean.kids <- cbind(mean(all$child),
                   mean(dems$child),
                   mean(reps$child),
                   mean(women$child),
                   mean(men$child))

# Second row of mean number of girls.
mean.girls <- cbind(mean(all$girls),
                   mean(dems$girls),
                   mean(reps$girls),
                   mean(women$girls),
                   mean(men$girls))

# Third through ninth rows with the number of children.
# Loops through the number of columns in each table
# that contains frequency of number of children.
# The final table is empty and gets appended with each
# additional child number. The special case for n=7
# is for when we find the sum proportion of the 6+ children.
final <- table(NA, NA, NA, NA, NA)

# Loops through the number of children slots.
for (n in 1:7) {
  if (n < 7) {
    p <- cbind(
      prop.table(table(na.omit(all$child)))[n],
      prop.table(table(na.omit(dems$child)))[n],
      prop.table(table(na.omit(reps$child)))[n],
      prop.table(table(na.omit(women$child)))[n],
      prop.table(table(na.omit(men$child)))[n]
    )
    
    # Save the values to the final table.
    final <- rbind(final, p)
  }
  
  # Calculate the proportions manually for 6+ children.
  if (n == 7) {
    aa <- table(na.omit(all$child))
    bb <- table(na.omit(dems$child))
    cc <- table(na.omit(reps$child))
    dd <- table(na.omit(women$child))
    ee <- table(na.omit(men$child))
    
    p <- cbind(
      sum(aa[n:length(aa)])/sum(aa),
      sum(bb[n:length(bb)])/sum(bb),
      sum(cc[n:length(cc)])/sum(cc),
      sum(dd[n:length(dd)])/sum(dd),
      sum(ee[n:length(ee)])/sum(ee)
    )
    
    # Again save the final table values.
    final <- rbind(final, p)
  }
}

# Proportion female.
mean.female <- cbind(mean(na.omit(all$woman)), 
                     mean(na.omit(dems$woman)),
                     mean(na.omit(reps$woman)),
                     mean(na.omit(women$woman)),
                     mean(na.omit(men$woman))
)

# Proportion republican.
mean.rep <- cbind(mean(na.omit(all$republican)), 
                  mean(na.omit(dems$republican)),
                  mean(na.omit(reps$republican)),
                  mean(na.omit(women$republican)),
                  mean(na.omit(men$republican))
)

# Proportion white.
mean.white <- cbind(mean(na.omit(all$race == 1)), 
                    mean(na.omit(dems$race == 1)),
                    mean(na.omit(reps$race == 1)),
                    mean(na.omit(women$race == 1)),
                    mean(na.omit(men$race == 1))
)

# Mean year born.
mean.yearb <- cbind(mean(na.omit(all$yearb)), 
                    mean(na.omit(dems$yearb)),
                    mean(na.omit(reps$yearb)),
                    mean(na.omit(women$yearb)),
                    mean(na.omit(men$yearb))
)

# Number of judges in each category.
n_judges <- cbind(nrow(all),
                  nrow(dems),
                  nrow(reps),
                  nrow(women),
                  nrow(men))

# Create the final table.
demographic_table <- rbind(mean.kids,
                           mean.girls,
                           final,
                           mean.female,
                           mean.rep,
                           mean.white,
                           mean.yearb,
                           n_judges)

# Format specifications for the final table.
colnames(demographic_table) <- c("All", "Democrats", "Republicans","Women", "Men")
rownames(demographic_table) <- c("Mean No. Children",
                                 "Mean No. Girls",
                                 "Proportion who have 0 children",
                                 "1 children", "2 children", "3 children",
                                 "4 children", "5 Children", "6 Children or More",
                                 "Proportion Female", "Proportion Republican",
                                 "Proportion White", "Mean Year Born", "N")
xtable(demographic_table,
       digits = 2,
       caption = "Demographics of U.S. Court of Appeal Judges who voted on gender-related cases (1996-2002)",
       label = "t:statsgender",
       align = "l|ccccc")


### Calculating weights
no_cases <- matrix(data = NA, nrow = nrow(judge.means), ncol = 1)
for(i in 1:length(no_cases)){
  no_cases[i] <- nrow(women.cases[which(women.cases$name == judge.means$name[i]),])
}

judge.means <- cbind(judge.means, no_cases)

### Calculating the outcome variable
no_liberalvote <- matrix(data = NA, nrow = nrow(judge.means), ncol = 1)
for(i in 1:length(no_liberalvote)){
  stuff <- women.cases[which(women.cases$name == judge.means$name[i]),]
  no_liberalvote[i] <- nrow(subset(stuff, vote == 2 | vote == 3))
}

# Additional variable for liberal vote share.
lib_vote_share <- no_liberalvote/no_cases

judge.means <- cbind(judge.means, no_liberalvote, lib_vote_share)
judge.means <- judge.means %>% 
  filter(! is.na(girls))

### Subsetting Data to Various Populations (for use later)
# Subset women, men, republicans, and democrats
women.means <- judge.means %>% 
                filter(woman == 1)
men.means <- judge.means %>% 
                filter(woman == 0)
rep.means <- judge.means %>% 
                filter(republican == 1)
dem.means <- judge.means %>% 
                filter(republican == 0)

##############################
### Fig. 1
##############################
# plot(density(judge.means$lib_vote_share), 
#      xlim = c(-.4,1.4), ylim = c(0,2.5),
#      ylab = "", xlab = "Proportion of Cases Decided in a Feminist Direction", 
#      yaxt = "n", 
#      bty = "n", 
#      main = "",
#      col = "black", lwd = 2)
# lines(density(rep.means$lib_vote_share), 
#       col = "firebrick", lwd = 2, lty  = 2)
# lines(density(dem.means$lib_vote_share), 
#       col = "dodgerblue", lwd = 2, lty  = 3)
# abline(v = .5, col = "grey50", lty = 2)
# text(x = .5, y = 2.4, "Less Feminist", col = "grey50", pos = 2, cex = 0.9)
# text(x = .5, y = 2.4, "More Feminist", col = "grey50", pos = 4, cex = 0.9)
# text(x = .25, y = 1.7, "Republicans", pos = 2, cex = 0.9)
# text(x = .7, y = 1, "Democrats", pos = 4, cex = 0.9)
# text(x = .075, y = .6, "All", pos = 4, cex = 0.9)

ggplot(judge.means, aes(lib_vote_share)) +
  geom_density() +
  geom_density(data = rep.means, aes(lib_vote_share), linetype = "dashed") +
  geom_density(data = dem.means, aes(lib_vote_share), linetype = "dotted") +
  geom_vline(xintercept = .5, linetype = 2, alpha = .25) +
  xlim(-.4,1.4) + ylim(0,2.5) +
  labs(x = "Proportion of Cases Decided in a Feminist Direction") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

########################################################
## Table 4: Core Results (WLS)
########################################################

################ Results for all judges

#judge.means <- subset(judge.means, child > 0)

my.out1 <- lm(lib_vote_share ~ as.factor(girls) + as.factor(child), 
              data = judge.means, weights = judge.means$no_cases)

my.out2 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child), 
              data = judge.means, weights = judge.means$no_cases)

my.out3 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3), 
              data = judge.means, weights = judge.means$no_cases)

my.out4 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3) + as.factor(circuit.1), 
              data = judge.means, weights = judge.means$no_cases)

################ Results for judges between 1 and 4 children

my.out5 <- lm(lib_vote_share ~ as.factor(girls) + as.factor(child), 
              data = subset(judge.means, child < 5 & child > 0), weights = judge.means$no_cases[which(judge.means$child > 0 & judge.means$child < 5)])

my.out6 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child), 
              data = subset(judge.means, child < 5 & child > 0), weights = judge.means$no_cases[which(judge.means$child > 0 & judge.means$child < 5)])

my.out7 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3), 
              data = subset(judge.means, child < 5 & child > 0), weights = judge.means$no_cases[which(judge.means$child > 0 & judge.means$child < 5)])

my.out8 <- lm(lib_vote_share ~ I(girls>0) + as.factor(child) + republican + age + I(religion == 4) + woman + I(race == 2) + I(race ==3) + as.factor(circuit.1), 
              data = subset(judge.means, child < 5 & child > 0), weights = judge.means$no_cases[which(judge.means$child > 0 & judge.means$child < 5)])

stargazer(my.out1, my.out2, my.out3, my.out4, my.out5, my.out6, my.out7, my.out8,
          style = "ajps", omit.stat = c("f","ser"), dep.var.labels = "Weighted least squares results, gender cases only. Outcome is proportion of feminist votes.  Models 1--4 are for all judges, while Models 5--8 are for judges with 1--4 children.  (No judge among those with 1--4 children had four girls.) All models include fixed effects for number of children and use weights based on the number of cases heard by each judge.", digits = 2, omit = c("circuit"), covariate.labels = c("1 Girl","2 Girls","3 Girls","4 Girls","5 Girls","At Least 1 Girl",	
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "1 Child","2 Children","3 Children","4 Children","5 Children","6 Children","7 Children","8 Children",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "9 Children","Republican","Age at Investiture","Catholic","Woman","African American","Hispanic","Constant"))




##############################
### Fig. 7
##############################

# Limit the sample to only judges with 1-4 kids.
rep.means <- filter(rep.means, child < 5 & child > 0)
dem.means <- filter(dem.means, child < 5 & child > 0)
men.means <- filter(men.means, child < 5 & child > 0)
women.means <- filter(women.means, child < 5 & child > 0)

# Do the same for cases.
rep.cases <- filter(women.cases, child < 5 & child > 0 & republican == 1)
dem.cases <- filter(women.cases, child < 5 & child > 0 & republican == 0)
men.cases <- filter(women.cases, woman == 0)
women.cases <- filter(women.cases, woman == 1)

### Party

my.outPARTY <- lm(lib_vote_share ~ I(girls > 0)*republican + as.factor(child), 
                  data = judge.means, weights = judge.means$no_cases)

## for republicans

my.outREP <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
                data = rep.means, weights = rep.means$no_cases)


## for democrats

my.outDEM <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
                data = dem.means, weights = dem.means$no_cases)

################ Gender

## for men

my.outGENDER <- lm(lib_vote_share ~ I(girls > 0)*woman + as.factor(child), 
                   data = judge.means, weights = judge.means$no_cases)

## for men

my.outMEN <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
                data = men.means, weights = men.means$no_cases)

## for women

my.outWOMEN <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
                  data = women.means, weights = women.means$no_cases)

my.outREPMEN <- lm(lib_vote_share ~ I(girls > 0) + as.factor(child), 
                   data = subset(men.means, republican == 1), weights = men.means$no_cases[which(men.means$republican == 1)])

stargazer(my.outREP, my.outDEM, my.outMEN, my.outWOMEN, my.outREPMEN,
          style = "ajps", omit.stat = c("f","ser"), dep.var.labels = "Share of Votes in Feminist Direction", digits = 2, covariate.labels = c("At Least 1 Girl", "2 Children", "3 Children", "4 Children","Constant"), title = "Weighted least squares results. Outcome is judges' proportion of feminist votes on gender-related cases. All models include fixed effects for total number of children and use weights based on the number of cases heard by each judge.", label = "t:results_party")

=======
# You should replicate everything EXCLUDING Column 6 in Table 5 and all of Table 6.
# Need 8 tables (1-9) and 1 figure?

# Load necessary packages.
library(tidyverse)
library(Zelig)
library("ZeligChoice")
library(xtable)		
library(reshape2)
library(apsrtable)
library(stargazer)
library(knitr)
# library(rms)

# Load women cases.
women.cases <- read.csv("glynn_sen_daughters_by_case_1.csv", stringsAsFactors = FALSE) %>% 

# Remove male plaintiffs and keep only relevant cases.
  filter(femplaintiff == 1) %>% 
  filter(area == "employment" |
         area == "Title IX" |
         area == "pregnancy" |
         area == "abortion" |
         area == "reproductive rights") %>% 
  mutate(area = factor(area))

# Load judges data.
judge.means <- read.csv("glynn_sen_daughters_by_judge.csv", stringsAsFactors = FALSE) 

# Subset judge data for just
# those with fertility data.
all <- judge.means %>% 
  filter(! is.na(girls))

##############################
### Table 1: Number of children and girls
##############################
aa <- table(all$republican, all$child)
bb <- table(all$republican, all$girls)
xtable(aa)
xtable(bb)

##############################
### Table 2: Judge Demographics
##############################
dems <- filter(all, republican == 0)
reps <- filter(all, republican == 1)
women <- filter(all, woman == 1)
men <- filter(all, woman == 0)

# First row of mean number of kids.
mean.kids <- cbind(mean(all$child),
                   mean(dems$child),
                   mean(reps$child),
                   mean(women$child),
                   mean(men$child))

# Second row of mean number of girls.
mean.girls <- cbind(mean(all$girls),
                   mean(dems$girls),
                   mean(reps$girls),
                   mean(women$girls),
                   mean(men$girls))

# Third through ninth rows with the number of children.
# Loops through the number of columns in each table
# that contains frequency of number of children.
# The final table is empty and gets appended with each
# additional child number. The special case for n=7
# is for when we find the sum proportion of the 6+ children.
final <- table(NA, NA, NA, NA, NA)

# Loops through the number of children slots.
for (n in 1:7) {
  if (n < 7) {
    p <- cbind(
      prop.table(table(na.omit(all$child)))[n],
      prop.table(table(na.omit(dems$child)))[n],
      prop.table(table(na.omit(reps$child)))[n],
      prop.table(table(na.omit(women$child)))[n],
      prop.table(table(na.omit(men$child)))[n]
    )
    
    # Save the values to the final table.
    final <- rbind(final, p)
  }
  
  # Calculate the proportions manually for 6+ children.
  if (n == 7) {
    aa <- table(na.omit(all$child))
    bb <- table(na.omit(dems$child))
    cc <- table(na.omit(reps$child))
    dd <- table(na.omit(women$child))
    ee <- table(na.omit(men$child))
    
    p <- cbind(
      sum(aa[n:length(aa)])/sum(aa),
      sum(bb[n:length(bb)])/sum(bb),
      sum(cc[n:length(cc)])/sum(cc),
      sum(dd[n:length(dd)])/sum(dd),
      sum(ee[n:length(ee)])/sum(ee)
    )
    
    # Again save the final table values.
    final <- rbind(final, p)
  }
}

# Proportion female.
mean.female <- cbind(mean(na.omit(all$woman)), 
                     mean(na.omit(dems$woman)),
                     mean(na.omit(reps$woman)),
                     mean(na.omit(women$woman)),
                     mean(na.omit(men$woman))
)

# Proportion republican.
mean.rep <- cbind(mean(na.omit(all$republican)), 
                  mean(na.omit(dems$republican)),
                  mean(na.omit(reps$republican)),
                  mean(na.omit(women$republican)),
                  mean(na.omit(men$republican))
)

# Proportion white.
mean.white <- cbind(mean(na.omit(all$race == 1)), 
                    mean(na.omit(dems$race == 1)),
                    mean(na.omit(reps$race == 1)),
                    mean(na.omit(women$race == 1)),
                    mean(na.omit(men$race == 1))
)

# Mean year born.
mean.yearb <- cbind(mean(na.omit(all$yearb)), 
                    mean(na.omit(dems$yearb)),
                    mean(na.omit(reps$yearb)),
                    mean(na.omit(women$yearb)),
                    mean(na.omit(men$yearb))
)

# Number of judges in each category.
n_judges <- cbind(nrow(all),
                  nrow(dems),
                  nrow(reps),
                  nrow(women),
                  nrow(men))

# Create the final table.
demographic_table <- rbind(mean.kids,
                           mean.girls,
                           final,
                           mean.female,
                           mean.rep,
                           mean.white,
                           mean.yearb,
                           n_judges)

# Format specifications for the final table.
colnames(demographic_table) <- c("All", "Democrats", "Republicans","Women", "Men")
rownames(demographic_table) <- c("Mean No. Children",
                                 "Mean No. Girls",
                                 "Proportion who have 0 children",
                                 "1 children", "2 children", "3 children",
                                 "4 children", "5 Children", "6 Children or More",
                                 "Proportion Female", "Proportion Republican",
                                 "Proportion White", "Mean Year Born", "N")
xtable(demographic_table,
       digits = 2,
       caption = "Demographics of U.S. Court of Appeal Judges who voted on gender-related cases (1996-2002)",
       label = "t:statsgender",
       align = "l|ccccc")


### Calculating weights
no_cases <- matrix(data = NA, nrow = nrow(judge.means), ncol = 1)
for(i in 1:length(no_cases)){
  no_cases[i] <- nrow(women.cases[which(women.cases$name == judge.means$name[i]),])
}

judge.means <- cbind(judge.means, no_cases)

### Calculating the outcome variable
no_liberalvote <- matrix(data = NA, nrow = nrow(judge.means), ncol = 1)
for(i in 1:length(no_liberalvote)){
  stuff <- women.cases[which(women.cases$name == judge.means$name[i]),]
  no_liberalvote[i] <- nrow(subset(stuff, vote == 2 | vote == 3))
}

lib_vote_share <- no_liberalvote/no_cases

judge.means <- cbind(judge.means, no_liberalvote, lib_vote_share)
judge.means <- judge.means %>% 
  filter(! is.na(girls))

### Subsetting Data to Various Populations (for use later)
# Subset women, men, republicans, and democrats
women.means <- judge.means %>% 
                filter(woman == 1)
men.means <- judge.means %>% 
                filter(woman == 0)
rep.means <- judge.means %>% 
  filter(republican == 1)
dem.means <- judge.means %>% 
  filter(republican == 0)

##############################
### Fig. 1
##############################
plot(density(judge.means$lib_vote_share), 
     xlim = c(-.4,1.4), ylim = c(0,2.5),
     ylab = "", xlab = "Proportion of Cases Decided in a Feminist Direction", 
     yaxt = "n", 
     bty = "n", 
     main = "",
     col = "black", lwd = 2)
lines(density(rep.means$lib_vote_share), 
      col = "firebrick", lwd = 2, lty  = 2)
lines(density(dem.means$lib_vote_share), 
      col = "dodgerblue", lwd = 2, lty  = 3)
abline(v = .5, col = "grey50", lty = 2)
text(x = .5, y = 2.4, "Less Feminist", col = "grey50", pos = 2, cex = 0.9)
text(x = .5, y = 2.4, "More Feminist", col = "grey50", pos = 4, cex = 0.9)
text(x = .25, y = 1.7, "Republicans", pos = 2, cex = 0.9)
text(x = .7, y = 1, "Democrats", pos = 4, cex = 0.9)
text(x = .075, y = .6, "All", pos = 4, cex = 0.9)

ggplot(judge.means, aes(lib_vote_share)) +
  geom_density() +
  geom_density(data = rep.means, aes(lib_vote_share), linetype = "dashed") +
  geom_density(data = dem.means, aes(lib_vote_share), linetype = "dotted") +
  geom_vline(xintercept = .5, linetype = 2, alpha = .25) +
  xlim(-.4,1.4) + ylim(0,2.5) +
  labs(x = "Proportion of Cases Decided in a Feminist Direction") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

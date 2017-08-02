# code for creating input data for ADMB model  
# ben.williams@alaska.gov
# last updated 2017-07-31

# load ----
library(tidyverse)
library(mgcv)
library(broom)

# data ----
fishery <- read_csv("data/fish_bio_data.csv")

# data cleaning ----
names (fishery) <- c('year', 'proj', 'trip', 'adfg', 'Vessel', 'sell_date', 
							'g_stat', 'Area', 'G_group', 'sample_code', 'Sample_type',
							'id', 'spec_code', 'Species', 'length_code', 'Length_type',
							'length', 'weight', 'age', 'Readability_code', 'Readability',
							'sex', 'maturity', 'Maturity_state', 'Gear_code', 'Gear')
fishery %>% 
	mutate(Year = factor(year)) -> fishery

fishery %>% 
	filter(sex==2, !is.na(length), !is.na(age)) -> female

# create maturity dataset
fishery %>% 
	filter(maturity < 9 & !is.na(maturity), 
			 sex==2, !is.na(age), !is.na(length)) %>% 
	mutate(mature = ifelse(maturity<3, 0, 1),
			 mature = ifelse(age > 40, 1, mature),
			 age = ifelse(age > 75, 75, age),
			 Mature = factor(mature)) -> mat
# eda ----

mat %>% 
	ggplot(aes(year, length, color=Mature)) + geom_jitter()

mat %>% 
	ggplot(aes(year, age, color=Mature)) + geom_jitter()


fit <- glm(Mature ~ age, data = mat, family=binomial)
summary(fit)


newd <- data.frame(age=8:75)
newd$pred = predict(fit, newd, type='response')

ggplot(newd, aes(age, pred)) + stat_summary(fun.data=mean_cl_boot, geom='smooth')
newd %>% 
	arrange(age)

# length-at-age ----
add <- nls(length ~ Linf * (1 - exp(-k * (age - t0))), start=list(Linf=600, k=0.1, t0=0.1), data=female)
mult <- nls(log(length) ~ log(Linf) + log(1 - exp(-k * (age - t0))), start=list(Linf=6, k=0.1, t0=-2), data=female)

tidy(add)

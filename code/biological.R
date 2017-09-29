# code for creating input data for ADMB model  
# ben.williams@alaska.gov
# last updated 2017-07-31

# load ----
library(tidyverse)
library(mgcv)
library(broom)

# data ----
fishery <- read_csv("data/fish_bio_data.csv")
rov <- read_csv("data/YE_Length_Data_forKray.csv")

# data cleaning ----
names (fishery) <- c('year', 'proj', 'trip', 'adfg', 'Vessel', 'sell_date', 
							'g_stat', 'Area', 'G_group', 'sample_code', 'Sample_type',
							'id', 'spec_code', 'Species', 'length_code', 'Length_type',
							'length', 'weight', 'age', 'Readability_code', 'Readability',
							'sex', 'maturity', 'Maturity_state', 'Gear_code', 'Gear')
fishery %>% 
	mutate(Year = factor(year)) -> fishery

fishery %>% 
	filter(sex==2, !is.na(age), !is.na(weight)) %>% 
	mutate(age = ifelse(age > 75, 75, age)) %>% 
	dplyr::select(age, weight)-> wt

write_delim(wt, "vonb/wt_dat")
# create maturity dataset
female %>% 
	filter(maturity < 9 & !is.na(maturity)) %>% 
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

ggplot(newd, aes(age, pred)) + geom_line()

# 50% maturity ----
-coef(fit)[1] / coef(fit)[2]

# length-at-age ----
# use the ADMB code 
Linf = 658.7700
k = 0.0389
t0 = -11.6100

lvb <- function(age, k, Linf, t0) 
{
	Linf*(1-exp(-k*(age-t0)))
}

plot(8:75, lvb(8:75, k, Linf, t0))

# weight-at-age ----

waa <- gam(weight~s(age, k=3), data=mat, gamma=1.4, Gamma(link="log"))
plot(waa)
summary(waa)
sum(resid(waa)^2)
# 10108.77



# use the ADMB code 
Winf = 6.33510564697
k = 0.0253631950840
t0 = 5.28567887127

wvb <- function(age, k, Winf, t0) 
{
	Winf*(1-exp(-k*(age-t0)))
}

plot(8:75, wvb(8:75, k, Winf, t0))

wt %>% 
	mutate(wvb = wvb(age,k, Winf, t0),
			 gam = predict(waa, ., type='response'), 
			 vbr = weight-wvb,
			 gr = weight-gam) %>% 
	summarise(vb = sum(vbr^2),
				 g = sum(gr^2))


%>% 
	ggplot(aes(age, wvb)) + geom_line() + 
	geom_line(aes(age, gam), color=4)
	

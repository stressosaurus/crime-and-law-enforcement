StageFiveData <- read_csv("~/police-violence-study/reports/report3/Data/stage5.csv")
df_MAIN <- read_csv("~/police-violence-study/reports-main/report3/Data/lagSlopesOIS.csv")

# keeping relevant variables
dem_s5 <- StageFiveData %>% filter(year > 2012) %>% 
  select(1,2,4,10,5,54,3,77,31,26,32,40,33,34,35,36,37,38,39,51,105,106,107,93, 85:102) %>% 
  mutate(prop_hispanic = (estimate_Hispanic/estimate_Total),
         prop_af_american = (estimate_Af_American/estimate_Total),
         prop_white = (estimate_White/estimate_Total),
         prop_asian = (estimate_Asian/estimate_Total),
         prop_under_pov = (estimate_Below_Poverty/estimate_Total))


dem_s5 <- merge(dem_s5, df_MAIN, by = c("GEOID", "Rdate", "OISperMonth"), all.x = TRUE)

# checking the assumptions ----
## making a OIStotal variable to sum the OIS per month per agency

dem_s5hist <- dem_s5 %>% filter(year > 2012) %>%  group_by(GEOID) %>% 
  summarise(OIStotal = sum(OISperMonth, na.rm = TRUE))

## making the histogram

dem_s5hist %>% ggplot(aes(x = OIStotal)) + geom_histogram( bins = 20)

# checking the mean and variance

mean(dem_s5hist$OIStotal)
var(dem_s5hist$OIStotal)

# as it turns out, our data has over-dispersion in it because variance is much 
# greater than the mean

modelinit <- glm(OISperMonth ~ slope_15 +  prop_under_pov + prop_af_american + prop_hispanic + prop_asian,
                   family = poisson, offset = log(U_TPOP), data = dem_s5)
summary(modelinit)
confint(modelinit, level = .90)
exp(modelinit$coefficients)
modelinit$fitted.values


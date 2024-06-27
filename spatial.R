library(spData)
library(ggplot2)
library(sf)
library(readr)
library(readxl)
library(stargazer)
library(spatialreg)
library(spdep)
library(sp)
# lets view the world data
View(world)
# lets import our asean countries dataset
library(readxl)
asia <- read_excel("asia.xlsx")
Data_Tables_1_ <- read_excel("Data Tables (1).xlsx")
pollution <- read_excel("Data Tables (1).xlsx", 
                        sheet = "Greenhouse Gases", skip = 1)
# lets add other columns to the original dataset
asia$co2=pollution$`CO2 Emissions (million tonnes)`
asia$ch4=pollution$`CH4 Emissions (thousand tonnes)`
asia$n20=pollution$`N2O Emissions (thousand tonnes)`



asia$gini_coefficient=Data_Tables_1_$`Gini Coefficient`
#lets edit a column with an issue
asia[1,1]=asia[2,1]
asia$Country[asia$Country=='Laos']='Lao PDR'
# clean the dataframe
asia[asia == '-']=0

# lets rename the columns

colnames(asia)=c('Country', 'Year', 
                 'tot_pop', 'urban_pop', 'lifeexp', 'pop_growth', 'GDP', 'GDP_per_capita', 'unemployment_rate'
                 ,'poverty_rate','HDI','energy_consumption', 'energy_production', 'co2',
                 'ch4', 'n20', 'Gini_coefficient')

# lets aggregate by mean the whole dataset

asia_means=aggregate(. ~ Country, data = asia[,-c(9,10, 11)], FUN = mean)

# lets merge the dataset with the world dataset
merged <- merge(world,asia_means, by.y ='Country', by.x = 'name_long', all.y = T)
# lets edit the data further

merged=merged[-8,]# we are removing singapor
# lets start the plotting

ggplot(merged) +
  geom_sf( aes(fill = tot_pop), color = "black") +
  geom_sf_text(aes(label=name_long))+
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "South Asean population") +
  theme_minimal()

  ggplot(merged) +
    geom_sf( aes(fill = urban_pop), color = "black") +
    geom_sf_text(aes(label=name_long))
  scale_fill_gradient(low = "blue", high = "red") +
    ggtitle( "South Asean urban population") +
    theme_minimal()
  
  
  ggplot(merged) +
    geom_sf( aes(fill = GDP), color = "black") +
    geom_sf_text(aes(label=name_long))+
  scale_fill_gradient(low = "blue", high = "red") +
    ggtitle( "South Asean GDP") +
    theme_minimal()
  
  ggplot(merged) +
    geom_sf( aes(fill = energy_consumption), color = "black") +
    geom_sf_text(aes(label=name_long))+
  scale_fill_gradient(low = "blue", high = "red") +
    ggtitle( "South Asean Energy consumption") +
    theme_minimal()
  
  ggplot(merged) +
    geom_sf( aes(fill = pop_growth), color = "black") +
    geom_sf_text(aes(label=name_long))+
  scale_fill_gradient(low = "blue", high = "red") +
    ggtitle( "South Asean population growth") +
    theme_minimal()
  
  ggplot(merged) +
    geom_sf( aes(fill = Gini_coefficient), color = "black") +
    geom_sf_text(aes(label=name_long))+
    scale_fill_gradient(low = "blue", high = "red") +
    ggtitle( "South Asean Gini coefficient") +
    theme_minimal()
# lets perform the linear regression of the countries data
  model1=lm(Gini_coefficient~., data = asia[, -c(1,2)])
summary(model1)  

stargazer(model1, out = 'reg1.docx', type = 'text')
# fit the spatial regression model
# Define the country names
ncovr_sp <- as(merged, "Spatial")
#Then we create a list of neighbours using the Queen criteria
w <- poly2nb(ncovr_sp, queen=T, row.names=ncovr_sp$name_long, set.ZeroPolicyOption(TRUE))
# Fit the spatial lag regression
rwm_n <- nb2listw(w, style='W',glist = NULL, zero.policy=TRUE)
# morans test
lm.morantest(model1, rwm_n)

fit_2_lag <- lagsarlm(Gini_coefficient~tot_pop+urban_pop+ lifeexp+ pop_growth+
                      GDP+ GDP_per_capita+energy_consumption+
                        energy_production+ co2+ch4+n20, data = ncovr_sp, rwm_n, zero.policy = T,na.action = na.omit)
summary(fit_2_lag)

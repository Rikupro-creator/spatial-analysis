library(spData)
library(ggplot2)
library(sf)
library(readr)
library(readxl)
library(stargazer)

# lets import the datasets that we will need for this analysis
# GDP data 2019
GDP <- read_csv("GDP.csv")
colnames(GDP)[2]='GDP_2019'
# unemployment data
unemployment <- read_csv("unemployment.csv")
# life expectancy
lifeexp <- read_csv("lifeexp.csv")
colnames(lifeexp)[2]='lifeexp_2019'
# get GRI data
GRI <- read_excel("GRI.xlsx")
colnames(GRI)[4]='GRI_score'

# we are importing the new dataset so that we can combine them and make sure that we have the dataset that we need.
# merge the datasets
merged_data <- merge(world, unemployment, by.x = "name_long", by.y = "Country Name", all.x=T)
# we have merged with employment
# lets merge with GDP
merged_data <- merge(merged_data, GDP, by.x = "name_long", by.y = "Country Name", all.x=T)
# merge with life expectancy
merged_data <- merge(merged_data, lifeexp, by.x = "name_long", by.y = "Country Name", all.x=T)
# lets edit the column names of GDP and Lifeexpectancy added to the dataset

# plot 2019 GDP

ggplot(merged_data) +
  geom_sf(aes(fill = GDP_2019), color = "black") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "World GDP 2019", fill = "GDP 2019") +
  theme_minimal()

# plot for unemployment

ggplot(merged_data) +
  geom_sf(aes(fill = unemployment), color = "black") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "World Unemployment rate in 2022", fill = "Unemployment") +
  theme_minimal()
# lets plot  life expectancy

ggplot(merged_data) +
  geom_sf(aes(fill = lifeexp_2019), color = "black") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "World life expectancy in 2019", fill = "Life expectancy 2019") +
  theme_minimal()

# regression analysis
 reg_data=merge(GRI, lifeexp, by.x = "Country", by.y = "Country Name", all.x=T)
 reg_data=merge(reg_data, GDP, by.x = "Country", by.y = "Country Name", all.x=T)
 reg_data=merge(reg_data, unemployment, by.x = "Country", by.y = "Country Name", all.x=T)
 
 # clean the data
 reg_data=na.omit(reg_data)
 # summary statistics
 
summary(reg_data)
# correlation analysis

# Calculate the correlation matrix
cor_matrix <- cor(reg_data[, 4:7])

# Plot the correlation matrix with correlation values
corrplot::corrplot(cor_matrix, method = "number", is.corr = TRUE)
# linear regressions

model1= lm(GDP_2019~GRI_score, data=reg_data)
model2=lm(unemployment~GRI_score, data=reg_data)
model3=lm(lifeexp_2019~GRI_score, data=reg_data)

stargazer(model1, model2, model3, type = "text", out = "regmodels.docx", title = "Regression Models", align = TRUE, header = FALSE, digits = 3, single.row = TRUE)

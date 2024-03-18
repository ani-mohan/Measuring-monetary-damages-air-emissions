###Script for calculation of economic damages from PM2.5 and CO2. 

library("tseries")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library("caTools")


co2_data_1970_2018 <- read_xls("Data/EDGARv5.0_FT2018_fossil_CO2_GHG_booklet2019.xls", sheet = "fossil_CO2_totals_by_country")
co2_emissions <- co2_data_1970_2018[, -c(2:29)]   #get rid of years we do not need
countries = unique(co2_emissions$country_name)   #list of countries in dataset
co2_emissions[,2:22] <- round(co2_emissions[, 2:22]*1000,2)   #convert to kilotonnes

#Name changes to ensure consistency of naming 
co2_emissions <- co2_emissions[order(co2_emissions$country_name), ]
co2_emissions[51,1] = 'Czech Republic'
co2_emissions[70,1] = 'France'   #technically france and monaco but assumed to be just france
co2_emissions[99,1] = 'Israel'
co2_emissions[100,1] = 'Italy'     #technically Italy and Vatican see but assumed to be just italy since no Vatican in gdp data
co2_emissions[109,1] = 'Lao PDR'
co2_emissions[158,1] = 'Russian Federation'
co2_emissions[179,1] = 'Spain'    #technically Spain and Andorra but assumed to be just Spain
co2_emissions[184,1] = 'Switzerland'    #technically Switzerland and Lichtenstein but assumed to be just Switzerland
co2_emissions[169,1] = 'Serbia'    #technically Serbia and Montenegro but assumed to be just Serba


#incorporate social cost of carbon 
year_scc <- c(2010, 2015, 2020, 2025, 2030,2035, 2040, 2045, 2050)
scc <- c(31, 36, 42, 46, 50, 55, 60, 64, 69)
model1 <- lm(log(scc) ~ year_scc)
year <- 1998:2018
log_scc_values <- model1$coefficients[1] + (year *model1$coefficients[2])
social_cost_perton <- exp(log_scc_values)
social_cost_perton <- social_cost_perton * 1.0517  #converting 2007 prices to 2010 prices https://www.in2013dollars.com/2007-dollars-in-2010?amount=100

#pull in gdp data
gdp_data <- read_xls("~/Desktop/GDP_econ_damages/2018_data/API_NY.GDP.MKTP.KD_DS2_en_excel_v2_820928.xls", sheet = "Data")
gdp_data <- gdp_data[-c(1:2), ]    #drop some rows from spreadsheet
colnames(gdp_data) <- gdp_data[1, ]   #change column names to row that has it
gdp_data <- gdp_data[-c(1), ]         #drop the header row since we incorporated it 
gdp_data <- gdp_data[, -c(2:42)]     #drop 1960-1997 and codes etc
gdp_data <- gdp_data[, -c(23)]     #drop 2019
gdp_data <- gdp_data[order(gdp_data$`Country Name`), ]
#change country names to match emissions data
gdp_data[15, 1] = 'Bahamas'
gdp_data[30, 1] = 'Brunei'
gdp_data[48, 1] = 'Democratic Republic of the Congo'
gdp_data[49, 1] = 'Congo'
gdp_data[66, 1] = 'Egypt'
gdp_data[85, 1] = 'The Gambia'
gdp_data[102, 1] = 'Hong Kong'
gdp_data[112, 1] = 'Iran'
gdp_data[124, 1] = 'North Korea'
gdp_data[125, 1] = 'South Korea'
gdp_data[146,1] = 'Macao'
gdp_data[213, 1] = 'Slovakia'
gdp_data[224,1] = 'Saint Kitts and Nevis'
gdp_data[257, 1] = 'Venezuela'
gdp_data[262, 1] = 'Yemen'

#pull in income_classifications
income_hist_class <- read_xlsx("Data/income_historic.xlsx")
income_hist_class$Code <- NULL
colnames(income_hist_class) <- c('Country', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008',
                                 '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')
income_hist_class <- income_hist_class[order(income_hist_class$`Country`), ]
rownames(income_hist_class) <- 1:218
income_hist_class$Country <- as.character(income_hist_class$Country)
#change country names to match emissions data
income_hist_class[14, 1] = 'Bahamas'
income_hist_class[29, 1] = 'Brunei'
income_hist_class[33, 1] = co2_emissions[46,1]
income_hist_class[46, 1] = 'Democratic Republic of the Congo'
income_hist_class[47, 1] = 'Congo'
income_hist_class[59, 1] = 'Egypt'
income_hist_class[72, 1] = 'The Gambia'
income_hist_class[87, 1] = 'Hong Kong'
income_hist_class[92, 1] = 'Iran'
income_hist_class[104, 1] = 'North Korea'
income_hist_class[105, 1] = 'South Korea'
income_hist_class[118,1] = 'Macao'
income_hist_class[174, 1] = 'Slovakia'
income_hist_class[182,1] = 'Saint Kitts and Nevis'
income_hist_class[212, 1] = 'Venezuela'
income_hist_class[216, 1] = 'Yemen'
income_hist_class[income_hist_class$Country=='Serbia', 2:9] = 'UM'    #no income hist class data for Serbia 1998-2005 so we assume it is the same class as it was in 2006 which is UM 

countries_final = country_list   #final list of 165 countries 

# pull in population data for ALL AGES ONLY - this is different from population across age groups in the PM mortality calculations
population_all <- read.csv('Data/population_all_2022.csv')
population_all<- population_all[population_all$location_name %in% countries_final, ]
population_all <- population_all[order(population_all$location_name), ]     #order by location name  

population_all_final <- co2_emissions[FALSE, ]
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    pop_values1 <- round(population_all[population_all$location_name==country, 7], 0)    #pull in the best estimate value for each country and create the dataframe
    population_all_final[i, 1] <- country
    population_all_final[i, 2:22] = t(pop_values1) #fill in 1998-2018 values
}


#calculate gdp growth rates
gdp_growthrates = co2_emissions[FALSE,] #create empty data frame
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    growth_rate <- c()
    growth_rate[1] = 'NA'   #add na for first year 1998
    for (j in 2:21) {
        gdp2 = as.numeric(gdp_data[gdp_data$`Country Name`==country, j+1])
        gdp1 = as.numeric(gdp_data[gdp_data$`Country Name`==country, j])
        if(is.na(gdp2)) {
            growth_rate[j] = 'NA'} 
        if(is.na(gdp1)) {
            growth_rate[j] = 'NA' 
        }
        else {
            growth_rate[j] = round(((gdp2 - gdp1) / gdp1)*100,2)
        }
    }
    gdp_growthrates[i, 1] <- country
    gdp_growthrates[i, 2:22] = t(as.numeric(growth_rate))     #total growth rates
}

#calculate gdp per capita
gdp_per_capita <- co2_emissions[FALSE, ]
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    incomes <- c()
    for (j in 1:21){
        pop = population_all_final[population_all_final$country_name==country, j+1]
        gdp1 = gdp_data[gdp_data$`Country Name`==country, j+1]
        if(is.na(gdp1)){
            incomes[j] = 'NA'}
        else{
            incomes[j] = round(as.numeric(gdp1)/as.numeric(pop), 1)
        }
    }
    gdp_per_capita[i, 1] <- country  
    gdp_per_capita[i, 2:22] <- t(as.numeric(incomes))
}


#calculate gdp growth rates per capita
gdp_growthrates_percapita = co2_emissions[FALSE,] #create empty data frame
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    growth_rate <- c()
    growth_rate[1] = 'NA'    #add na for first year 1998 as we don't have 1997 data for pollution so don't need gdp either
    for (j in 2:21) {
        gdp2 = as.numeric(gdp_per_capita[gdp_per_capita$country_name==country, j+1])
        gdp1 = as.numeric(gdp_per_capita[gdp_per_capita$country_name==country, j])
        if(is.na(gdp2)) {
            growth_rate[j] = 'NA'} 
        if(is.na(gdp1)) {
            growth_rate[j] = 'NA' 
        }
        else {
            growth_rate[j] = round(((gdp2 - gdp1) / gdp1)*100,2)
        }
    }
    gdp_growthrates_percapita[i, 1] <- country
    gdp_growthrates_percapita[i, 2:22] <- t(as.numeric(growth_rate))
}


### VSL calculations
vsl_2006 <- co2_emissions[FALSE, ]
vsl_2006 <- vsl_2006[, -c(2:9)]
vsl_2006 <- vsl_2006[, -c(3:14)]
us_p_income_2006 = as.numeric(gdp_per_capita[gdp_per_capita$country_name=='United States', 10])
for (i in 1:length(countries_final)){
    country = countries_final[i]
    class = as.character(income_hist_class[income_hist_class$Country==country, 10])
    p_income_2006 = as.numeric(gdp_per_capita[gdp_per_capita$country_name==country, 10])
    if (class == 'H'){
        elasticity = 0.8
        country_vsl_2006 =  round((p_income_2006/us_p_income_2006)^elasticity * 7.4e6*1.08,1)        #Calculating all US VLS in 2010 prices from US value of 7.4 million in 2006
    }
    else if (class==".."){
        elasticity = 'NA'
        country_vsl_2006 = 'NA'
    }
    else{
        elasticity = 1.2
        country_vsl_2006 =  round((p_income_2006/us_p_income_2006)^elasticity * 7.4e6*1.08,1)        #Calculating all US VLS in 2010 prices from US value of 7.4 million in 2006
    }
    vsl_2006[i, 1] <- country
    vsl_2006[i, 2] <- t(country_vsl_2006)
}


#calculate pm damages and VSL values
vsl_data_WB <- co2_emissions[FALSE, ]
pm_damages_WB <- co2_emissions[FALSE, ]
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    damages <- c()
    vsl_x <- c()
    for (j in 1:21) {
        percapitaincome = as.numeric(gdp_per_capita[gdp_per_capita$country_name==country, j+1])
        percapita_2006 = as.numeric(gdp_per_capita[gdp_per_capita$country_name==country, 10])
        deaths = PM_deaths[PM_deaths$Country==country, j+1]
        vsl_value_2006 = as.numeric(vsl_2006[vsl_2006$country_name==country, 2])
        class = as.character(income_hist_class[income_hist_class$Country==country, j+1])
        if (class == "H"){
            elasticity = 0.8
            damages[j] = round(((percapitaincome/percapita_2006)^(elasticity) * vsl_value_2006 * deaths)/1e9, 3)  #benchmarking to 2006 vsl and converting to billions of dollars
            vsl_x[j] = round(((percapitaincome/percapita_2006)^(elasticity) * vsl_value_2006),1)     #calculating vsl values for country-year
        }
        else if (class==".."){
            elasticity = 'NA'
            damages[j] = 'NA'
            vsl_x[j] = 'NA'
        }
        else{
            elasticity = 1.2
            damages[j] = round(((percapitaincome/percapita_2006)^(elasticity) * vsl_value_2006 * deaths)/1e9, 3)  #benchmarking to 2006 vsl and converting to billions of dollars 
            vsl_x[j] = round(((percapitaincome/percapita_2006)^(elasticity) * vsl_value_2006),1)     #calculating vsl values for country-year
        }
    }
    pm_damages_WB[i, 1] <- country
    pm_damages_WB[i, 2:22] <- t(unlist(damages))
    
    vsl_data_WB[i, 1] <- country
    vsl_data_WB[i, 2:22] <- t(vsl_x)
}


#calculate EVA and gross environmental damage
EVA_all_WB = co2_emissions[FALSE,]           #create empty data frame
GED_all_WB = co2_emissions[FALSE,]           #create empty data frame
Share_PM_damages_WB = co2_emissions[FALSE,]           #create empty data frame
CO2_damages_WB = co2_emissions[FALSE,]           #create empty data frame
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    eva <- c()
    ged <- c()
    share <- c()
    co2_d <- c()
    for (j in 1:21) {
        co2_damages = (as.numeric(unlist(co2_emissions[co2_emissions$country_name==country, j+1]))[[1]])*1000*social_cost_perton[j]/ 1e9     #multiply by thousand because thousand tons, divide by 1e9 because we're converting to billions 
        gdp = as.character(gdp_data[gdp_data$`Country Name`==country, j+1]) 
        poll_damages = as.numeric(pm_damages_WB[pm_damages_WB$country_name==country, j+1]) 
        if(is.na(gdp)) {
            eva[j] = 'NA' 
            ged[j] = 'NA'
            share[j]='NA'
            co2_d[j] = 'NA'
        }
        else if(is.null(poll_damages)) {
            eva[j] = 'NA'
            ged[j] = 'NA'
            share[j] ='NA'
            co2_d[j] = 'NA'
        }
        else {
            eva[j] = round(as.numeric(gdp)/1e9 - co2_damages - poll_damages, 2)    #final answer in billions
            ged[j] = round(co2_damages + poll_damages, 2)
            share[j] = round(poll_damages*100/(co2_damages + poll_damages),2)
            co2_d[j] = round(co2_damages,2)
        }
    }
    EVA_all_WB[i, 1] = country
    EVA_all_WB[i, 2:22] = t(as.numeric(eva))  #all in billions USD
    
    GED_all_WB[i, 1] = country
    GED_all_WB[i, 2:22] = t(as.numeric(ged))  #all in billions USD
    
    Share_PM_damages_WB[i, 1] = country 
    Share_PM_damages_WB[i, 2:22] = t(as.numeric(share)) #ratio of PM damages to total damages from PM and CO2 combined
    
    CO2_damages_WB[i, 1] = country
    CO2_damages_WB[i, 2:22] = t(as.numeric(co2_d)) #co2 damages only
}


#calculate EVA and gross environmental damage per capita
EVA_all_WB_percapita = co2_emissions[FALSE,]           #create empty data frame
GED_all_WB_percapita = co2_emissions[FALSE,]           #create empty data frame
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    eva_percapita <- c()
    ged_percapita <- c()
    for (j in 1:21) {
        pop1 = as.numeric(population_all_final[population_all_final$country_name== country, j+1])
        eva = as.numeric(EVA_all_WB[EVA_all_WB$country_name==country, j+1]) 
        ged = as.numeric(GED_all_WB[GED_all_WB$country_name==country, j+1])
        if(is.na(pop1)) {
            eva_percapita[j] = 'NA' 
            ged_percapita[j] = 'NA'
        }
        else if(is.null(eva)) {
            eva_percapita[j] = 'NA'
            ged_percapita[j] = 'NA'
        }
        else {
            eva_percapita[j] = round(eva*1e9/pop1, 2)    #eva and ged are both in billions so need to convert
            ged_percapita[j] = round(ged*1e9/pop1, 2)
        }
    }
    EVA_all_WB_percapita[i, 1] = country
    EVA_all_WB_percapita[i, 2:22] = t(as.numeric(eva_percapita))
    
    GED_all_WB_percapita[i, 1] = country
    GED_all_WB_percapita[i, 2:22] = t(as.numeric(ged_percapita)) 
}


#calculate EVA growth rates
eva_all_growthrates_WB = EVA_all_WB[FALSE,] #create empty data frame
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    eva_rate <- c()
    eva_rate[1] = 'NA'    #add na for first year 1998
    for (j in 2:21) {
        eva2 = as.numeric(EVA_all_WB[EVA_all_WB$country_name==country, j+1])
        eva1 = as.numeric(EVA_all_WB[EVA_all_WB$country_name==country, j])
        if(is.na(eva2)) {
            eva_rate[j] = 'NA'} 
        if(is.na(eva1)) {
            eva_rate[j] = 'NA' 
        }
        else {
            eva_rate[j] = round(((eva2 - eva1) / eva1)*100,2)
        }
    }
    eva_all_growthrates_WB[i, 1] = country
    eva_all_growthrates_WB[i, 2:22] = t(as.numeric(eva_rate))
}



#calculate EVA growth rates per capita
eva_all_growthrates_WB_percapita = EVA_all_WB[FALSE,] #create empty data frame
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    eva_rate <- c()
    eva_rate[1] = 'NA'    #add na for first year 1998
    for (j in 2:21) {
        eva2 = as.numeric(EVA_all_WB_percapita[EVA_all_WB_percapita$country_name==country, j+1])
        eva1 = as.numeric(EVA_all_WB_percapita[EVA_all_WB_percapita$country_name==country, j])
        if(is.na(eva2)) {
            eva_rate[j] = 'NA'} 
        if(is.na(eva1)) {
            eva_rate[j] = 'NA' 
        }
        else {
            eva_rate[j] = round(((eva2 - eva1) / eva1)*100,2)
        }
    }
    eva_all_growthrates_WB_percapita[i, 1] = country  
    eva_all_growthrates_WB_percapita[i, 2:22] = t(as.numeric(eva_rate))
}


### CALCULATE DIFFERENCE IN GROWTH RATES PER CAPITA
diff_rates_all_WB_percapita = EVA_all_WB[FALSE,]
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    diff_rate <- c()
    diff_rate[1] = 'NA'   #add na for first year 1998
    for (j in 2:21) {
        gdpr = as.numeric(gdp_growthrates_percapita [gdp_growthrates_percapita$country_name==country, j+1])     
        evar = as.numeric(eva_all_growthrates_WB_percapita[eva_all_growthrates_WB_percapita$country_name==country, j+1])
        if(is.na(gdpr)) {
            diff_rate[j] = 'NA'} 
        if(is.na(evar)) {
            diff_rate[j] = 'NA' 
        }
        else {
            diff_rate[j] = round(evar-gdpr, 2)
        }
    }
    diff_rates_all_WB_percapita[i, 1] = country
    diff_rates_all_WB_percapita[i, 2:22] = t(as.numeric(diff_rate))
}

#ged/gdp
ged_over_gdp_WB = EVA_all_WB[FALSE,]
for (i in 1:length(countries_final)){
    country = countries_final[i]
    ans<-c()
    for (j in 1:21){
        damages1 = GED_all_WB[GED_all_WB$country_name==country, j+1]
        gdp1 = as.numeric(gdp_data[gdp_data$`Country Name`==country, j+1]) /1e9
        ans[j] = round((as.numeric(damages1)/ as.numeric(gdp1)) *100, 2)
    }
    ged_over_gdp_WB[i, 1] <- country
    ged_over_gdp_WB[i, 2:22] <- t(as.numeric(ans))
}


### DO WEIGHTED MEANS USING GDP FOR INCOME CLASS GROUPS AND REGION GROUPS USING 2018 CLASSIFICATION
classes = c('L', 'LM', 'UM', 'H')
income_ged_ratio_new = diff_rates_all_WB_percapita[FALSE, ]
income_diffrate_new = diff_rates_all_WB_percapita[FALSE, ]
income_gdpgrowth = diff_rates_all_WB_percapita[FALSE, ]
colnames(income_ged_ratio_new)[colnames(income_ged_ratio_new)=="country_name"] <- "Income Class"
for (i in 1:length(classes)){
    class = classes[i]
    wmean_r <- c()
    wmean_g <- c()
    wmean_growth <- c()
    for (j in 1:21){
        countries_inclass = as.vector(income_hist_class[income_hist_class[,22]==class, 1])$Country
        country1 <- c()
        rate1 <- c()
        growthgdp <- c()
        gdp_weight <- c()
        ged_ratio1 <- c()
        for (k in 1:length(countries_inclass)){
            country1[k] = countries_inclass[k]
            rate1[k] = as.numeric(diff_rates_all_WB_percapita[diff_rates_all_WB_percapita$country_name==country1[k], j+1])
            gdp_weight[k] = as.numeric(gdp_data[gdp_data$`Country Name`==country1[k], j+1])
            growthgdp[k] = as.numeric(gdp_growthrates_percapita[gdp_growthrates_percapita$country_name==country1[k], j+1])
            ged_ratio1[k] = as.numeric(ged_over_gdp_WB[ged_over_gdp_WB$country_name==country1[k], j+1])
        }
        df1 = cbind.data.frame(countries_inclass, rate1, gdp_weight)
        df1 = na.omit(df1)    #DROP ALL NA VALUES
        df1$wdiff = df1$rate1*df1$gdp_weight   #WEIGHTED DIFF
        wmean_r[j] = round(sum(df1$wdiff) / sum(df1$gdp_weight), 2)   #WEIGHTED MEAN OF DIFF RATES
        
        df2 = cbind.data.frame(countries_inclass, ged_ratio1, gdp_weight)
        df2 = na.omit(df2)    #DROP ALL NA VALUES
        #dgf2 = df2[df2$countries_inclass!='China', ]
        df2$wratio = df2$ged_ratio1*df2$gdp_weight  #WEIGHTED GED
        wmean_g[j] = round(sum(df2$wratio) / sum(df2$gdp_weight), 2)   #WEIGHTED MEAN OF ged/gdp ratios
        
        df3 = cbind.data.frame(countries_inclass, growthgdp, gdp_weight)
        df3 = na.omit(df3)
        df3$wgrowth = df3$growthgdp*df3$gdp_weight  #WEIGHTED GDP growth rate
        wmean_growth[j] = round(sum(df3$wgrowth) / sum(df3$gdp_weight), 2)   #WEIGHTED MEAN OF gdp growth rate
    }
    
    income_diffrate_new[i, 1] = class
    income_diffrate_new[i, 2:22] = t(wmean_r)
    
    income_ged_ratio_new[i, 1] = class
    income_ged_ratio_new[i, 2:22] = t(wmean_g)
    
    income_gdpgrowth[i, 1] = class
    income_gdpgrowth[i, 2:22] = t(wmean_growth)
}
income_diffrate_new[,1] = c('Low Income', 'Lower Middle Income', 'Upper Middle Income', 'High Income')
income_ged_ratio_new[,1] = c('Low Income', 'Lower Middle Income', 'Upper Middle Income', 'High Income')
income_gdpgrowth[, 1] = c('Low Income', 'Lower Middle Income', 'Upper Middle Income', 'High Income')
income_diffrate_new$`1998`<- NULL   #drop the first column since it has nothing
income_gdpgrowth$`1998`<- NULL   #drop the first column since it has nothing


#### GLOBAL ANALYSIS WORLD GDP VS EVA
test_gdp <- filter(gdp_data, gdp_data$`Country Name` %in% gdp_growthrates$country_name)  #gdp has 264 countries, we want only 165
world_gdp <- c()
world_eva <- c()
for (i in 1:21){
    world_gdp[i] = sum(as.numeric(unlist(test_gdp[,i+1])), na.rm = TRUE) /1e12    #CONVERTING TO trillions
    world_eva[i] = sum(as.numeric(unlist(EVA_all_WB[, i+1])), na.rm = TRUE) /1e3   #CONVERTING TO trillions
}

world_ged <- world_gdp-world_eva

#growth rates global
world_gdp_growthrate <- c()
world_eva_growthrate <- c()
world_gdp_growthrate[1] = 'NA'
world_eva_growthrate[1] = 'NA'
for (i in 2:21) {
    world_gdp_growthrate[i] <- round(as.numeric(((world_gdp[i]- world_gdp[i-1]) /world_gdp[i-1])*100),2)
    world_eva_growthrate[i] <- round(as.numeric(((world_eva[i] - world_eva[i-1]) /world_eva[i-1])*100),2)
}
world_diff_rates <- c()
world_diff_rates[1] = 'NA'
world_diff_rates[2:21] = as.numeric(world_eva_growthrate[2:21]) - as.numeric(world_gdp_growthrate[2:21])


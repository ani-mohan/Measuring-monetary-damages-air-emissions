##    This script calculates PM2.5 attributable mortality across all end points 

rm(list=ls())
library(readxl)
library(here)
library(stringr)

country_list <- read.csv('Data/165_countrylist.csv')
country_list <- as.character(country_list$Country)
years <- c(1998:2018)

#pull in ambient PM2.5 concentration data
pm_conc <- read_xlsx("Data/PM_POP_NEW.xlsx")

#pull in GBD population data
population <- read.csv('Data/population_GBD.csv')


#lung cancer 
lungcancer_baseline_mortality <- read.csv("Data/New_Mortality_Risk/IHME-GBD_2019_lung_cancer_mortality_risk.csv")
lungcancer_baseline_mortality <- lungcancer_baseline_mortality[, -c(1:3, 5:6, 9, 11, 12, 15, 16)]
lungcancer_baseline_mortality <- lungcancer_baseline_mortality[lungcancer_baseline_mortality$year!=2019, ]  #remove 2019 

#Lung Cancer PAFs 
Lung_PAF <- read.csv("Data/PAFs/PAF_Lung_Cancer.csv")
Lung_PAF$X <- NULL


#Population Lung Cancer
population_lung <- population[population$age_group_id %in% unique(lungcancer_baseline_mortality$age_id), ]  #need only relevant age groups that apply for lung cancer 


#calculate lung cancer deaths by country
deaths_lungcancer = pm_conc[FALSE,] #create empty data frame
for (i in 1:length(country_list)){
    country = country_list[i]
    pafvalues = as.numeric(as.vector(Lung_PAF[Lung_PAF$Country==country, 2:22]))   #get the 1998-2018 PAF values
    df <- population_lung[population_lung$location_name ==country, ]
    df2 <- lungcancer_baseline_mortality[lungcancer_baseline_mortality$location_name==country, ]
    for (j in 1:21){
        year1 = years[j]
        population_year <- df[df$year_id == year1, ]
        population_year <- population_year[order(population_year$age_group_id), ]
            
        mortalityrisk_year <- df2[df2$year == year1, ]
        mortalityrisk_year <- mortalityrisk_year[order(mortalityrisk_year$age_id), ]
            
        deaths_lungcancer[i, j+1] = sum(population_year$val * pafvalues[j] * mortalityrisk_year$val/1e5)
        }
}
deaths_lungcancer[, 1] <- country_list


#COPD 
COPD_baseline_mortality <- read.csv("Data/New_Mortality_Risk/IHME-GBD_2019_COPD_mortality_risk.csv")
COPD_baseline_mortality <- COPD_baseline_mortality[, -c(1:3, 5:6, 9, 11, 12, 15, 16)]

#COPD PAFs 
COPD_PAF <- read.csv("Data/PAFs/PAF_COPD.csv")
COPD_PAF$X <- NULL

#Population COPD
population_COPD <- population[population$age_group_id %in% unique(COPD_baseline_mortality$age_id), ]  #need only relevant age groups that apply for COPD 

#calculate COPD deaths by country
deaths_COPD = deaths_lungcancer[FALSE,] #create empty data frame
for (i in 1:length(country_list)){
    country = country_list[i]
    pafvalues = as.numeric(as.vector(COPD_PAF[COPD_PAF$Country==country, 2:22]))   #get the 1998-2018 PAF values for the country
    df <- population_COPD[population_COPD$location_name ==country, ]
    df2 <- COPD_baseline_mortality[COPD_baseline_mortality$location_name==country, ]
    for (j in 1:21){
        year1 = years[j]
        population_year <- df[df$year_id == year1, ]
        population_year <- population_year[order(population_year$age_group_id), ]
            
        mortalityrisk_year <- df2[df2$year == year1, ]
        mortalityrisk_year <- mortalityrisk_year[order(mortalityrisk_year$age_id), ]
            
        deaths_COPD[i, j+1] = sum(population_year$val * pafvalues[j] * mortalityrisk_year$val/1e5)
        }
}
deaths_COPD[, 1] <- country_list


## Type 2 diabetes
diabetes_baseline_mortality <- read.csv("Data/New_Mortality_Risk/IHME-GBD_2019_Diabetes_mortality_risk.csv")
diabetes_baseline_mortality <- diabetes_baseline_mortality[, -c(1:3, 5:6, 9, 11, 12, 15, 16)]
diabetes_baseline_mortality <- diabetes_baseline_mortality[diabetes_baseline_mortality$year!=2019, ]  #remove 2019 


#Diabetes PAFs 
Diabetes_PAF <- read.csv("Data/PAFs/PAF_Diabetes.csv")
Diabetes_PAF$X <- NULL

#Population Diabetes
population_Diabetes <- population[population$age_group_id %in% unique(diabetes_baseline_mortality$age_id), ]  #need only relevant age groups that apply for Diabetes 

#calculate diabetes deaths by country
deaths_Diabetes = pm_conc[FALSE,] #create empty data frame
for (i in 1:length(country_list)){
    country = country_list[i]
    pafvalues = as.numeric(as.vector(Diabetes_PAF[Diabetes_PAF$Country==country, 2:22]))   #get the 1998-2018 PAF values
    df <- population_Diabetes[population_Diabetes$location_name ==country, ]
    df2 <- diabetes_baseline_mortality[diabetes_baseline_mortality$location_name==country, ]
    for (j in 1:21){
        year1 = years[j]
        population_year <- df[df$year_id == year1, ]
        population_year <- population_year[order(population_year$age_group_id), ]
            
        mortalityrisk_year <- df2[df2$year == year1, ]
        mortalityrisk_year <- mortalityrisk_year[order(mortalityrisk_year$age_id), ]
            
        deaths_Diabetes[i, j+1] = sum(population_year$val * pafvalues[j] * mortalityrisk_year$val/1e5)
    }
}
deaths_Diabetes[, 1] <- country_list


#LRI mortality risk - NEED UNDER 5 age groups here
LRI_baseline_mortality <- read.csv("Data/New_Mortality_Risk/IHME-GBD_2019_LRI_mortality_risk.csv")
LRI_baseline_mortality <- LRI_baseline_mortality[, -c(1:3, 5:6, 9, 11, 12, 15, 16)]


#LRI PAFs 
LRI_PAF <- read.csv("Data/PAFs/PAF_LRI.csv")
LRI_PAF$X <- NULL

#Population LRI
population_LRI <- population[population$age_group_id %in% unique(LRI_baseline_mortality$age_id), ]  #need only relevant age groups that apply for LRI 

#calculate LRI deaths by country
deaths_LRI = deaths_Diabetes[FALSE,] #create empty data frame
for (i in 1:length(country_list)){
    country = country_list[i]
    pafvalues = as.numeric(as.vector(LRI_PAF[LRI_PAF$Country==country, 2:22]))   #get the 1998-2018 PAF values for the country
    df <- population_LRI[population_LRI$location_name ==country, ]
    df2 <- LRI_baseline_mortality[LRI_baseline_mortality$location_name==country, ]
    
    for (j in 1:21){
        year1 = years[j]
        population_year <- df[df$year_id == year1, ]
        population_year <- population_year[order(population_year$age_group_id), ]
            
        mortalityrisk_year <- df2[df2$year == year1, ]            
        mortalityrisk_year <- mortalityrisk_year[order(mortalityrisk_year$age_id), ]
        
        deaths_LRI[i, j+1] = sum(population_year$val * pafvalues[j] * mortalityrisk_year$val/1e5)
        }
}
deaths_LRI[, 1] <- country_list


## IHD 
IHD_baseline_mortality <- read.csv("Data/New_Mortality_Risk/IHME-GBD_2019_IHD_mortality_risk.csv")
IHD_baseline_mortality <- IHD_baseline_mortality[, -c(1:3, 5:6, 9, 11, 12, 15, 16)]
IHD_baseline_mortality <- IHD_baseline_mortality[IHD_baseline_mortality$year!=2019, ]

PAF_IHD25 <- read.csv("Data/PAFs/IHD//PAF_IHD_25.csv")
PAF_IHD25$X1 <- NULL

PAF_IHD30 <- read.csv("Data/PAFs/IHD//PAF_IHD_30.csv")
PAF_IHD30$X1 <- NULL

PAF_IHD35 <- read.csv("Data/PAFs/IHD//PAF_IHD_35.csv")
PAF_IHD35$X1 <- NULL

PAF_IHD40 <- read.csv("Data/PAFs/IHD//PAF_IHD_40.csv")
PAF_IHD40$X1 <- NULL

PAF_IHD45 <- read.csv("Data/PAFs/IHD//PAF_IHD_45.csv")
PAF_IHD45$X1 <- NULL

PAF_IHD50 <- read.csv("Data/PAFs/IHD//PAF_IHD_50.csv")
PAF_IHD50$X1 <- NULL

PAF_IHD55 <- read.csv("Data/PAFs/IHD//PAF_IHD_55.csv")
PAF_IHD55$X1 <- NULL

PAF_IHD60 <- read.csv("Data/PAFs/IHD//PAF_IHD_60.csv")
PAF_IHD60$X1 <- NULL

PAF_IHD65 <- read.csv("Data/PAFs/IHD//PAF_IHD_65.csv")
PAF_IHD65$X1 <- NULL

PAF_IHD70 <- read.csv("Data/PAFs/IHD//PAF_IHD_70.csv")
PAF_IHD70$X1 <- NULL

PAF_IHD75 <- read.csv("Data/PAFs/IHD//PAF_IHD_75.csv")
PAF_IHD75$X1 <- NULL

PAF_IHD80 <- read.csv("Data/PAFs/IHD//PAF_IHD_80.csv")
PAF_IHD80$X1 <- NULL

PAF_IHD85 <- read.csv("Data/PAFs/IHD//PAF_IHD_85.csv")
PAF_IHD85$X1 <- NULL

PAF_IHD90 <- read.csv("Data/PAFs/IHD//PAF_IHD_90.csv")
PAF_IHD90$X1 <- NULL

PAF_IHD95 <- read.csv("Data/PAFs/IHD//PAF_IHD_95.csv")
PAF_IHD95$X1 <- NULL

PAF_IHD_all <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Country", "val", "AgeGroup", "Year"))
for (i in 1:length(country_list)){
    country = country_list[i]
    df1 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD25[PAF_IHD25$Country==country, 3:23])), rep('25 to 29', 21), 1998:2018)
    df2 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD30[PAF_IHD30$Country==country, 3:23])), rep('30 to 34', 21), 1998:2018)
    df3 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD35[PAF_IHD35$Country==country, 3:23])), rep('35 to 39', 21), 1998:2018)
    df4 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD40[PAF_IHD40$Country==country, 3:23])), rep('40 to 44', 21), 1998:2018)
    df5 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD45[PAF_IHD45$Country==country, 3:23])), rep('45 to 49', 21), 1998:2018)
    df6 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD50[PAF_IHD50$Country==country, 3:23])), rep('50 to 54', 21), 1998:2018)
    df7 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD55[PAF_IHD55$Country==country, 3:23])), rep('55 to 59', 21), 1998:2018)
    df8 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD60[PAF_IHD60$Country==country, 3:23])), rep('60 to 64', 21), 1998:2018)
    df9 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD65[PAF_IHD65$Country==country, 3:23])), rep('65 to 69', 21), 1998:2018)
    df10 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD70[PAF_IHD70$Country==country, 3:23])), rep('70 to 74', 21), 1998:2018)
    df11 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD75[PAF_IHD75$Country==country, 3:23])), rep('75 to 79', 21), 1998:2018)
    df12 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD80[PAF_IHD80$Country==country, 3:23])), rep('80 to 84', 21), 1998:2018)
    df13 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD85[PAF_IHD85$Country==country, 3:23])), rep('85 to 89', 21), 1998:2018)
    df14 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD90[PAF_IHD90$Country==country, 3:23])), rep('90 to 94', 21), 1998:2018)
    df15 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_IHD95[PAF_IHD95$Country==country, 3:23])), rep('95 plus', 21), 1998:2018)
    colnames(df1) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df2) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df3) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df4) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df5) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df6) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df7) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df8) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df9) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df10) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df11) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df12) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df13) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df14) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df15) <- c("Country", "val", "AgeGroup", "Year")
    PAF_IHD_all <- rbind.data.frame(PAF_IHD_all, df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)
}


#Population IHD
population_IHD <- population[population$age_group_id %in% unique(IHD_baseline_mortality$age_id), ]  #need only relevant age groups that apply for IHD 


#calculate IHD deaths by country
deaths_IHD = deaths_Diabetes[FALSE,] #create empty data frame
for (i in 1:length(country_list)){
    country = country_list[i]
    pafvalues = PAF_IHD_all[PAF_IHD_all$Country==country, ]   #get the PAF values for the country
    df = population_IHD[population_IHD$location_name ==country, ]
    df2 = IHD_baseline_mortality[IHD_baseline_mortality$location_name==country, ]
    for (j in 1:21){
        year1 = years[j]
        population_year <- df[df$year_id == year1, ]
        population_year <- population_year[order(population_year$age_group_id), ]
            
        mortalityrisk_year <- df2[df2$year == year1, ]            
        mortalityrisk_year <- mortalityrisk_year[order(mortalityrisk_year$age_id), ]
            
        paf_year <- pafvalues[pafvalues$Year == year1, ]
        paf_year <- paf_year[order(paf_year$AgeGroup), ]
        
        deaths_IHD[i, j+1] = sum(population_year$val[1:15] * paf_year$val[1:15] * mortalityrisk_year$val[1:15]/1e5)
        }
}
deaths_IHD[, 1] <- country_list


### Stroke ####
Stroke_baseline_mortality <- read.csv("Data/New_Mortality_Risk/IHME-GBD_2019_Stroke_mortality_risk.csv")
Stroke_baseline_mortality <- Stroke_baseline_mortality[, -c(1:3, 5:6, 9, 11, 12, 15, 16)]
Stroke_baseline_mortality <- Stroke_baseline_mortality[Stroke_baseline_mortality$year!=2019, ]


PAF_stroke25 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_25.csv")
PAF_stroke25$X1 <- NULL

PAF_stroke30 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_30.csv")
PAF_stroke30$X1 <- NULL

PAF_stroke35 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_35.csv")
PAF_stroke35$X1 <- NULL

PAF_stroke40 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_40.csv")
PAF_stroke40$X1 <- NULL

PAF_stroke45 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_45.csv")
PAF_stroke45$X1 <- NULL

PAF_stroke50 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_50.csv")
PAF_stroke50$X1 <- NULL

PAF_stroke55 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_55.csv")
PAF_stroke55$X1 <- NULL

PAF_stroke60 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_60.csv")
PAF_stroke60$X1 <- NULL

PAF_stroke65 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_65.csv")
PAF_stroke65$X1 <- NULL

PAF_stroke70 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_70.csv")
PAF_stroke70$X1 <- NULL

PAF_stroke75 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_75.csv")
PAF_stroke75$X1 <- NULL

PAF_stroke80 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_80.csv")
PAF_stroke80$X1 <- NULL

PAF_stroke85 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_85.csv")
PAF_stroke85$X1 <- NULL

PAF_stroke90 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_90.csv")
PAF_stroke90$X1 <- NULL

PAF_stroke95 <- read.csv("Data/PAFs/Stroke/PAF_Stroke_95.csv")
PAF_stroke95$X1 <- NULL

PAF_stroke_all <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Country", "val", "AgeGroup", "Year"))
for (i in 1:length(country_list)){
    country = country_list[i]
    df1 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke25[PAF_stroke25$Country==country, 3:23])), rep('25 to 29', 21), 1998:2018)
    df2 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke30[PAF_stroke30$Country==country, 3:23])), rep('30 to 34', 21), 1998:2018)
    df3 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke35[PAF_stroke35$Country==country, 3:23])), rep('35 to 39', 21), 1998:2018)
    df4 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke40[PAF_stroke40$Country==country, 3:23])), rep('40 to 44', 21), 1998:2018)
    df5 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke45[PAF_stroke45$Country==country, 3:23])), rep('45 to 49', 21), 1998:2018)
    df6 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke50[PAF_stroke50$Country==country, 3:23])), rep('50 to 54', 21), 1998:2018)
    df7 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke55[PAF_stroke55$Country==country, 3:23])), rep('55 to 59', 21), 1998:2018)
    df8 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke60[PAF_stroke60$Country==country, 3:23])), rep('60 to 64', 21), 1998:2018)
    df9 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke65[PAF_stroke65$Country==country, 3:23])), rep('65 to 69', 21), 1998:2018)
    df10 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke70[PAF_stroke70$Country==country, 3:23])), rep('70 to 74', 21), 1998:2018)
    df11 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke75[PAF_stroke75$Country==country, 3:23])), rep('75 to 79', 21), 1998:2018)
    df12 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke80[PAF_stroke80$Country==country, 3:23])), rep('80 to 84', 21), 1998:2018)
    df13 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke85[PAF_stroke85$Country==country, 3:23])), rep('85 to 89', 21), 1998:2018)
    df14 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke90[PAF_stroke90$Country==country, 3:23])), rep('90 to 94', 21), 1998:2018)
    df15 <- cbind.data.frame(rep(country, 21), as.numeric(as.vector(PAF_stroke95[PAF_stroke95$Country==country, 3:23])), rep('95 plus', 21), 1998:2018)
    colnames(df1) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df2) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df3) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df4) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df5) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df6) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df7) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df8) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df9) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df10) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df11) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df12) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df13) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df14) <- c("Country", "val", "AgeGroup", "Year")
    colnames(df15) <- c("Country", "val", "AgeGroup", "Year")
    PAF_stroke_all <- rbind.data.frame(PAF_IHD_all, df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)
}

#Population Stroke
population_Stroke <- population[population$age_group_id %in% unique(Stroke_baseline_mortality$age_id), ]  #need only relevant age groups that apply for Stroke 

#calculate stroke deaths by country
deaths_stroke = deaths_Diabetes[FALSE,] #create empty data frame
for (i in 1:length(country_list)){
    country = country_list[i]
    pafvalues = PAF_stroke_all[PAF_stroke_all$Country==country, ]   #get the PAF values for the country
    df = population_Stroke[population_Stroke$location_name ==country, ]
    df2 = Stroke_baseline_mortality[Stroke_baseline_mortality$location_name==country, ]
    for (j in 1:21){
        year1 = years[j]
        population_year <- df[df$year_id == year1, ]
        population_year <- population_year[order(population_year$age_group_id), ]
            
        mortalityrisk_year <- df2[df2$year == year1, ]
        mortalityrisk_year <- mortalityrisk_year[order(mortalityrisk_year$age_id), ]
            
        paf_year <- pafvalues[pafvalues$Year == year1, ]
        deaths_stroke[i, j+1] = sum(population_year$val[1:15]  * as.vector(paf_year$val)[1:15] * mortalityrisk_year$val[1:15] /1e5)
    }
}
deaths_stroke[, 1] <- country_list


### Calculate Total PM Deaths across all end points
PM_deaths = deaths_Diabetes[FALSE,]   #create empty data frame
for (i in 1:length(country_list)) {
    country = country_list[i]
    deaths1 <- c()
    for (j in 1:21) {
        lung = deaths_lungcancer[deaths_lungcancer$Country==country, j+1]
        lri = deaths_LRI[deaths_LRI$Country==country, j+1]
        diabetes = deaths_Diabetes[deaths_Diabetes$Country==country, j+1]
        copd = deaths_COPD[deaths_COPD$Country==country, j+1]
        ihd = deaths_IHD[deaths_IHD$Country==country, j+1]
        stroke = deaths_stroke[deaths_stroke$Country==country, j+1]
        PM_deaths[i, j+1] = lung+lri+diabetes+copd+ihd+stroke
    }
}
PM_deaths[, 1] <- country_list  #add in country list in first column

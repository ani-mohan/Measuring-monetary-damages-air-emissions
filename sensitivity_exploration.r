##### some sensitivity analysis and exploration for results


###1998 PM 
pm_1998_conc <- co2_emissions[FALSE,]      
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    pm1998 <- as.numeric(pm_conc[pm_conc$Country==country, 2])
    pm_1998_conc[i, 1] <- country  
    pm_1998_conc[i, 2:22] <- t(as.numeric(pm1998))
}

#calculate pm damages and VSL values
pm_damages_1998_conc <- co2_emissions[FALSE, ]
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    damages1998 <- c()
    for (j in 1:21) {
        deaths1998 = PM_deaths[PM_deaths$Country==country, 2]
        vsl= as.numeric(vsl_data_WB[vsl_data_WB$country_name==country, j+1])
        damages1998[j] <- round(deaths1998*vsl/1e9, 2)
    }
    pm_damages_1998_conc[i, 1] <- country
    pm_damages_1998_conc[i, 2:22] <- t(as.numeric(damages1998))
}

#calculate pm damages and VSL values
pm_damages_1998_vsl <- co2_emissions[FALSE, ]
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    damages_v_1998 <- c()
    for (j in 1:21) {
        deaths = PM_deaths[PM_deaths$Country==country, j+1]
        vsl_1998= as.numeric(vsl_data_WB[vsl_data_WB$country_name==country, 2])
        damages_v_1998[j] <- round(deaths*vsl_1998/1e9, 2)
    }
    pm_damages_1998_vsl[i, 1] <- country
    pm_damages_1998_vsl[i, 2:22] <- t(as.numeric(damages_v_1998))
}


income_driven_pm_damages <- co2_emissions[FALSE, ]
for (i in 1:length(countries_final)) {
    country = countries_final[i]
    incomedriven <- c()
    for (j in 1:21) {
        damages1998 = as.numeric(pm_damages_1998_conc[pm_damages_1998_conc$country_name==country, j+1])   ##these are damages assuming 1998 deaths but increased income over time
        damagespm = as.numeric(pm_damages_WB[pm_damages_WB$country_name==country, j+1])   #these are actual damages incorporating changes in population, income, and pm
        incomedriven[j] <- round((damages1998-damagespm)/(damagespm), 2)
    }
    income_driven_pm_damages[i, 1] <- country
    income_driven_pm_damages[i, 2:22] <- t(as.numeric(incomedriven))
}


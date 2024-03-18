##### SCRIPT FOR PRODUCING ALL IMAGES IN THE MAIN MANUSCRIPT
### This only runs after you have run 3 scripts 1) new_pmmortality_2022.r ; 2) econ_damages_2022.r ; 3) sensitivity_exploration.r


library(ggplot2)

###### Figure 1 ##### Global EVA vs GDP 

# World EVA and GDP growth rates
th <- theme_classic() +  theme(plot.title = element_text(hjust = 0.5, vjust = -10, face="bold", size = 16) , 
                               axis.title.x = element_text(color = "black", size = 18, face = "bold"),
                               axis.title.y.left = element_text(color = "black", size = 14, face = "bold", vjust = 2), 
                               axis.title.y.right = element_text(color = "black", size = 14, face = "bold", vjust = 2),
                               axis.text.x= element_text(color="black", size = 14, face="bold"), legend.position = "",
                               legend.text = element_text(colour="black", size=16, face="bold"),   
                               axis.text.y= element_text(color="black", size = 16, face="bold"))

th2 <- theme_classic() +  theme(plot.title = element_text(hjust = 0.5, vjust = -10, face="bold", size = 16) , 
                                axis.title.x = element_blank(),
                                axis.title.y.left = element_text(color = "black", size = 14, face = "bold", vjust = 2), 
                                axis.title.y.right = element_text(color = "black", size = 14, face = "bold", vjust = 2),
                                axis.text.x= element_text(color="black", size = 14, face="bold"), legend.position = "",
                                legend.text = element_text(colour="black", size=16, face="bold"),   
                                axis.text.y= element_text(color="black", size = 16, face="bold"))

g1 <- ggplot(data = NULL, aes(x = 1999:2018)) + 
    geom_line(aes(x = 1999:2018, y = as.numeric(world_gdp_growthrate[2:21])), color = "red", size=1, linetype='dashed') + 
    geom_line(aes(y = as.numeric(world_eva_growthrate[2:21])), color = "steelblue", size=1, linetype='dashed')  + th2 +
    geom_line(aes(y = runmean(as.numeric(world_diff_rates[2:21]),5)*10), color = "darkseagreen4", size=1)  + 
    scale_y_continuous(sec.axis = sec_axis(~./10, name="Difference in Growth Rates"), limits = c(-2,6))  + 
    geom_abline(intercept = 0, slope = 0, linetype = 'dashed') + xlab('Year') + ylab('GDP, EVA Growth Rates (%)')
g1   




#### Figure 2 - ###INCOME GROUPS GED/GDP RATIO with countries pegged to 2018 income group
th <- theme_classic() +  theme(plot.title = element_text(hjust = 0.5, vjust = 0, face="bold", size = 24) , 
                               axis.title.x = element_text(color = "black", size = 26, face = "bold"),
                               axis.title.y = element_text(color = "black", size = 26, face = "bold"), 
                               axis.text.x= element_text(color="black", size = 20, face="bold"), 
                               axis.text.y= element_text(color="black", size = 20, face="bold"))

th2 <- theme_classic() +  theme(plot.title = element_text(hjust = 0.5,vjust = 0, face="bold", size = 24) , 
                                axis.title.x = element_text(color = "black", size = 26, face = "bold"),
                                axis.title.y = element_blank(), 
                                axis.text.x= element_text(color="black", size = 20, face="bold"), 
                                axis.text.y= element_text(color="black", size = 20, face="bold"))

g1 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(income_ged_ratio_new[4,2:22]))) +
    ggtitle('High Income') +geom_line(colour = 'red', size=2, linetype='dashed') + geom_point(colour='red') + 
    xlab('Year') + ylab('GED/GDP (%)') + ylim(c(0,12.0)) + th

g2 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(income_ged_ratio_new[3,2:22]))) +
    ggtitle('Upper Middle Income') +geom_line(colour = 'red', size=2, linetype='dashed') + geom_point(colour='red') + 
    xlab('Year') + ylab('GED/GDP (%)') + ylim(c(0,12.0)) + th2

g3 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(income_ged_ratio_new[2,2:22]))) +
    ggtitle('Low Middle Income') +geom_line(colour = 'red', size=2, linetype='dashed') + geom_point(colour='red') + 
    xlab('Year') + ylab('GED/GDP (%)') + ylim(c(0,12.0)) + th

g4 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(income_ged_ratio_new[1,2:22]))) +
    ggtitle('Low Income') +geom_line(colour = 'red', size=2, linetype='dashed') + geom_point(colour='red') + 
    xlab('Year') + ylab('GED/GDP (%)') + ylim(c(0,12.0)) + th2

grid.arrange(g1,g2,g3,g4, nrow = 2, ncol = 2)  




# Figure 3 ################ Damages all vs only pm vs only CO2   #######################
gx <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(GED_all_WB[GED_all_WB$country_name=='United States', 2:22])/as.numeric(GED_all_WB[GED_all_WB$country_name=='United States', 2]))) + 
    ggtitle('USA') + geom_point(colour='red', size=1) +xlab('Year') + ylab('Damages (Indexed)') + 
    ylim(c(0,2.0)) + scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    th + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='United States', 2:22])/as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='United States', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United States', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United States', 2])), colour = 'darkgreen', size=1)

gy <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(GED_all_WB[GED_all_WB$country_name=='China', 2:22])/as.numeric(GED_all_WB[GED_all_WB$country_name=='China', 2]))) + 
    ggtitle('China') +  geom_point(colour='red', size=1) +xlab('Year') + ylab('Damages (Indexed)') + 
    ylim(c(0,10.0)) + scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='China', 2:22])/as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='China', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_WB[pm_damages_WB$country_name=='China', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='China', 2])), colour = 'darkgreen', size=1)

gz <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(GED_all_WB[GED_all_WB$country_name=='Japan', 2:22])/as.numeric(GED_all_WB[GED_all_WB$country_name=='Japan', 2]))) + 
    ggtitle('Japan') +  geom_point(colour='red', size=1) +xlab('Year') + ylab('Damages (Indexed)') + 
    ylim(c(0,2.0)) + scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='Japan', 2:22])/as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='Japan', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Japan', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Japan', 2])), colour = 'darkgreen', size=1)

gq <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(GED_all_WB[GED_all_WB$country_name=='Germany', 2:22])/as.numeric(GED_all_WB[GED_all_WB$country_name=='Germany', 2]))) + 
    ggtitle('Germany') + geom_point(colour='red', size=1) +xlab('Year') + ylab('Damages (Indexed)') + 
    ylim(c(0,2.0)) + scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='Germany', 2:22])/as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='Germany', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Germany', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Germany', 2])), colour = 'darkgreen', size=1)

gm <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(GED_all_WB[GED_all_WB$country_name=='United Kingdom', 2:22])/as.numeric(GED_all_WB[GED_all_WB$country_name=='United Kingdom', 2]))) + 
    ggtitle('UK') + geom_point(colour='red', size=1) +xlab('Year') + ylab('Damages (Indexed)') + 
    ylim(c(0,2.0)) + scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    th + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='United Kingdom', 2:22])/as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='United Kingdom', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United Kingdom', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United Kingdom', 2])), colour = 'darkgreen', size=1)

gr <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(GED_all_WB[GED_all_WB$country_name=='France', 2:22])/as.numeric(GED_all_WB[GED_all_WB$country_name=='France', 2]))) + 
    ggtitle('France') +  geom_point(colour='red', size=1) +xlab('Year') + ylab('Damages (Indexed)') + 
    ylim(c(0,2.0)) + scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='France', 2:22])/as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='France', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_WB[pm_damages_WB$country_name=='France', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='France', 2])), colour = 'darkgreen', size=1)

gp <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(GED_all_WB[GED_all_WB$country_name=='India', 2:22])/as.numeric(GED_all_WB[GED_all_WB$country_name=='India', 2]))) + 
    ggtitle('India') + geom_point(colour='red', size=1) +xlab('Year') + ylab('Damages (Indexed)') + 
    ylim(c(0,6.0)) + scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='India', 2:22])/as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='India', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_WB[pm_damages_WB$country_name=='India', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='India', 2])), colour = 'darkgreen', size=1)

go <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(GED_all_WB[GED_all_WB$country_name=='Italy', 2:22])/as.numeric(GED_all_WB[GED_all_WB$country_name=='Italy', 2]))) + 
    ggtitle('Italy') + geom_point(colour = 'red', size=1) + xlab('Year') + ylab('Damages (Indexed)') + 
    ylim(c(0,2.0)) + scale_x_continuous(breaks = c(1998, 2008, 2018)) + theme(plot.margin= unit(rep(.20, 4), "lines")) +
    th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='Italy', 2:22])/as.numeric(CO2_damages_WB[CO2_damages_WB$country_name=='Italy', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Italy', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Italy', 2])), colour = 'darkgreen', size=1)

grid.arrange(gx,gy,gz,gq,gm,gr,gp,go, nrow=2, ncol=4)





##### Figure 4 EKC ####
##### EKC PANEL IMAGE WITH BOTH MASS AND DAMAGES
# READ IN EKC FIT Results
ekc_mass_damages <- read.csv('Results/ekc_mass_damages.csv')

th <- theme_classic() +  theme(plot.title = element_text(hjust = 0.5, vjust = -10, face="bold", size = 16) , 
                               axis.title.x = element_blank(),
                               axis.title.y.left = element_text(color = "black", size = 14, face = "bold", vjust = 2), 
                               axis.title.y.right = element_text(color = "black", size = 12, face = "bold", vjust = 2),
                               axis.text.x= element_text(color="black", size = 12, face="bold"), legend.position = "",
                               legend.text = element_text(colour="black", size=16, face="bold"),   
                               axis.text.y= element_text(color="black", size = 12, face="bold"))

th2 <- theme_classic() +  theme(plot.title = element_text(hjust = 0.5, vjust = -10, face="bold", size = 16) , 
                                axis.title.x = element_text(color = "black", size = 14, face = "bold"),
                                axis.title.y.left = element_text(color = "black", size = 14, face = "bold", vjust = 2), 
                                axis.title.y.right = element_text(color = "black", size = 12, face = "bold", vjust = 2),
                                axis.text.x= element_text(color="black", size = 12, face="bold"), legend.position = "",
                                legend.text = element_text(colour="black", size=16, face="bold"),   
                                axis.text.y= element_text(color="black", size = 12, face="bold"))


figa <- ggplot(data = ekc_mass_damages, aes(x = GDP.Cap)) + 
    geom_line(aes(y = PM_mass, color = "PM levels"), size=2, linetype='dashed') +
    geom_line(aes(y = PM/100, color = "PM damages"), size=2, linetype='dashed') + 
    scale_y_continuous(sec.axis = sec_axis(~.*100, name="PM damages per capita ($)"), limits = c(0,30))  + 
    xlab('GDP Per Capita ($)') + ylab(expression(bold('Ambient '~PM[2.5]~' (ug/m3) ')))  +
    scale_colour_manual("", values = c("PM damages"="red", "PM levels"="steelblue")) +
    xlim(c(0,120000)) + th 
figa

figb <- ggplot(data = ekc_mass_damages, aes(x = GDP.Cap)) + 
    geom_line(aes(y = CO2_mass, color = "CO2 levels"), size=2, linetype='dashed') +
    geom_line(aes(y = CO2/50, color = "CO2 damages"), size=2, linetype='dashed') + 
    scale_y_continuous(sec.axis = sec_axis(~.*50, name="CO2 damages per capita ($)"), limits = c(0,15))  + 
    xlab('GDP Per Capita ($)') + ylab(expression(bold(' '~CO[2]~' (tons per capita) ')))  +
    scale_colour_manual("", values = c("CO2 damages"="red", "CO2 levels"="steelblue")) +
    xlim(c(0,120000)) + th2
figb

grid.arrange(figa,figb, nrow = 2, ncol = 1)  





####### Figure 5 Decomposition analysis ############

#PM damages and income driven PM damages indexed for top 8 economies
g1 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United States', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United States', 2]))) + 
    ggtitle('USA') + geom_point(colour = 'red', size=1) +xlab('Year') + ylab('PM[2.5] Damages (Indexed)') + 
    ylim(c(0,2.0)) + scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    th + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_conc[pm_damages_1998_conc$country_name=='United States', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United States', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_vsl[pm_damages_1998_vsl$country_name=='United States', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United States', 2])), colour = 'darkgreen', size=1)

g2 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(pm_damages_WB[pm_damages_WB$country_name=='China', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='China', 2]))) + ggtitle('China') + 
    geom_point(colour = 'red', size=1) + 
    scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    xlab('Year') + ylab('PM[2.5] Damages (Indexed)') + ylim(c(0,10.0)) +  th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_conc[pm_damages_1998_conc$country_name=='China', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='China', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_vsl[pm_damages_1998_vsl$country_name=='China', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='China', 2])), colour = 'darkgreen', size=1)

g3 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Japan', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Japan', 2]))) + ggtitle('Japan') + 
    geom_point(colour = 'red', size=1) + 
    scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    xlab('Year') + ylab('PM[2.5] Damages (Indexed)') + ylim(c(0,2.0)) +  th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_conc[pm_damages_1998_conc$country_name=='Japan', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Japan', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_vsl[pm_damages_1998_vsl$country_name=='Japan', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Japan', 2])), colour = 'darkgreen', size=1)

g4 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Germany', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Germany', 2]))) + ggtitle('Germany') + 
    geom_point(colour = 'red', size=1) + 
    scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    xlab('Year') + ylab('PM[2.5] Damages (Indexed)') + ylim(c(0,2.0)) +  th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_conc[pm_damages_1998_conc$country_name=='Germany', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Germany', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_vsl[pm_damages_1998_vsl$country_name=='Germany', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Germany', 2])), colour = 'darkgreen', size=1) 

g5 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United Kingdom', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United Kingdom', 2]))) + ggtitle('UK') + 
    geom_point(colour = 'red', size=1) + 
    scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    xlab('Year') + ylab('PM[2.5] Damages (Indexed)') + ylim(c(0,2.0)) +  th + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_conc[pm_damages_1998_conc$country_name=='United Kingdom', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United Kingdom', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_vsl[pm_damages_1998_vsl$country_name=='United Kingdom', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='United Kingdom', 2])), colour = 'darkgreen', size=1)

g6 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(pm_damages_WB[pm_damages_WB$country_name=='France', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='France', 2]))) + ggtitle('France') + 
    geom_point(colour = 'red', size=1) + 
    scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    xlab('Year') + ylab('PM[2.5] Damages (Indexed)') + ylim(c(0,2.0)) +  th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_conc[pm_damages_1998_conc$country_name=='France', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='France', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_vsl[pm_damages_1998_vsl$country_name=='France', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='France', 2])), colour = 'darkgreen', size=1)

g7 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(pm_damages_WB[pm_damages_WB$country_name=='India', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='India', 2]))) + ggtitle('India') + 
    geom_point(colour = 'red', size=1) + 
    scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    xlab('Year') + ylab('PM[2.5] Damages (Indexed)') + ylim(c(0,6.0)) +  th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_conc[pm_damages_1998_conc$country_name=='India', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='India', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_vsl[pm_damages_1998_vsl$country_name=='India', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='India', 2])), colour = 'darkgreen', size=1)

g8 <- ggplot(data = NULL, aes(x=1998:2018, y = as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Italy', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Italy', 2]))) + ggtitle('Italy') + 
    geom_point(colour = 'red', size=1) + 
    scale_x_continuous(breaks = c(1998, 2008, 2018)) +
    xlab('Year') + ylab('PM[2.5] Damages (Indexed)') + ylim(c(0,2.0)) +  th2 + geom_hline(yintercept = 1.0, linetype = 'dashed') + 
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_conc[pm_damages_1998_conc$country_name=='Italy', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Italy', 2])), colour = 'steelblue', size=1) +
    geom_point(aes(x=1998:2018, y= as.numeric(pm_damages_1998_vsl[pm_damages_1998_vsl$country_name=='Italy', 2:22])/as.numeric(pm_damages_WB[pm_damages_WB$country_name=='Italy', 2])), colour = 'darkgreen', size=1)

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8, nrow=2, ncol=4)


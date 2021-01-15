setwd("C:/Users/bednar/Documents/R/Own packages/")  #Needs to be a local drive!
#devtools::create("CROmodel")  #create empty package
#roxygen2::roxygenise("CROmodel")     #create documentation (folder "man") and update NAMESPACE file
#devtools::install("CROmodel")  #compile the package


library(usethis)
library(gdxrrw)
gdxrrw::igdx(gamsSysDir = "C:/GAMS/win64/26.1")


usethis::use_package("dplyr")    #This adds necessary libs to description file
usethis::use_package("reshape2")
usethis::use_package("ggplot2")
usethis::use_package("RColorBrewer")
use_mit_license()   #This adds a license



load(file = "data/historicalEmissions.RData")
load(file = "data/scenarioData.RData")
load(file = "data/scenarioSetups.RData")
source("R/CROModelFunctions.R")

results <- runCROModel(model="GCAM4", ssp=2, rcp=19, r_d=0.00, dacs_cost="LoCost", dacs_cap="HiCap")



#####################
library(readxl)
setwd("H:/Documents/R/Phi-Contract/MAC curves from SSP scenarios")
gdp <- read_excel(  path = "GDP.xlsx", range = "A1:P127")
gdp <- gdp[,c(1,2,4,5,8:16)]
gdp <- melt(gdp, id=c("Model","Scenario","Variable","Unit"), variable.name = "Year")
gdp$SSP <- substr(gdp$Scenario,1,4)
gdp$RCP <- substr(gdp$Scenario,6,nchar(gdp$Scenario))

gdp$gdp_loss <- 0
for(i in seq(nrow(gdp))){
  temp <- gdp %>% filter(Model==gdp$Model[i]) %>% filter( SSP==gdp$SSP[i]) %>% filter( Year==gdp$Year[i]) %>% filter(RCP == "Baseline")

  gdp$gdp_loss[i] <- temp$value-gdp$value[i]

}
gdp <- as_tibble(gdp)
gdp$Year <- as.numeric(as.character(gdp$Year))

price <- read_excel(  path = "price_all.xlsx", range = "A1:P117")
price <- price[,c(1,2,4,5,8:16)]
price[,5:13] <- price[,5:13]*3.67
price$Unit <- "USD/tC"
price <- melt(price, id=c("Model","Scenario","Variable","Unit"), variable.name = "Year")
price$SSP <- substr(price$Scenario,1,4)
price$RCP <- substr(price$Scenario,6,nchar(price$Scenario))


source("H:/Documents/R/Phi-Contract/tech_downscaling.R")
tech_downsc$gdp_bn_usd <- 0

for(i in seq(nrow(tech_downsc))){



  }



tech_downsc$abatement <- 0
tech_downsc$price <- 0
for(i in seq(nrow(tech_downsc))){
  temp <- tech_downsc %>% filter(Model==tech_downsc$Model[i]) %>% filter( SSP==tech_downsc$SSP[i]) %>% filter( Year==tech_downsc$Year[i]) %>% filter(RCP == "Baseline")

  tech_downsc$abatement[i] <- (temp$em_tot-tech_downsc$em_tot[i])/temp$em_tot

  temp <- price %>% filter(Model==tech_downsc$Model[i]) %>% filter( SSP==tech_downsc$SSP[i]) %>% filter( Year==tech_downsc$Year[i]) %>% filter(RCP == tech_downsc$RCP[i])
  if(nrow(temp) > 1) stop()
  if(nrow(temp) == 0) {
    tech_downsc$price[i] <-  0
    print(paste("no C-Price for", tech_downsc$Model[i],tech_downsc$SSP[i],tech_downsc$RCP[i]))
  }
  if(nrow(temp) == 1) tech_downsc$price[i] <-  temp$value


  temp <- gdp %>% filter(Model == tech_downsc$Model[i]) %>% filter(Scenario == tech_downsc$Scenario[i]) %>% filter(Year == tech_downsc$Year[i])
  if(nrow(temp) != 1) stop()
  tech_downsc$gdp_bn_usd[i] <- temp$value

}



setwd("H:/Documents/R/test")

scenarioData <- tech_downsc
save(
  scenarioData,
  file = "data/scenarioData.RData")

historicalEmissions <- read_excel("H:/Documents/Papers & Books, Conferences & Workshops/Phi contract/2nd Submission/historical_emissions_from_1980.xlsx")
save(
  historicalEmissions,
  file = "data/historicalEmissions.RData")







library(stringr)

setwd("C:/Users/bednar/Documents/gamsdir/CROmodel/")
loadScenarios <- function(){

  # all_scenarios <- list.files("GDX input files/RCP26/")
  all_scenarios <- c(list.files("GDX input files/RCP26/"), list.files("GDX input files/RCP19/"))
  for(i in seq(length(all_scenarios))){
    if(i==1){
      scenarioSetups <-
        data.frame(Model = str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][1],
                   SSP = str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][2],
                   RCP = str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][3],
                   r_d = substr(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][4],3,nchar(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][4])),
                   DACS_cost = substr(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][5],5,nchar(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][5])),
                   DACS_cap = ifelse(is.na(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][6]),
                                     "None",
                                     str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][6])
        )
    } else {
      scenarioSetups <-
        rbind(scenarioSetups, data.frame(Model = str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][1],
                                         SSP = str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][2],
                                         RCP = str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][3],
                                         r_d = substr(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][4],3,nchar(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][4])),
                                         DACS_cost = substr(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][5],5,nchar(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][5])),
                                         DACS_cap = ifelse(is.na(str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][6]),
                                                           "None",
                                                           str_split(substr(all_scenarios[i],10,nchar(all_scenarios[i])-4), "_")[[1]][6])
        )
        )

    }
  }

  scenarioSetups$Model[scenarioSetups$Model == "AIM-CGE"] <- "AIM/CGE"

  return(scenarioSetups)


}


scenarioSetups <- loadScenarios()

save(
  scenarioSetups,
  file = "../../R/Own packages/CROmodel/data-raw/scenarioSetups.RData")




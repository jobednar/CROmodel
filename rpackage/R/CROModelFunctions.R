#' Shows all possible scenarios that can be computed using the CRO-model.
#' Use function parameters to filter for subsets of possible scenarios.
#'
#' @title Show scenario specifications serving as input to the CRO-model
#' @param model Character string  to restrict selection to one Integrated Assessment Model (IAM). By default all models are shown.
#' @param ssp Numerical value to restrict selection to one Shared Socioeconomic Pathway (SSP). By default all SSPs are shown.
#' @param rcp Numerical value to restrict selection to one Representative Concentration Pathway (RCP). By default all RCPs are shown.
#' @param r_d Numerical value to restrict selection to one interest rate on carbon debt. By default all rates are shown.
#' @param dacs_cost Character string  to restrict selection to low, medium or high direct air caputure and storage (DACS) marginal abatement costs (MAC). By default all MAC-curves are shown.
#' @param dacs_cap Character string  to restrict selection to low or high capacity direct air caputure and storage (DACS). By default all MAC-curves are shown.
#' @return A data.frame containing all scenario specifications compatible with the given inputs.
#'
#' @import ggplot2
#' @import dplyr
#' @import reshape2
#' @import RColorBrewer
#'
#' @export
showScenarios <- function(model="all", ssp="all", rcp="all", r_d="all", dacs_cost="all", dacs_cap="all"){
  temp <- scenarioSetups
  temp$SSP <- substr(temp$SSP, 4, 4)
  temp$RCP <- substr(temp$RCP, 4, 5)

  rd <- r_d
  if(model != "all"){
    if(!(model %in% .getModels())) warning(paste(c("Model",model,"is unknown or not supported. Choose one of the following:",.getModels()), collapse=" "))
    temp <- filter(temp, Model == model)
  }

  if(ssp != "all"){
    if(!(ssp %in% .getSSPs())) warning(paste(c("SSP",ssp,"is unknown or not supported. Choose one of the following:",.getSSPs()), collapse=" "))
    temp <- filter(temp, SSP == ssp)
  }

  if(rcp != "all"){
    if(!(rcp %in% .getRCPs())) warning(paste(c("RCP",rcp,"is unknown or not supported. Choose one of the following:",.getRCPs()), collapse=" "))
    temp <- filter(temp, RCP == rcp)
  }

  if(r_d != "all"){
    if(!(r_d %in% .getRd())) warning(paste(c("r_d =",r_d,"is not supported. Choose one of the following:",.getRd()), collapse=" "))
    temp <- filter(temp, r_d == rd)
  }

  if(dacs_cost != "all"){
    if(!(dacs_cost %in% .getDACSCost())) warning(paste(c("dacs_cost =",dacs_cost,"is not supported. Choose one of the following:",.getDACSCost()), collapse=" "))
    temp <- filter(temp, DACS_cost == dacs_cost)
  }

  if(dacs_cap != "all"){
    if(!(dacs_cap %in% .getDACSCap())) warning(paste(c("dacs_cap =",dacs_cap,"is not supported. Choose one of the following:",.getDACSCap()), collapse=" "))
    temp <- filter(temp, DACS_cap == dacs_cap)
  }


  if(nrow(temp) == 0) stop("No scenarios with the given specification exist!")
  return(temp)
}

#' Executes the CRO-model for a scenario specification and plots resultant emission paths.
#'
#' @title Execute the CRO-model
#' @param model Character string to define Integrated Assessment Model (IAM).
#' @param ssp Numerical value to define Shared Socioeconomic Pathway (SSP)
#' @param rcp Numerical value to define climate target as Representative Concentration Pathway (RCP).
#' @param r_d Numerical value to define interest rate on carbon debt.
#' @param dacs_cost Character string to define direct air caputure and storage (DACS) marginal abatement costs (MAC).
#' @param dacs_cap Character string  to define capacity limit of direct air caputure and storage (DACS).
#' @return A list-object containing detailed results, the scenario specification and model parameters.
#'
#' \itemize{
#'   \item $data contains annual results,
#'   \item $scenario contains the defined scenario specification,
#'   \item $parameters contains the scenario's parameters (e.g. specification of marginal cost curves, etc.)
#'
#' }
#' @details The correct scenario specification can be obtained using \code{\link{showScenarios}}
#' @examples
#' \notrun{
#'  setwd("~/gamsdir/CROmodel/")
#'  results <- runCROModel(model="MESSAGE-GLOBIOM", ssp=2, rcp=19, r_d=0.01, dacs_cost="HiCost", dacs_cap="LoCap")
#' }
#'
#'
#' @export
runCROModel <- function(model, ssp, rcp, r_d, dacs_cost, dacs_cap){

  results <- .loadPars(model, ssp, rcp, r_d, dacs_cost, dacs_cap)
  results <- .readPars(results)
  .runModel()
  results <- .loadResults(results)
  results <- .techDownscale(results)
  plotEmissions(results)
  return(results)
}


#' Plots the emission profile of the scenario computed by the CRO-model.
#'
#' @title Plot emission profile obtained from CRO-model
#' @param results A list object as returned by \code{\link{runCROModel}}
#' @export
plotEmissions <- function(results) {

  model <- results$scenario$Model
  ssp <- results$scenario$SSP
  rcp <- results$scenario$RCP
  r_d <- results$scenario$r_d
  dacs_cost <- results$scenario$DACS_cost
  dacs_cap <- results$scenario$DACS_cap

  results_plot <- results$data
  results_plot$LUC_emissions_pos <- results_plot$LUC_emissions
  results_plot$LUC_emissions_pos[results_plot$LUC_emissions_pos < 0] <- 0
  results_plot$LUC_emissions_neg <- results_plot$LUC_emissions
  results_plot$LUC_emissions_neg[results_plot$LUC_emissions_neg > 0] <- 0

  dist <- 1

  legend_em <- tibble(x_pos=rep(2080,5), x_pos_end = x_pos+1 , y_pos=seq(12,12-4*dist,-dist),
                      labels=c("FFI","BECCS","LUC", "DACS", "Net. Em."),
                      colors=c( "grey", "orange", "darkgreen", "red", "black")  )

  pl <- ggplot() +
    geom_ribbon(data=results_plot,
                aes(x=Year, ymin=LUC_emissions_pos, ymax=LUC_emissions_pos+FFI_emissions), fill="grey") +

    geom_ribbon(data=results_plot,
                aes(x=Year, ymin=0, ymax=LUC_emissions_pos), fill="darkgreen") +

    geom_ribbon(data=results_plot,
                aes(x=Year, ymin=0, ymax=LUC_emissions_neg), fill="darkgreen") +

    geom_ribbon(data=results_plot,
                aes(x=Year, ymin=LUC_emissions_neg, ymax=LUC_emissions_neg+BECCS_emissions), fill="orange") +

    geom_ribbon(data=results_plot,
                aes(x=Year, ymin=LUC_emissions_neg+BECCS_emissions, ymax=LUC_emissions_neg+BECCS_emissions+DACS_emissions), fill="red") +


    geom_hline(yintercept = 0) +
    geom_line(data=results_plot, aes(x=Year, y=LUC_emissions_neg+LUC_emissions_pos+FFI_emissions+BECCS_emissions+DACS_emissions), lwd=2) +

    geom_segment(data=NULL, aes(x=legend_em$x_pos[1], y=legend_em$y_pos[1], xend=legend_em$x_pos_end[1], yend=legend_em$y_pos[1]),  lwd=4, color=legend_em$colors[1] )+
    geom_segment(data=NULL, aes(x=legend_em$x_pos[2], y=legend_em$y_pos[2], xend=legend_em$x_pos_end[2], yend=legend_em$y_pos[2]),  lwd=4, color=legend_em$colors[2] )+
    geom_segment(data=NULL, aes(x=legend_em$x_pos[3], y=legend_em$y_pos[3], xend=legend_em$x_pos_end[3], yend=legend_em$y_pos[3]),  lwd=4, color=legend_em$colors[3] )+
    geom_segment(data=NULL, aes(x=legend_em$x_pos[4], y=legend_em$y_pos[4], xend=legend_em$x_pos_end[4], yend=legend_em$y_pos[4]),  lwd=4, color=legend_em$colors[4] )+
    geom_segment(data=NULL, aes(x=legend_em$x_pos[5], y=legend_em$y_pos[5], xend=legend_em$x_pos_end[5], yend=legend_em$y_pos[5]),  lwd=1.2, color=legend_em$colors[5] )+
    geom_text(data=legend_em, aes(x=x_pos_end+1, y=y_pos, label=labels),parse=FALSE,size=3.2, hjust=0, vjust=0.5) +

    coord_cartesian(ylim=c(-10,13)) +
    labs(x="Time [year]", y=expression("Annual "*CO[2]*" emissions [GtC/a]"))+
    ggtitle(paste(model, "; RCP", rcp, "; SSP", ssp, "; r_d=", r_d, "; DACS: ", dacs_cost, ", ", dacs_cap, sep="")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.title=element_text(size=10),
          plot.subtitle = element_text(size = 10),
          plot.caption = element_text(size = 10))

  print(pl)


}



#' Plots marginal abatement cost curves (MACCs) from the given scenario specification which is used by the CRO-model.
#'
#' @title Plot marginal abatement cost curves (MACCs)
#' @param results A list object as returned by \code{\link{runCROModel}}
#' @param scale y-axis scale (costs). Use "log" for logarithmic scale.
#' @param x_min Min value on the x-axis (abatement rate).
#' @param x_max Max value on the x-axis (abatement rate).
#' @param y_min Min value on the y-axis (costs).
#' @param y_max Max value on the y-axis (costs).

#' @export
plotMACCs <- function(results, scale="linear", x_min=0, x_max="max", y_min=0, y_max="max"){

  model <- as.character(results$scenario$Model)
  ssp <- results$scenario$SSP
  data <- scenarioData %>% filter(Model==as.character(results$scenario$Model)) %>% filter(SSP==paste0("SSP",results$scenario$SSP)) %>% filter(Year > 2020)

  if(x_max == "max") x_max <- max(data$abatement)
  if(y_max == "max") y_max <- max(data$price)/3.67

  macc <- results$parameters$macc
  for(yr in seq(nrow(macc))){

    ab_max <- max(data$abatement[data$Year==macc$Year[yr]])
    temp <- data.frame(year=macc$Year[yr],
                       abatement=seq(0,ab_max, ab_max/10000),
                       marginal_cost=1/3.67*exp(macc$P[yr]+1/macc$k[yr]*log(1/macc$nu[yr]*(((macc$L[yr]-macc$A[yr])/(seq(0,ab_max, ab_max/10000)-macc$A[yr]))^macc$nu[yr]-1)))
    )


    if(yr == 1){
      plot_maccs <- temp
    } else {
      plot_maccs <- rbind(plot_maccs, temp)
    }


  }

  macc_dacs <- results$parameters$macc_dacs
  plot_maccs_dacs <- data.frame(abatement=seq(0,macc_dacs$Value[macc_dacs$Parameter == "A"],
                                              (macc_dacs$Value[macc_dacs$Parameter == "A"]-macc_dacs$Value[macc_dacs$Parameter == "A"]/10000)/10000),
                                marginal_cost=1/3.67*exp(macc_dacs$Value[macc_dacs$Parameter == "P"] +
                                                    1/macc_dacs$Value[macc_dacs$Parameter == "k"] *
                                                    log(1/macc_dacs$Value[macc_dacs$Parameter == "nu"] *
                                                          (((macc_dacs$Value[macc_dacs$Parameter == "L"]-macc_dacs$Value[macc_dacs$Parameter == "A"]) /
                                                              (seq(0,macc_dacs$Value[macc_dacs$Parameter == "A"],
                                                                   (macc_dacs$Value[macc_dacs$Parameter == "A"]-macc_dacs$Value[macc_dacs$Parameter == "A"]/10000)/10000) -
                                                                 macc_dacs$Value[macc_dacs$Parameter == "A"]))^macc_dacs$Value[macc_dacs$Parameter == "nu"]-1)))
  )


  pl <- ggplot(NULL)

  dist <- (y_max-y_min)/30
  legend_all <- tibble(x_pos=(x_max-x_min)*0.85, x_pos_end = (x_max-x_min)*0.9 , y_pos=c(seq(y_max,y_max-8*dist,-dist), y_max-10*dist),
                       labels=c(seq(2030,2100,10), "DACS", "C-price from\nscenario database"),
                       colors=c(brewer.pal(n=8, name="Set1"), "black", "black"))

  if(scale=="linear"){
    pl <-
      ggplot() +
      geom_line(data=plot_maccs, aes(x=abatement, y=marginal_cost, group=year, color=as.factor(year)), lwd=1, alpha=0.8) +
      geom_point(data=data, aes(x=abatement, y=price/3.67, group=Year, color=as.factor(Year)), cex=2.5) +
      geom_line(data=plot_maccs_dacs, aes(x=abatement, y=marginal_cost), lwd=1, alpha=0.8, lty=2 ) +
      coord_cartesian(xlim=c(x_min,x_max), ylim=c(y_min, y_max)) +
      scale_color_brewer(palette = "Set1") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title.y=element_text(size=10),
            legend.position = "none",
            axis.title=element_text(size=10),
            plot.subtitle = element_text(size = 10),
            plot.caption = element_text(size = 10))+
      labs(x=expression("a [abated fraction of "*CO[2]*" emissions]"), y=expression("MAC [USD/t"*CO[2]*"]"), color="MACC Year") +
      ggtitle(paste(model,paste0("SSP",ssp)))   +
      geom_segment(data=NULL, aes(x=legend_all$x_pos[1], y=legend_all$y_pos[1], xend=legend_all$x_pos_end[1], yend=legend_all$y_pos[1]),  lwd=1, color=legend_all$colors[1] )+
      geom_segment(data=NULL, aes(x=legend_all$x_pos[2], y=legend_all$y_pos[2], xend=legend_all$x_pos_end[2], yend=legend_all$y_pos[2]),  lwd=1, color=legend_all$colors[2] )+
      geom_segment(data=NULL, aes(x=legend_all$x_pos[3], y=legend_all$y_pos[3], xend=legend_all$x_pos_end[3], yend=legend_all$y_pos[3]),  lwd=1, color=legend_all$colors[3] )+
      geom_segment(data=NULL, aes(x=legend_all$x_pos[4], y=legend_all$y_pos[4], xend=legend_all$x_pos_end[4], yend=legend_all$y_pos[4]),  lwd=1, color=legend_all$colors[4] )+
      geom_segment(data=NULL, aes(x=legend_all$x_pos[5], y=legend_all$y_pos[5], xend=legend_all$x_pos_end[5], yend=legend_all$y_pos[5]),  lwd=1, color=legend_all$colors[5] )+
      geom_segment(data=NULL, aes(x=legend_all$x_pos[6], y=legend_all$y_pos[6], xend=legend_all$x_pos_end[6], yend=legend_all$y_pos[6]),  lwd=1, color=legend_all$colors[6] )+
      geom_segment(data=NULL, aes(x=legend_all$x_pos[7], y=legend_all$y_pos[7], xend=legend_all$x_pos_end[7], yend=legend_all$y_pos[7]),  lwd=1, color=legend_all$colors[7] )+
      geom_segment(data=NULL, aes(x=legend_all$x_pos[8], y=legend_all$y_pos[8], xend=legend_all$x_pos_end[8], yend=legend_all$y_pos[8]),  lwd=1, color=legend_all$colors[8] )+
      geom_segment(data=NULL, aes(x=legend_all$x_pos[9], y=legend_all$y_pos[9], xend=legend_all$x_pos_end[9], yend=legend_all$y_pos[9]),  lwd=1, lty=2, color=legend_all$colors[9] )+
      geom_point(data=NULL, aes(x=(legend_all$x_pos[10]+legend_all$x_pos_end[10])/2, y=legend_all$y_pos[10]),  color=legend_all$colors[10], cex=2.5 )+

      geom_text(data=legend_all, aes(x=x_pos_end+0.01, y=y_pos, label=labels),parse=FALSE,size=3.2, hjust=0, vjust=0.5)

  } else if(scale =="log") {

    pl <- ggplot() +
      geom_line(data=plot_maccs, aes(x=abatement, y=marginal_cost, group=year, color=as.factor(year)), lwd=1, alpha=0.8) +
      geom_point(data=data, aes(x=abatement, y=price/3.67, group=Year, color=as.factor(Year)), cex=2.5) +
      geom_line(data=plot_maccs_dacs, aes(x=abatement, y=marginal_cost), lwd=1, alpha=0.8, lty=2 ) +
      coord_cartesian(xlim=c(x_min,x_max), ylim=c(max(1,y_min), y_max)) +
      scale_color_brewer(palette = "Set1") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title.y=element_text(size=10),
            legend.position = "none",
            axis.title=element_text(size=10),
            plot.subtitle = element_text(size = 10),
            plot.caption = element_text(size = 10))+
      labs(x=expression("a [abated fraction of "*CO[2]*" emissions]"), y=expression("MAC [USD/t"*CO[2]*"]"), color="MACC Year") +
      scale_y_continuous(trans="log10") +
      ggtitle(paste(model,paste0("SSP",ssp))) +
      geom_text(data=NULL, aes(x=legend_all$x_pos_end[1], y=legend_all$y_pos[1], label="To see legend\nplease use\nlinear scale."),parse=FALSE,size=3.2, hjust=0, vjust=0.5)

  } else {
    stop("Scale parameter unknown. Use linear or log.")
  }

  suppressWarnings(print(pl))


}



.loadPars <- function(model, ssp, rcp, r_d, dacs_cost, dacs_cap){

  .checkDir()
  .checkPars(model, ssp, rcp, r_d, dacs_cost, dacs_cap)
  .checkModelSSP(model, ssp)
  .checkDACS(rcp, dacs_cost, dacs_cap)


  suppressWarnings(if(file.remove("PARS_IN.gdx")){ message("Old parameter file removed.") } )
  if((dacs_cost == "None") && (dacs_cap == "None")){
    par_file_path <- paste0("GDX input files/RCP",rcp,"/MITIG_IN_", ifelse(model=="AIM/CGE","AIM-CGE",model), "_SSP",ssp,"_RCP",rcp,"_Rd",r_d,"_DACS",dacs_cost, ".gdx")

  } else {
    par_file_path <- paste0("GDX input files/RCP",rcp,"/MITIG_IN_", ifelse(model=="AIM/CGE","AIM-CGE",model), "_SSP",ssp,"_RCP",rcp,"_Rd",r_d,"_DACS",dacs_cost,"_",dacs_cap, ".gdx")

  }
  if(!file.exists(par_file_path)){
    stop("Paramater file does not exist. Please check parameters.")
  } else {
    if(!file.copy(from = par_file_path, to = "PARS_IN.gdx", overwrite = TRUE)){
      stop("Parameter file exists but could not be copied.")
    } else {
      message("Parameter file loaded.")
    }
  }

  results <- list(scenario=data.frame(Model=model, SSP=ssp, RCP=rcp, r_d=r_d, DACS_cost=dacs_cost, DACS_cap=dacs_cap), parameters=NA, data=NA)
  return(results)

}

.readPars <- function(results){

  parameters <- list(r=NA, use_bs=NA, macc=NA, macc_dacs=NA)
  .checkDir()
  if(!file.exists("PARS_IN.gdx")){
    stop("Paramater file does not exist.")
  } else {


    parameters$r <- .readVar("PARS_IN.gdx", "R")[1,1]
    parameters$use_bs <- .readVar("PARS_IN.gdx", "USE_BS")[1,1]
    macc_parameters <- as.data.frame(.readVar("PARS_IN.gdx", "PARS"))
    names(macc_parameters) <- c("Year", "Parameter", "Value")
    macc_parameters$Year <- seq(2020,2100,10)[macc_parameters$Year]
    macc_parameters$Parameter <- c("A", "L", "k", "P", "nu")[macc_parameters$Parameter]
    suppressMessages(macc_parameters <- dcast(macc_parameters, Year~Parameter) %>% filter(Year >= 2030))
    macc_parameters[is.na(macc_parameters)] <- 0
    parameters$macc <- macc_parameters

    macc_dacs_parameters <- as.data.frame(.readVar("PARS_IN.gdx", "PARS_BS"))
    names(macc_dacs_parameters) <- c("Parameter", "Value")
    macc_dacs_parameters$Parameter <- c("A", "L", "k", "P", "nu")[macc_dacs_parameters$Parameter]

    if(!("A" %in% macc_dacs_parameters$Parameter)){
      macc_dacs_parameters <- add_row(macc_dacs_parameters, Parameter="A", Value=0)
    }
    if(!("L" %in% macc_dacs_parameters$Parameter)){
      macc_dacs_parameters <- add_row(macc_dacs_parameters, Parameter="L", Value=0)
    }
    if(!("k" %in% macc_dacs_parameters$Parameter)){
      macc_dacs_parameters <- add_row(macc_dacs_parameters, Parameter="k", Value=0)
    }
    if(!("P" %in% macc_dacs_parameters$Parameter)){
      macc_dacs_parameters <- add_row(macc_dacs_parameters, Parameter="P", Value=0)
    }
    if(!("nu" %in% macc_dacs_parameters$Parameter)){
      macc_dacs_parameters <- add_row(macc_dacs_parameters, Parameter="nu", Value=0)
    }

        parameters$macc_dacs <- macc_dacs_parameters

  }

  results$parameters <- parameters
  return(results)
}

.runModel <- function(){

  .checkDir()
  if(file.exists("RESULTS_OUT.gdx")) file.remove("RESULTS_OUT.gdx")

  return_code <- gdxrrw::gams("CRO_model.gms")
  if(return_code != 0){
    stop(paste("Optimization failed. Exit status =",return_code,". Please see https://www.gams.com/latest/docs/UG_GAMSReturnCodes.html."))
  } else {
    message("Successful model execution!")
  }

}


.loadResults <- function(results){

  .checkDir()
  if(!file.exists("RESULTS_OUT.gdx")) stop("No results file found!")

  data <- data.frame(Year = c(2020:2100),
                     Net_emissions = .readVart("em_net"),
                     Abatement_rate = .readVart("em_ab_rel"),
                     Abatement_rate_DACS = .readVart("em_ab_rel_bs"),
                     Marginal_abatement_cost = .readVart("mac")/3.67,
                     Carbon_price = 0,
                     Abatement_cost = .readVart("tot_cost"),
                     Interest_cost_discounted = .readVardt("contr_cost_dt"),
                     Repayment_term = round(.readVardt("repay_term_dt")-c(1:81),2)
  )

  data$Repayment_term[data$Net_emissions < 0] <- NA

  r <- results$parameters$r

  c_price <- gdxrrw::rgdx("RESULTS_OUT.gdx",list(name="emSum1", field="m"))$val
  c_price <- ifelse(length(c_price) == 1, c_price[1,1],  0)
  c_price <- -c_price*(1+r)^c(0:80)/3.67
  data$Carbon_price <- c_price


  use_bs <- results$parameters$use_bs
  data$Abatement_rate_DACS <- data$Abatement_rate_DACS * use_bs


  total_interest_cost <- ifelse(length(gdxrrw::rgdx("RESULTS_OUT.gdx",list(name="contr_cost_sum"))$val)==0, 0,
                                gdxrrw::rgdx("RESULTS_OUT.gdx",list(name="contr_cost_sum"))$val[1,1])
  total_abatement_cost <- ifelse(length(gdxrrw::rgdx("RESULTS_OUT.gdx",list(name="ab_cost_sum"))$val)==0, 0,
                                 gdxrrw::rgdx("RESULTS_OUT.gdx",list(name="ab_cost_sum"))$val[1,1])

  if(round(sum(data$Abatement_cost/(1+r)^c(0:80)) - total_abatement_cost,5) != 0) stop("Costs should be equal")
  if(round(sum(data$Interest_cost_discounted) - total_interest_cost,5) != 0) stop("Costs should be equal")

  results$data <- data
  message("Results loaded.")

  return(results)

}




.techDownscale <- function(results) {

  model <- as.character(results$scenario$Model)
  ssp <- results$scenario$SSP
  .checkModelSSP(model, ssp)

  results_temp <- results$data %>% filter(Year %in% seq(2020,2100,10))

  results_temp <- cbind(results_temp, data.frame(em_foss=0, em_ccs_bio=0, em_lu=0, em_backstop=0, em_net_no_bs=0))



  for(i in seq(nrow(results_temp))){
    alph <- -1
    eps <- 0
    scen_data_temp <- scenarioData %>% filter(Model==model) %>% filter(SSP==paste0("SSP",ssp)) %>% filter(RCP=="Baseline") %>% filter(Year==results_temp$Year[i])
    results_temp$em_backstop[i]  <- (-scen_data_temp$em_tot* results_temp$Abatement_rate_DACS[i])
    results_temp$em_net_no_bs[i] <- results_temp$Net_emissions[i] - results_temp$em_backstop[i]

    if(results_temp$Year[i]==2020){
      results_temp$em_foss[i] <- tail(historicalEmissions$`fossil fuel and industry`,1)
      results_temp$em_ccs_bio[i] <- 0
      results_temp$em_lu[i] <-  tail(historicalEmissions$`land-use change emissions`,1)


    } else {

      temp <- scenarioData %>% filter(Model==model) %>% filter(SSP==paste0("SSP",ssp)) %>% filter(Year==results_temp$Year[i])
      temp <- arrange(temp, em_tot)

      upper <- which(round(temp$em_tot,5) >= round(results_temp$em_net_no_bs[i],5))[1]
      lower <- tail(which(round(temp$em_tot,5) <= round(results_temp$em_net_no_bs[i],5)),1)
      if(length(lower) == 0) {

        eps <- results_temp$em_net_no_bs[i] - min(temp$em_tot)
        results_temp$em_net_no_bs[i] <-  min(temp$em_tot)
        upper <- which(round(temp$em_tot,5) >= round(results_temp$em_net_no_bs[i],5))[1]
        lower <- tail(which(round(temp$em_tot,5) <= round(results_temp$em_net_no_bs[i],5)),1)
        alph <- 1

      } else if(length(upper) == 0) { stop("Error in technology downscaling!")


      } else if(upper == lower){
        alph <- 1
      } else {
        alph <- (results_temp$em_net_no_bs[i]-temp$em_tot[upper])/(temp$em_tot[lower]-temp$em_tot[upper])
      }
      if(alph==-1) stop(paste(i,"Error in technology downscaling!"))

      results_temp$em_foss[i] <- alph * temp$em_FOSSIL[lower] + (1-alph) * temp$em_FOSSIL[upper]
      results_temp$em_ccs_bio[i] <- alph * temp$em_CCS_BIO[lower] + (1-alph) * temp$em_CCS_BIO[upper] + eps
      results_temp$em_lu[i] <- alph * temp$em_LU[lower] + (1-alph) * temp$em_LU[upper]
    }
  }

  gdp <- (scenarioData %>% filter(Model==model) %>% filter(SSP==paste0("SSP",ssp)) %>% filter(RCP=="Baseline"))$gdp_bn_usd

  results$data <- cbind(results$data, data.frame(FFI_emissions = approx(x = results_temp$Year, y = results_temp$em_foss, xout = c(2020:2100))$y,
                                                 BECCS_emissions = approx(x = results_temp$Year, y = results_temp$em_ccs_bio, xout = c(2020:2100))$y,
                                                 LUC_emissions = approx(x = results_temp$Year, y = results_temp$em_lu, xout = c(2020:2100))$y,
                                                 DACS_emissions = approx(x = results_temp$Year, y = results_temp$em_backstop, xout = c(2020:2100))$y,
                                                 GDP_baseline = approx(x = seq(2020,2100,10), y = gdp, xout = c(2020:2100))$y)
  )

  message("Successful downscaling of emission profile.")

  return(results)
}






.checkDir <- function(){
  if(file.exists("CRO_model.gms") == FALSE) stop("Model GAMS file not located in working directory. Set working directory to model GAMS file location!")
  if(dir.exists("GDX input files") == FALSE) stop("Parameter files directory not located in working directory.")
}

.checkPars <- function(model, ssp, rcp, r_d, dacs_cost, dacs_cap){

  if(!(model %in% .getModels())) warning(paste(c("Model",model,"is unknown or not supported. Choose one of the following:",.getModels()), collapse=" "))
  if(!(ssp %in% .getSSPs())) warning(paste(c("SSP",ssp,"is unknown or not supported. Choose one of the following:",.getSSPs()), collapse=" "))
  if(!(rcp %in% .getRCPs())) warning(paste(c("RCP",rcp,"is unknown or not supported. Choose one of the following:",.getRCPs()), collapse=" "))
  if(!(r_d %in% .getRd())) warning(paste(c("r_d =",r_d,"is not supported. Choose one of the following:",.getRd()), collapse=" "))
  if(!(dacs_cost %in% .getDACSCost())) warning(paste(c("dacs_cost =",dacs_cost,"is not supported. Choose one of the following:",.getDACSCost()), collapse=" "))
  if(!(dacs_cap %in% .getDACSCap())) warning(paste(c("dacs_cap =",dacs_cap,"is not supported. Choose one of the following:",.getDACSCap()), collapse=" "))

}

.checkDACS <- function(rcp, dacs_cost, dacs_cap){

  if((rcp==19) && (dacs_cost == "None")){
    warning(paste(c("dacs_cost =",dacs_cost,"is not supported for RCP", rcp, "\nChoose one of the following:",.getDACSCost(rcp)), collapse=" "))
  }

  if((rcp==19) && (dacs_cap == "None")){
    warning(paste(c("dacs_cap =",dacs_cap,"is not supported for RCP", rcp, "\nChoose one of the following:",.getDACSCap(rcp)), collapse=" "))
  }

  if((rcp==26) && (dacs_cost != "None")){
    warning(paste(c("dacs_cost =",dacs_cost,"is not supported for RCP", rcp, "\nChoose one of the following:",.getDACSCost(rcp)), collapse=" "))
  }

  if((rcp==26) && (dacs_cap != "None")){
    warning(paste(c("dacs_cap =",dacs_cap,"is not supported for RCP", rcp, "\nChoose one of the following:",.getDACSCap(rcp)), collapse=" "))
  }


}

.getModels <- function(){
  return(sort(unique(as.character(scenarioSetups$Model))))
}


.getSSPs <- function(){
  return(sort(substr(unique(as.character(scenarioSetups$SSP)), 4, 4)))
}

.getRCPs <- function(){
  return(sort(substr(unique(as.character(scenarioSetups$RCP)), 4, 5)))
}

.getRd <- function(){
  return(sort(as.numeric(as.character(unique(scenarioSetups$r_d)))))
}

.getDACSCost <- function(rcp="all"){
  if(rcp != "all") scenarioSetups <- scenarioSetups %>% filter(RCP==paste0("RCP",rcp))
  return(sort(unique(as.character(scenarioSetups$DACS_cost))))
}

.getDACSCap <- function(rcp="all"){
  if(rcp != "all") scenarioSetups <- scenarioSetups %>% filter(RCP==paste0("RCP",rcp))
  return(sort(unique(as.character(scenarioSetups$DACS_cap))))
}


.readVar <- function(file, var_name){
  out <- gdxrrw::rgdx(file,list(name=var_name))$val
  if(nrow(out)==0) out <- matrix(0)
  return(out)

}

.readVart <- function(var_name){
  out <- gdxrrw::rgdx("RESULTS_OUT.gdx",list(name=var_name))$val
  out <- as.data.frame(out)
  names(out) <- c("id", "value")
  var <- data.frame(year=seq(2020,2100,10), value=NA)
  for(i in seq(nrow(var))){
    var$value[i] <- ifelse(length(out$value[out$id==i])==1, out$value[out$id==i], 0)
  }
  var <- approx(x = var$year, y = var$value, xout = c(2020:2100))$y
  return(var)
}

.readVardt <- function(var_name){
  out <- gdxrrw::rgdx("RESULTS_OUT.gdx",list(name=var_name))$val
  if(nrow(out)==0){
    return(data.frame(year=c(2020:2100), value=0))
  }

  out <- as.data.frame(out)
  names(out) <- c("id", "value")
  out$id <- out$id - 9
  var <- data.frame(year=c(2020:2100), value=NA)
  for(i in seq(nrow(var))){
    var$value[i] <- ifelse(length(out$value[out$id==i])==1, out$value[out$id==i], 0)
  }
  return(var$value)
}


.checkModelSSP <- function(model, ssp){

  temp <- scenarioSetups
  temp <- aggregate(x = temp[c("Model", "SSP")], by = list(temp$Model, temp$SSP), FUN=length) %>% select(Group.1, Group.2)
  names(temp) <- c("Model", "SSP")
  if((model %in% .getModels()) && (ssp %in% .getSSPs())) {
    if(nrow(temp %>% filter(Model==model) %>% filter(SSP==paste0("SSP", ssp))) == 0){
      message("Model and SSP are known, but combination of model and SSP is not supported!\nChoose one of the following:")
      print(temp)
      stop("Stopping model run.")
    }
  }

}





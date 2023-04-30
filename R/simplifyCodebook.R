#' simplifyCodebook
#' @description
#' A function to take a dataframe with columns from output of formatCoding2distilBert.R and simplify some of the variables
#' 
#' @param codebook a data frame with columns indicating a binary 0 1 for each value of each variable
#' 
#' @param idCols The names of the identification columns (never simplified)


simplifyCodebook <- function(codebook, idCols){
  
  # seperate out idColumns from the data columns
  idMat <- codebook[,idCols]
  codebook <- codebook[,which(!(colnames(codebook) %in% idCols))]
  
  # simplify data columns
  
  
  
  # add a column for ORO_branch
  codebook$oro_branch.Mitigation <- rowSums(codebook[,c("oro_type.M_Renewables","oro_type.M_Increase_efficiency","oro_type.M_CO2_removal_or_storage")], na.rm = TRUE)
  codebook$oro_branch.Mitigation[codebook$oro_branch.Mitigation>1] <- 1
  
  codebook$oro_branch.Nature <- rowSums(codebook[,c("oro_type.N_Human_assisted_evolution","oro_type.N_Protection","oro_type.N_Restoration")], na.rm = TRUE)
  codebook$oro_branch.Nature[codebook$oro_branch.Nature>1] <- 1
  
  codebook$oro_branch.Societal <- rowSums(codebook[,c("oro_type.SA_Built_infrastructure_and_technology","oro_type.SA_Socioinstitutional")], na.rm = TRUE)
  codebook$oro_branch.Societal[codebook$oro_branch.Societal>1] <- 1
  
  codebook$oro_branch.Unclear <- rowSums(codebook[,c("oro_type.Unclear", "impact_oro.Unclear")], na.rm=T)
  codebook$oro_branch.Unclear[codebook$oro_branch.Unclear > 1] <- 1
  
  
  # make a column that is just whether co-benefits or dis-benefits were reported (any impact)
  codebook$impact_any <- rowSums(codebook[,grep("impact", colnames(codebook))], na.rm=T)
  codebook$impact_any[codebook$impact_any > 1] <- 1
  
  
  # merge undersampled value types
  
  # climate_threat : 
  # Sea_ice_changes, Deoxygenation, Natural_impacts, Human_impacts --> put into other
  codebook$climate_threat.Other <- rowSums(codebook[,c("climate_threat.Sea_ice_changes", 
                                                       "climate_threat.Deoxygenation",
                                                       "climate_threat.Natural_impacts",
                                                       "climate_threat.Human_impacts",
                                                       "climate_threat.Other")], na.rm=T)
  codebook$climate_threat.Other[codebook$climate_threat.Other > 1] <- 1
  codebook <- codebook[,which(!(colnames(codebook) %in% c("climate_threat.Sea_ice_changes","climate_threat.Deoxygenation","climate_threat.Natural_impacts","climate_threat.Human_impacts")))]
  # merge Temperature and marine heatwave 
  codebook$climate_threat.Temperature <- rowSums(codebook[,c("climate_threat.Temperature","climate_threat.Marine_heatwave")])
  codebook$climate_threat.Temperature[codebook$climate_threat.Temperature > 1] <- 1
  codebook <- codebook[,which(colnames(codebook) != "climate_threat.Marine_heatwave")]
  
  
  # merge economic_sector_involved and impact_economy
  codebook$economic_sector.Aquaculture <- ifelse(
    codebook$economic_sector_involved.Aquaculture == 0 & 
      codebook$impact_economy.Aquaculture == 1, 
    1, codebook$economic_sector_involved.Aquaculture)
  
  codebook$economic_sector.Coastal_development <- ifelse(
    codebook$economic_sector_involved.Coastal_development == 0 &
      codebook$impact_economy.Coastal_development ==1,
    1, codebook$economic_sector_involved.Coastal_development
  )
  
  codebook$economic_sector.Fisheries <- ifelse(
    codebook$economic_sector_involved.Fisheries == 0 &
      codebook$impact_economy.Fisheries ==1,
    1, codebook$economic_sector_involved.Fisheries
  )
  
  codebook$economic_sector.Informal <- ifelse(
    codebook$economic_sector_involved.Informal == 0 &
      codebook$impact_economy.Informal == 1,
    1, codebook$economic_sector_involved.Informal
  )
  
  codebook$economic_sector.Livelihood <- ifelse(
    codebook$economic_sector_involved.Livelihood ==0 &
      codebook$impact_economy.Livelihood == 1,
    1, codebook$economic_sector_involved.Livelihood
  )
  
  codebook$economic_sector.Other <- ifelse(
    codebook$economic_sector_involved.Other ==0 &
      codebook$impact_economy.Other==1,
    1, codebook$economic_sector_involved.Other
  )
  
  codebook$economic_sector.Pharmaceuticals <- ifelse(
    codebook$economic_sector_involved.Pharmaceuticals == 0 &
      codebook$impact_economy.Pharmaceuticals ==1,
    1, codebook$economic_sector_involved.Pharmaceuticals
  )
  
  codebook$economic_sector.Port_infrastructure <- ifelse(
    codebook$economic_sector_involved.Port_infrastructure == 0 &
      codebook$impact_economy.Port_infrastructure == 1,
    1, codebook$economic_sector_involved.Port_infrastructure
  )
  
  codebook$economic_sector.Renewable_energy <- ifelse(
    codebook$economic_sector_involved.Renewable_energy == 0&
      codebook$impact_economy.Renewable_energy ==1,
    1,codebook$economic_sector_involved.Renewable_energy
  )
  
  codebook$economic_sector.Seabed_miningoil__gas <- ifelse(
    codebook$economic_sector_involved.Seabed_miningoil__gas == 0 &
      codebook$impact_economy.Seabed_miningoil__gas == 1,
    1, codebook$economic_sector_involved.Seabed_miningoil__gas
  )
  
  codebook$economic_sector.Shipping <- ifelse(
    codebook$economic_sector_involved.Shipping == 0 &
      codebook$impact_economy.Shipping == 1, 
    1, codebook$economic_sector_involved.Shipping
  )
  
  codebook$economic_sector.Tourism__Recreation <- ifelse(
    codebook$economic_sector_involved.Tourism__Recreation == 0 &
      codebook$impact_economy.Tourism__Recreation == 1,
    1, codebook$economic_sector_involved.Tourism__Recreation
  )
  
  # remove old columns
  rmCols <- c(
    grep("economic_sector_involved", colnames(codebook)),
    grep("impact_economy", colnames(codebook))
  )
  codebook <- codebook[,-c(rmCols)]
  
  
  
  # Create a column that is just whether or not an ORO interaction was present
  codebook$oro_interaction <- rowSums(codebook[,grep("impact_oro", colnames(codebook))], na.rm = TRUE)
  codebook$oro_interaction[codebook$oro_interaction > 1] <- 1
  
  
  
  # Create column for oro_any which is a combination or oro_type and impact_oro
  codebook$oro_any.M_Renewables <- rowSums(codebook[,grep("M_Renewables", colnames(codebook))], na.rm = TRUE)
  codebook$oro_any.M_Renewables[codebook$oro_any.M_Renewables > 1] <- 1
  
  codebook$oro_any.M_Increase_efficiency <- rowSums(codebook[,grep("Increase_efficiency", colnames(codebook))], na.rm = TRUE)
  codebook$oro_any.M_Increase_efficiency[codebook$oro_any.M_Increase_efficiency > 1] <- 1
  
  codebook$oro_any.M_CO2_removal_or_storage <- rowSums(codebook[,grep("M_CO2_removal_or_storage", colnames(codebook))], na.rm = TRUE)
  codebook$oro_any.M_CO2_removal_or_storage[codebook$oro_any.M_CO2_removal_or_storage > 1] <- 1
  
  codebook$oro_any.N_Human_assisted_evolution <- rowSums(codebook[,grep("N_Human_assisted_evolution", colnames(codebook))], na.rm = TRUE)
  codebook$oro_any.N_Human_assisted_evolution[codebook$oro_any.N_Human_assisted_evolution > 1] <- 1
  
  codebook$oro_any.N_Protection <- rowSums(codebook[,grep("N_Protection", colnames(codebook))], na.rm = TRUE)
  codebook$oro_any.N_Protection[codebook$oro_any.N_Protection > 1] <- 1
  
  codebook$oro_any.N_Restoration <- rowSums(codebook[,grep("N_Restoration", colnames(codebook))], na.rm = TRUE)
  codebook$oro_any.N_Restoration[codebook$oro_any.N_Restoration > 1] <- 1
  
  codebook$oro_any.SA_Built_infrastructure_and_technology <- rowSums(codebook[,grep("SA_Built_infrastructure_and_technology", colnames(codebook))], na.rm = TRUE)
  codebook$oro_any.SA_Built_infrastructure_and_technology[codebook$oro_any.SA_Built_infrastructure_and_technology > 1] <- 1
  
  codebook$oro_any.SA_Socioinstitutional <- rowSums(codebook[,grep("SA_Socioinstitutional", colnames(codebook))], na.rm = TRUE)
  codebook$oro_any.SA_Socioinstitutional[codebook$oro_any.SA_Socioinstitutional > 1] <- 1
  
  
  
  # Column of just P/A is there an impact on NCP
  codebook$impact_ncp.Any <- rowSums(codebook[,grep("impact_ncp", colnames(codebook))], na.rm=TRUE)
  codebook$impact_ncp.Any[codebook$impact_ncp.Any > 1] <- 1
  
  
  # remove old ORO columns
  rmCols <- c(
    grep("impact_oro", colnames(codebook)),
    grep("oro_type", colnames(codebook))
  )
  codebook <- codebook[,-c(rmCols)]
  
  
  # order the columns and return new data frame
  colOrder <- c(
    colnames(codebook)[grep("oro_branch", colnames(codebook))],
    colnames(codebook)[grep("oro_any", colnames(codebook))],
    
    colnames(codebook)[grep("biodiversity_metric", colnames(codebook))],
    colnames(codebook)[grep("restoration", colnames(codebook))],
    colnames(codebook)[grep("safe_fish", colnames(codebook))],
    colnames(codebook)[grep("safe_space", colnames(codebook))],
    
    colnames(codebook)[grep("oro_development", colnames(codebook))],
    colnames(codebook)[grep("climate_threat", colnames(codebook))],
    colnames(codebook)[grep("data_type", colnames(codebook))],
    
    colnames(codebook)[grep("climate_mitigation", colnames(codebook))],
    colnames(codebook)[grep("adapt_to_threat", colnames(codebook))],
    colnames(codebook)[grep("impact_any", colnames(codebook))],
    colnames(codebook)[grep("oro_interaction", colnames(codebook))],
    
    colnames(codebook)[grep("method_type", colnames(codebook))],
    colnames(codebook)[grep("m_co2_removal", colnames(codebook))],
    colnames(codebook)[grep("m_co2_ocean_storage", colnames(codebook))],
    colnames(codebook)[grep("ecosystem_type", colnames(codebook))],
    colnames(codebook)[grep("economic_sector", colnames(codebook))],
    
    colnames(codebook)[grep("impact_nature", colnames(codebook))],
    colnames(codebook)[grep("impact_ncp", colnames(codebook))],
    
    colnames(codebook)[grep("scalability", colnames(codebook))],
    colnames(codebook)[grep("readiness", colnames(codebook))],
    colnames(codebook)[grep("lead_time", colnames(codebook))],
    colnames(codebook)[grep("duration", colnames(codebook))],
    colnames(codebook)[grep("robustness", colnames(codebook))],
    colnames(codebook)[grep("cost", colnames(codebook))],
    colnames(codebook)[grep("enabling_factors", colnames(codebook))],
    colnames(codebook)[grep("governability", colnames(codebook))],
    
    colnames(codebook)[grep("marine_system", colnames(codebook))],
    colnames(codebook)[grep("time_period", colnames(codebook))],
    colnames(codebook)[grep("data_scale_spatial", colnames(codebook))],
    
    colnames(codebook)[grep("scientific_discipline", colnames(codebook))]
  )
  
  codebook <- codebook[,colOrder]
  codebook <- cbind(idMat, codebook)
  
  return(codebook)
  
} # end function
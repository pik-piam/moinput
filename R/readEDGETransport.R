#' Read EDGETransport inputs
#' 
#' Read-in EDGETransport inputs csv file as magclass object
#' 
#' 
#' @return magpie object of EDGEtransport iterative inputs
#' @author Marianna Rottoli, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @param subtype logit_exponents, SW, VOT_nonmot, intensity
#'
#' @examples
#' \dontrun{ a <- readSource(type="EDGETransport")
#' }
#' @importFrom magclass read.magpie
#' @importFrom data.table rbindlist fread setcolorder
#' 

readEDGETransport <- function(subtype = "logit_exponent") {
  ## mask variable for code checks
  vehicle_type <- NULL
  SSP <- NULL
  all_GDPscen <- NULL
  switch(subtype,
         
         "logit_exponent" = {
           ## do not call with convert=T, there is only global data!
           
           tmp <- list.files(path="./", pattern = subtype)
           tmp_dfs <- stats::setNames(
                               object = lapply(tmp, fread),
                               nm = sub("\\..*","", tmp))
           
           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }
           
           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"
           
           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("sector","subsector_L3","subsector_L2","subsector_L1","vehicle_type","SSP","EDGE_scenario","logit.exponent"))
           setnames(tmp_dfs, old = "logit.exponent", new = "logitexp")
           mlogitexp <- as.magpie(tmp_dfs, datacol = 8)
           
           mdata <- mlogitexp
          
    
         },
         
         "SW" = {
           tmp = list.files(path="./", pattern = subtype)
           tmp_dfs <- stats::setNames(object = lapply(tmp, fread), nm = sub("\\..*","",tmp))
           
           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }
           
           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"
           
           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "SSP", "EDGE_scenario", "sw"))
           setnames(tmp_dfs, old ="sw", new ="value")
           
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (i in unique(tmp_dfs$SSP)) {
             tmp_SSP <- tmp_dfs[SSP == i]
             tmp_SSP <- as.magpie(tmp_SSP, datacol = 12, spatial = 1, temporal = 2)
             mdata <- mbind(mdata, tmp_SSP)
           }
           
           
         },
         
         "value_time" = {
           pattern <- "value_time"
           tmp = list.files(path="./", pattern = pattern)
           tmp_dfs <- stats::setNames(object = lapply(tmp, fread), nm = sub("\\..*","",tmp))
           
           for (i in names(tmp_dfs)) {
             tmp_dfs[[i]]$varname <- i
           }
           
           tmp_dfs <- rbindlist(tmp_dfs, fill= TRUE)
           tmp_dfs[is.na(tmp_dfs)] <- "tmp"
           
           tmp_dfs=tmp_dfs[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp_dfs, c("iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "varname", "SSP", "EDGE_scenario", "time_price"))
           setnames(tmp_dfs, old ="time_price", new ="value")
           
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (i in unique(tmp_dfs$SSP)) {
             tmp_SSP <- tmp_dfs[SSP == i]
             tmp_SSP <- as.magpie(tmp_SSP, datacol = 11, spatial = 1, temporal = 2)
             mdata <- mbind(mdata, tmp_SSP)
           }

         },
         
         "harmonized_intensities" = {
           
           tmp <- fread(paste0(subtype, ".csv"))
           tmp$varname <- subtype
           
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("iso", "year", "sector", "subsector_L3", "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "sector_fuel", "SSP", "EDGE_scenario", "EJ_Mpkm_final"))
           setnames(tmp, old ="EJ_Mpkm_final", new ="value")
           
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (i in unique(tmp$SSP)) {
             tmp_SSP <- tmp[SSP == i]
             tmp_SSP <- as.magpie(tmp_SSP, datacol = 13, spatial = 1, temporal = 2)
             mdata <- mbind(mdata, tmp_SSP)
           }
           

         },
         
         "price_nonmot" = {
    
           tmp <- fread(paste0(subtype, ".csv"))
           tmp$varname <- subtype
           
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "SSP", "EDGE_scenario", "tot_price"))
           setnames(tmp, old ="tot_price", new ="value")
           
           mdata <- as.magpie(tmp, datacol = 12, spatial = 1, temporal = 2)
           
         },
         
         "UCD_NEC_iso" = {
           tmp <- fread(paste0(subtype, ".csv"))
           
           tmp$varname <- subtype
           
           tmp=tmp[, vehicle_type := gsub("\\.", "DOT", vehicle_type)]
           setcolorder(tmp, c("iso", "year", "sector", "subsector_L3",  "subsector_L2", "subsector_L1", "vehicle_type", "technology", "varname", "SSP", "EDGE_scenario", "non_fuel_price"))
           setnames(tmp, old ="non_fuel_price", new ="value")
           
           ## concatenate multiple magpie objects each one containing one SSP realization to avoid large objects
           mdata <- NULL
           for (i in unique(tmp$SSP)) {
             tmp_SSP <- tmp[SSP == i]
             tmp_SSP <- as.magpie(tmp_SSP, datacol = 12, spatial = 1, temporal = 2)
             mdata <- mbind(mdata, tmp_SSP)
           }
           
         },
         
         "fe2es" = {
           tmp <- fread(paste0(subtype, ".cs4r"))
           setcolorder(tmp, c("tall", "iso", "all_teEs", "SSP", "EDGE_scenario", "value"))
           mdata <- as.magpie(tmp, datacol=6, spatial=2, temporal=1)
         },
         
         "esCapCost" = {
           tmp <- fread(paste0(subtype, ".cs4r"))
           setcolorder(tmp, c("tall", "iso", "all_teEs", "SSP", "EDGE_scenario","value"))
           mdata <- as.magpie(tmp, datacol=6, spatial=2, temporal=1)
         },

         "pm_trp_demand" = {
           tmp <- fread(paste0(subtype, ".cs4r"))
           ## temporarily delete the redundant column all_GDPscen
           tmp[,all_GDPscen := NULL]
           setcolorder(tmp, c("year", "iso", "SSP", "EDGE_scenario", "all_in","value"))
           mdata <- as.magpie(tmp, datacol=6, spatial=2, temporal=1)
         },

         "fe_demand_tech" = {
           tmp <- fread(paste0(subtype, ".cs4r"))
           setcolorder(tmp, c("tall", "iso", "all_enty", "all_in", "all_teEs", "SSP", "EDGE_scenario", "value"))
           mdata <- as.magpie(tmp, datacol=8, spatial=2, temporal=1)
         }, 
         {
           ## default
           stop(sprintf("Subtype %s is not valid for EDGETransport.", subtype))
         })
  
  return(mdata)
}

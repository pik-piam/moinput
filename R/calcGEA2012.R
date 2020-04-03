#' @title calcGEA2012
#' @description Extracts oil, gas and coal data from the GEA 2012 into a scenario- and time-dependent grade structure
#' @param subtype oil, coal, or gas
#' @param realization Determines whether to aggregate data into coal, oil or gas (timeDepGrades) or to keep subtype granularity (MOFEX)
#' @return MAgPIE object containing regionally aggregated GEA 2012 data
#' @author Stephen Bi
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput("GEA2012")
#' }
#' 

calcGEA2012 <- function(subtype,realization="timeDepGrades") {
  
  mapping <- toolGetMapping("regionmappingH12.csv","regional")
  regionsH12 <- c("CAZ","CHA","EUR","IND","JPN","LAM","MEA","NEU","OAS","REF","SSA","USA")
  
  if (realization=="timeDepGrades") {
    if (subtype=='coal') {
      x <- readSource("GEA2012",subtype=subtype)
      w <- read.csv(paste0(getConfig("sourcefolder"),"/BGR/coal_reserves.csv"),header=TRUE,sep=";")[,c("Land_Region","Remaining_Potential")]
      #convert the data into a magpie object, convert countries to ISO code and set missing countries to 0
      w <- as.magpie(w,spatial=1,temporal=0,datacol=2)
      getRegions(w) <- toolCountry2isocode(getRegions(w))
      w <- toolNAreplace(toolCountryFill(w,fill=0))[[1]]
    }else if (subtype %in% c('oil','gas')) {
      x <- readSource("GEA2012",subtype=subtype)
      w <- read.csv(paste0(getConfig("sourcefolder"),"/BGR/",subtype,"_reserves.csv"),header=TRUE,sep=";")[,c("Land_Region","Reserves","Resources")]
      #Remove NAs
      w[is.na(w)] <- 0
      #Convert to magpie for use as a disaggregation weight, convert countries to ISO code and set missing countries to 0
      w <- as.magpie(w,spatial=1,temporal=0,datacol=2)
      getRegions(w) <- toolCountry2isocode(getRegions(w))
      w <- toolNAreplace(toolCountryFill(w,fill=0))[[1]]
      #Disaggregate the GEA data according to the BGR data on country-level oil/gas combined reserves + resources
      w <- dimSums(w,dim=3)
    }else if (subtype=="bounds") {
      #SB 031720 This "decline offset" parameter is taken from the initial implementation of 31_fossil/timeDepGrades pre-moinput by NB & JH
      #Comment from *NB, IM* The parameter values are based on World Energy Outlook (2008, 2009) and further assumptions/approximations
      x <- new.magpie(regionsH12,NULL,names=paste("decoffset",c("pecoal","peoil","pegas"),sep="."),fill=-2e-4)
      x[c("REF","MEA"),,c("peoil","pegas")] <- -2e-5
      x <- toolAggregate(x,mapping,weight=NULL)
      w=NULL
    }else {
      stop("Invalid subtype. Please choose oil, gas or coal.")
    }
    
  }else if (realization=="MOFEX") {
    stop("input data processing for MOFEX realization not yet available.")
    
  }else {
    stop("invalid realization! Please choose timeDepGrades or MOFEX.")
  }
  
  return(list(x=x,
              weight=w,
              unit="mixed",
              description=paste("Cost grades and quantities of",subtype),
              note=c("Data from the IIASA Global Energy Assessment 2012")))
  
  
  # y <- new.magpie(regions,ttot[-1],names=paste(types[1],rep(xis,each=length(grades)),scens[1],grades,sep="."),0)
  #coal <- mbind(new.magpie(regions,ttot,names=paste("HAC",rep(scens,each=length(xis)*length(grades[[1]])),rep(xis,each=length(grades[[1]])),grades[[1]],sep="."),sets=sets),
  #              new.magpie(regions,ttot,names=paste("LIC",rep(scens,each=length(xis)*length(grades[[2]])),rep(xis,each=length(grades[[2]])),grades[[2]],sep="."),sets=sets))
  #  
  # all_FF <- lapply(c("oil","coal","gas"),function(type){
  #      mbind(lapply(scens,function(scen){
  #        x[,t0,paste(type,scen,sep=".")] <- x[,t0,paste(type,"SSP2",sep=".")]
  #        coal[,t0,paste("HAC",scen,sep=".")] <- x[,t0,paste("HAC","SSP5",sep=".")]
  #        coal[,t0,paste("LIC",scen,sep=".")] <- x[,t0,paste("LIC","SSP5",sep=".")]
  #        grades <- getNames(x[,,paste(type,scen,sep=".")],fulldim=TRUE,"grade")
  #        if (exists("oil_gas")) {
  #          oil_gas <- mbind(oil_gas,new.magpie(regions,ttot,names=paste(type,scen,rep(xis,each=length(grades)),grades,sep="."),sets=sets))
  #        }else {
  #          oil_gas <- new.magpie(regions,ttot,names=paste(type,scen,rep(xis,each=length(grades)),grades,sep="."),sets=sets)
  #        }
  #        oil_gas[,t0,paste(type,scen,sep=".")] <- x[,t0,paste(type,scen,sep=".")]
  #        for (t in ttot[-1]) {
  #          if (scen == "SSP2") {
  #            oil_gas[,t,paste(type,scen,sep=".")] <- x[,t0,paste(type,scen,sep=".")]
  #            if (type==types[3]) {
  #              coal[,t,"HAC"] <- x[,,"HAC"]
  #              coal[,t,"LIC"] <- x[,,"LIC"]
  #            }
  #          }else {
  #            if (t <= t_cutoff) {
  #              slope <- (x[,t0,paste(type,scen,sep=".")] - x[,t0,paste(type,"SSP2",sep=".")])/t_trans
  #              oil_gas[,t,paste(type,scen,sep=".")] <- x[,t0,paste(type,"SSP2",sep=".")] + slope*(t-t0)
  #            }else {
  #              oil_gas[,t,paste(type,scen,sep=".")] <- x[,t0,paste(type,scen,sep=".")]
  #            }
  #          }
  #        }
  #        all_FF <- mbind(coal,oil_gas)
  #        return(all_FF)
  #      }))
  #  })
  #  
  
  #all_FF <- mbind(coal,oil_gas)
  
}
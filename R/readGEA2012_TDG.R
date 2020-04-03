#' @title read GEA 2012
#' @description Read in datafiles comprising fossil fuel data from the Global Energy Assessment 2012
#' @param subtype Type of fossil fuel (oil coal or gas)
#' @return MAgPIE object of the GEA data
#' @author Stephen Bi
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("GEA2012","coal")
#' }
#' 
#' @importFrom readxl read_excel

readGEA2012_TDG <- function(subtype) {
  filepath <- paste0(getConfig("sourcefolder"),"/FFECCM/")
  
  #================================================================
  # GEA data processing functions fpprg_GEA2011_### derive FF cost grades from GEA 2012 cost and quantity data
  # Developed by Jerome Hilaire in 2011
  # 
  #================================================================
  fpprg_GEA2011_10_v1 <- function (
    inData   # Input data to be  (see format above)
  )
  {
    #==============================================================
    # Initialise environment
    #==============================================================
    nbcol_b4_dist  <- 1
    nbcol_b4_cost  <- 5
    
    nb_dist_pt     <- 4
    nb_cost_pt     <- 5
    nb_inter_pt    <- 4 
    
    nb_reg     <- length(inData[,1])  #Count number of regions
    outData    <- array(NA, dim=c(nb_reg, nb_dist_pt*nb_inter_pt+1, 2))  #Output array
    
    
    #==============================================================
    # Calculate extraction costs
    # TODO (jh): replace for loops by apply functions...
    #==============================================================
    for (i in 1:nb_reg) {
      # Cost data
      for (k in 1:nb_cost_pt-1) {
        delta <- (inData[i,nbcol_b4_cost+k+1]-inData[i,nbcol_b4_cost+k])/nb_inter_pt
        for (kk in 1:nb_inter_pt) {
          outData[i,4*(k-1)+kk,1] <- inData[i,nbcol_b4_cost+k] + (kk-1)*delta
        }
      }  
      outData[i,nb_dist_pt*nb_inter_pt+1,1] <- inData[i,nbcol_b4_cost+nb_cost_pt]
      
      # Fossil-fuel quantity data 
      for (k in 1:nb_dist_pt) {
        delta <- inData[i,nbcol_b4_dist]*inData[i,nbcol_b4_dist+k]/nb_inter_pt
        for (kk in 1:nb_inter_pt) {
          outData[i,4*(k-1)+kk,2] <- delta
        }
      }  
      outData[i,nb_dist_pt*nb_inter_pt+1,2] <- 0.0
    }
    #CHECK (jh): remove later
    #print(sum(apply(outData[,,2], 2, sum)))
    
    
    #==============================================================
    # Return data
    #==============================================================
    return(outData)
  }
  
  fpprg_GEA2011_13_v1 <- function (
    inData   # Input data to be processed
  )
  {
    #==============================================================
    # Initialise environment
    #==============================================================
    # Number of columns before...
    p_nbColB4Dist  <- 1  # Distribution data
    p_nbColB4Cost  <- 8  # Cost data
    
    # Number of columns...
    p_NbColDist    <- 3   # Reserves and Resources
    p_NbColCost    <- 5   # Cost categories (also called grades)
    p_nbInterPt    <- 3   # 
    p_nbInterPt2   <- 4
    
    p_nbReg        <- length(inData[,1])                                    #Count number of regions
    v_outData      <- array(NA, dim=c(p_nbReg, (p_NbColCost-1)*p_nbInterPt,2))  #Output array
    
    # Output array indices for cost and quantity (last dimension)
    p_idCost       <- 1
    p_idQuantity   <- 2
    
    # Unit conversion
    p_factor <- 1000.0
    
    #==============================================================
    # Calculate extraction costs
    #==============================================================
    for (i in 1:p_nbReg) {
      # Cost data
      # Calculate for first 2 categories
      for (k in 1:(p_NbColCost-2)) {
        v_delta <- (inData[i,p_nbColB4Cost+k+1]-inData[i,p_nbColB4Cost+k])/  # Cost difference   [USD/TJ]
          p_factor                                               /  # Convert to EJ
          p_nbInterPt                                               # Number of extra data point               
        for (kk in 1:p_nbInterPt) {
          v_outData[i,(p_nbInterPt+1)*(k-1)+kk,p_idCost] <- inData[i,p_nbColB4Cost+k]/p_factor + (kk-1)*v_delta
        }
        v_outData[i,(p_nbInterPt+1)*k,p_idCost] <- inData[i,p_nbColB4Cost+k+1]/p_factor
      }  
      # Special case for 3rd category
      v_delta <- (inData[i,p_nbColB4Cost+p_NbColCost]-inData[i,p_nbColB4Cost+p_NbColCost-1])/  # Cost difference   [USD/TJ]
        p_factor                                                                 /  # Convert to EJ
        p_nbInterPt                                                                 # Number of extra data point
      for (kk in 1:p_nbInterPt) {
        v_outData[i,(p_nbInterPt+1)*2+kk,p_idCost] <- inData[i,p_nbColB4Cost+p_NbColCost-1]/p_factor + (kk-1)*v_delta
      }
      v_outData[i,(p_NbColCost-1)*p_nbInterPt,p_idCost] <- inData[i,p_nbColB4Cost+p_NbColCost]/p_factor
      
      # Fossil-fuel quantity data 
      for (k in 1:p_NbColDist) {
        v_temp <- inData[i,p_nbColB4Dist]*  # Total reserve + resources quantity [EJ]
          inData[i,p_nbColB4Dist+k]   # Distribution factor 1              [ratio] 
        
        for (kk in 1:p_nbInterPt2) {
          v_outData[i,p_nbInterPt2*(k-1)+kk,p_idQuantity] <- v_temp*inData[i,p_nbColB4Dist+p_NbColDist+kk]
        }
      }
      v_outData[i,p_NbColDist*p_nbInterPt2,p_idQuantity] <- v_outData[i,p_NbColDist*p_nbInterPt2,p_idQuantity]
    }
    #CHECK (jh): remove later
    #print(sum(apply(v_outData[,,2], 2, sum)))
    
    #==============================================================
    # Return data
    #==============================================================
    return(v_outData)
  }
  
  fpprg_GEA2011_16_v1 <- function (
    inData   # Input data to be processed
  )
  {
    #==============================================================
    # Initialise environment
    #==============================================================
    # Number of columns before...
    p_nbColB4Dist  <- 3   # Distribution data
    p_nbColB4Cost  <- 11  # Cost data
    
    # Number of columns...
    p_NbColDist    <- 4   # Reserves and Resources
    p_NbColCost    <- 5   # Same as above
    p_nbInterPt    <- 3   # Should stay to 3
    p_nbInterPt2   <- 4   # Should stay to 4
    
    p_nbReg        <- length(inData[,1])                                    #Count number of regions
    v_outData      <- array(NA, dim=c(p_nbReg, p_NbColCost*p_nbInterPt+1,2))  #Output array
    
    # Output array indices for cost and quantity (last dimension)
    p_idCost       <- 1
    p_idQuantity   <- 2
    
    p_factor       <- 1000.0
    p_factor2      <- 100.0
    
    #==============================================================
    # Calculate extraction costs
    #==============================================================
    for (i in 1:p_nbReg) {
      # Cost data
      for (k in 1:p_NbColCost-1) {
        v_delta <- (inData[i,p_nbColB4Cost+k+1]-inData[i,p_nbColB4Cost+k])/  # Cost difference   [USD/TJ]
          p_factor                                               /  # Convert to EJ
          p_nbInterPt                                               # Data point number
        for (kk in 1:p_nbInterPt) {
          v_outData[i,(p_nbInterPt+1)*(k-1)+kk,p_idCost] <- inData[i,p_nbColB4Cost+k]/p_factor + (kk-1)*v_delta
        }
        v_outData[i,(p_nbInterPt+1)*k,p_idCost] <- inData[i,p_nbColB4Cost+k+1]/p_factor
      }  
      
      # Fossil-fuel quantity data 
      for (k in 1:p_NbColDist) {
        v_temp <- inData[i,p_nbColB4Dist-2]*  # Total reserve + resources quantity [EJ]
          inData[i,p_nbColB4Dist+k]/  # Distribution factor 1              [percentage] 
          p_factor2  # Percentage factor
        
        for (kk in 1:p_nbInterPt2) {
          v_outData[i,p_nbInterPt2*(k-1)+kk,p_idQuantity] <- v_temp*inData[i,p_nbColB4Dist+p_NbColDist+kk]
        }
      }
    }
    #CHECK (jh): remove later
    #print(sum(apply(v_outData[,,2], 2, sum)))
    
    #==============================================================
    # Return data
    #==============================================================
    return(v_outData)
  }
  
  fpprg_GEA2011_17_v3 <- function (
    inData   # Input data to be processed
  )
  {
    #==============================================================
    # Initialise environment
    #==============================================================
    nbcol_b4_dist  <- 7
    nbcol_b4_cost  <- 12
    
    nb_dist_pt  <- 5
    nb_cost_pt  <- 5
    nb_inter_pt <- 1
    
    nb_reg     <- length(inData[,1])  #Count number of regions
    outData    <- array(NA, dim=c(nb_reg, nb_dist_pt*nb_inter_pt,2))  #Output array
    
    
    #==============================================================
    # Calculate extraction costs
    #==============================================================
    for (i in 1:nb_reg) {
      # Cost data
      for (k in 1:nb_cost_pt-1) {
        delta <- (inData[i,nbcol_b4_cost+k+1]-inData[i,nbcol_b4_cost+k])/nb_inter_pt
        for (kk in 1:nb_inter_pt) {
          outData[i,nb_inter_pt*(k-1)+kk,1] <- inData[i,nbcol_b4_cost+k] + (kk-1)*delta
        }
      }  
      
      outData[i,nb_dist_pt*nb_inter_pt,1] <- inData[i,nbcol_b4_cost+nb_cost_pt]
      
      # Fossil-fuel quantity data 
      # For each cost category, the amount of gas hydrates is multiplied by a factor of 5000/12638=0.40
      # This scaling factor has been moved to scenario data
      for (k in 1:nb_dist_pt) {
        #delta <- inData[i,nbcol_b4_dist-1]*inData[i,nbcol_b4_dist]*inData[i,nbcol_b4_dist+k]/nb_inter_pt
        delta <- inData[i,nbcol_b4_dist-1]*inData[i,nbcol_b4_dist+k]/nb_inter_pt
        for (kk in 1:nb_inter_pt) {
          outData[i,nb_inter_pt*(k-1)+kk,2] <- delta
        }
      }  
      #outData[i,nb_dist_pt*nb_inter_pt,2] <- inData[i,nbcol_b4_dist-1]*inData[i,nbcol_b4_dist]*inData[i,nbcol_b4_dist+nb_dist_pt]
      outData[i,nb_dist_pt*nb_inter_pt,2] <- inData[i,nbcol_b4_dist-1]*inData[i,nbcol_b4_dist+nb_dist_pt]
      
    }
    #CHECK (jh): remove later
    #print(sum(apply(outData[,,2], 2, sum)))
    
    
    #==============================================================
    # Return data
    #==============================================================
    return(outData)
  }
  
  fpprg_GEA2011_21_v2 <- function (
    inData   # Input data to be processed
  )
  {
    #==============================================================
    # Initialise environment
    #==============================================================
    nbcol_b4_dist  <- 3
    nbcol_b4_cost  <- 12
    
    nb_dist_pt_rsv <- 4
    nb_dist_pt_rso <- 5
    nb_dist_pt     <- nb_dist_pt_rsv+nb_dist_pt_rso
    nb_cost_pt     <- 9
    nb_inter_pt    <- 1 
    
    nb_reg         <- length(inData[,1])  #Count number of regions
    outData        <- array(NA, dim=c(nb_reg, nb_dist_pt*nb_inter_pt,2))  #Output array
    
    
    #==============================================================
    # Calculate extraction costs
    #==============================================================
    for (i in 1:nb_reg) {
      # Cost data
      for (k in 1:nb_cost_pt-1) {
        delta <- (inData[i,nbcol_b4_cost+k+1]-inData[i,nbcol_b4_cost+k])/nb_inter_pt
        for (kk in 1:nb_inter_pt) {
          outData[i,nb_inter_pt*(k-1)+kk,1] <- inData[i,nbcol_b4_cost+k] + (kk-1)*delta
        }
      }  
      outData[i,nb_dist_pt*nb_inter_pt,1] <- inData[i,nbcol_b4_cost+nb_cost_pt]
      
      # Fossil-fuel quantity data 
      #100% Reserves
      for (k in 1:nb_dist_pt_rsv) {
        delta <- inData[i,nbcol_b4_dist-2]*inData[i,nbcol_b4_dist+k]/nb_inter_pt
        for (kk in 1:nb_inter_pt) {
          outData[i,nb_inter_pt*(k-1)+kk,2] <- delta
        }
      }
      #20% Resources (scaling factor moved to scenario data)
      for (k in 1:nb_dist_pt_rso) {
        #delta <- inData[i,nbcol_b4_dist-1]*inData[i,nbcol_b4_dist]*inData[i,nbcol_b4_dist+nb_dist_pt_rsv+k]/nb_inter_pt
        delta <- inData[i,nbcol_b4_dist-1]*inData[i,nbcol_b4_dist+nb_dist_pt_rsv+k]/nb_inter_pt
        for (kk in 1:nb_inter_pt) {
          outData[i,nb_inter_pt*(k+nb_dist_pt_rsv-1)+kk,2] <- delta
        }
      }
      #outData[i,nb_dist_pt*nb_inter_pt,2] <- inData[i,nbcol_b4_dist-1]*inData[i,nbcol_b4_dist]*inData[i,nbcol_b4_dist+nb_dist_pt_rsv+nb_dist_pt_rso]
      outData[i,nb_dist_pt*nb_inter_pt,2] <- inData[i,nbcol_b4_dist-1]*inData[i,nbcol_b4_dist+nb_dist_pt_rsv+nb_dist_pt_rso]
    }
    #CHECK (jh): remove later
    #print(sum(apply(outData[,,2], 2, sum)))
    
    
    #==============================================================
    # Return data
    #==============================================================
    return(outData)
  }
  
  fpprg_GEA2011_31_v1 <- function (
    inData   # Input data to be processed
  )
  {
    #==============================================================
    # Initialise environment
    #==============================================================
    # Number of columns before...
    p_nbColB4Dist  <- 3   # Distribution data
    p_nbColB4Cost  <- 17  # Cost data
    
    # Number of columns...
    p_NbColDistRsv <- 6                             # Reserves
    p_NbColDistRso <- 8                             # Resources
    p_NbColDist    <- p_NbColDistRsv+p_NbColDistRso # Reserves and Resources
    p_NbColCost    <- p_NbColDist                   # Same as above
    p_nbInterPt    <- 1                             # Should stay to 1
    
    p_nbReg        <- length(inData[,1])                                    #Count number of regions
    v_outData      <- array(NA, dim=c(p_nbReg, p_NbColDist*p_nbInterPt,2))  #Output array
    
    # Output array indices for cost and quantity (last dimension)
    p_idCost       <- 1
    p_idQuantity   <- 2
    
    #==============================================================
    # Calculate extraction costs
    #==============================================================
    for (i in 1:p_nbReg) {
      # Cost data
      for (k in 1:p_NbColCost-1) {
        delta <- (inData[i,p_nbColB4Cost+k+1]-inData[i,p_nbColB4Cost+k])/  # Cost difference   [USD]
          p_nbInterPt                                               # Data point number
        for (kk in 1:p_nbInterPt) {
          v_outData[i,p_nbInterPt*(k-1)+kk,p_idCost] <- inData[i,p_nbColB4Cost+k] + (kk-1)*delta
        }
      }  
      v_outData[i,p_NbColDist*p_nbInterPt,p_idCost] <- inData[i,p_nbColB4Cost+p_NbColCost]
      
      # Fossil-fuel quantity data 
      #100% Reserves
      for (k in 1:p_NbColDistRsv) {
        delta <- inData[i,p_nbColB4Dist-2]*  # Total resources quantity [EJ]
          inData[i,p_nbColB4Dist+k]/  # Distribution factor      [ratio] 
          p_nbInterPt                 # Data point number
        for (kk in 1:p_nbInterPt) {
          v_outData[i,p_nbInterPt*(k-1)+kk,p_idQuantity] <- delta
        }
      }
      #33% Resources (GEA assumption - moved to scenario file)
      for (k in 1:p_NbColDistRso) {
        delta <- inData[i,p_nbColB4Dist-1]*                 # Total resources quantity [EJ]
          #               inData[i,p_nbColB4Dist]*                   # Recovery factor          [ratio] -> moved to scenario file
          inData[i,p_nbColB4Dist+p_NbColDistRsv+k]/  # Distribution factor      [ratio]
          p_nbInterPt                                # Data point number
        for (kk in 1:p_nbInterPt) {
          v_outData[i,p_nbInterPt*(k+p_NbColDistRsv-1)+kk,p_idQuantity] <- delta
        }
      }
    }
    #CHECK (jh): remove later
    #print(sum(apply(v_outData[,,2], 2, sum)))
    
    
    #==============================================================
    # Return data
    #==============================================================
    return(v_outData)
  }
  
  #================================================================
  # SSP data adjustments 
  # Functions developed by Jerome Hilaire in 2011 
  # Factors and data defined in Bauer et al. 2013
  #================================================================
  
  fs_SSP2012_2_v1 <- function (
    inData,   # Input data to be processed
    inScenAss # Scenario assumptions
  )
  {
    #==============================================================
    # Initialise environment
    #==============================================================
    p_idCost     <- 1
    p_idQuantity <- 2
    
    
    #==============================================================
    # Apply Scenario assumptions
    #==============================================================
    for (k in 1:length(inData[,1,1])) {
      
      # Cost factor
      inData[k, ,p_idCost] <- inData[k, ,p_idCost]*inScenAss[k, p_idCost] #Cost
      
      # Quantity factor
      inData[k, ,p_idQuantity] <- inData[k, ,p_idQuantity]*inScenAss[k, p_idQuantity] #Quantity
      
    }
    
    
    #==============================================================
    # Return data
    #==============================================================
    outData <- inData
    return(outData)
  }
  
  
  fs_SSP2012_2_v2 <- function (
    inData,   # Input data to be processed
    inScenAss # Scenario assumptions
  )
  {
    #==============================================================
    # Initialise environment
    #==============================================================
    p_idCost     <- 1
    p_idQuantity <- 2
    p_USDpBBL2USDpGJ = 1/5.712 #(Rogner et al., 2012)
    
    #==============================================================
    # Apply Scenario assumptions
    #==============================================================
    for (k in 1:length(inData[,1,1])) {
      
      # Cost mark-up
      inData[k, ,p_idCost] <- inData[k, ,p_idCost]+inScenAss[k, p_idCost]*p_USDpBBL2USDpGJ #Cost mark-up
      
      # Quantity factor
      inData[k, ,p_idQuantity] <- inData[k, ,p_idQuantity]*inScenAss[k, p_idQuantity] #Quantity
      
    }
    
    
    #==============================================================
    # Return data
    #==============================================================
    outData <- inData
    return(outData)
  }
  
  
  #================================================================
  # Read source commands begin here
  #================================================================  
  
  #================================================================
  # Data retrieval and processing function selection
  #================================================================
  ffTypeData <- list()
  ffTypeScenData <- list()
  tmp <- NULL
  
  #Ordering of SSPs in this vector corresponds to ordering of coded scenarios in "Scenario data XX.xlsx"
  scen <- c('SSP5','SSP2','SSP1','SSP3','SSP4')
  if (subtype=="gas")  ffType <- c('SHG-rv','COG-rv','CMG-rv','TIG-rv','HYG-rv','DEG-rv','SHG-rs','COG-rs','CMG-rs','TIG-rs','HYG-rs')
  if (subtype=="oil")  ffType <- c('TAO-rv','SHO-rv','EHO-rv','COO-rv','TAO-rs','SHO-rs','EHO-rs','COO-rs')
  #subtype <- c('SHG-rv','COG-rv','CMG-rv','TIG-rv','HYG-rv','DEG-rv','SHG-rs','COG-rs','CMG-rs','TIG-rs','HYG-rs','DEG-rs',
  #             'TAO-rv','SHO-rv','EHO-rv','COO-rv','TAO-rs','SHO-rs','EHO-rs','COO-rs','HAC','LIC')
  
  if (subtype=="coal") {
    out <- read.csv2(paste0(getConfig("sourcefolder"),"/GEA2012/scenarios/Scenario Data HAC_LIC.csv"),header=TRUE,as.is = T)
    out$value <- as.numeric(out$value)
    out <- as.magpie(out)
  }else {
    # Loop over FF types
    for (i in ffType) {
      #Read FF type data
      typeFilename <- paste0("FF Data ",i,".xlsx")
      rawData <- as.data.frame(read_excel(paste0(filepath,"fossil fuels/",typeFilename)))
      rawData <- rawData[,which(!is.na(rawData[1,]))]
      if (!exists("nreg")) {
        regions <- rawData[,"Region code"]
        nreg <- dim(rawData)[1]
      }
      #Read appropriate pre-processing function from the file
      ppFunc <- as.character(rawData[1,"R Pre-Proc Function"])
      #Retain only numerical data
      numData <- rawData[1:nreg,5:dim(rawData)[2]]
      #Execute pre-processing function
      ffTypeData[[i]] <- eval(parse(text=paste0(ppFunc,"(numData)")))
      
      #Read Scenario data (cost and quantity mark-ups/factors)
      scenFilename <- paste0("Scenario data ",i,".xlsx")
      scenData <- as.data.frame(read_excel(paste0(filepath,"scenarios/",scenFilename)))
      scenData <- scenData[,which(!is.na(scenData[1,]))]
      
      #Some data files are associated with 2 scenario adjustment functions -- these must be handled differently (EHO and TAO)
      n_scenFuncs <- length(unique(scenData[,"R Scenario Function"]))
      #Loop over scenarios
      for (j in 1:length(scen)) {
        #Case 1: only 1 scenario adjustment function for the FF type
        if (n_scenFuncs==1) {
          ffTypeScenData[[i]][[scen[j]]] <- fs_SSP2012_2_v1(ffTypeData[[i]],scenData[((j-1)*nreg+1):(j*nreg),6:7])
        }else {
          #Case 2: 1 scenario adustment function for this scenario of the FF type
          if (length(unique(scenData[((j-1)*nreg+1):(j*nreg),"R Scenario Function"]))==1) {
            scenFunc <- as.character(scenData[1+(j-1)*nreg,"R Scenario Function"])
            ffTypeScenData[[i]][[scen[j]]] <- eval(parse(text = paste0(scenFunc,"(ffTypeData[[i]],scenData[((j-1)*nreg+1):(j*nreg),6:7])")))
            #Case 3: Different scenario adjustment functions across regions within the scenario of the FF type 
          }else {
            ffTypeScenData[[i]][[scen[j]]] <- array(NA,dim=dim(ffTypeData[[i]]))
            #Read and use the scenario function for each region
            for (k in 1:nreg) {
              scenFunc <- as.character(scenData[k+(j-1)*nreg,"R Scenario Function"])
              ffTypeScenData[[i]][[scen[j]]][k,,] <- eval(parse(text = paste0(
                scenFunc,"(array(ffTypeData[[i]][k,,],
                dim=c(1,dim(ffTypeData[[i]])[2],2)),
                scenData[k+(j-1)*nreg,6:7])")))
            }
          }
        }
      }
    }
    for (ii in names(ffTypeScenData)) {
      for (jj in scen) {
        grades <- dim(ffTypeScenData[[ii]][[jj]])[2]
        tmp <- mbind(tmp,new.magpie(regions,"y2005",paste(rep(c("costs","qtys"),each=grades),ii,jj,paste0("rlf",1:grades),sep="."),
                                    ffTypeScenData[[ii]][[jj]]))
      }
    }
  
    getSets(tmp) <- c("region","year","xi","type","scen","grade")
    tmp <- toolNAreplace(tmp,replaceby=0)[[1]]
    
    
    #Conversion to REMIND-readable data
    ts1 <- 5
    ts2 <- 10
    ttot <- c(seq(2005,2055,ts1),seq(2060,2150,ts2))
    t_cutoff <- 2035
    t0 <- 2005
    t_trans <- (t_cutoff - t0)
    #Store regions, FF types, data type (xi), and scenarios from x
    regions <- getRegions(tmp)
    types <- getNames(tmp,fulldim=TRUE,"type")
    scens <- getNames(tmp,fulldim=TRUE,"scen")
    #Move SSP2 to the front - important for loop below
    scens <- c("SSP2",scens[which(scens!="SSP2")])
    sets <- c("region","year","type","scen","xi","grade")
    xis <- c(paste0('xi',1:3),"dec")
    EJ_2_TWyr <- 1/31.536
    
    # Cost grades taken from expert judgment of production cost curves by JH & NB (FFECCM) in 2012
    costGrades = list(
      SSP1 = list(    # US$(2005)/TWa
        "oil_mea"  = c(0.054909051, 0.093408501, 0.204015527, 0.747400879, 1.390996080, 2.076113553, 2.480955695, 3.170979198),
        "oil_row"  = c(0.109818102, 0.186817002, 0.276754241, 0.415222711, 0.747400879, 1.141862454, 2.480955695, 3.170979198),
        "gas_mea"  = c(0.022089848, 0.060589298, 0.080943515, 0.236355064, 0.286536888, 0.863137859, 3.170979198),
        "gas_row"  = c(0.044179696, 0.121178596, 0.157626845, 0.236355064, 0.350155651, 0.863137859, 3.170979198),
        "coal"     = c(0.0158548960, 0.0761035008, 0.0919583968, 0.1331811263, 0.1997716895, 0.2980720446, 0.9512937595)
      ),
      SSP2 = list(    # US$(2005)/TWa
        "oil_mea"  = c(0.048594510, 0.082666523, 0.165612325, 0.328856387, 0.493284580, 0.918057413, 1.637430759, 3.170979198),
        "oil_row"  = c(0.069185405, 0.117694711, 0.174355172, 0.257059564, 0.493284580, 0.918057413, 1.637430759, 3.170979198),
        "gas_mea"  = c(0.019549516, 0.036585522, 0.071635011, 0.201339499, 0.262616738, 0.779742897, 3.170979198),
        "gas_row"  = c(0.027833209, 0.076342515, 0.108871395, 0.198808634, 0.262616738, 0.779742897, 3.170979198),
        "coal"     = c(0.0158548960, 0.0761035008, 0.0919583968, 0.1331811263, 0.1997716895, 0.2980720446, 0.9512937595)
      ),
      SSP5 = list(    # US$(2005)/TWa
        "oil_mea"  = c(0.027454526, 0.046704250, 0.093566286, 0.304322191, 0.463490897, 0.909855311, 1.240477848, 3.170979198),
        "oil_row"  = c(0.027454526, 0.046704250, 0.102007763, 0.166089084, 0.332003705, 0.546535439, 1.240477848, 3.170979198),
        "gas_mea"  = c(0.011044924, 0.030294649, 0.040471758, 0.094670778, 0.175077825, 0.416974811, 3.170979198),
        "gas_row"  = c(0.011044924, 0.030294649, 0.043864127, 0.094670778, 0.175077825, 0.416974811, 3.170979198),
        "coal"     = c(0.0158548960, 0.0761035008, 0.0919583968, 0.1331811263, 0.1997716895, 0.2980720446, 0.9512937595)
      )  
    )

    # IEA decline rate data from WEO 2008/09
    if (subtype=="oil") {
      sp_IEADecRat <- new.magpie(c("MEA", "EUR", "USA", "JPN", "RUS", "LAM", "CHN", "IND", "OAS", "AFR", "ROW"),years=NULL,names=c("conv","unconv"),fill=0)
      sp_IEADecRat[,,"conv"] <- c(0.034,    0.119,    0.097,    0.126,    0.058, 0.066,    0.067,    0.067,    0.067,    0.068,    0.067)
      sp_IEADecRat[,,"unconv"] <- c(0.150,    0.150,    0.150,    0.150,    0.150, 0.150,    0.150,    0.150,    0.150,    0.150,    0.150)
    }else if (subtype=="gas") {
      sp_IEADecRat <- new.magpie(c("MEA", "EUR", "USA", "JPN", "RUS", "LAM", "CHN", "IND", "OAS", "AFR", "ROW"),years=NULL,names=c("conv","unconv"),fill=0)
      sp_IEADecRat[,,"conv"] <- c(0.041,    0.111,    0.111,    0.111,    0.041, 0.111,    0.082,    0.082,    0.082,    0.082,    0.111)
      sp_IEADecRat[,,"unconv"] <- c(0.150,    0.150,    0.150,    0.150,    0.150, 0.150,    0.150,    0.150,    0.150,    0.150,    0.150)
    }
    mappingGEA <- toolGetMapping("regionmappingREMIND.csv","regional")
    mappingREM11 <- toolGetMapping("regionmappingGEA2012.csv","regional")
    w <- read.csv(paste0(getConfig("sourcefolder"),"/BGR/",subtype,"_reserves.csv"),header=TRUE,sep=";")[,c("Land_Region","Reserves","Resources")]
    #Remove NAs
    w[is.na(w)] <- 0
    #Convert to magpie for use as a disaggregation weight, convert countries to ISO code and set missing countries to 0
    w <- as.magpie(w,spatial=1,temporal=0,datacol=2)
    getRegions(w) <- toolCountry2isocode(getRegions(w))
    w <- toolNAreplace(toolCountryFill(w,fill=0))[[1]]
    #Disaggregate the GEA data according to the BGR data on country-level oil/gas combined reserves + resources
    w <- dimSums(w,dim=3)
    sp_IEADecRat <- toolAggregate(sp_IEADecRat,mappingGEA,weight=NULL)
    sp_IEADecRat <- toolAggregate(sp_IEADecRat,mappingREM11,weight=w)
    
    for (r in regions) {
      for (scen in names(costGrades)) {
        tmp[r,,'qtys'][,,scen] <- tmp[r,,'qtys'][,,scen][order(tmp[r,,'costs'][,,scen])]
        getNames(tmp[r,,'qtys'][,,scen]) <- getNames(tmp[r,,'qtys'][,,scen][,,order(tmp[r,,'costs'][,,scen])])
        getNames(tmp[r,,'costs'][,,scen]) <- getNames(tmp[r,,'costs'][,,scen][,,order(tmp[r,,'costs'][,,scen])])
        tmp[r,,'costs'][,,scen] <- tmp[r,,'costs'][,,scen][order(tmp[r,,'costs'][,,scen])] * EJ_2_TWyr
      }
    }
    row <- paste0(subtype,'_row')
    mea <- paste0(subtype,'_mea')
    ngrades <- length(costGrades[['SSP2']][[row]])-1
    out <- new.magpie(regions,ttot,paste(rep(names(costGrades),each=ngrades*length(xis)),rep(xis,each=ngrades),paste0("rlf",1:ngrades),sep="."),fill=0)
    conv <- new.magpie(regions,ttot,paste(rep(names(costGrades),each=ngrades),"dec",paste0("rlf",1:ngrades),sep="."),fill=0)
    unconv <- new.magpie(regions,ttot,paste(rep(names(costGrades),each=ngrades),"dec",paste0("rlf",1:ngrades),sep="."),fill=0)
    
    for (s in scens) {
      if (s %in% names(costGrades)) {
        #if (s=="SSP2") {
          t <- ttot
        #}else {
        #  t <- ttot[which(ttot>=t_cutoff)]
        #}
        for (r in regions) {
          i <- 1
          costs <- as.numeric(tmp[r,,s][,,'costs'])
          if (r=="MEE") {
            grades <- costGrades[[s]][[mea]]
          }else {
            grades <- costGrades[[s]][[row]]
          }
          ngrades <- length(grades)-1
          for (g in 1:ngrades) {
            out[r,t,paste0(s,'.xi1')][,,g] <- grades[g]
            out[r,t,paste0(s,'.xi2')][,,g] <- grades[g+1]
            for (c in i:(length(costs))) {
              if (costs[c] <= grades[g+1] && costs[c] >= grades[g]) {
                out[r,t,paste0(s,'.xi3')][,,g] <- out[r,t,paste0(s,'.xi3')][,,g] + tmp[r,,'qtys'][,,c]
                #Distinguish between conventional and unconventional reservoirs in each cost grade to calculate decline rates
                if (grepl('CO',getNames(tmp[r,,'qtys'][,,s][,,c]))) {
                  conv[r,t,s][,,g] <- conv[r,t,s][,,g] + tmp[r,,'qtys'][,,c]
                }else {
                  unconv[r,t,s][,,g] <- unconv[r,t,s][,,g] + tmp[r,,'qtys'][,,c]
                }
              }else if (costs[c] > grades[g+1]) {
                i <- c
                break
              }
            }
          }
        }
        #out[,,s][,,'xi3'] <- conv[,,s] + unconv[,,s]
        #Introduce time dependence of grades: costs start at SSP2 levels in initial year and change linearly until the cutoff year
        if (s!="SSP2") {
          m1 <- (out[,t_cutoff,'xi1'][,,s] - out[,t0,'xi1'][,,'SSP2'])/t_trans
          m2 <- (out[,t_cutoff,'xi2'][,,s] - out[,t0,'xi2'][,,'SSP2'])/t_trans
          m3 <- (out[,t_cutoff,'xi3'][,,s] - out[,t0,'xi3'][,,'SSP2'])/t_trans
          for (t1 in seq(t0,t_cutoff-ts1,ts1)) {
            out[,t1,'xi1'][,,s] <- out[,t1,'xi1'][,,'SSP2'] + m1*(t1-t0)
            out[,t1,'xi2'][,,s] <- out[,t1,'xi2'][,,'SSP2'] + m2*(t1-t0)
            out[,t1,'xi3'][,,s] <- out[,t1,'xi3'][,,'SSP2'] + m3*(t1-t0)
            #for (r1 in regions) {
            #  j <- 1
            #  for (g1 in 1:ngrades) {
            #    for (c1 in j:length(tmp[r1,,'costs'][,,s])) {
            #      if (tmp[r1,,'costs'][,,s][,,c1] >= out[r1,t1,'xi1'][,,s][,,g1] && tmp[r1,,'costs'][,,s][,,c1] <= out[r1,t1,'xi2'][,,s][,,g1]) {
            #        out[r1,t1,'xi3'][,,s][,,g1] <- out[r1,t1,'xi3'][,,s][,,g1] + tmp[r1,,'qtys'][,,s][,,c1]
            #      }else if (tmp[r1,,'costs'][,,s][,,c1] > out[r1,t1,'xi2'][,,s][,,g1]) {
            #        j <- c1
            #        break
            #      }
            #    }
            #  }
            #}
          }
        }
        #Calculate decline rates
          convRatio <- conv[,,s]/unconv[,,s]
          convRatio[which(is.nan(convRatio))] <- 0
          convRatio[which(is.infinite(convRatio))] <- 1
          out[,,s][,,'dec'] <- sp_IEADecRat[,,"conv"] * convRatio +
            sp_IEADecRat[,,"unconv"] * (1-convRatio)

      }
    }
  }
  return(out)
}


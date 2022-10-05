#' Create farm list
#'
#' @description \code{create_farmslist} Generate the list of farms
#'     with export destination characteristics
#' @usage create_farmslist(listfarms, fi2ges, fa2pw, pw2fi)
#' @param dataframefarms Data.frame listing farms and their
#'     characteristics: id, type, nbRP, nbPW, nbFat
#' @param fa2pw data.frame with three columns: tail, head and p.  Each
#'     row represent a possible connection from the tail to the head
#'     having a probability p to happen in case of external fa-pw
#'     movement.
#' @param pw2fi data.frame with three columns: tail, head and p.  Each
#'     row represent a possible connection from the tail to the head
#'     having a probability p to happen in case of external pw-fi
#'     movement.
#' @param fi2ges_w2 data.frame with three columns: tail, head and p.
#'     Each row represent a possible connection from the tail to the
#'     head having a probability p to happen in case of external
#'     fi-ges movement.
#' @param beta_ges Transmission rate in the gestation sector (default
#'     value = 0.7),
#' @param beta_fa Transmission rate in the farrowing sector (default
#'     value = 0.7),
#' @param beta_pw Transmission rate in the post-weaning sector
#'     (default value = 0.7),
#' @param beta_fi Transmission rate in the finishing sector (default
#'     value = 0.7)
#' @param qIng Average quantity of faeces ingested by a pig (g/day)
#' @return List
#' @examples farmslist <- create_farmslist(listfarms, fi2ges, fa2pw, pw2fi)
#'
#' @export
#'
# require magrittr


create_farmslist <- function(premises,
                             gesfromfiprob,
                             fa2pwprob,
                             pw2fiprob,
                             premises2slprob,
                             BRS.str,
                             ProbContactThreshold = 0.975,
                             maxNContactsPW = 50,
                             maxNContactsFI = 50,
                             maxNContactsGES = 50,
                             cleaning_rates = c(high = 0.98,
                                                low = 0.50),
                             ext_BioSec_frequency,
                             qIng = 25,
                             withnames = TRUE){

    # Check inputs
    if(FALSE %in% (premises[type != "SL", "BRS"] %>% unique %>% unlist %in% BRS.str$BRS.structure$id.BRS))
      stop(
        c(premises[type != "SL", "BRS"] %>% unique %>% unlist[which(!(premises[type != "SL", "BRS"] %>% unique %>% unlist %in% BRS.str$BRS.structure$id.BRS))]
          ," batch rearing systems (BRS) are not defined in BRS.str"))

    if(!("siteID" %in% (premises %>% colnames))) stop ("premises should include a column named siteID")
    if(FALSE %in% ((premises$type %>% unique) %in% c("SL","NU","MU","FF","FA","PW","FI","FPW","PWF"))) stop("premises types currently implemented are ", paste(c("SL","NU","MU","FF","FA","PW","FI","FPW","PWF"), collapse=", "))

    if(TRUE %in% sapply(c("gesfromfiprob", "fa2pwprob", "pw2fiprob", "premises2slprob"), function(x) !inherits(get(x), "data.frame"))
    ) stop ("gesfromfiprob, fa2pwprob, pw2fiprob and premises2slprob must be of class data.frame")


    if(TRUE %in% sapply(c("gesfromfiprob", "fa2pwprob", "pw2fiprob", "premises2slprob"), function(x){
      FALSE %in% (c("tail", "head", "p") %in% (get(x) %>% colnames)) })
    ) stop ("gesfromfiprob, fa2pwprob, pw2fiprob and premises2slprob must contain tail, head and p columns")

    BRSstructure <- BRS.str$BRS.structure
    
    BRSstructure %>% setDT
  
    p_gesfromfi <- gesfromfiprob
    p_fa2pw <- fa2pwprob
    p_pw2fi <- pw2fiprob
    p_2sl <- premises2slprob
    
    setDT(p_gesfromfi)
    setDT(p_fa2pw)
    setDT(p_pw2fi)
    setDT(p_2sl)
    
    p_gesfromfi %<>% .[tail %in% premises[["siteID"]] & head %in% premises[["siteID"]]]
    p_fa2pw %<>% .[tail %in% premises[["siteID"]] & head %in% premises[["siteID"]]]
    p_pw2fi %<>% .[tail %in% premises[["siteID"]] & head %in% premises[["siteID"]]]

    # initialize farm list
    premises %>% setDT 
    npremises <- premises[,.N]
    message(npremises, " premises are being processed")

    premises[["type"]] %<>% factor(., levels = c("SL","NU","MU","FF","FA","PW","FI","FPW","PWF"))
    premises %<>% .[order(type)]

    # Create numerical IDs better handled by C/C++
    premises[,id:=seq(.N)]

    # For each origin premise,
    # select the destination premises with the highest probability of contact and
    # a cumulative probability explaining of 'ProbContactThreshold' % of contacts.
    
    recalc <- function(x) {
           values <- sum(x)
           x / values
    }

    limit.order.Dest <- function(prob.df, focusFarm = "tail", maxNContacts){
      if(focusFarm == "tail"){
        prob.df %<>% .[order(tail, p, decreasing = T)]
        prob.df[, p := recalc(p), by = .(tail)]
        prob.df[,cumsum := cumsum(p), by = .(tail)]
        prob.df %<>% .[, .SD[1:maxNContacts], tail]
        }
      if(focusFarm == "head"){
        prob.df %<>% .[order(head, p, decreasing = T)]
        prob.df[, p := recalc(p), by = .(head)]
        prob.df[,cumsum := cumsum(p), by = .(head)]
        prob.df %<>% .[, .SD[1:maxNContacts], head]
        }

      prob.df %<>% .[cumsum < ProbContactThreshold]
      prob.df[, cumsum := NULL]
      return(prob.df)
    }

    p_gesfromfi %<>% limit.order.Dest(., focusFarm = "head", maxNContacts = maxNContactsGES)
    p_fa2pw %<>% limit.order.Dest(., maxNContacts = maxNContactsPW)
    p_pw2fi %<>% limit.order.Dest(., maxNContacts = maxNContactsFI)

    # Transform character IDs into numerical ID within premises. Numerical IDs are better handled by C/C++
    p_fa2pw[, tail := premises[match(tail, siteID),"id"]]
    p_fa2pw[, head := premises[match(head, siteID),"id"]]
    # setnames(p_fa2pw, "p", "topw_prob")
    # setnames(p_fa2pw, "head", "topw_id")
    
    p_pw2fi[, tail := premises[match(tail, siteID),"id"]]
    p_pw2fi[, head := premises[match(head, siteID),"id"]]
    # setnames(p_pw2fi, "p", "tofi_prob")
    # setnames(p_pw2fi, "head", "tofi_id")
    
    p_gesfromfi[, tail := premises[match(tail, siteID),"id"]]
    p_gesfromfi[, head := premises[match(head, siteID),"id"]]
    # setnames(p_gesfromfi, "p", "fromfi_prob")
    # setnames(p_gesfromfi, "tail", "fromfi_id")
    
    p_2sl[, tail := premises[match(tail, siteID),"id"]]
    p_2sl[, head := premises[match(head, siteID),"id"]]
    
      # number of batches
    premises[type != "SL", nbatches := BRSstructure[match(premises[type != "SL", BRS], id.BRS), nbatches]]
    
    premises[type != "SL", `:=`(
      # number individuals in each pen.
      size_ges_pen = ceiling(nbRP/nbatches),
      size_fa_pen  = size_FA_pen,
      size_pw_pen  = size_PW_pen,
      size_fi_pen  = size_FI_pen
      )]

    premises[type != "SL", `:=`(
      # total number of rooms in each sector in that specific farm
      size_ges_sector = ifelse(nbRP > 0, nbatches, 0),
      size_fa_sector = BRSstructure[match(premises[type != "SL", BRS], id.BRS), size_fa_sector],
      size_pw_sector = BRSstructure[match(premises[type != "SL", BRS], id.BRS), size_pw_sector],
      size_fi_sector = BRSstructure[match(premises[type != "SL", BRS], id.BRS), size_fi_sector]
    )]

    premises[type != "SL", `:=`(
      # number of pens in each room in that specific farm
      size_ges_room = ifelse(nbRP > 0, 1, 0),
      size_fa_room  = nbRP %>% divide_by(nbatches) %>% ceiling,
      size_pw_room  = nbPW %>% divide_by(size_pw_sector) %>% divide_by(size_PW_pen)  %>% ceiling,
      size_fi_room  = nbFat %>% divide_by(size_fi_sector) %>% divide_by(size_FI_pen) %>% ceiling
    )]

    premises[type != "SL", `:=`(
      # duration to wait before taking the next action in a fill pen.
      duration_ges = BRSstructure[match(premises[type != "SL", BRS], id.BRS), duration_ges],
      duration_pre_birth = BRSstructure[match(premises[type != "SL", BRS], id.BRS), duration_pre_birth],
      duration_fa = BRSstructure[match(premises[type != "SL", BRS], id.BRS), duration_fa],
      duration_pw = BRSstructure[match(premises[type != "SL", BRS], id.BRS), duration_pw],
      duration_fi = BRSstructure[match(premises[type != "SL", BRS], id.BRS), duration_fi]
    )]

    premises[type != "SL", `:=`(
      # duration to wait before taking the next action in an empty pen.
      duration_post_ges = BRSstructure[match(premises[type != "SL", BRS], id.BRS), duration_post_ges],
      duration_post_fa  = BRSstructure[match(premises[type != "SL", BRS], id.BRS), duration_post_fa],
      duration_post_pw  = BRSstructure[match(premises[type != "SL", BRS], id.BRS), duration_post_pw],
      duration_post_fi  = BRSstructure[match(premises[type != "SL", BRS], id.BRS), duration_post_fi]
    )]

    premises[type != "SL", `:=`(
      # # Faeces removal rate by cleaning, we assume a better cleaning rate based on time
      cleaning_rate_ges = ifelse(duration_post_ges > 1, cleaning_rates["high"], cleaning_rates["low"]),
      cleaning_rate_fa  = ifelse(duration_post_fa > 1, cleaning_rates["high"], cleaning_rates["low"]),
      cleaning_rate_pw  = ifelse(duration_post_pw > 1, cleaning_rates["high"], cleaning_rates["low"]),
      cleaning_rate_fi  = ifelse(duration_post_fi > 1, cleaning_rates["high"], cleaning_rates["low"])
    )]

    ########################## 
    ## External biosecurity ##
    ##########################
    
    premises[type != "SL",  NGES := sum(size_ges_pen * size_ges_room * size_ges_sector), by = .(extBioSecLev_ges_sector)]
    premises[type != "SL",  NFA := sum(size_fa_pen * size_fa_room * size_fa_sector), by = .(extBioSecLev_fa_sector)]
    premises[type != "SL",  NPW := sum(size_pw_pen * size_pw_room * size_pw_sector), by = .(extBioSecLev_pw_sector)]
    premises[type != "SL",  NFI := sum(size_fi_pen * size_fi_room * size_fi_sector), by = .(extBioSecLev_fi_sector)]
    
    premises[type != "SL", `:=`(
      # External introduction rates based on levels of external biosecurity
      ext_BioSec_ges_sector = ext_BioSec_frequency[extBioSecLev_ges_sector]/ NGES / (qIng * beta_EWP_ges_sector),
      ext_BioSec_fa_sector = ext_BioSec_frequency[extBioSecLev_fa_sector]/ NFA / (qIng * beta_EWP_ges_sector),
      ext_BioSec_pw_sector = ext_BioSec_frequency[extBioSecLev_pw_sector]/ NPW / (qIng * beta_EWP_ges_sector),
      ext_BioSec_fi_sector = ext_BioSec_frequency[extBioSecLev_fi_sector]/ NFI / (qIng * beta_EWP_ges_sector)
    )]
    
    setkey(premises, "id")
    setkey(p_fa2pw, "tail")
    setkey(p_pw2fi, "tail")
    setkey(p_gesfromfi, "head")
    setkey(p_2sl, "tail")

    # FIX ME: requires too much memory
    # premises %<>% merge(., p_fa2pw, by.x = "id", by.y= "tail", all = TRUE)
    # premises %<>% merge(., p_pw2fi, by.x = "id", by.y= "tail", all = TRUE)
    # premises %<>% merge(., p_gesfromfi, by.x = "id", by.y= "head", all = TRUE)
    # premises %<>% merge(., p_2sl, by.x = "id", by.y= "tail", all = TRUE)

    # initialize farm list
    empty_list <- split(premises, by="id", keep.by=TRUE)

    
    # fill farm list with basic features
    if(withnames){ 
      farmlist <- lapply(empty_list, function(x){

      # probs <- x[, c("topw_id", "topw_prob", "tofi_id", "tofi_prob", "fromfi_id", "fromfi_prob", "tosl_id", "tosl_prob")]
      # x[, `:=`(
      #   topw_id = NULL,
      #   topw_prob = NULL,
      #   tofi_id = NULL,
      #   tofi_prob = NULL,
      #   fromfi_id = NULL,
      #   fromfi_prob = NULL,
      #   tosl_id = NULL,
      #   tosl_prob = NULL)]

      x %<>% unique


      # If Slautherhouse
      if(x$type == "SL"){
        farm <- list(
          farm_name = x$siteID,
          farm_id =x$id,
          farm_type = x$type %>% as.character
        )
        
        # class for SimInf nice printing
        class(farm) <- "slaughterhouse"
        }

      # If Farm
      if(x$type  != "SL"){
        farm <- list(
          farm_name = x$siteID,
          farm_id   = x$id,
          farm_type = x$type %>% as.character,
          farm_BRS  = x$BRS,
          renewalrate  = x$renewal,
          ext_BioSec_ges_sector = x$ext_BioSec_ges_sector,
          ext_BioSec_fa_sector =  x$ext_BioSec_fa_sector,
          ext_BioSec_pw_sector =  x$ext_BioSec_pw_sector,
          ext_BioSec_fi_sector =  x$ext_BioSec_fi_sector,
          # direct transmission rates (pigs/day) in each sectors (currently 0.7)
          beta_D_ges_sector = x$beta_D_ges_sector ,
          beta_D_fa_sector  = x$beta_D_fa_sector ,
          beta_D_pw_sector  = x$beta_D_pw_sector ,
          beta_D_fi_sector  = x$beta_D_fi_sector ,
          # within-pen environmental transmission rate (g/ge/day) in each sectors (currently 0.7)
          beta_EWP_ges_sector = x$beta_EWP_ges_sector ,
          beta_EWP_fa_sector  = x$beta_EWP_fa_sector ,
          beta_EWP_pw_sector  = x$beta_EWP_pw_sector ,
          beta_EWP_fi_sector  = x$beta_EWP_fi_sector ,
          # between adjacent pen environmental transmission rate (g/ge/day) in each sectors (currently 0.7)
          beta_EBAP_ges_sector = x$beta_EBAP_ges_sector ,
          beta_EBAP_fa_sector  = x$beta_EBAP_fa_sector ,
          beta_EBAP_pw_sector  = x$beta_EBAP_pw_sector ,
          beta_EBAP_fi_sector  = x$beta_EBAP_fi_sector ,
          # HEV Latency duration (days)
          Dlat = x$Dlat , #13.1,
          # HEV Infectious period (days)
          Dinf = x$Dinf , #48.6,
          # Quantity of HEV particles shed in faeces (ge/g/day)
          shedrate = x$shedrate , #10^6,
          # number of animals in each sector in that specific farm
          n_sows_farm = x$nbRP ,
          n_pw_farm = x$nbPW ,
          n_fi_farm = x$nbFat ,
          # total number of rooms in each sector in that specific farm
          size_ges_sector = x$size_ges_sector ,
          size_fa_sector  = x$size_fa_sector ,
          size_pw_sector  = x$size_pw_sector ,
          size_fi_sector  = x$size_fi_sector ,
          # number of pens in each room in that specific farm
          size_ges_room = x$size_ges_room ,
          size_fa_room  = x$size_fa_room ,
          size_pw_room  = x$size_pw_room ,
          size_fi_room  = x$size_fi_room ,
          # number individuals in each pen.
          size_ges_pen = x$size_ges_pen ,
          size_fa_pen  = x$size_fa_pen ,
          size_pw_pen  = x$size_pw_pen ,
          size_fi_pen  = x$size_fi_pen ,
          #  interval between successive batches
          interval = BRS.str$batches.intervals %>% .[[as.character(x$BRS)]],
          # duration to wait before taking the next action in a fill pen.
          duration_ges = x$duration_ges ,
          duration_pre_birth = x$duration_pre_birth ,
          duration_fa = x$duration_fa ,
          duration_pw = x$duration_pw ,
          duration_fi = x$duration_fi ,
          # duration to wait before taking the next action in an empty pen.
          duration_post_ges = x$duration_post_ges ,
          duration_post_fa  = x$duration_post_fa ,
          duration_post_pw  = x$duration_post_pw ,
          duration_post_fi  = x$duration_post_fi ,
          # Faeces removal rate by cleaning, we assume a better cleaning rate based on time
          cleaning_rate_ges = x$cleaning_rate_ges ,
          cleaning_rate_fa  = x$cleaning_rate_fa ,
          cleaning_rate_pw  = x$cleaning_rate_pw ,
          cleaning_rate_fi  = x$cleaning_rate_fi ,
          # Contact ids and associated probabilities
          # FIX ME -- add probabilities only for relevant premises type -- maybe create an independant function to clarify the script
          # FIX ME -- following lines are very slow
          topw_id = p_fa2pw[tail==x$id,head],
          topw_prob = p_fa2pw[tail==x$id,p],
          tofi_id = p_pw2fi[tail==x$id,head],
          tofi_prob = p_pw2fi[tail==x$id,p],
          fromfi_id = p_gesfromfi[head==x$id,tail],
          fromfi_prob = p_gesfromfi[head==x$id,p],
          tosl_id = p_2sl[tail==x$id,head],
          tosl_prob = p_2sl[tail==x$id,p]
        )
      
      # class for SimInf nice printing
      class(farm) <- "farm"}
      
      farm
      
    } )
      }else{
      farmlist <- lapply(empty_list, function(x){
        
        # probs <- x[, c("topw_id", "topw_prob", "tofi_id", "tofi_prob", "fromfi_id", "fromfi_prob", "tosl_id", "tosl_prob")]
        # x[, `:=`(
        #   topw_id = NULL,
        #   topw_prob = NULL,
        #   tofi_id = NULL,
        #   tofi_prob = NULL,
        #   fromfi_id = NULL,
        #   fromfi_prob = NULL,
        #   tosl_id = NULL,
        #   tosl_prob = NULL)]
        
        x %<>% unique
        
        
        # If Slautherhouse
        if(x$type == "SL"){
          farm <- list(
            # farm_name = x$siteID,
            farm_id =x$id,
            farm_type = x$type %>% as.character
          )
          
          # class for SimInf nice printing
          class(farm) <- "slaughterhouse"
        }
        
        # If Farm
        if(x$type  != "SL"){
          farm <- list(
            # farm_name = x$siteID,
            farm_id   = x$id,
            farm_type = x$type %>% as.character,
            farm_BRS  = x$BRS,
            renewalrate  = x$renewal,
            ext_BioSec_ges_sector = x$ext_BioSec_ges_sector,
            ext_BioSec_fa_sector =  x$ext_BioSec_fa_sector,
            ext_BioSec_pw_sector =  x$ext_BioSec_pw_sector,
            ext_BioSec_fi_sector =  x$ext_BioSec_fi_sector,
            # direct transmission rates (pigs/day) in each sectors (currently 0.7)
            beta_D_ges_sector = x$beta_D_ges_sector ,
            beta_D_fa_sector  = x$beta_D_fa_sector ,
            beta_D_pw_sector  = x$beta_D_pw_sector ,
            beta_D_fi_sector  = x$beta_D_fi_sector ,
            # within-pen environmental transmission rate (g/ge/day) in each sectors (currently 0.7)
            beta_EWP_ges_sector = x$beta_EWP_ges_sector ,
            beta_EWP_fa_sector  = x$beta_EWP_fa_sector ,
            beta_EWP_pw_sector  = x$beta_EWP_pw_sector ,
            beta_EWP_fi_sector  = x$beta_EWP_fi_sector ,
            # between adjacent pen environmental transmission rate (g/ge/day) in each sectors (currently 0.7)
            beta_EBAP_ges_sector = x$beta_EBAP_ges_sector ,
            beta_EBAP_fa_sector  = x$beta_EBAP_fa_sector ,
            beta_EBAP_pw_sector  = x$beta_EBAP_pw_sector ,
            beta_EBAP_fi_sector  = x$beta_EBAP_fi_sector ,
            # HEV Latency duration (days)
            Dlat = x$Dlat , #13.1,
            # HEV Infectious period (days)
            Dinf = x$Dinf , #48.6,
            # Quantity of HEV particles shed in faeces (ge/g/day)
            shedrate = x$shedrate , #10^6,
            # number of animals in each sector in that specific farm
            n_sows_farm = x$nbRP ,
            n_pw_farm = x$nbPW ,
            n_fi_farm = x$nbFat ,
            # total number of rooms in each sector in that specific farm
            size_ges_sector = x$size_ges_sector ,
            size_fa_sector  = x$size_fa_sector ,
            size_pw_sector  = x$size_pw_sector ,
            size_fi_sector  = x$size_fi_sector ,
            # number of pens in each room in that specific farm
            size_ges_room = x$size_ges_room ,
            size_fa_room  = x$size_fa_room ,
            size_pw_room  = x$size_pw_room ,
            size_fi_room  = x$size_fi_room ,
            # number individuals in each pen.
            size_ges_pen = x$size_ges_pen ,
            size_fa_pen  = x$size_fa_pen ,
            size_pw_pen  = x$size_pw_pen ,
            size_fi_pen  = x$size_fi_pen ,
            #  interval between successive batches
            interval = BRS.str$batches.intervals %>% .[[as.character(x$BRS)]],
            # duration to wait before taking the next action in a fill pen.
            duration_ges = x$duration_ges ,
            duration_pre_birth = x$duration_pre_birth ,
            duration_fa = x$duration_fa ,
            duration_pw = x$duration_pw ,
            duration_fi = x$duration_fi ,
            # duration to wait before taking the next action in an empty pen.
            duration_post_ges = x$duration_post_ges ,
            duration_post_fa  = x$duration_post_fa ,
            duration_post_pw  = x$duration_post_pw ,
            duration_post_fi  = x$duration_post_fi ,
            # Faeces removal rate by cleaning, we assume a better cleaning rate based on time
            cleaning_rate_ges = x$cleaning_rate_ges ,
            cleaning_rate_fa  = x$cleaning_rate_fa ,
            cleaning_rate_pw  = x$cleaning_rate_pw ,
            cleaning_rate_fi  = x$cleaning_rate_fi ,
            # Contact ids and associated probabilities
            # FIX ME -- add probabilities only for relevant premises type -- maybe create an independant function to clarify the script
            # FIX ME -- following lines are very slow
            topw_id = p_fa2pw[tail==x$id,head],
            topw_prob = p_fa2pw[tail==x$id,p],
            tofi_id = p_pw2fi[tail==x$id,head],
            tofi_prob = p_pw2fi[tail==x$id,p],
            fromfi_id = p_gesfromfi[head==x$id,tail],
            fromfi_prob = p_gesfromfi[head==x$id,p],
            tosl_id = p_2sl[tail==x$id,head],
            tosl_prob = p_2sl[tail==x$id,p]
          )
          
          # class for SimInf nice printing
          class(farm) <- "farm"}
        
        farm
      })
      }
    
    
    cat("##############\n",
        farmlist %>% lapply(., function(x) x$farm_type == "SL") %>% unlist %>% sum, " slaughterhouse(s), ",
        farmlist %>% lapply(., function(x) x$farm_type != "SL") %>% unlist %>% sum, " farm(s) \n \n",
        farmlist %>% lapply(., function(x) x$farm_type == "MU") %>% unlist %>% sum, " multiplier(s),  \n",
        farmlist %>% lapply(., function(x) x$farm_type == "NU") %>% unlist %>% sum, " nucleus,  \n",
        farmlist %>% lapply(., function(x) x$farm_type == "FF") %>% unlist %>% sum, " farrow-to-finish, \n",
        farmlist %>% lapply(., function(x) x$farm_type == "FA") %>% unlist %>% sum, " farrower(s),  \n",
        farmlist %>% lapply(., function(x) x$farm_type == "PW") %>% unlist %>% sum, " post-weaner(s),  \n",
        farmlist %>% lapply(., function(x) x$farm_type == "FI") %>% unlist %>% sum, " finisher(s), \n",
        farmlist %>% lapply(., function(x) x$farm_type == "PWF") %>% unlist %>% sum, "post-weaners-finisher(s), \n",
        farmlist %>% lapply(., function(x) x$farm_type == "FPW") %>% unlist %>% sum, "farrowers-post-weaner(s) \n##############\n")

    farmlist
  }

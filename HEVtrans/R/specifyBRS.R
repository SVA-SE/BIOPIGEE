#' Characterize the batch rearing systems
#'
#' @description \code{specifyBRS} FIX ME
#' @usage FIX ME
#'
#' @param id.BRS FIX ME
#' @param nbatches FIX ME
#' @param size_ges_sector FIX ME
#' @param size_fa_sector FIX ME
#' @param size_pw_sector FIX ME
#' @param size_fi_sector FIX ME
#' @param duration_ges FIX ME
#' @param duration_pre_birth  FIX ME
#' @param duration_fa  FIX ME
#' @param duration_pw FIX ME
#' @param duration_fi FIX ME
#' @param duration_post_ges FIX ME
#' @param duration_post_fa FIX ME
#' @param duration_post_pw FIX ME
#' @param duration_post_fi FIX ME
#' @param batches.intervals FIX ME
#'
#' @return FIX ME
#' @examples 
#' BRS.str <- specifyBRS()
#'
#' @import DiagrammeR
#' @import DiagrammeRsvg
#' @import rsvg
#' 
#' @export
#' 
specifyBRS <- function(id.BRS = c(4,5,7,10,20),
                       nbatches = c(4,5,7,10,20),
                       size_ges_sector =  1,
                       size_fa_sector = c(1,1,2,3,4),
                       size_pw_sector = c(2,2,3,4,7),
                       size_fi_sector =  c(3,2,5,7,18),
                       duration_ges = 32+82,
                       duration_pre_birth = 7,
                       duration_fa = c(21,21,28,28,21),
                       duration_pw= c(67,53,59,53,46),
                       duration_fi= c(101,108,102,94,119),
                       duration_post_ges = 0,
                       duration_post_fa = c(7,0,7,7,0),
                       duration_post_pw = c(3,3,4,3,3),
                       duration_post_fi = c(4,4,3,4,4),
                       batches.intervals = list(rep(35,4), rep(28,5), rep(21,7), c(rep(14,9), 21), rep(7,20)),verbose = FALSE){

  BRS.charac <- data.frame(id.BRS = id.BRS %>% as.character,
                           nbatches = nbatches %>% as.character %>% as.numeric,
                           size_ges_sector = size_ges_sector,
                           size_fa_sector = size_fa_sector,
                           size_pw_sector = size_pw_sector,
                           size_fi_sector = size_fi_sector,
                           duration_ges = duration_ges ,
                           duration_pre_birth = duration_pre_birth,
                           duration_fa = duration_fa,
                           duration_pw = duration_pw,
                           duration_fi = duration_fi,
                           duration_post_ges = duration_post_ges,
                           duration_post_fa = duration_post_fa,
                           duration_post_pw = duration_post_pw,
                           duration_post_fi = duration_post_fi)

  names(batches.intervals) <- BRS.charac$id.BRS
  
  nsectors <- 4
  
  graph <- sapply(BRS.charac %>% nrow %>% seq,function(i){
    nodes <- stringi::stri_rand_strings(nsectors, 6, "[A-Z]")
    paste0(c("subgraph cluster_",i - 1," {
        graph[shape = rectangle]
        style = rounded
        bgcolor = Gold

        label = 'BRS:",BRS.charac[i,"id.BRS"],"'
        node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25] \n",
             nodes[1],"[label = 'Gestation\n",BRS.charac[i,"size_ges_sector"]," rooms\n dur: ",BRS.charac[i,"duration_ges"]," days'] \n",
             nodes[2],"[label = 'Farrowing\n",BRS.charac[i,"size_fa_sector"]," rooms\n dur: ",BRS.charac[i,"duration_pre_birth"]," + ",BRS.charac[i,"duration_fa"]," days'] \n",
             nodes[3],"[label = 'Post-Weaning\n",BRS.charac[i,"size_pw_sector"]," rooms\n dur: ",BRS.charac[i,"duration_pw"]," days'] \n",
             nodes[4],"[label = 'Fininshing\n",BRS.charac[i,"size_fi_sector"]," rooms\n dur: ",BRS.charac[i,"duration_fi"]," days']
      }
      edge[color = orange, arrowhead = vee, arrowsize = .9] \n ",
             nodes[1]," -> ",nodes[1]," [label=\"",BRS.charac[i,"duration_post_ges"]," days \n btw batches\"]\n ",
             nodes[2]," -> ",nodes[2]," [label=\"",BRS.charac[i,"duration_post_fa"]," days \n btw batches\"]\n ",
             nodes[3]," -> ",nodes[3]," [label=\"",BRS.charac[i,"duration_post_pw"]," days \n btw batches\"]\n ",
             nodes[4]," -> ",nodes[4]," [label=\"",BRS.charac[i,"duration_post_fi"]," days \n btw batches\"]\n ",
             "
      edge[color = black, arrowhead = vee, arrowsize = .95] \n ",
             nodes[1]," -> ",nodes[2]," [label=\" RP sows\" dir=\"both\"]\n ",
             nodes[2]," -> ",nodes[3]," [label=\" piglets\"]\n ",
             nodes[3]," -> ",nodes[4]," [label=\" growing pigs\"]\n ",
             nodes[4]," -> Slaughterhouse \n "), collapse="")
  })
  
  graph %<>% unlist %>% paste(., collapse= "\n ") %>% c("digraph{
      graph[rankdir = TB]
      node[shape = rectangle, style = filled]",., "}") %>% unlist %>% paste(., collapse= "\n ") %>% grViz
  
  if(verbose) print(graph)
  
  return(list(BRS.structure = BRS.charac, batches.intervals = batches.intervals, graph = graph))
}




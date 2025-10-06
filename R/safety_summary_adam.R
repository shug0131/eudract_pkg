#' Calculate frequency tables based on ADaM data.
#'
#' @param adsl ADaM Subject-level analysis data set
#' @param adae ADaM Adverse Event data set
#' @inheritParams safety_summary
#' @param related_terms The set of terms used in adae$AEREL to identify related events.
#'
#' @returns a list of three data frames: GROUP, SERIOUS, NON_SERIOUS. Each contains the summary statistics required by EudraCT, and is suitable for export.
#'
#' @seealso \code{\link{safety_summary }} \code{\link{eudract_convert}} \code{\link{simple_safety_xml}}
#'
#'
#' @export
#' @importFrom dplyr group_by summarise left_join mutate select rename ungroup filter %>%
#' @importFrom stats na.fail
#' @examples
#' 
#' safety_summary_adam(
#'  pharmaverseadam::adsl |> dplyr::filter(ARM != "Screen Failure"), 
#'  pharmaverseadam::adae)
#' 
#' 
safety_summary_adam <- function(adsl, adae, freq_threshold = 0,
                                na.action = na.fail, 
                                related_terms = c("POSSIBLE","PROBABLE","DEFINITELY","Y")
                                ){
  
  #table of exposed
  exposed <- table(adsl$ARM) |> as.vector()
  names(exposed) <- table(adsl$ARM) |> names()
  
  # count excess deaths
  all_death <- adsl |> group_by(ARM) |> 
    summarise(all_deaths = sum(DTHFL == "Y", na.rm = TRUE), .groups = "drop")
  ae_death <- adae |> group_by(SUBJID, ARM) |> 
    summarise( deaths = any(AESDTH == "Y", na.rm  = TRUE), .groups = "drop") |> 
    group_by(ARM) |> 
    summarise( ae_deaths = sum(deaths), .groups = "drop")
  deaths <- left_join(all_death, ae_death, by = "ARM") |> 
    mutate(excess = all_deaths - ae_deaths)
  excess <- deaths$excess
  names(excess) <- deaths$ARM
  
  
  #reformat the adverse event data
  
  ae_df <- adae |> 
    select(SUBJID, AEDECOD, AEREL, AEBODSYS, AEOUT, AESER, ARM, AESDTH) |> 
    mutate(related = AEREL %in% related_terms,
           fatal = AESDTH == "Y",
           serious = AESER == "Y",
           term = AEDECOD,
           soc = soc_string_format(AEBODSYS),
           pt = AEDECOD, 
           subjid = SUBJID,
           group = ARM
    ) |> 
    select(pt, subjid, related, soc, fatal, serious, group, term)
  
  
  safety_summary(ae_df, exposed = exposed, excess_deaths = excess,
                 soc_index = "soc_term", freq_threshold = freq_threshold, 
                 na.action = na.action
  )
}



soc_string_format <- function(x){
  x <- tolower(x)
  first <- substr(x,1,1) |> toupper()
  paste0(first, substr(x,2,1000))
}



if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("AEBODSYS",  "AEDECOD", "AEOUT", "AEREL", "AESDTH", "AESER", "ARM", 
      "DTHFL", "SUBJID", "ae_deaths", "all_deaths", "pt", "serious"
    ))
}

 


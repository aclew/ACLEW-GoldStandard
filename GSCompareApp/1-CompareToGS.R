# rm(list = ls())
library(tidyverse)
library(gtools)
source("0-Helper_CompareToGS.R")

compare.files <- function(nw.filename, recording, native,
                          minute, evaltype, coder, lab) {
  ################################################################################
  # Set up
  ################################################################################
  
  # nw.filename <- "VanDamFJ11-GS_Training_Round_1-TS.txt"
  # recording <- "VanFJ11"
  # native <- "Yes"
  # minute <- 1
  # evaltype <- "Last-chance"
  # coder <- "MC"
  # lab <- "MC"
  
  # Input files
  nw.file <- read.annot(nw.filename)
  nw.file$code[which(is.na(nw.file$code))] <- "<empty>"
  gs.file <- read.annot(paste0(recording, "-0GS0.txt"))
  ntvness <- ifelse(native == "Yes", "native", "NON-native")
  
  # Input arguments
  slice_sz <- 50 # size of time slices compared
  strict <- ifelse(evaltype == "Normal", 1, 0)
  
  if (strict == 1) {
  compare.stmt <- paste0("Comparing minute ", minute, " of recording ",
                        recording, " to the gold standard.")
  } else {
  compare.stmt <- paste0("Comparing minute ", minute, " of recording ",
                        recording, " to the gold standard using LAST-CHANCE mode.")
  }
  coder.stmt <- paste0("Submitted by coder ", coder, " from the ", lab,
                       " lab, who is a ", ntvness,
                       " speaker of the language in the recording.")
  
  # Currently set up so we could have different score minima for
  # normal vs. last-chance mode and their use with native vs. non-native
  # coders
  if (strict == 1) {
    min_overall_score <- 0.95 # minimum overall weighted score
    min_score_univ <- 0.85 # minumum score allowed on diarization and vcm
    if (native == "Yes") {
      min_score_lgsp <- 0.85 # minumum score allowed on lex, mwu, and xds
    } else {
      min_score_lgsp <- 0.75 # minumum score allowed on lex, mwu, and xds
    }
  } else {
    min_overall_score <- 0.95 # minimum overall weighted score
    min_score_univ <- 0.85 # minumum score allowed on diarization and vcm
    if (native == "Yes") {
      min_score_lgsp <- 0.85 # minumum score allowed on lex, mwu, and xds
    } else {
      min_score_lgsp <- 0.75 # minumum score allowed on lex, mwu, and xds
    }
  }
  

  ################################################################################
  # Run comparison
  ################################################################################
  # Determine the onset and offset times of the segment to be compared
  # if minute is NA then the whole file is compared
  seg_stt <- (minute-1)*60000
  seg_end <- minute*60000
  
  ### SPECIAL CASE: Last-chance mode ####
  if (strict == 0) {
    # Collapse MA tiers, FA tiers, and all C tiers except CHI (lena-like): 
    gs.file.lna <- collapse.tiers.lena(gs.file, seg_stt, seg_end)
    nw.file.lna <- collapse.tiers.lena(nw.file, seg_stt, seg_end)
    
    # Run the normal tier comparison, but allowing for partial matches
    gs.speakers <- gs.file.lna %>%
      filter(stop > seg_stt & start < seg_end & !is.na(speaker)) %>%
      distinct(speaker)
    nw.speakers <- nw.file.lna %>%
      filter(stop > seg_stt & start < seg_end & !is.na(speaker)) %>%
      distinct(speaker)
    nw.orig.speakers <- nw.file %>%
      filter(stop > seg_stt & start < seg_end & !is.na(speaker)) %>%
      distinct(speaker)
    
    # Create output tables
    # Tier equivalence
    tier.equiv <- tibble(
      gs.spkr = gs.speakers$speaker,
      your.spkr = ""
    )
    # Score summary
    gs.tiers <- gs.file.lna %>%
      filter(stop > seg_stt & start < seg_end & (!is.na(speaker))) %>%
      select(tier, speaker) %>% distinct() %>% arrange(speaker) %>%
      mutate(slice_match = "", n_annots = "", sec_annots = "",
             slice_match_n = 0, nsec_spch = 0)
    # Errors
    errors.tbl <- tibble()
  
    # CHI is always matched with CHI
    tier.equiv$your.spkr[which(tier.equiv$gs.spkr == "CHI")] <- "CHI"
    
    # The others are matched to their collapsed original tiers
    nonchi.tiers <- c("FA1", "MA1", "UC1")
    nonchi.ptns <- c('^FA', '^MA', '^[MFU]C')
    for (j in 1:length(nonchi.tiers)) {
      if (nonchi.tiers[j] %in% tier.equiv$gs.spkr) {
        tier.equiv$your.spkr[which(tier.equiv$gs.spkr == nonchi.tiers[j])] <-
          paste(nw.orig.speakers$speaker[which(grepl(nonchi.ptns[j], nw.orig.speakers$speaker))], collapse = " and ")
      }
    }
    tier.equiv$your.spkr[which(tier.equiv$your.spkr == "")] <- "<no match>"

    # Fill in report values
    for (tiertype in gs.tiers$tier) {
      tierspkr <- tiertype
      if ((grepl("@", tiertype))) {
        tierspkr <- substr(tiertype, 5, 7)
      }
      gs.row <- which(gs.tiers$tier == tiertype)
      if (tier.equiv$your.spkr[which(
        tier.equiv$gs.spkr == tierspkr)] == "<no match>") {
        if (!(grepl("@", tiertype))) {
          gs.tiers$n_annots[gs.row] <- "MISSING"
          gs.tiers$sec_annots[gs.row] <- "MISSING"
        }
        gs.tiers$slice_match[gs.row] <- "0%"
        gs.tiers$slice_match_n[gs.row] <- 0
        gs.tiers$nsec_spch[gs.row] <- round(sum(segA$duration)/1000,2)
      } else {
        # Fill in n_annots and sec_annots values
        segA <- gs.file.lna %>%
          filter(tier == tiertype & stop > seg_stt & start < seg_end) %>%
          mutate(coder = "A")
        segB <- nw.file.lna %>%
          filter(tier == tiertype & stop > seg_stt & start < seg_end) %>%
          mutate(coder = "B")
        if (!(grepl("@", tiertype))) {
          gs.tiers$n_annots[gs.row] <-
            paste("∆ = ",(nrow(segB)-nrow(segA)),
                " (GS:",nrow(segA),", You:",nrow(segB),")", sep="")
          gs.tiers$sec_annots[gs.row] <-
            paste("∆ = ",round((sum(segB$duration)/1000-sum(segA$duration)/1000),2),
                " (GS:",round(sum(segA$duration)/1000,2),", You:",
                round(sum(segB$duration)/1000,2),")", sep="")
        }
        gs.tiers$nsec_spch[gs.row] <- round(sum(segA$duration)/1000,2)
        # Fill in the slice_match value
        comparison.tbl <- intersect.tiers.multi(gs.file.lna, nw.file.lna, tiertype,
                                                seg_stt, seg_end, slice_sz)
        if (nrow(comparison.tbl) == 0) {
          gs.tiers$slice_match[gs.row] <- "100%"
          gs.tiers$slice_match_n[gs.row] <- 1
          gs.tiers$n_annots[gs.row] <- "No non-'U' utts"
        } else {
          gs.tiers$slice_match[gs.row] <-
            paste(round(mean(comparison.tbl$match)*100, 2),"%", sep="")
          gs.tiers$slice_match_n[gs.row] <- mean(comparison.tbl$match)
          # Add slice match errors to the reporting table
          errors.tbl <- bind_rows(errors.tbl,
                                  lapply(subset(comparison.tbl, match == 0),
                                         as.character))
        }
      }
    }
  }
  
  ### TYPICAL CASE: Last-chance mode ####
  if (strict == 1) {
    # Match up the nw file tiers to the gold standard as closely as possible
    gs.speakers <- gs.file %>%
      filter(stop > seg_stt & start < seg_end & !is.na(speaker)) %>%
      distinct(speaker)
    nw.speakers <- nw.file %>%
      filter(stop > seg_stt & start < seg_end & !is.na(speaker)) %>%
      distinct(speaker)
    
    # Create output tables
    # Tier equivalence
    tier.equiv <- tibble(
      gs.spkr = gs.speakers$speaker,
      your.spkr = ""
    )

    # Score summary
    gs.tiers <- gs.file %>%
      filter(stop > seg_stt & start < seg_end & (!is.na(speaker))) %>%
      select(tier, speaker) %>% distinct() %>% arrange(speaker) %>%
      mutate(slice_match = "", n_annots = "", sec_annots = "",
             slice_match_n = 0, nsec_spch = 0)
    # Errors
    errors.tbl <- tibble()
  
    # CHI is always matched with CHI
    tier.equiv$your.spkr[which(tier.equiv$gs.spkr == "CHI")] <- "CHI"
    
    # The others are matched as a set...
    nonchi.gs.s <- subset(gs.speakers, speaker != "CHI")$speaker
    nonchi.nw.s <- subset(nw.speakers, speaker != "CHI")$speaker
    
    # For files with a lot of tiers to match, it is computationally
    # infeasible to try every matching permutation, so we go for a 
    # more efficient method
    if (length(nonchi.gs.s) > 5) {
      # Start with the coder's most speech-heavy tier and assign it to the GS tier
      # it matches best, then remove those tiers and repeat with the remaining tiers
      # until there are no more coder tiers/GS tiers to pair up
      nchi.spk.summ <- nw.file %>%
        filter(!(grepl('@', tier)) & speaker != "CHI") %>%
        filter(start < seg_end & stop > seg_stt)
      clipped.ends <- which(nchi.spk.summ$stop > seg_end)
      clipped.starts <- which(nchi.spk.summ$start < seg_stt)
      if (length(clipped.ends) > 0) {
        for (idx in clipped.ends) {
          nchi.spk.summ$stop[idx] <- seg_end
          nchi.spk.summ$duration[idx] <- seg_end - nchi.spk.summ$start[idx]
        }
      }
      if (length(clipped.starts) > 0) {
        for (idx in clipped.starts) {
          nchi.spk.summ$start[idx] <- seg_stt # not strictly necessary, but good for sanity checking
          nchi.spk.summ$duration[idx] <- nchi.spk.summ$stop - seg_stt[idx]
        }
      }
      nchi.spk.tots <- nchi.spk.summ %>%
        group_by(tier) %>%
        summarise(tot.ms = sum(duration)) %>%
        arrange(-tot.ms)
      nw.tiers <- nchi.spk.tots$tier
      for (nw.tier in nw.tiers) {
        avail.gs.tiers <- tier.equiv$gs.spkr[which(tier.equiv$your.spkr == "")]
        if (length(avail.gs.tiers > 0)) {
          top.score <- 0
          top.tier <- ""
          for (gs.tier in avail.gs.tiers) {
            match.mean <- mean(intersect.spk.tiers(gs.file, nw.file,
                                                   gs.tier, nw.tier,
                                                   seg_stt, seg_end,
                                                   strict, slice_sz)$match)    
            if (match.mean > top.score) {
              top.score <- match.mean
              top.tier <- gs.tier
            }
          }
          tier.equiv$your.spkr[which(tier.equiv$gs.spkr == top.tier)] <- nw.tier
        }
      }
      tier.equiv$your.spkr[which(tier.equiv$your.spkr == "")] <- "<no match>"
      
      # Internally rename in nw.speakers/remove non-matched tiers
      # Careful not to overwrite names/collapse speakers in the process!
      nw.file.temp <- nw.file %>%
        filter(speaker %in% tier.equiv$your.spkr) %>%
        mutate(speaker2 = speaker)
      for (row in 1:nrow(tier.equiv)) {
        if (tier.equiv$gs.spkr[row] != tier.equiv$your.spkr[row]) {
          toChange <- which(nw.file.temp$speaker2 ==
                              tier.equiv$your.spkr[row])
          nw.file.temp$speaker[toChange] <- tier.equiv$gs.spkr[row]
          nw.file.temp$tier[toChange] <- gsub(tier.equiv$your.spkr[row],
                           tier.equiv$gs.spkr[row], nw.file.temp$tier[toChange])
        }
      }
      nw.file.temp$speaker2 <- NULL
      
      # Set up table for tier comparison and error-reporting
      errors.tbl.temp <- errors.tbl
      gs.tiers.temp <- gs.file %>%
        filter(stop > seg_stt & start < seg_end & (!is.na(speaker))) %>%
        select(tier, speaker) %>% distinct() %>% arrange(speaker) %>%
        mutate(slice_match = "", n_annots = "", sec_annots = "",
               slice_match_n = 0, nsec_spch = 0)
    
      # Fill in report values
      for (tiertype in gs.tiers.temp$tier) {
        tierspkr <- tiertype
        if ((grepl("@", tiertype))) {
          tierspkr <- substr(tiertype, 5, 7)
        }
        gs.row <- which(gs.tiers.temp$tier == tiertype)
        if (tier.equiv$your.spkr[which(
          tier.equiv$gs.spkr == tierspkr)] == "<no match>") {
          if (!(grepl("@", tiertype))) {
            gs.tiers.temp$n_annots[gs.row] <- "MISSING"
            gs.tiers.temp$sec_annots[gs.row] <- "MISSING"
          }
          gs.tiers.temp$slice_match[gs.row] <- "0%"
          gs.tiers.temp$slice_match_n[gs.row] <- 0
          gs.tiers.temp$nsec_spch[gs.row] <- round(sum(segA$duration)/1000,2)
        } else {
          # Fill in n_annots and sec_annots values
          segA <- gs.file %>%
            filter(tier == tiertype & stop > seg_stt & start < seg_end) %>%
            mutate(coder = "A")
          segB <- nw.file.temp %>%
            filter(tier == tiertype & stop > seg_stt & start < seg_end) %>%
            mutate(coder = "B")
          if (!(grepl("@", tiertype))) {
            gs.tiers.temp$n_annots[gs.row] <-
              paste("∆ = ",(nrow(segB)-nrow(segA)),
                  " (GS:",nrow(segA),", You:",nrow(segB),")", sep="")
            gs.tiers.temp$sec_annots[gs.row] <-
              paste("∆ = ",round((sum(segB$duration)/1000-sum(segA$duration)/1000),2),
                  " (GS:",round(sum(segA$duration)/1000,2),", You:",
                  round(sum(segB$duration)/1000,2),")", sep="")
          }
          gs.tiers.temp$nsec_spch[gs.row] <- round(sum(segA$duration)/1000,2)
          # Fill in the slice_match value
          comparison.tbl <- intersect.tiers(gs.file, nw.file.temp,
                                            tiertype, seg_stt, seg_end, strict, slice_sz)
          if (nrow(comparison.tbl) == 0) {
            gs.tiers.temp$slice_match[gs.row] <- "0%"
            gs.tiers.temp$slice_match_n[gs.row] <- 0
          } else {
            gs.tiers.temp$slice_match[gs.row] <-
              paste(round(mean(comparison.tbl$match)*100, 2),"%", sep="")
            gs.tiers.temp$slice_match_n[gs.row] <- mean(comparison.tbl$match)
            # Add slice match errors to the reporting table
            errors.tbl.temp <- bind_rows(errors.tbl.temp,
                                    lapply(subset(comparison.tbl, match == 0),
                                           as.character))
          }
        }
      }
      gs.tiers <- gs.tiers.temp
      errors.tbl <- errors.tbl.temp
    } else { # When there are 5 or fewer non-CHI tiers in the GS file, try every
             # permutation of tier matches to give the coder the best chance of passing
      gs.tier.perms <- permutations(n=length(nonchi.gs.s), r=length(nonchi.gs.s),
                   v=nonchi.gs.s,repeats.allowed=F)
      # For each permutation of the non-CHI speakers in the GS,
      # find the best set of non-CHI speakers in the coder's file
      top.score <- 0
      for (perm in 1:nrow(gs.tier.perms)) {
        tier.equiv.temp <- tibble(
          gs.spkr = gs.speakers$speaker,
          your.spkr = ""
        )
        tier.equiv.temp$your.spkr[which(
          tier.equiv.temp$gs.spkr == "CHI")] <- "CHI"
        nonchi.gs.s.p <- gs.tier.perms[perm,]
        nonchi.nw.s.p <- nonchi.nw.s
        while (length(nonchi.gs.s.p) > 0) {
          gs.tier.tomatch <- nonchi.gs.s.p[1]
          nonchi.gs.s.p <- nonchi.gs.s.p[!nonchi.gs.s.p %in% gs.tier.tomatch]
          # Find the slice_match value between the chosen GS tier and every NW option
          nonchi.nw.s.p <- sample(nonchi.nw.s.p) # randomize the NW tiers
          best.nw.match <- "<no match>"
          best.slice.score <- 0
          if (length(nonchi.nw.s.p) > 0) {
            for (tier.opt in nonchi.nw.s.p) {
              match.mean <- mean(intersect.spk.tiers(gs.file, nw.file,
                                                     gs.tier.tomatch, tier.opt,
                                                     seg_stt, seg_end,
                                                     strict, slice_sz)$match)
              if (match.mean > best.slice.score) {
                best.slice.score <- match.mean
                best.nw.match <- tier.opt
              }
            }
          }
          tier.equiv.temp$your.spkr[which(
            tier.equiv.temp$gs.spkr == gs.tier.tomatch)] <- best.nw.match
          nonchi.nw.s.p <- nonchi.nw.s.p[!nonchi.nw.s.p %in% best.nw.match]
        }
    
        # Internally rename in nw.speakers/remove non-matched tiers
        # Careful not to overwrite names/collapse speakers in the process!
        nw.file.temp <- nw.file %>%
          filter(speaker %in% tier.equiv.temp$your.spkr) %>%
          mutate(speaker2 = speaker)
        for (row in 1:nrow(tier.equiv.temp)) {
          if (tier.equiv.temp$gs.spkr[row] != tier.equiv.temp$your.spkr[row]) {
            toChange <- which(nw.file.temp$speaker2 ==
                                tier.equiv.temp$your.spkr[row])
            nw.file.temp$speaker[toChange] <- tier.equiv.temp$gs.spkr[row]
            nw.file.temp$tier[toChange] <- gsub(tier.equiv.temp$your.spkr[row],
                             tier.equiv.temp$gs.spkr[row], nw.file.temp$tier[toChange])
          }
        }
        nw.file.temp$speaker2 <- NULL
        
        # Set up table for tier comparison and error-reporting
        errors.tbl.temp <- tibble()
        gs.tiers.temp <- gs.file %>%
          filter(stop > seg_stt & start < seg_end & (!is.na(speaker))) %>%
          select(tier, speaker) %>% distinct() %>% arrange(speaker) %>%
          mutate(slice_match = "", n_annots = "", sec_annots = "",
                 slice_match_n = 0, nsec_spch = 0)
        
        # Fill in report values
        for (tiertype in gs.tiers.temp$tier) {
          tierspkr <- tiertype
          if ((grepl("@", tiertype))) {
            tierspkr <- substr(tiertype, 5, 7)
          }
          gs.row <- which(gs.tiers.temp$tier == tiertype)
          if (tier.equiv.temp$your.spkr[which(
            tier.equiv.temp$gs.spkr == tierspkr)] == "<no match>") {
            if (!(grepl("@", tiertype))) {
              gs.tiers.temp$n_annots[gs.row] <- "MISSING"
              gs.tiers.temp$sec_annots[gs.row] <- "MISSING"
            }
            gs.tiers.temp$slice_match[gs.row] <- "0%"
            gs.tiers.temp$slice_match_n[gs.row] <- 0
            gs.tiers.temp$nsec_spch[gs.row] <- round(sum(segA$duration)/1000,2)
          } else {
            # Fill in n_annots and sec_annots values
            segA <- gs.file %>%
              filter(tier == tiertype & stop > seg_stt & start < seg_end) %>%
              mutate(coder = "A")
            segB <- nw.file.temp %>%
              filter(tier == tiertype & stop > seg_stt & start < seg_end) %>%
              mutate(coder = "B")
            if (!(grepl("@", tiertype))) {
              gs.tiers.temp$n_annots[gs.row] <-
                paste("∆ = ",(nrow(segB)-nrow(segA)),
                    " (GS:",nrow(segA),", You:",nrow(segB),")", sep="")
              gs.tiers.temp$sec_annots[gs.row] <-
                paste("∆ = ",round((sum(segB$duration)/1000-sum(segA$duration)/1000),2),
                    " (GS:",round(sum(segA$duration)/1000,2),", You:",
                    round(sum(segB$duration)/1000,2),")", sep="")
            }
            gs.tiers.temp$nsec_spch[gs.row] <- round(sum(segA$duration)/1000,2)
            # Fill in the slice_match value
            comparison.tbl <- intersect.tiers(gs.file, nw.file.temp,
                                              tiertype, seg_stt, seg_end, strict, slice_sz)
            if (nrow(comparison.tbl) == 0) {
              gs.tiers.temp$slice_match[gs.row] <- "0%"
              gs.tiers.temp$slice_match_n[gs.row] <- 0
            } else {
              gs.tiers.temp$slice_match[gs.row] <-
                paste(round(mean(comparison.tbl$match)*100, 2),"%", sep="")
              gs.tiers.temp$slice_match_n[gs.row] <- mean(comparison.tbl$match)
              # Add slice match errors to the reporting table
              errors.tbl.temp <- bind_rows(errors.tbl.temp,
                                      lapply(subset(comparison.tbl, match == 0),
                                             as.character))
            }
          }
        }
        if (mean(gs.tiers.temp$slice_match_n) > top.score) {
          tier.equiv <- tier.equiv.temp
          gs.tiers <- gs.tiers.temp
          errors.tbl <- errors.tbl.temp
          top.score <- mean(gs.tiers.temp$slice_match_n)
        }
      }
    }
  }
    
  
  
  # ################################################################################
  # # Report results
  # ################################################################################
  # Tier inconsistencies
  tier.incons <- ""
  if (nrow(nw.speakers) > nrow(tier.equiv)) {
    extratiers <- nw.speakers$speaker[!nw.speakers$speaker %in%
                                           tier.equiv$your.spkr]
    extratiers <- paste(extratiers, collapse=", ")
    tier.incons <- paste("We did not find a GS match for your tier(s) named: ",
                     extratiers, sep="")
  } else if (nrow(nw.speakers) < nrow(tier.equiv)) {
    missedtiers <- tier.equiv$gs.spkr[which(
                       tier.equiv$your.spkr == "<no match>")]
    missedtiers <- paste(missedtiers,collapse=", ")
    tier.incons <- paste(
      "We did not find a tier in your annotations for the GS tier(s) named: ",
                     missedtiers, sep="")
  }

  # Tiers with speech
  tiers.w.spch <- tier.equiv[tier.equiv$gs.spkr %in% unique(gs.tiers$speaker),]
  
  # Add tier weights
  gs.tiers$weight <- 0
  gs.tiers$weight[which(gs.tiers$tier == "CHI")] <- 1
  nonCHI.spch.rows <- which(!(grepl('@|CHI', gs.tiers$tier)))
  gs.tiers$weight[nonCHI.spch.rows] <- round(gs.tiers$nsec_spch[nonCHI.spch.rows]/
    sum(gs.tiers$nsec_spch[nonCHI.spch.rows]),5)
  xds.rows <- which((grepl('xds@', gs.tiers$tier)))
  gs.tiers$weight[xds.rows] <- gs.tiers$weight[xds.rows-1]
  chi.dep.rows <- which((grepl('@CHI', gs.tiers$tier)))
  gs.tiers$weight[chi.dep.rows] <- 1

  # Clean up tier-based report
  gs.tiers.print <- gs.tiers %>%
    select(-speaker, -slice_match_n, -nsec_spch) %>%
    mutate(slice_match = replace(slice_match, slice_match=="NaN%", "0%"))

  # Sub-part scores
  chi.diar <- ""
  nch.diar <- ""
  xds.acc <- ""
  chi.dep.acc <- ""
  vcm.acc <- ""
  lex.acc <- ""
  mwu.acc <- ""
  if (nrow(filter(gs.tiers, tier == "CHI")) > 0) {
    chi.score <- as.numeric(gs.tiers %>% filter(tier == "CHI") %>%
                              select(slice_match_n) %>%
                              replace_na(list(slice_match_n = 0)))
    chi.diar <- paste("CHI diarization: ",round(chi.score*100, 2),"%", sep="")
  } else {
    chi.score <- 1
    chi.diar <- "CHI diarization: <nothing to evaluate>"
  }
  if (nrow(filter(gs.tiers, tier != "CHI")) > 0) {
    non.chi.score <- as.numeric(gs.tiers %>%
                                  filter(tier == speaker & tier != "CHI") %>%
                                  replace_na(list(slice_match_n = 0)) %>%
                                  mutate(wgtd.tier.score = slice_match_n * weight) %>%
                                  summarise(sum(wgtd.tier.score)))
    nch.diar <- paste("Non-CHI diarization: ", round(non.chi.score*100, 2),"%",
                   sep="")
  } else {
    non.chi.score <- 1
    nch.diar <- "Non-CHI diarization: <nothing to evaluate>"
  }
  if (sum(grepl('xds@', gs.tiers$tier)) > 0) {
    xds.score <- as.numeric(gs.tiers %>%
                              filter(grepl('xds@', tier)) %>%
                              replace_na(list(slice_match_n = 0)) %>%
                              mutate(wgtd.tier.score = slice_match_n * weight) %>%
                              summarise(sum(wgtd.tier.score)))
    xds.acc <- paste("Overall xds: ", round(xds.score*100, 2),"%", sep="")
  } else {
    xds.score <- 1
    xds.acc <- "Overall xds: <nothing to evaluate>"
  }
  if (sum(grepl('@CHI', gs.tiers$tier)) > 0) {
    chi.dep.score <- as.numeric(gs.tiers %>%
                              filter(tier != speaker & speaker == "CHI") %>%
                              select(slice_match_n) %>%
                              summarise(mean(slice_match_n)))
    chi.dep.acc <- paste0("CHI vcm/lex/mwu: ", round(chi.dep.score*100, 2), "%")
  } else {
    chi.dep.score <- 1
    chi.dep.acc <- "CHI vcm/lex/mwu: <nothing to evaluate>"
  }
  if (sum(grepl('vcm@', gs.tiers$tier)) > 0) {
    vcm.score <- as.numeric(gs.tiers %>%
                              filter(tier == "vcm@CHI") %>%
                              select(slice_match_n))
    vcm.acc <- paste0("vcm: ", round(vcm.score*100, 2), "%")
  } else {
    vcm.score <- 1
    vcm.acc <- "vcm: <nothing to evaluate>"
  }
  if (sum(grepl('lex@', gs.tiers$tier)) > 0) {
    lex.score <- as.numeric(gs.tiers %>%
                              filter(tier == "lex@CHI") %>%
                              select(slice_match_n))
    lex.acc <- paste0("lex: ", round(lex.score*100, 2), "%")
  } else {
    lex.score <- 1
    lex.acc <- "lex: <nothing to evaluate>"
  }
  if (sum(grepl('mwu@', gs.tiers$tier)) > 0) {
    mwu.score <- as.numeric(gs.tiers %>%
                              filter(tier == "mwu@CHI") %>%
                              select(slice_match_n))
    mwu.acc <- paste0("mwu: ", round(mwu.score*100, 2), "%")
  } else {
    mwu.score <- 1
    mwu.acc <- "mwu: <nothing to evaluate>"
  }
  
  # Summary scores
  summ.bad.tiers <- ""
  summ.weighted.score <- ""
  pass.message <- ""
  subminscores.univ <- gs.tiers %>%
    filter(tier == speaker | tier == "vcm@CHI") %>%
    replace_na(list(slice_match_n = 0)) %>%
    filter(slice_match_n < min_score_univ) %>%
    select(tier)
  if (native == "Yes") {
    subminscores.lgsp <- gs.tiers %>%
      filter(tier != speaker & tier != "vcm@CHI") %>%
      replace_na(list(slice_match_n = 0)) %>%
      filter(slice_match_n < min_score_lgsp) %>%
      select(tier)
    overall.score <- round((
      (chi.score * 0.35) +
      (non.chi.score * 0.35) +
      (chi.dep.score * 0.15) +
      (xds.score * 0.15))*100,2)
  } else {
    subminscores.lgsp <- gs.tiers %>%
      filter(grepl('xds@', tier)) %>%
      replace_na(list(slice_match_n = 0)) %>%
      filter(slice_match_n < min_score_lgsp) %>%
      select(tier)
    overall.score <- round((
      (chi.score * 0.4) +
      (non.chi.score * 0.34) +
      (xds.score * 0.2))*100,2)
  }
  subminscores <- bind_rows(subminscores.univ, subminscores.lgsp)
  if(nrow(subminscores) > 0) {
    submins <- subminscores$tier[1]
    if (nrow(subminscores) > 1) {
      for (row in 2:nrow(subminscores)) {
        submins <- paste(submins,subminscores$tier[row], sep=", ")
      }
    }
    summ.bad.tiers <- paste("Poor-performance tiers: ",submins, sep="")
  } else {
    summ.bad.tiers <- "Poor-performance tiers: <none! hooray!>"
  }
  summ.weighted.score <- paste("Weighted score: ", overall.score, "%", sep="")
  if (overall.score >= min_overall_score &
      nrow(subminscores) == 0) {
    pass.message <- "Congratulations, you pass for this segment! Please pass this report on to your lab's PI."
  } else {
    pass.message <- "Unfortunately you didn't pass this segment. Please consult with your lab's PI."
  }

  # Notes on requirements
  req.wscore <- paste("- An overall weighted score higher than ",
                      min_overall_score*100, "%", sep="")
  req.tiers.univ <- paste("- At least ", min_score_univ*100,
                     "% accuracy on ALL speaker tiers and vcm@CHI", sep="")
  if (native == "Yes") {
    req.tiers.lgsp <- paste("- At least ", min_score_lgsp*100,
                       "% accuracy on ALL xds tiers, lex@CHI, and mwu@CHI (as applicable).", sep="")
  } else {
    req.tiers.lgsp <- paste("- At least ", min_score_lgsp*100,
                       "% accuracy on ALL xds tiers.", sep="")
  }

  # Prep error table for return
  errors.tbl <- errors.tbl %>%
    rename(Slice.Start = slice, GS = valA, You = valB, Tier = tier) %>%
    select(-match)
  
  # Rename speaker tier values for clarity
  spkr.tier.errs <- which(nchar(errors.tbl$Tier) == 3)
  errors.tbl$GS[spkr.tier.errs] <- ifelse(errors.tbl$GS[spkr.tier.errs] == 0,
                                          "silence", "speech")
  errors.tbl$You[spkr.tier.errs] <- ifelse(errors.tbl$You[spkr.tier.errs] == 0,
                                          "silence", "speech")

    return(list(
      tier.equiv = tier.equiv,
      tier.incons = tier.incons,
      tiers.w.spch = tiers.w.spch,
      gs.tiers.print = gs.tiers.print,
      chi.diar = chi.diar,
      nch.diar = nch.diar,
      xds.acc = xds.acc,
      summ.bad.tiers = summ.bad.tiers,
      summ.weighted.score = summ.weighted.score,
      pass.message = pass.message,
      req.wscore = req.wscore,
      req.tiers.univ = req.tiers.univ,
      req.tiers.lgsp = req.tiers.lgsp,
      errors.tbl = errors.tbl,
      compare.stmt = compare.stmt,
      coder.stmt = coder.stmt
    ))
}


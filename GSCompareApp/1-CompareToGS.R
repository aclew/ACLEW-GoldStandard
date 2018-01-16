library(tidyverse)
library(gtools)
source("0-Helper_CompareToGS.R")

compare.files <- function(nw.filename, recording, minute, coder, lab) {
  ################################################################################
  # Set up
  ################################################################################
  
  # Input files
  nw.file <- read.annot(nw.filename)
  nw.file$code[which(is.na(nw.file$code))] <- "<empty>"
  gs.file <- read.annot(paste0(recording, "-0GS0.txt"))
  
  compare.stmt <- paste0("Comparing minute ", minute, " of recording ",
                        recording, " to the gold standard.")
  coder.stmt <- paste0("Submitted by coder ", coder, " from the ", lab, " lab.")
  
  # Input arguments
  slice_sz <- 50 # size of time slices compared
  min_score <- 0.85 # minumum score allowed on any tier
  min_subpart <- 0.9 # the minimum score needed to pass a sub-part
  min_subpart_passes <- 3 # minimum number of sub-parts they must pass (3 max)
  min_overall_score <- 0.95 # minimum overall weighted score
  strict <- "yes" # # Possibly TO-DO for later: we could use this to test 
                  # different equivalencies (e.g., if we want softer constraints,
                  # like 'B' can count as 'A' or 'C')
  

  ################################################################################
  # Run comparison
  ################################################################################
  # Determine the onset and offset times of the segment to be compared
  # if minute is NA then the whole file is compared
  seg_stt <- (minute-1)*60000
  seg_end <- minute*60000

  # Match up the nw file tiers to the gold standard as closely as possible
  gs.speakers <- gs.file %>%
    filter(stop > seg_stt & start < seg_end & !is.na(speaker)) %>%
    distinct(speaker)
  nw.speakers <- nw.file %>%
    filter(stop > seg_stt & start < seg_end & !is.na(speaker)) %>%
    distinct(speaker)
  
  # Create a tier equivalence table: match based on speech on/off similarity
  tier.equiv <- tibble(
    gs.spkr = gs.speakers$speaker,
    your.spkr = ""
  )
  gs.tiers <- gs.file %>%
  filter(stop > seg_stt & start < seg_end & (!is.na(speaker))) %>%
  select(tier, speaker) %>% distinct() %>% arrange(speaker) %>%
  mutate(slice_match = "", n_annots = "", sec_annots = "",
         slice_match_n = 0)

  # CHI is always matched with CHI
  tier.equiv$your.spkr[which(tier.equiv$gs.spkr == "CHI")] <- "CHI"
  
  # The others are matched as a set...
  nonchi.gs.s <- subset(gs.speakers, speaker != "CHI")$speaker
  nonchi.nw.s <- subset(nw.speakers, speaker != "CHI")$speaker
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
    nw.file.temp <- nw.file %>% filter(speaker %in%
                                         tier.equiv.temp$your.spkr)
    for (row in 1:nrow(tier.equiv.temp)) {
      if (tier.equiv.temp$gs.spkr[row] != tier.equiv.temp$your.spkr[row]) {
        nw.file.temp <- nw.file.temp %>% mutate(
          speaker = gsub(tier.equiv.temp$your.spkr[row],
                         tier.equiv.temp$gs.spkr[row], speaker),
          tier = gsub(tier.equiv.temp$your.spkr[row],
                      tier.equiv.temp$gs.spkr[row], tier))
      }
    }
    
    # Set up table for tier comparison and error-reporting
    errors.tbl.temp <- tibble()
    gs.tiers.temp <- gs.file %>%
      filter(stop > seg_stt & start < seg_end & (!is.na(speaker))) %>%
      select(tier, speaker) %>% distinct() %>% arrange(speaker) %>%
      mutate(slice_match = "", n_annots = "", sec_annots = "",
             slice_match_n = 0)
    
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
  
  gs.tiers.print <- gs.tiers %>%
    select(-speaker, -slice_match_n) %>%
    mutate(slice_match = replace(slice_match, slice_match=="NaN%", "0%"))

  # Sub-part scores
  chi.diar <- ""
  nch.diar <- ""
  dep.acc <- ""
  if (nrow(filter(gs.tiers, tier == "CHI") > 0)) {
    chi.score <- as.numeric(gs.tiers %>% filter(tier == "CHI") %>%
                              select(slice_match_n) %>%
                              replace_na(list(slice_match_n = 0)))
    chi.diar <- paste("CHI diarization: ",round(chi.score*100, 2),"%", sep="")
  } else {
    chi.diar <- "CHI diarization: <nothing to evaluate>"
  }
  if (nrow(filter(gs.tiers, tier != "CHI") > 0)) {
    non.chi.score <- as.numeric(gs.tiers %>%
                                  filter(tier == speaker & tier != "CHI") %>%
                                  replace_na(list(slice_match_n = 0)) %>%
                                  summarise(mean(slice_match_n)))
    nch.diar <- paste("Non-CHI diarization: ",round(non.chi.score*100, 2),"%",
                   sep="")
  } else {
    nch.diar <- "Non-CHI diarization: <nothing to evaluate>"
  }
  if (sum(grepl('@', gs.tiers$tier)) > 0) {
    dep.score <- as.numeric(gs.tiers %>% filter(tier != speaker) %>%
                              replace_na(list(slice_match_n = 0)) %>%
                              summarise(mean(slice_match_n)))
    dep.acc <- paste("vcm/lex/mlu/xds: ",round(dep.score*100, 2),"%", sep="")
  } else {
    dep.acc <- "vcm/lex/mlu/xds: <nothing to evaluate>"
  }

  # Summary scores
  summ.bad.tiers <- ""
  summ.weighted.score <- ""
  summ.total.subparts <- ""
  pass.message <- ""
  subminscores <- gs.tiers %>%
      replace_na(list(slice_match_n = 0)) %>%
      filter(slice_match_n < min_score) %>%
      select(tier)
  overall.score <- round(((chi.score * 0.3) + (non.chi.score * 0.3) +
    (non.chi.score * 0.4))*100,2)
  subtests.passed <- sum(c(chi.score, non.chi.score, dep.score) > min_subpart)
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
  summ.weighted.score <- paste("Weighted score: ",overall.score, sep="")
  summ.total.subparts <- paste("Sub-parts passed: ",subtests.passed," out of 3", sep="")
  if (subtests.passed >= min_subpart_passes &
      overall.score >= min_overall_score &
      nrow(subminscores) == 0) {
    pass.message <- "Congratulations, you pass for this segment! Please pass this report on to your lab's PI."
  } else {
    pass.message <- "Unfortunately you didn't pass this segment. Please consult with your lab's PI."
  }

  # Notes on requirements
  req.tiers <- paste("- At least ", min_score*100,
                     "% accuracy on ALL individual tiers", sep="")
  req.wscore <- paste("- An overall weighted score higher than ",
                      min_overall_score*100, "%", sep="")
  req.subpts <- paste("- At least ", min_subpart*100, "% accuracy on ",
                      min_subpart_passes, " of the 3 sub-parts", sep="")

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
      dep.acc = dep.acc,
      summ.bad.tiers = summ.bad.tiers,
      summ.weighted.score = summ.weighted.score,
      summ.total.subparts = summ.total.subparts,
      pass.message = pass.message,
      req.tiers = req.tiers,
      req.wscore = req.wscore,
      req.subpts = req.subpts,
      errors.tbl = errors.tbl,
      compare.stmt = compare.stmt,
      coder.stmt = coder.stmt
    ))
}


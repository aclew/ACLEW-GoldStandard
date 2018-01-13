library(tidyverse)
source("0-Helper_CompareToGS.R")

compare.files <- function(nw.filename, recording, minute, coder, lab) {
  ################################################################################
  # Set up
  ################################################################################
  
  # Input files
  nw.file <- read.annot(nw.filename)
  rec.name <- unlist(strsplit(nw.filename, '-')[1])
  gs.file <- read.annot(paste0(recording, "-0GS0.txt"))
  compare.stmt <- paste0("Comparando el minuto ", minute, " de grabación ",
                        recording, " al gold standard.")
  coder.stmt <- paste0("Actualizado por el anotador ", coder, " del grupo de ", lab, ".")
  
  # Input arguments
  slice_sz <- 50 # size of time slices compared
  min_score <- 0.85 # minumum score allowed on any tier
  min_subpart <- 0.9 # the minimum score needed to pass a sub-part
  min_subpart_passes <- 3 # minimum number of sub-parts they must pass (3 max)
  min_overall_score <- 0.95 # minimum overall weighted score
  strict <- "yes" # # TO-DO for later: we could use this to test different
                  # equivalencies (e.g., if we want softer constraints,
                  # like 'B' can count as 'A' or 'C')
  

  ################################################################################
  # Run comparison
  ################################################################################
  # Determine the onset and offset times of the segment to be compared
  # if minute is NA then the whole file is compared
  seg_stt <- (minute-1)*60000
  seg_end <- minute*60000

  # Match up the nw file tiers to the gold standard as closely as possible
  gs.speakers <- gs.file %>% distinct(speaker) %>% filter(!is.na(speaker))
  nw.speakers <- nw.file %>% distinct(speaker) %>% filter(!is.na(speaker))
  
  # Create a tier equivalence table: match based on speech on/off similarity
  tier.equiv <- tibble(
    gs.hblt = gs.speakers$speaker,
    vos.hblt = ""
  )
  
  # CHI is always matched with CHI
  tier.equiv$vos.hblt[which(tier.equiv$gs.hblt == "CHI")] <- "CHI"
  
  # The others are matched as a set...
  nonchi.gs.s <- subset(gs.speakers, speaker != "CHI")$speaker
  nonchi.nw.s <- subset(nw.speakers, speaker != "CHI")$speaker
  # One at a time, randomly select a GS tier and find the best match
  # for it in the NW tiers
  while (length(nonchi.gs.s) > 0) {
    gs.tier.tomatch <- sample(nonchi.gs.s)[1]
    nonchi.gs.s <- nonchi.gs.s[!nonchi.gs.s %in% gs.tier.tomatch]
    # Find the slice_match value between the chosen GS tier and every NW option
    nonchi.nw.s <- sample(nonchi.nw.s) # randomize the NW tiers
    best.nw.match <- "<no match>"
    best.slice.score <- 0
    if (length(nonchi.nw.s) > 0) {
      for (tier.opt in nonchi.nw.s) {
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
    tier.equiv$vos.hblt[which(
      tier.equiv$gs.hblt == gs.tier.tomatch)] <- best.nw.match
    nonchi.nw.s <- nonchi.nw.s[!nonchi.nw.s %in% best.nw.match]
  }
  
  # Internally rename in nw.speakers/remove non-matched tiers
  nw.file <- nw.file %>% filter(speaker %in% tier.equiv$vos.hblt)
  for (row in 1:nrow(tier.equiv)) {
    if (tier.equiv$gs.hblt[row] != tier.equiv$vos.hblt[row]) {
      nw.file <- nw.file %>% mutate(
        speaker = gsub(tier.equiv$vos.hblt[row],
                       tier.equiv$gs.hblt[row], speaker),
        tier = gsub(tier.equiv$vos.hblt[row],
                    tier.equiv$gs.hblt[row], tier))
    }
  }
  
  # Set up table for tier comparison and error-reporting
  errors.tbl <- tibble()
  gs.tiers <- gs.file %>%
    filter(stop > seg_stt & start < seg_end & (!is.na(speaker))) %>%
    select(tier, speaker) %>% distinct() %>% arrange(speaker) %>%
    mutate(prec_rebanada = "", n_annots = "", seg_annots = "",
           slice_match_n = 0)
  
  # Fill in report values
  for (tiertype in gs.tiers$tier) {
    gs.row <- which(gs.tiers$tier == tiertype)
    # Fill in n_annots and seg_annots values
    segA <- gs.file %>%
      filter(tier == tiertype & stop > seg_stt & start < seg_end) %>%
      mutate(coder = "A")
    segB <- nw.file %>%
      filter(tier == tiertype & stop > seg_stt & start < seg_end) %>%
      mutate(coder = "B")
    if (!(grepl("@", tiertype))) {
      gs.tiers$n_annots[gs.row] <-
      paste("∆ = ",(nrow(segB)-nrow(segA)),
            " (GS:",nrow(segA),", Vos:",nrow(segB),")", sep="")
      gs.tiers$seg_annots[gs.row] <-
      paste("∆ = ",round((sum(segB$duration)/1000-sum(segA$duration)/1000),2),
            " (GS:",round(sum(segA$duration)/1000,2),", Vos:",
            round(sum(segB$duration)/1000,2),")", sep="")
    }
    # Fill in the slice_match value
    comparison.tbl <- intersect.tiers(gs.file, nw.file,
                                      tiertype, seg_stt, seg_end, strict, slice_sz)
    gs.tiers$prec_rebanada[gs.row] <-
      paste(round(mean(comparison.tbl$match)*100, 2),"%", sep="")
    gs.tiers$slice_match_n[gs.row] <- mean(comparison.tbl$match)
    # Add slice match errors to the reporting table
    errors.tbl <- bind_rows(errors.tbl,
                            lapply(subset(comparison.tbl, match == 0),
                                   as.character))
  }
  
  
  # ################################################################################
  # # Report results
  # ################################################################################
  # Tier inconsistencies
  tier.incons <- ""
  if (nrow(nw.speakers) > nrow(tier.equiv)) {
    tier.incons <- paste("No encontramos una línea apareada en el GS para tu(s) línea(s) siguiente(s): ",
                     nw.speakers$speaker[!nw.speakers$speaker %in%
                                           tier.equiv$vos.hblt], sep="")
  } else if (nrow(nw.speakers) < nrow(tier.equiv)) {
    tier.incons <- paste(
      "No encontramos una línea en tus annotaciones para aparear con la(s) línea(s) de GS siguiente(s): ",
                     tier.equiv$gs.hblt[which(
                       tier.equiv$vos.hblt == "<no match>")], sep="")
  }

  # Tiers with speech
  tiers.w.spch <- tier.equiv[tier.equiv$gs.hblt %in% unique(gs.tiers$speaker),]
  
  gs.tiers.print <- gs.tiers %>%
    select(-speaker, -slice_match_n) %>%
    rename(linea = tier) %>%
    mutate(prec_rebanada = replace(prec_rebanada, prec_rebanada=="NaN%", "0%"))

  # Sub-part scores
  chi.diar <- ""
  nch.diar <- ""
  dep.acc <- ""
  if (nrow(filter(gs.tiers, tier == "CHI") > 0)) {
    chi.score <- as.numeric(gs.tiers %>% filter(tier == "CHI") %>%
                              select(slice_match_n) %>%
                              replace_na(list(slice_match_n = 0)))
    chi.diar <- paste("CHI diarización: ",round(chi.score*100, 2),"%", sep="")
  } else {
    chi.diar <- "CHI diarización: <nada de verificar>"
  }
  if (nrow(filter(gs.tiers, tier != "CHI") > 0)) {
    non.chi.score <- as.numeric(gs.tiers %>%
                                  filter(tier == speaker & tier != "CHI") %>%
                                  replace_na(list(slice_match_n = 0)) %>%
                                  summarise(mean(slice_match_n)))
    nch.diar <- paste("No-CHI diarización: ",round(non.chi.score*100, 2),"%",
                   sep="")
  } else {
    nch.diar <- "No-CHI diarización: <nada de verificar>"
  }
  if (sum(grepl('@', gs.tiers$tier)) > 0) {
    dep.score <- as.numeric(gs.tiers %>% filter(tier != speaker) %>%
                              replace_na(list(slice_match_n = 0)) %>%
                              summarise(mean(slice_match_n)))
    dep.acc <- paste("vcm/lex/mlu/xds: ",round(dep.score*100, 2),"%", sep="")
  } else {
    dep.acc <- "vcm/lex/mlu/xds: <nada de verificar>"
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
    summ.bad.tiers <- paste("Líneas con bajo desempeño: ",submins, sep="")
  } else {
    summ.bad.tiers <- "Líneas con bajo desempeño: <ninguna! hurra!>"
  }
  summ.weighted.score <- paste("Puntaje ponderado: ",overall.score, sep="")
  summ.total.subparts <- paste("Subpartes aprobadas: ",subtests.passed," de 3", sep="")
  if (subtests.passed >= min_subpart_passes &
      overall.score >= min_overall_score &
      nrow(subminscores) == 0) {
    pass.message <- "Felicitaciones, has aprobado la prueba para este segmento! Por favor pasa este informe a la jefa de tu grupo."
  } else {
    pass.message <- "Desafortunadamente no has aprobado la prueba para este segmento. Por favor consultá con la jefa de tu grupo."
  }

  # Notes on requirements
  req.tiers <- paste("- Al menos ", min_score*100,
                     "% de precisión en TODAS las líneas individuales", sep="")
  req.wscore <- paste("- Un puntaje ponderado total mayor que ",
                      min_overall_score*100, "%", sep="")
  req.subpts <- paste("- Al menos ", min_subpart*100, "% de precisión en ",
                      min_subpart_passes, " de las 3 subpartes", sep="")

  # Prep error table for return
  errors.tbl <- errors.tbl %>%
    rename(Slice.Start = slice, GS = valA, Vos = valB, Tier = tier) %>%
    select(-match)
  
  # Rename speaker tier values for clarity
  spkr.tier.errs <- which(nchar(errors.tbl$Tier) == 3)
  errors.tbl$GS[spkr.tier.errs] <- ifelse(errors.tbl$GS[spkr.tier.errs] == 0,
                                          "silencio", "habla")
  errors.tbl$Vos[spkr.tier.errs] <- ifelse(errors.tbl$Vos[spkr.tier.errs] == 0,
                                          "silencio", "habla")

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


read.annot <- function(filename) {
  colnames <- c("tier","speaker","start","stop","duration","code")
  annots <- read_tsv(filename, col_names = colnames)
  return(annots)
}

intersect.spk.tiers <- function(annA, annB, tierA, tierB,
                            seg_stt, seg_end, strict="yes", slice_sz) {
  # Use only the comparison subset of the annotations
  segA <- annA %>%
    filter(tier == tierA & stop > seg_stt & start < seg_end) %>%
    mutate(coder = "A")
  segB <- annB %>%
    filter(tier == tierB & stop > seg_stt & start < seg_end) %>%
    mutate(coder = "B")
  segAB.spch <- bind_rows(segA, segB)
  # Set up comparison table
  ABtbl <- tibble(
    slice = seq(seg_stt, seg_end + slice_sz, slice_sz),
    spchA = 0,
    spchB = 0)
  # Fill in speech-on/off values for each tier
  if(nrow(segAB.spch) > 0) {
    for (row in 1:nrow(segAB.spch)) {
      startannot <- ifelse(segAB.spch$start[row] < seg_stt, seg_stt,
                           segAB.spch$start[row])
      stopannot <- ifelse(segAB.spch$stop[row] > (seg_end - slice_sz),
                          (seg_end - slice_sz),
                          segAB.spch$stop[row])
      if (segAB.spch$coder[row] == "A") {
        ABtbl$spchA[max(which(ABtbl$slice <= startannot)):
                     (min(which(ABtbl$slice >= stopannot)))] <- 1
      } else {
        ABtbl$spchB[max(which(ABtbl$slice <= startannot)):
                     (min(which(ABtbl$slice >= stopannot)))] <- 1
      }
    }
  }
  ABtbl <- ABtbl %>%
    rename(valA = spchA, valB = spchB) %>%
    mutate(match = as.numeric(valA == valB))
  return(ABtbl)
}



intersect.tiers <- function(annA, annB, tiertype,
                            seg_stt, seg_end, strict, slice_sz) {
  if (grepl("@", tiertype)) {
    spkr <- substr(tiertype, 5, 7)
    ttyp <- substr(tiertype, 1, 3)
  } else {
    spkr <- tiertype
    ttyp <- "ort"
  }
  # Use only the comparison subset of the annotations
  segA <- annA %>%
    filter(speaker == spkr & stop > seg_stt & start < seg_end) %>%
    mutate(coder = "A")
  segB <- annB %>%
    filter(speaker == spkr & stop > seg_stt & start < seg_end) %>%
    mutate(coder = "B")
  segAB.spch <- bind_rows(segA, segB) %>% filter(tier == spkr)
  segAB.dpdt <- bind_rows(segA, segB) %>% filter(tier == tiertype)
  # Set up comparison table
  if (ttyp == "ort") {
    ABtbl <- tibble(
      slice = seq(seg_stt, seg_end - slice_sz, slice_sz),
      spchA = 0,
      spchB = 0)
  } else {
    ABtbl <- tibble(
      slice = seq(seg_stt, seg_end - slice_sz, slice_sz),
      spchA = 0,
      spchB = 0,
      valA = "NA",
      valB = "NA")
  }
  # Fill in speech-on/off values for each tier
  if(nrow(segAB.spch) > 0) {
    for (row in 1:nrow(segAB.spch)) {
      startannot <- ifelse(segAB.spch$start[row] < seg_stt, seg_stt,
                           segAB.spch$start[row])
      stopannot <- ifelse(segAB.spch$stop[row] > (seg_end - slice_sz),
                          (seg_end - slice_sz),
                          segAB.spch$stop[row])
      if (segAB.spch$coder[row] == "A") {
        ABtbl$spchA[max(which(ABtbl$slice <= startannot)):
                     (min(which(ABtbl$slice >= stopannot)))] <- 1
      } else {
        ABtbl$spchB[max(which(ABtbl$slice <= startannot)):
                     (min(which(ABtbl$slice >= stopannot)))] <- 1
      }
    }
  }
  # For non-orth tiers, fill in annotation values for each tier, then
  # only take the intersection of speech == 1 for both A and B
  if (ttyp != "ort") {
    if(nrow(segAB.dpdt) > 0) {
      for (row in 1:nrow(segAB.dpdt)) {
        startannot <- ifelse(segAB.dpdt$start[row] < seg_stt, seg_stt,
                             segAB.dpdt$start[row])
        stopannot <- ifelse(segAB.dpdt$stop[row] > (seg_end - slice_sz),
                          (seg_end - slice_sz),
                            segAB.dpdt$stop[row])
        valannot <- segAB.dpdt$code[row]
        if (segAB.dpdt$coder[row] == "A") {
          ABtbl$valA[max(which(ABtbl$slice <= startannot)):
                       (min(which(ABtbl$slice >= stopannot)))] <- valannot
        } else {
          ABtbl$valB[max(which(ABtbl$slice <= startannot)):
                       (min(which(ABtbl$slice >= stopannot)))] <- valannot
        }
      }
    }
    if (strict == 1) {
      ABtbl <- ABtbl %>%
        filter(spchA == 1 & spchB == 1) %>%
        mutate(match = as.numeric(valA == valB), tier = tiertype) %>%
        select(-spchA, -spchB)
    } else {
      ABtbl <- ABtbl %>%
        filter(spchA == 1 & spchB == 1)
      if (ttyp == "xds") {
        xds.loose.matches <- tibble(
          GS = c('C', 'C', 'B', 'B', 'B', 'A', 'A', 'P', 'P', 'P', 'O', 'O', 'O', 'U', 'U', 'U'),
          NW = c('C', 'B', 'B', 'C', 'A', 'A', 'B', 'P', 'O', 'U', 'O', 'P', 'U', 'P', 'O', 'U')
        )
        ABtbl$match <- ifelse(paste0(ABtbl$valA, ABtbl$valA) %in%
                                paste0(xds.loose.matches$GS, xds.loose.matches$NW), 1, 0)
        ABtbl <- filter(ABtbl, valA != 'U') %>%
          mutate(tier = tiertype) %>%
          select(-spchA, -spchB)
      } else if (ttyp == "vcm") {
        vcm.loose.matches <- tibble(
          GS = c('N', 'N', 'C', 'C', 'L', 'L', 'Y', 'Y', 'U'),
          NW = c('N', 'C', 'C', 'N', 'L', 'Y', 'Y', 'L', 'U')
        )
        ABtbl$match <- ifelse(paste0(ABtbl$valA, ABtbl$valA) %in%
                                paste0(vcm.loose.matches$GS, vcm.loose.matches$NW), 1, 0)
        ABtbl <- filter(ABtbl, valA != 'U') %>%
          mutate(tier = tiertype) %>%
          select(-spchA, -spchB)
      } else {
        ABtbl <- ABtbl %>%
          filter(spchA == 1 & spchB == 1) %>%
          mutate(match = as.numeric(valA == valB), tier = tiertype) %>%
          select(-spchA, -spchB)
      }
    }
  } else {
    ABtbl <- ABtbl %>%
      rename(valA = spchA, valB = spchB) %>%
      mutate(match = as.numeric(valA == valB), tier = tiertype)
  }
    return(ABtbl)
}

collapse.tiers.lena <- function(ann, seg_stt, seg_end) {
  ann <- filter(ann, stop > seg_stt  & start < seg_end)
  # Add the child's tiers to the output file
  ann.lna <- filter(ann, speaker == "CHI")
  # Add the other (collapsed) tiers (MA1, FA1, and UC1)
  # to the output file in a loop
  nonchi.ptns <- c("^MA", "^FA", "^[MFU]C")
  nonchi.spkrs <- c("MA1", "FA1", "UC1")
  for (i in 1:length(nonchi.spkrs)) {
    ann.spkrs <- ann %>%
      filter(grepl(nonchi.ptns[i], speaker))
    if (nrow(ann.spkrs) > 0) {
      # If there's more than one speaker for a pattern,
      # collapse and add them to the output
      if (length(unique(ann.spkrs$speaker)) > 1) {
        tiertypes <- unique(gsub("[MFU][ACU][0-9]", "", ann.spkrs$tier))
        for (tiertype in tiertypes) {
          if (tiertype == "") {
            ann.spkrs.type <- ann.spkrs %>%
              filter(tier == speaker)
          } else {
            ann.spkrs.type <- ann.spkrs %>%
              filter(grepl(tiertype, tier))
          }
          ann.spkrs.type$code2 <- paste0(ann.spkrs.type$start, ann.spkrs.type$code)
          uniq.tiers <- unique(ann.spkrs.type$tier)
          tbl.type <- tibble()
          for (curr.tier in uniq.tiers) {
            ann.spkrs.type.tier <- filter(ann.spkrs.type, tier == curr.tier)
            spkr.tbl <- tibble(
              slice = seq(seg_stt, seg_end - 1, 1),
              code = rep(NA, seg_end - seg_stt),
              code2 = rep(NA, seg_end - seg_stt)
            )
            for (row in 1:nrow(ann.spkrs.type.tier)) {
              startannot <- ifelse(ann.spkrs.type.tier$start[row] < seg_stt, seg_stt,
                                   ann.spkrs.type.tier$start[row])
              stopannot <- ifelse(ann.spkrs.type.tier$stop[row] > (seg_end - 1),
                                  (seg_end - 1),
                                  ann.spkrs.type.tier$stop[row])
              spkr.tbl$code[(max(which(spkr.tbl$slice <= startannot))):
                               ((min(which(spkr.tbl$slice >= stopannot))))] <-
                ann.spkrs.type.tier$code[row]
              spkr.tbl$code2[(max(which(spkr.tbl$slice <= startannot))):
                               ((min(which(spkr.tbl$slice >= stopannot))))] <-
                ann.spkrs.type.tier$code2[row]

            }
            names(spkr.tbl)[which(names(spkr.tbl) == "code")] <- curr.tier
            names(spkr.tbl)[which(names(spkr.tbl) == "code2")] <- paste0(curr.tier,"_2")
            tbl.type <- tbl.type %>% bind_rows(spkr.tbl)
          }
          # convert ms vals back to the original .tsv format
          val.cols <- which(grepl("[MFU][ACU][0-9]$", names(tbl.type)))
          tbl.type <- tbl.type %>%
            unite(combo, val.cols)
          val2.cols <- which(grepl("_", names(tbl.type)))
          tbl.type <- tbl.type %>%
            unite(combo2, val2.cols) %>%
            filter(combo != "NA_NA")
          tbl.type$combo2.shift <- c("", tbl.type$combo2[1:(nrow(tbl.type)-1)])
          tbl.type$start <- ifelse(tbl.type$combo2 == tbl.type$combo2.shift,
                                    0, 1)
          tbl.type$stop <- 0
          tbl.type$stop[which(tbl.type$start == 1)[2:sum(tbl.type$start)] - 1] <- 1
          tbl.type$stop[nrow(tbl.type)] <- 1
          tbl.type <- filter(tbl.type, start == 1 | stop == 1)
          tbl.type$slice.shift <- c(tbl.type$slice[2:nrow(tbl.type)],0)
          tbl.type <- tbl.type %>%
            filter(stop == 0) %>%
            select(-combo2, -combo2.shift, -start, -stop) %>%
            rename(start = slice, stop = slice.shift, code = combo) %>%
            mutate(duration = stop - start,
                   speaker = nonchi.spkrs[i],
                   tier = paste0(tiertype, nonchi.spkrs[i])) %>%
            select(tier, speaker, start, stop, duration, code)
          ann.lna <- bind_rows(ann.lna, tbl.type)
        }
      } else { # Otherwise rename the speaker and add them to the output
        curr.name <- unique(ann.spkrs$speaker)[1]
        ann.spkrs$tier <- gsub(curr.name, nonchi.spkrs[i], ann.spkrs$tier)
        ann.spkrs$speaker <- gsub(curr.name, nonchi.spkrs[i], ann.spkrs$speaker)
        ann.lna <- bind_rows(ann.lna, ann.spkrs)
      }
    }
  }
  return(ann.lna)
}


partial.match <- function(vecA, vecB) {
  matches <- rep(0, length(vecA))
  for (i in 1:length(vecA)) {
    A.parts <- strsplit(vecA[i], '_')[[1]]
    B.parts <- strsplit(vecB[i], '_')[[1]]
    matches[i] <- ifelse(sum(A.parts %in% B.parts) > 0, 1, 0)
  }
  return(matches)
}

partial.match.loose <- function(vecGS, vecNW, loose.map) {
  matches <- rep(0, length(vecGS))
  for (i in 1:length(vecGS)) {
    GS.parts <- strsplit(vecGS[i], '_')[[1]]
    NW.parts <- strsplit(vecNW[i], '_')[[1]]
    overlaps <- 0
    for (part in GS.parts) {
      if (part %in% loose.map$GS) {
        acceptable.NW <- subset(loose.map, GS == part)$NW
        overlaps <- overlaps + ifelse(sum(acceptable.NW %in% NW.parts) > 0, 1, 0)
      }
    }
    matches[i] <- ifelse(overlaps > 0, 1, 0)
  }
  return(matches)
}

intersect.tiers.multi <- function(annA, annB, tiertype,
                            seg_stt, seg_end, slice_sz) {
  if (grepl("@", tiertype)) {
    spkr <- substr(tiertype, 5, 7)
    ttyp <- substr(tiertype, 1, 3)
  } else {
    spkr <- tiertype
    ttyp <- "ort"
  }
  # Use only the comparison subset of the annotations
  segA <- annA %>%
    filter(speaker == spkr & stop > seg_stt & start < seg_end) %>%
    mutate(coder = "A")
  segB <- annB %>%
    filter(speaker == spkr & stop > seg_stt & start < seg_end) %>%
    mutate(coder = "B")
  segAB.spch <- bind_rows(segA, segB) %>% filter(tier == spkr)
  segAB.dpdt <- bind_rows(segA, segB) %>% filter(tier == tiertype)
  # Set up comparison table
  if (ttyp == "ort") {
    ABtbl <- tibble(
      slice = seq(seg_stt, seg_end - slice_sz, slice_sz),
      spchA = 0,
      spchB = 0)
  } else {
    ABtbl <- tibble(
      slice = seq(seg_stt, seg_end - slice_sz, slice_sz),
      spchA = 0,
      spchB = 0,
      valA = "NA",
      valB = "NA")
  }
  # Fill in speech-on/off values for each tier
  if(nrow(segAB.spch) > 0) {
    for (row in 1:nrow(segAB.spch)) {
      startannot <- ifelse(segAB.spch$start[row] < seg_stt, seg_stt,
                           segAB.spch$start[row])
      stopannot <- ifelse(segAB.spch$stop[row] > (seg_end - slice_sz),
                          (seg_end - slice_sz),
                          segAB.spch$stop[row])
      if (segAB.spch$coder[row] == "A") {
        ABtbl$spchA[max(which(ABtbl$slice <= startannot)):
                     (min(which(ABtbl$slice >= stopannot)))] <- 1
      } else {
        ABtbl$spchB[max(which(ABtbl$slice <= startannot)):
                     (min(which(ABtbl$slice >= stopannot)))] <- 1
      }
    }
  }
  # For non-orth tiers, fill in annotation values for each tier, then
  # only take the intersection of speech == 1 for both A and B
  if (ttyp != "ort") {
    if(nrow(segAB.dpdt) > 0) {
      for (row in 1:nrow(segAB.dpdt)) {
        startannot <- ifelse(segAB.dpdt$start[row] < seg_stt, seg_stt,
                             segAB.dpdt$start[row])
        stopannot <- ifelse(segAB.dpdt$stop[row] > (seg_end - slice_sz),
                          (seg_end - slice_sz),
                            segAB.dpdt$stop[row])
        valannot <- segAB.dpdt$code[row]
        if (segAB.dpdt$coder[row] == "A") {
          ABtbl$valA[max(which(ABtbl$slice <= startannot)):
                       (min(which(ABtbl$slice >= stopannot)))] <- valannot
        } else {
          ABtbl$valB[max(which(ABtbl$slice <= startannot)):
                       (min(which(ABtbl$slice >= stopannot)))] <- valannot
        }
      }
    }
    ABtbl <- ABtbl %>%
      filter(spchA == 1 & spchB == 1)
    if (ttyp == "xds" | ttyp == "vcm") {
      if (ttyp == "xds") {
        loose.matches <- tibble(
          GS = c('C', 'C', 'B', 'B', 'B', 'A', 'A', 'P', 'P', 'P', 'O', 'O', 'O', 'U', 'U', 'U', 'NA'),
          NW = c('C', 'B', 'B', 'C', 'A', 'A', 'B', 'P', 'O', 'U', 'O', 'P', 'U', 'P', 'O', 'U', 'NA')
          )
      } else {
        loose.matches <- tibble(
          GS = c('N', 'N', 'C', 'C', 'L', 'L', 'Y', 'Y', 'U'),
          NW = c('N', 'C', 'C', 'N', 'L', 'Y', 'Y', 'L', 'U')
        )
      }
      ABtbl <- ABtbl %>%
        filter(!(grepl('^(U|NA)+(_(U|NA)+)*$', valA)))
      if (nrow(ABtbl) > 0) {
        ABtbl <- ABtbl %>%
          mutate(match = partial.match.loose(valA, valB, loose.matches), tier = tiertype) %>%
          select(-spchA, -spchB)
      }
    } else {
      ABtbl <- ABtbl %>%
        filter(!(grepl('^U(_U)*$', valA)))
      if (nrow(ABtbl) > 0) {
        ABtbl <- ABtbl %>%
          mutate(match = partial.match(valA, valB), tier = tiertype) %>%
          select(-spchA, -spchB)
      }
    }
  } else {
    ABtbl <- ABtbl %>%
      rename(valA = spchA, valB = spchB) %>%
      mutate(match = as.numeric(valA == valB), tier = tiertype)
  }
    return(ABtbl)
}


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
          GS = c('C', 'C', 'B', 'B', 'B', 'A', 'A', 'P', 'P', 'P', 'O', 'O', 'O', 'U', 'O', 'U'),
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


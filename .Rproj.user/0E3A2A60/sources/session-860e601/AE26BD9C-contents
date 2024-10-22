library(textgRid)
library(ipa)

# load_textGrids: func for gatherings and loading all textGrids
# If previous version of textgrids exits, use this file.
#allGrids is a list of gridNames and TextGrids (see textgRid for formats)
load_textGrids <- function(){
  if(file.exists("textGrid_Processed_File.RData")) {
    load(file="textGrid_Processed_File.RData")
    return(save)
  }
  else {
  addResourcePath("txt", "./textgrid")
  grid_files <- file.path("textgrid", list.files("./textgrid", ".txt"))
  allGrids <- lapply(grid_files, TextGrid)
  save <- list(grid_files, allGrids)
  save(save, file="textGrid_Processed_File.RData")
  return(save)
  }
}

#input: a regex to search for, a tier to search on, and a list of loaded textGrids. Output: three lists, gridNames, startTimes, endTimes
#The index of each list represents a start time, endtime, and origin grid for each search.
#outputs empty start and end times if cannot find pattern in textgrids on this tier.
#cannot handle a tier other than phoneme, syllable, word 
get_timestamps_for <- function(regex, tierSearch, allGrids) {
  startTimes = list()
  endTimes = list()
  gridNames = list()
  labels = list()
  
  for (index in 1:length(allGrids[[1]])) {
    name <- allGrids[[1]][[index]]
    grid <- allGrids[[2]][[index]]
    #search the correct tier
    t <- grid[[tierSearch]]

    #find intervals for this regex
    if (!is.null(t)) {
    intervals <- findIntervals(tier=t, pattern=regex)
      if (!is.null(intervals)){
        #add to lists
        startTimes <- append(startTimes, intervals$StartTime)
        endTimes <- append(endTimes, intervals$EndTime)
        labels <- append(labels, intervals$Label)
        #Grid name
        for (x in intervals$StartTime) {
          gridNames <- append(gridNames, name)
        }
      }
    }
  }
  #return object
  return(list(gridNames, startTimes, endTimes, labels))
}

#input: a regex to search for, a tier to search on, and a list of loaded textGrids. Output: three lists, gridNames, startTimes, endTimes
#The index of each list represents a start time, endtime, and origin grid for each search.
#outputs empty start and end times if cannot find pattern in textgrids on this tier.
#cannot handle a tier other than phoneme, immediate neighbor, syllable, word 
get_timestamps_for_neighbor <- function(regex, tierSearch, allGrids) {
  checkLeft <- function(x) max((x-1),1)
  startTimes = list()
  endTimes = list()
  gridNames = list()
  leftRegex = list()
  rightRegex = list()
  
  for (index in 1:length(allGrids[[1]])) {
    name <- allGrids[[1]][[index]]
    grid <- allGrids[[2]][[index]]
    #search the given tier
    t <- grid[[tierSearch]]
    
    #find intervals for this regex
    if (!is.null(t)) {
      intervals <- findIntervals(tier=t, pattern=regex)
      if (!is.null(intervals) && (length(intervals$Label)>0)){
        # print(intervals)
        checkRight <- function(x) min((x+1), length(t@labels)) 
        leftIndex <- lapply(intervals$Index, checkLeft)
        rightIndex <- lapply(intervals$Index, checkRight)
        # print("stop_1")
        for (x in 1:length(intervals$Index)) {
          #Grid name
          gridNames <- append(gridNames, name)
          # print("stop_2")
          #ERROR
          # print("checking")
          # print(x)
          # print(leftIndex)
          # print(leftIndex[[x]])
          # print(t@labels[leftIndex[[x]]])
          leftRegex <- append(leftRegex, t@labels[leftIndex[[x]]])
          #ERROR
          # print("stop_3")
          rightRegex <- append(rightRegex, t@labels[rightIndex[[x]]])
          startTimes <- append(startTimes, t@startTimes[leftIndex[[x]]])
          # print("stop_4")
          endTimes <- append(endTimes, t@endTimes[rightIndex[[x]]])
          
          #left and right neighbor regex
          #TODO, if it was the beginning or end of file, give a blank regex to express that there was no left or righthand context
          #TODO MUST APPEND SEARCH WITH CURRENT START AND END TIMES
          #leftRegex <- append(leftRegex, t@labels[leftIndex[x]])
          #rightRegex <- append(rightRegex, t@labels[rightIndex[x]])
        }
      }
    }
  }
  return(list(gridNames, startTimes, endTimes, leftRegex, rightRegex))
}

#given a vector of X-SAMPA symbols, OR a string of X_SAMPA symbols, returns its IPA equivalent
#NOTE in X-SAMPA, when a backslash is used it MUST be replaced by 2 backslashes to avoid parsing errors for R
translate_sampa <- function(sampa_vector){
  return(convert_phonetics(sampa_vector, from = "xsampa", to = "ipa"))
}

#given a vector of ascii/ipa symbols, OR a string of ascii symbols, returns its X_SAMPA equivalent
#NOTE in X-SAMPA, when a backslash is used it MUST be replaced by 2 backslashes to avoid parsing errors for R
translate_ipa <- function(ipa_vector){
  return(convert_phonetics(ipa_vector, from = "ipa", to = "xsampa"))
}

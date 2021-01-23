library(purrr); library(stringr)

# function environment objects --------------------------------------------
members <- jsonlite::fromJSON("https://www.dropbox.com/s/olji1q29t2autec/ntpumeetMembers.json?dl=1", simplifyDataFrame = F)
memberIds <- purrr::map_chr(members, ~{.x[["memberID"]]})


# core functions ----------------------------------------------------------

#' Login current users
#'
#' @param memberID A character of memberID
#' @param currentGPS A numeric vector of 2 
#'
#' @return a list of current user
#' @export
#'
#' @examples none
appLogin <- function(memberID, currentGPS){
  
  # look for member's element location
  pick_currentUser <- findWhich_basedOnMemberId(memberID)
  
  # update user's latestGPS
  updateGPS(currentGPS, memberID)
  currentUser <- members[[pick_currentUser]]
  
  return(currentUser)
}

#' Refine member IDs to meet current user's like/block preference
#'
#' @param selectedMemberIds A character vector of member IDs
#' @param currentUser A list of current Users
#'
#' @return A subset list of selectedMemberIds that meet current user's like/block preference
#' @export
#'
#' @examples none
refine_byLikesBlocks <- function(selectedMemberIds, currentUser){
  pick_nonBlocks <-
    !(selectedMemberIds %in% currentUser$blocks)
  validselectedMemberIds <- selectedMemberIds[pick_nonBlocks]
  whichIs_frontMember <-
    which(validselectedMemberIds %in% currentUser$likes)
  
  refined_selectedMemberIds <-
    c(
      validselectedMemberIds[whichIs_frontMember],
      validselectedMemberIds[-whichIs_frontMember]
    )
  return(refined_selectedMemberIds)
}

# helpers -----------------------------------------------------------------
findWhich_basedOnMemberId <- function(memberID){
  {
    which(
      map_lgl(members,
              ~str_detect(.x$memberID,memberID))
    )
  }
}

updateGPS <- function(currentGPS, memberID){
  whichIsCurrentUser <- findWhich_basedOnMemberId(memberID)
  members[[whichIsCurrentUser]]$latestGPS <- currentGPS
  members <<- members # <<- is a super-assignment
}

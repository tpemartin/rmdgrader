notifyStudentRevision <-   function(rv){
    require(dplyr)
    gm_oauth()
    for(.x in seq_along(rv$revisionEmails)){
      Xbody <- paste0(
        rv$revisionEmails[[.x]]$body, collapse = "\n")
      Xemail <- rv$revisionEmails[[.x]]$email
      title <- names(rv$revisionHistory[[1]])

      gmailr::gm_mime() %>%
        gmailr::gm_to(Xemail) %>%
        gmailr::gm_cc("alex0938396656@gmail.com") %>%
        gmailr::gm_from("mtlin@gm.ntpu.edu.tw") %>%
        gmailr::gm_subject(glue::glue("資料科學程設： {title} dispute回覆")) -> myMime

      myMime %>%
        gmailr::gm_text_body(Xbody) -> myMime

      gmailr::gm_create_draft(myMime) -> result
    }

}


# helper ------------------------------------------------------------------

gm_oauth <- function() {
  gmailr::gm_auth_configure(key = "808460346772-29ro7jm166d57n6epv2bis3odvao1vpd.apps.googleusercontent.com", secret = "wC8BRDmj3EGPgNTjU719OwCe", appname = "gmail")
  gmailr::gm_auth(
    scopes = c("compose")
  )
}


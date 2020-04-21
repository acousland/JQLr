library(httr)

fix_empty <- function(x) {ifelse(length(x)==0,NA,x)}

get_issues <- function(query, username, token)
{
  r <- GET(query,
           authenticate(username, token))

  result <- content(r,"parsed")
  result <- result$issues

  return(result)
}

prep_blank_dataframe <- function(results)
{
  number_results <- length(results)
  items <- data.frame(   Task   = character(number_results),
                         Epic = character(number_results),
                         Status = character(number_results),
                         Assignee = character(number_results),
                         Last_Modified = character(number_results),
                         Issue_Question = character(number_results),
                         stringsAsFactors = FALSE)
  return(items)
}


populate_dataframe <- function(JSON_results, destination_dataframe, issue_question)
{
  number_results <- length(JSON_results)
  if(number_results == 0)
  {
    return(destination_dataframe)
  }
  for(i in seq(1:number_results)){
    destination_dataframe$Task[i] <- fix_empty(as.character(JSON_results[[i]]$fields$summary))
    destination_dataframe$Epic[i] <- fix_empty(as.character(JSON_results[[i]]$fields$parent$fields$summary))
    destination_dataframe$Status[i] <- fix_empty(as.character(JSON_results[[i]]$fields$status$statusCategory$name))
    destination_dataframe$Assignee[i] <- fix_empty(as.character(JSON_results[[i]]$fields$assignee$displayName))
    destination_dataframe$Last_Modified[i] <- fix_empty(as.character(JSON_results[[i]]$fields$updated))
    destination_dataframe$Issue_Question[i] <- issue_question
  }

  return(destination_dataframe)
}

fetch_issue_page <- function(query, username, token, issue_stamp)
{
  JSON_results <- get_issues(query, username, token)
  issues <- prep_blank_dataframe(JSON_results)
  issues <- populate_dataframe(JSON_results, issues, issue_stamp)
  return(issues)
}


#' Fetch Issues from Jira Cloud
#'
#' This function takes a JQL query and authentican artifacts and
#' returns a dataframe of the issues with a small number of key fields.
#'
#' @param query JQL Query
#' @param username Jira cloud username
#' @param token Jira cloud API token
#' @param issue_stamp A name to mark each item with
#' @return A matrix of the infile
#' @export
fetch_issues <- function(query, username, token, issue_stamp)
{
  #function will always try to fetch the second set of results instead of checking.
  #There is a logic hole here that assumes the query will always have a maximum of 50 results returned.
  #TODO Can fix by altering the URL to only fetch 50 for the first batch and iterate from there.

  running_results <- fetch_issue_page(query, username, token, issue_stamp)

  batch <- 1
  range <- batch * 50

  repeat{
    paginated_query <- paste0(query, '&startAt=',range)
    next_page_results <- fetch_issue_page(paginated_query, username, token, "all_items")

    if(length(next_page_results$Task)==0)
    {
      break
    }

    running_results <- rbind(running_results,next_page_results, stringsAsFactors=FALSE)

    batch = batch + 1
    range <- batch * 50
  }
  return(running_results)
}





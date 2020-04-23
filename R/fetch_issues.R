
#' Helper function to blank out empyt results
#'
#' @param x thing to check if it's empty
#' @return x or null
fix_empty <- function(x) {ifelse(length(x)==0,NA,x)}

#' Fetch the raw results
#'
#' @param query JQL results
#' @param username authentication username
#' @param token API token
#' @return A empty dataframe
get_issues <- function(query, username, token)
{
  r <- httr::GET(query,
           authenticate(username, token))

  result <- httr::content(r,"parsed")
  result <- result$issues

  return(result)
}

#' Make a blank dataframe that is the right size to put the JQL results into
#'
#' @param results JQL results
#' @return A empty dataframe
prep_blank_dataframe <- function(results)
{
  number_results <- length(results)
  items <- data.frame(   Task   = character(number_results),
                         Epic = character(number_results),
                         Status = character(number_results),
                         Assignee = character(number_results),
                         Last_Modified = character(number_results),
                         stringsAsFactors = FALSE)
  return(items)
}

#' Populate data frame with results from JQL query
#'
#' @param JSON_results JQL results
#' @param destination_dataframe The dataframe to write the results to
#' @return A dataframe of the query results
populate_dataframe <- function(JSON_results, destination_dataframe)
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
  }

  return(destination_dataframe)
}



#' Fetch Issues from Jira Cloud - using URL
#'
#' This function takes a JQL query and authentican artifacts and
#' returns a dataframe of the issues with a small number of key fields.
#' @param query JQL Query
#' @param username Jira cloud username
#' @param token Jira cloud API token
#' @return A dataframe of the query results
fetch_issue_page <- function(query, username, token)
{
  JSON_results <- get_issues(query, username, token)
  issues <- prep_blank_dataframe(JSON_results)
  issues <- populate_dataframe(JSON_results, issues)
  return(issues)
}


#' Fetch Issues from Jira Cloud
#'
#' This function takes a JQL query and authentican artifacts and
#' returns a dataframe of the issues with a small number of key fields.
#' @param site The Jira Cloud site to run the query against
#' @param jql JQL Query
#' @param username Jira cloud username
#' @param token Jira cloud API token
#' @return A dataframe of the query results
#' @export
jqlQuery <- function(site, jql, username, token)
{
  #function will always try to fetch the second set of results instead of checking.
  #There is a logic hole here that assumes the query will always have a maximum of 50 results returned.
  #TODO Can fix by altering the URL to only fetch 50 for the first batch and iterate from there.

  query <- paste0('https://', site, '/rest/api/2/search?jql=', urltools::url_encode(jql))

  running_results <- fetch_issue_page(query, username, token)

  batch <- 1
  range <- batch * 50

  repeat{
    paginated_query <- paste0(query, '&startAt=',range)
    next_page_results <- fetch_issue_page(paginated_query, username, token)

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





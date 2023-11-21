load_EML_df <- function(datapackage, directory = here::here("data")){
  #construct path to downloaded data package:
  path <- here::here(directory, datapackage)
  
  #load metadata
  metadata <- DPchecker::load_metadata(directory = path)  
  
  title <- EMLeditor::get_title(metadata)
  pub_date <- metadata$dataset$pubDate
  
  authors <- get_authors(metadata) #returns dataframe [x,2]
  authors$eml_element = "author"
  authors <- authors[,c(3,1,2)]
  
  contacts <- get_contacts(metadata) # returns dataframe [x,2]
  contacts$eml_element = "contact"
  contacts <- contacts[,c(3,1,2)]
  
  publisher <- metadata$dataset$publisher$organizationName
  doi <- EMLeditor::get_doi(metadata)
  
  #get publication location (city, state)
  pub_city <- metadata$dataset$publisher$address$city
  pub_state <- metadata$dataset$publisher$address$administrativeArea
  pub_location <- paste0(pub_city, ", ", pub_state)
  
  content_begin <- EMLeditor::get_begin_date(metadata)
  content_end <- EMLeditor::get_end_date(metadata)
  abstract <- EMLeditor::get_abstract(metadata)
  notes <- metadata$dataset$additionalInfo
  
  #get by or for NPS
  nps <- unlist(metadata$additionalMetadata, recursive = FALSE)
  nps2 <- unlist(nps, recursive = FALSE)
  agency <- nps2$metadata.agencyOriginated$agency
  by_for <-nps2$metadata.agencyOriginated$byOrForNPS
  if(agency == "NPS" & by_for == TRUE){
    origination <- "The information resource described by this reference was created by or for the NPS"
  } else {
    origination <- "The information resource described by this reference was not created by or for the NPS"
  }
  
  license_name <- metadata$dataset$licensed$licenseName
  
  #build dataframe for return object:
  EML_metadata <- data.frame(EML_element = as.character(),
                             EML_data = as.character(),
                             EML_data2 = as.character())
  
  EML_metadata[nrow(EML_metadata) + 1,] <- list("Title", title, title)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("Published_date",
                                                pub_date,
                                                pub_date)
  EML_metadata <- data.frame(mapply(c, EML_metadata, authors, SIMPLIFY=FALSE))
  EML_metadata <- data.frame(mapply(c, EML_metadata, contacts, SIMPLIFY=FALSE))
  EML_metadata[nrow(EML_metadata) + 1,] <- list("publisher",
                                                publisher,
                                                publisher)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("doi", doi, doi)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("pub_city", pub_city, pub_city)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("pub_state",
                                                pub_state,
                                                pub_state)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("content_begin",
                                                content_begin,
                                                content_begin)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("content_end",
                                                content_end,
                                                content_end)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("absract", abstract, abstract)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("notes", notes, notes)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("origination",
                                                origination,
                                                origination)
  EML_metadata[nrow(EML_metadata) + 1,] <- list("license_name",
                                                license_name,
                                                license_name)
  return(EML_metadata)
}

get_authors <- function(metadata){
  #get authors
  creators <- metadata$dataset$creator
  #set up empty dataframe to hold creator info:
  individual <- data.frame(author = as.character(),
                           contact = as.character())
  for(i in 1:length(seq_along(creators))){
    creator <- unlist(creators[[i]], recursive = FALSE)
    #if there is an individual name:
    if(!is.null(creator$individualName.surName)){
      #if there is a given name:
      if(!is.null(creator$individualName.givenName)){
        #if there are two given names (e.g. first and middle)
        if(length(seq_along(creator$individualName.givenName)) == 2){
          given <- paste(creator$individualName.givenName[[1]],
                         creator$individualName.givenName[[2]],
                         sep = " ")
          #if there is only one given name (first)
        } else if(length(seq_along(creator$individualNAme.givenName)) == 1){
          given <- creator$individualName.givenName
        } else {
          #More than 2 given names (e.g. first, middle, middle), use only the first given name:
          given <- creator$individualName.givenName[[1]]
        }
        
      } else {
        #if there is no given name:
        given <- NA
      }
      #get last name
      sur <- creator$individualName.surName
      #generate full name as first (first) last
      full_name <- paste(given, sur, sep = " ")
      if(!is.null(creator$electronicMailAddress)){
        mail <- creator$electronicMailAddress
      } else {
        mail <- NA
      }
      #turn name & contact into a dataframe
      author <- data.frame(full_name, mail)
      #append to the pre-existing dataframe
      individual <- rbind(individual, author)
    } else {
      #if there are not individual names (just organization)
      author <- data.frame(creator$organizationName, NA)
      individual <- rbind(individual, author)
    }
  }
  colnames(individual)<-c("author", "email")
  return(individual)
}

get_contacts <- function(metadata){
  contact <- metadata$dataset$contact
  individual <- NULL
  email <- NULL
  for(i in 1:length(seq_along(contact))){
    #get name as first-laste:
    name_list <- unlist(contact[[i]]$individualName, recursive = FALSE)
    if(length(seq_along(name_list)) == 3){
      ind_name <- paste(name_list$givenName1,
                        name_list$givenName2,
                        name_list$surName,
                        sep =" ")
    } else if (length(seq_along(name_list)) == 2){
      ind_name <- paste(name_list[1],
                        name_list[2],
                        sep =" ")
    } else {
      ind_name <- name_list
    }
    individual <- append(individual, ind_name)
    email <- append(email, contact[[i]]$electronicMailAddress)
  }
  contact_df <- data.frame(individual, email)
  names(contact_df)[1] = "contact"
  return(contact_df)
}

# functions to get dhis data

# require(httr)
# require(jsonlite)
# require(assertthat)
# require(rlist)
# require(reshape2)
# require(tidyverse)

loginDHIS2<-function( baseurl, username, password) {
  url<-paste0(baseurl,"api/me")
  r<-GET(url,authenticate(username,password))
  assert_that(r$status_code == 200L) }

# Generate password and UID ####  
genPassword<-function(passwordLength=8) {
  a<-sample(LETTERS,1)
  b<-sample(c(0:9),1)
  c<-sample(letters,passwordLength-2)
  d<-c(a,b,c)
  password<-paste(sample(d,size=passwordLength,replace=FALSE),sep="",collapse="")
  return(password)
}

generateUID <- function(codeSize = 11) {
  #Generate a random seed
  runif(1)
  allowedLetters <- c(LETTERS, letters)
  allowedChars <- c(LETTERS, letters, 0:9)
  #First character must be a letter according to the DHIS2 spec
  firstChar <- sample(allowedLetters, 1)
  otherChars <- sample(allowedChars, codeSize - 1)
  uid <- paste(c(firstChar,paste(
    otherChars, sep = "", collapse = "" )), sep = "", collapse = "")
  return(uid)
}

# Custom get and post functions ####
## To shorten the amount of typing for some functions we use many times

## gets json text from url and converts to data frame 
get = function( source_url , ...){
  
  g = fromJSON( suppressMessages(
    content( GET( source_url ), "text") ) 
  )
  
  return( g )
  
}

# posts data and returns TRUE or FALSE if succeeded   
post = function( .url, data, ... ){
  
    p = POST( .url, 
              body = toJSON( data , auto_unbox = TRUE) ,
              content_type_json()
    )
    
    result = if (p$status_code == 200L ){
      
      suppressMessages(
        fromJSON(content(p, "text" ))$stats %>% as.data.frame()
      )
      
    } else {
      "Failed.  May need to re-login with loginDHIS2()."
    }
    
    return( result )
}


 

# Retrieve metadata from dhis2 system ####
metadataDHIS2 = function(
  baseurl ,
  element, # e.g. data_elements, indicators, osu 
  fields = 'name,id,domainType,description,displayShortName,formName,code,lastUpdated,dataElementGroups,formName,code' ,
  version  # option to specify version
  
){
  
  # The whole shebang...All Metadata   ####
  if (element %in% 'all'){
    
    url<-paste0( baseurl,"api/metadata.json")
    get_met = get(url)
    return( get_met )
    
  }
  
  
  # Data Categories ####
  if (element %in% 'categories'){
    
    url<-paste0(baseurl,"api/categories?&paging=false")
    cgs = fromJSON(content(GET(url),"text")) 
    dataCategories = cgs[[1]] %>% as_tibble()
    return( dataCategories ) 
    
    
  }
  
  # Data Elements       ####
  ## selected data element fields
  if ( element %in% 'data_elements' ){
    
    if( !is.null(fields) ) {
      
      url<-paste0(baseurl,"api/dataElements.json?fields=", fields, "&paging=false")
      
    } else {
      
      # data element fields
      url<-paste0(baseurl,"api/dataElements.json?fields=:all&paging=false")
      
    }
    
    els = fromJSON(content(GET(url),"text"))
    
    # extract data element groups
    deg = els[[1]][, c('id', "dataElementGroups")] %>% bind_rows 
    
    # create data frame from each list element
    a = function(d){
      
      group_ids = unlist(d$dataElementGroups)
      
      data_frame(
        
        id = if( is.null( group_ids) ){ d$id
        } else { rep( d$id, length(group_ids)  ) } ,
        
        group = if( is.null( group_ids) ){ NA
        } else { group_ids }
      )
    }
    
    deg. = map( 1:nrow(deg), function(x) a(deg[x, ]) ) %>% bind_rows() %>%
      distinct()
    
    # look up names for dataElementGroups
    url<-paste0(baseurl,"api/dataElementGroups?&paging=false")
    elgs = fromJSON(content(GET(url),"text")) 
    dataElementGroupNames = elgs[[1]] %>% as_tibble() %>%
      distinct() %>%
      rename( group_id = id , groupName = displayName )
    
    # extract categoryCombos
    cat = els[[1]][, c('id', "categoryCombo")] 
    
    # create data frame from each category combo
    b = function(d){
      
      cat_ids = unlist( d$categoryCombo )
      
      data_frame(
        
        id = if( is.null( group_ids) ){ d$id
        } else { rep( d$id, length(group_ids)  ) } ,
        
        group = if( is.null( group_ids) ){ NA
        } else { group_ids }
      )
    }
    
    cat = map( 1:nrow(deg), function(x) a(deg[x, ]) ) %>% bind_rows() %>%
      distinct()
    
    # look up names for dataElementGroups
    url<-paste0(baseurl,"api/dataElementGroups?&paging=false")
    elgs = fromJSON(content(GET(url),"text")) 
    dataElementGroupNames = elgs[[1]] %>% as_tibble() %>%
      distinct() %>%
      rename( group_id = id , groupName = displayName )
    
    
    # extract atomic elements because not sure if other elements worth effort 
    els_atomic = map_lgl(els[[1]], is_atomic ) 
    els = els[[1]][, els_atomic ] 
    
    de = els %>%  as_tibble() %>% 
      left_join( deg., by = 'id' ) %>%
      left_join( dataElementGroupNames, by = c('group' = 'group_id') ) %>%
      group_by( id ) %>%
      mutate( n_groups = n() ) %>%
      ungroup
    
    return(de)
    
  }
  
  
  # Data Element Groups ####
  if ( element %in% 'data_element_groups' ){
    
    url<-paste0(baseurl,"api/dataElementGroups?&paging=false")
    elgs = fromJSON(content(GET(url),"text")) 
    dataElementGroups = elgs[[1]] %>% as_tibble()
    return(dataElementGroups) 
    
  }
  
  # Indicators  ####
  if ( element %in% 'indicators' ){
    
    url<-paste0(baseurl,"api/indicators?&paging=false")
    els = fromJSON(content(GET(url),"text")) 
    dataIndicators = els[[1]] %>% as_tibble()
    return(dataIndicators) 
    
  }
  
  # Program Indicators  ####
  if ( element %in% 'program_indicators' ){
    
    url<-paste0(baseurl,"api/programIndicators?&paging=false")
    els = fromJSON(content(GET(url),"text")) 
    dataIndicators = els[[1]] %>% as_tibble()
    return(dataIndicators) 
    
  }
  
  
  # Organisational unit (osu) ids  ####
  if (element %in% c('osu', 'orgUnits') ){
    url<-paste0(baseurl,"/api/organisationUnits.json?&paging=false")
    ous_from_server<-fromJSON(content(GET(url),"text"))
    ous = reduce( ous_from_server , bind_cols )
    return(ous)
  }
  
  # geoFeatures ####
  if (element %in% 'geoFeatures'){
    
    
    # geoFeatures_from_server = fromJSON( content(GET(url),"text") ) %>% as_tibble()
    
    geoFeatures_download = function( level ){
      url<-paste0(baseurl,"api/geoFeatures.json?ou=ou:LEVEL-", level, "&paging=false")
      fromJSON( content(GET(url),"text") ) %>% as_tibble()
    }
    
    geoFeatures_from_server = map( 0:8 , geoFeatures_download )
    geoFeatures = reduce(geoFeatures_from_server, bind_rows)
    
    # glimpse(geoFeatures)
    
    # remove potential duplicates
    geoFeatures = geoFeatures[ !is.na(geoFeatures$id) ,]
    # geoFeatures = geoFeatures[ !duplicated(geoFeatures) ,]
    
    return(geoFeatures)
  }
  
  ## for description of properties, see table 1.59, 
  ## https://docs.dhis2.org/2.22/en/developer/html/ch01s32.html
  
  # geoJson  #####
  if (element %in% 'geojson'){
    
    #   url<-paste0(baseurl,"api/organisationUnits.geojson?level=",level,"&paging=false")
    #   geojson_from_server = map(2:3, fromJSON( content(GET(url),"text") ) )
    #   geojson = reduce(geojson_from_server, bind_rows)
    # }
    
    geojson_download = function( level ){
      url<-paste0(baseurl,"api/geoFeatures.json?ou=ou:LEVEL-", level, "&paging=false")
      fromJSON( content(GET(url),"text") ) %>% as_tibble()
    }
    
    geojson_from_server = map( 1:5, geojson_download )
    geojson = reduce(geojson_from_server, bind_rows)
    
    # remove potential duplicates
    geojson = geojson[!duplicated(geojson),]
    
    return(geojson)
  }
  
}
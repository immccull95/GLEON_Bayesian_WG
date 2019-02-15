
google_drive_get = function(file, config_file = 'lib/cfg/gd_config.yml', verbose = FALSE, type = NULL, overwrite = TRUE){
  
  require_libaries('googledrive')
  
  # bypass the download from google drive if the right local file already exists
  if(file.exists(file)) {
    return(file)
  }
  
  # figure out whether and where the file exists on gdrive
  remote_path <- google_drive_locate_file(file, config_file)
  remote_id <- tail(remote_path$id, 1)
  
  # download the file from Google Drive to the local data_file
  if(!is.na(remote_id)) {
    if(verbose) message("Downloading ", file, " from Google Drive")
    if(!dir.exists(dirname(file))) dir.create(dirname(file), recursive = TRUE)
    googledrive::drive_download(
      file=googledrive::as_id(remote_id), path=file,
      type=type, overwrite=overwrite, verbose=verbose)
  } else {
    stop(paste0("Could not locate ", file, " for download from Google Drive"))
  }
  
  return(file)
}


google_drive_locate_file = function(file, config_file = 'lib/cfg/gd_config.yml') {
  # tell R CMD check not to worry about symbols used for dplyr non-standard eval
  drive_resource <- parents <- name <- '.dplyr.var'
  
  # load the project's googledrive configuration
  gd_config <- yaml::yaml.load_file(config_file)
  
  # normalize the relative path for this file so we can use in confidently as a
  # relative path from both the local working directory and the google drive
  # parent folder
  relative_path <- get_relative_path(file)
  relative_path_escaped <- relative_path %>% 
    gsub(pattern = '[', replacement = '\\[', fixed = TRUE) %>% 
    gsub(pattern = ']', replacement = '\\]', fixed = TRUE) %>% 
    gsub(pattern = '.', replacement = '\\.', fixed=TRUE) %>% 
    gsub(pattern = '/', replacement = '$|^') 
  
  # query google drive for all possibly relevant files and add their parents as
  # a simple column
  relevant_files <- bind_rows(
    googledrive::drive_get(
      id=googledrive::as_id(gd_config$folder)),
    googledrive::drive_ls(
      path=googledrive::as_id(gd_config$folder), 
      pattern=sprintf("^%s$", relative_path_escaped),
      verbose=FALSE,
      recursive=TRUE)
  ) %>%
    dplyr::mutate(parents=lapply(drive_resource, function(dr) {
      parent <- unlist(dr$parents)
      if(is.character(parent)) parent else NA
    })) %>%
    tidyr::unnest(parents, .preserve=c('drive_resource')) # make it a single row per item-parent combination
  
  # navigate from the outermost directory down to the file to identify the file
  # by both its name and its directory location
  path_elements <- strsplit(relative_path, split='/')[[1]]
  path_df <- filter(relevant_files, id==googledrive::as_id(gd_config$folder))
  for(i in seq_along(path_elements)) {
    elem <- path_elements[i]
    parent <- path_df[[i,'id']]
    elem_row <- filter(relevant_files, name==elem, parents==parent)
    if(nrow(elem_row) == 1) {
      path_exists <- TRUE
      path_df <- bind_rows(path_df, elem_row)
    } else {
      path_exists <- FALSE
      path_df <- bind_rows(path_df, data_frame(id=NA))
      break
    }
  }
  
  return(path_df)
}


google_drive_put = function(file, local_file = file, mock_get=c('copy','move','none'),
  on_exists=c('update','replace','stop'), type=NULL, verbose=FALSE,
  dry_put=FALSE, config_file='lib/cfg/gd_config.yml') {
  
  # check arguments
  mock_get <- match.arg(mock_get)
  
  # tell R CMD check not to worry about symbols used for dplyr non-standard eval
  . <- name <- drive_resource <- '.dplyr.var'
  
  # decide whether local_source is an indicator or data file and find the data file
  local_file <- find_local_file(local_file)
  
  # identify the remote data file to be indicated by remote_ind
  data_file <- local_file
  
  # prepare to use google drive
  require_libaries('googledrive')
  gd_config <- yaml::yaml.load_file(config_file)
  
  # determine whether and where the remote file exists
  remote_path <- google_drive_locate_file(data_file, config_file)
  remote_id <- tail(remote_path$id, 1)
  
  # determine the last known parent, which is either already or soon to be the
  # proximate parent of the item to create
  parent <- remote_path %>% slice(nrow(remote_path)-1) %>% pull(id)
  
  # create the parent folder[s] on google drive as needed
  if(is.na(remote_id)) {
    # determine what exists
    remote_dirs <- remote_path %>%
      slice(-1) %>%slice(-nrow(.)) %>% # chop off the top parent-dir row and the bottom all-NA row
      pull(name)
    final_dirs <- strsplit(dirname(get_relative_path(data_file)), split='/')[[1]]
    
    # double-check that any overlapping path elements agree
    stopifnot(all.equal(final_dirs[seq_along(remote_dirs)], remote_dirs))
    
    # create any needed directories on google drive, updating the parent until
    # it's the proximate parent
    if(length(final_dirs) > length(remote_dirs)) {
      needed_dirs <- if(length(remote_dirs) == 0) {
        final_dirs
      } else {
        final_dirs[-seq_along(remote_dirs)]
      }
      # add the needed directories in order
      for(i in seq_along(needed_dirs)) {
        parent <- googledrive::drive_mkdir(name=needed_dirs[i], parent=googledrive::as_id(parent))$id
      }
    }
  }
  
  # post the file (create or update) from the local data file to Google Drive
  remote_file_changed <- TRUE # assume we'll be changing it
  if(is.na(remote_id)) {
    if(verbose) message("Uploading ", local_file, " to Google Drive")
    remote_id <- googledrive::drive_upload(media=local_file, path=googledrive::as_id(parent), type=type, verbose=verbose)$id
  } else {
    on_exists <- match.arg(on_exists)
    if(on_exists == 'stop') {
      stop('File already exists and on_exists==stop')
    } else {
      # check Drive to see whether the file we want to post is identical to what's already up there
      local_hash <- unname(tools::md5sum(local_file))
      remote_hash <- remote_path %>% slice(n()) %>% pull(drive_resource) %>% .[[1]] %>% .[['md5Checksum']]
      # if the local file is different from the file on Drive, update or replace it
      if(local_hash == remote_hash) {
        if(verbose) message("Not re-posting identical ", local_file, " to Google Drive")
        remote_file_changed <- FALSE
      } else {
        switch(
          on_exists,
          update={
            if(verbose) message("Updating ", local_file, " on Google Drive")
            remote_id <- googledrive::drive_update(googledrive::as_id(remote_id), media=local_file, verbose=verbose)$id
          },
          replace={
            if(verbose) message("Replacing ", local_file, " on Google Drive")
            googledrive::drive_rm(googledrive::as_id(remote_id), verbose=verbose)
            remote_id <- googledrive::drive_upload(media=local_file, path=googledrive::as_id(parent), type=type, verbose=verbose)$id
          }
        )
      }
    }
  }
  
  # write the indicator file
  if(remote_file_changed) {
    # most common case. involves another check on Google Drive, with patience in
    # case Drive doesn't fully recognized the new file right away
    retry_google_put(google_drive_confirmed_posted(file=file, config_file=config_file), verbose=verbose)
  }

  invisible()
}

find_local_file = function(local_file) {
  if(!file.exists(local_file)) {
    stop(paste('data file matching local_file does not exist:', local_file))
  }
  return(local_file)
}


get_relative_path = function(file) {
  
  # tell R CMD check not to worry about symbols used for dplyr non-standard eval
  . <- '.dplyr.var'
  
  file %>% 
    normalizePath(winslash='/', mustWork=FALSE) %>%
    gsub(normalizePath(getwd(), winslash='/'), '', .) %>% # remove the working directory if present
    gsub('^/', '', .) # remove the leading slash if present
}

require_libaries = function(...) {
  libs <- list(...)
  for(i in seq_along(libs)) {
    if(!requireNamespace(libs[[i]], quietly=TRUE)) {
      stop(paste0("library ", libs[[i]], " required but unavailable"))
    }
  }
}

retry_google_put = function(expr, max_wait_times=c(0, 3^c(0:4)), min_wait_frac=0.5, verbose=FALSE) {
  expr <- substitute(expr)
  env <- parent.frame(1)
  
  final_wait_times <- runif(n=length(max_wait_times), min=min_wait_frac*max_wait_times, max=max_wait_times)
  
  i <- 1
  do_retry <- TRUE
  if(verbose) message(sprintf("Trying \"%s\":", deparse(expr)))
  while(do_retry) {
    wait_time <- final_wait_times[i]
    if(verbose) message(sprintf("  Waiting %0.1f seconds before %sevaluating", wait_time, if(i>1) 're-' else ''))
    Sys.sleep(wait_time)
    result <- tryCatch(eval(expr, envir=env), error = function(e) e)
    i <- i + 1
    do_retry <- inherits(result, "error") && i <= length(final_wait_times)
  }
  
  if (inherits(result, "error")) {
    if(verbose) message(sprintf("  All retries used, still erroring on \"%s\"", deparse(expr)))
    stop(result)
  } else {
    return(result)
  }
}

google_drive_confirmed_posted = function(file, config_file='lib/cfg/gd_config.yml') {
  
  require_libaries('googledrive')
  
  # tell R CMD check not to worry about symbols used for dplyr non-standard eval
  . <- drive_resource <- '.dplyr.var'
  
  # look on Google Drive for the specified file
  data_file <- file
  remote_path <- google_drive_locate_file(data_file, config_file)
  remote_id <- tail(remote_path$id, 1)
  if(is.na(remote_id)){
    stop(paste('failed to find Google Drive file:', data_file))
  } else {
    remote_info <- remote_path %>% slice(n()) %>% pull(drive_resource) %>% .[[1]]
  }
}
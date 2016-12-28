library(RODBC, quietly = TRUE, verbose = FALSE)
library(neotoma)
library(dplyr)
library(tidyr)

datasets <- neotoma::get_dataset()

# A function to check decimal precision:
# `x` - a vector or a single numeric value.
prec <- function(x, minmax = 'min'){
  
}


test_dwc_export <- function(x){
  
  cat(paste0('Testing: ',x$dataset.meta$dataset.id, '_test_', x$dataset.meta$dataset.type, '\n'))
  
  if (x$dataset.meta$dataset.type %in% c('geochronologic', 
                                         'loss-on-ignition',
                                         'paleomagnetic',
                                         'energy dispersive X-ray spectroscopy (EDS/EDX)',
                                         'X-ray diffraction (XRD)',
                                         'X-ray fluorescence (XRF)',
                                         'charcoal',
                                         'physical sedimentology',
                                         'geochemistry',
                                         'water chemistry')) {
    odbcCloseAll()
    return(NULL)
  }
  
  dataset <- x$dataset.meta$dataset.id
  con <- RODBC::odbcDriverConnect('driver={SQL Server};server=SIMONGORING-PC\\SQLEXPRESS;database=Neotoma;trusted_connection=true')
  
  # For each dataset, get all the analysis units:
  query_out <- RODBC::sqlQuery(con, 
                        query = paste0("SELECT 'PhysicalObject' AS [dcterms:type], ds.RecDateCreated AS [gbif:year], 
                                        ds.RecDateModified AS [dcterms:modified], ds.DatasetID, smp.AnalysisUnitID, 
                                        smp.SampleID,
                                        cu.CollDate AS eventDate, 
                                        data.Value AS sampleSizeValue, au.AnalysisUnitID AS eventID, 
                                        cu.CollectionUnitID AS parentEventID, taxa.TaxonID, 
                                        taxa.TaxonName AS scientificName, taxa.TaxaGroupID, 
                                        taxa.Author AS scientificNameAuthorship, 
                                        eltyp.ElementType, elpor.Portion, elsym.Symmetry, elmat.Maturity,
                                        dst.DatasetType AS samplingProtocol, cu.Notes AS eventRemarks, 
                                        varu.VariableUnits AS sampleSizeUnit, sts.SiteID, sts.Altitude, 
                                        sts.SiteDescription AS locationRemarks, sts.SiteName, 
                                        sts.SiteDescription, sts.SiteName AS Expr1, 
                                        sts.LongitudeEast AS lonE, sts.LongitudeWest AS lonW, 
                                        sts.LatitudeNorth AS latN, sts.LatitudeSouth AS latS, 
                                        sage.Age, sage.AgeYounger, sage.AgeOlder, atyp.AgeType, 
                                        chron.IsDefault, chron.DatePrepared
                                       FROM NDB.Datasets AS ds INNER JOIN
                                       NDB.Samples AS smp ON smp.DatasetID = ds.DatasetID INNER JOIN
                                       NDB.CollectionUnits AS cu ON cu.CollectionUnitID = ds.CollectionUnitID LEFT OUTER JOIN
                                       NDB.AnalysisUnits AS au ON au.AnalysisUnitID = smp.AnalysisUnitID INNER JOIN
                                       NDB.Data AS data ON smp.SampleID = data.SampleID INNER JOIN
                                       NDB.Variables AS vari ON data.VariableID = vari.VariableID LEFT OUTER JOIN
                                       NDB.VariableElements AS variel ON vari.VariableElementID = variel.VariableElementID  LEFT OUTER JOIN
                                       NDB.ElementTypes AS eltyp ON variel.ElementTypeID = eltyp.ElementTypeID  LEFT OUTER JOIN
                                       NDB.ElementPortions as elpor ON variel.PortionID = elpor.PortionID  LEFT OUTER JOIN
                                       NDB.ElementSymmetries as elsym ON variel.SymmetryID = elsym.SymmetryID LEFT OUTER JOIN
                                       NDB.ElementMaturities as elmat ON variel.MaturityID = elmat.MaturityID LEFT OUTER JOIN
                                       NDB.VariableUnits AS varu ON vari.VariableUnitsID = varu.VariableUnitsID INNER JOIN
                                       NDB.Taxa AS taxa ON vari.TaxonID = taxa.TaxonID INNER JOIN
                                       NDB.DatasetTypes AS dst ON ds.DatasetTypeID = dst.DatasetTypeID INNER JOIN
                                       NDB.Sites AS sts ON cu.SiteID = sts.SiteID LEFT OUTER JOIN
                                       NDB.SampleAges AS sage ON smp.SampleID = sage.SampleID LEFT OUTER JOIN
                                       NDB.Chronologies AS chron ON sage.ChronologyID = chron.ChronologyID LEFT OUTER JOIN
                                       NDB.AgeTypes AS atyp ON chron.AgeTypeID = atyp.AgeTypeID
                                       WHERE        (ds.DatasetID = ",dataset,")"), stringsAsFactors = FALSE)
  
  if (nrow(query_out) == 0) {
    warning("No entry in the local database.")
    output <- c(NA, NA)
    write.csv(output, 
              paste0('dwc_test_output/empty_',dataset, '_test_', x$dataset.meta$dataset.type, '.csv'), 
              row.names = FALSE)
    odbcCloseAll()
    return(NULL)
  }
  
  # Bind all the modifiers on the element, excluding NAs
  query_out <- query_out %>% unite(Element_Full, ElementType, Portion, Symmetry, Maturity, sep = ";")
  
  query_out$Element_Full <- gsub("(NA;)|(;NA)*|(;$)", "", query_out$Element_Full, perl = TRUE)
  
  # Some of the datasets have multiple contacts, these need to be piped:
  contacts <- RODBC::sqlQuery(con, 
                       query = paste0("SELECT cnt.ContactName FROM
                                      NDB.Datasets AS ds INNER JOIN
                                      NDB.DatasetPIs AS dpi ON dpi.DatasetID = ds.DatasetID LEFT OUTER JOIN
                                      NDB.Contacts AS cnt ON cnt.ContactID = dpi.ContactID
                                      WHERE (ds.DatasetID = ",dataset,")"), stringsAsFactors = FALSE)
  if (nrow(contacts) > 0) {
    query_out$ContactName <- paste0(unlist(contacts),
                                    collapse = " | ")
  } else {
    warning("No contact name associated with this dataset.")
    query_out$ContactName <- NA
  }
  
  if (nrow(query_out) == 0) {
    warning("No output returned.\n")
    output <- c(NA, NA)
    write.csv(output, 
              paste0('dwc_test_output/empty_',dataset, '_test_', x$dataset.meta$dataset.type, '.csv'), 
              row.names = FALSE)
    odbcCloseAll()
    return(NULL)
  }
  
  if (all(query_out$TaxaGroupID %in% c("LAB", "LOI", "MAG", "ISO", "BIM", "CHM", "WCH", "GCH", "CHR", "SED"))) {
    warning("Dataset is not an acceptable type for upload to GBIF.")
    output <- c(NA, NA)
    write.csv(output, 
              paste0('dwc_test_output/empty_',dataset, '_test_', x$dataset.meta$dataset.type, '.csv'), 
              row.names = FALSE)
    odbcCloseAll()
    return(NULL)
  }
  
  if (any(query_out$TaxaGroupID %in% c("LAB", "LOI", "MAG", "ISO", "BIM", "CHM", "WCH", "GCH", "CHR", "SED", "UPA"))) {
    query_out <- subset(query_out, 
                        !TaxaGroupID %in% c("LAB", "LOI", "MAG", "ISO", "BIM", "CHM", "WCH", "GCH", "CHR", "SED", "UPA"))
  }

  # I get some full duplicates for some reason.  I don't see why.  For now I'm going to kill them:
  if (any(duplicated(query_out))) {
    warning("There's a duplicate row (for some reason).\n")
    output <- c(NA, NA)
    write.csv(output, 
              paste0('dwc_test_output/empty_',dataset, '_test_', x$dataset.meta$dataset.type, '.csv'), 
              row.names = FALSE)
    odbcCloseAll()
    return(NULL)
  }
  
  dup_rows <- c("AnalysisUnitID", "SampleID", "TaxonID", "sampleSizeUnit", "Element_Full")
  
  # This is the section that deals with multiple chronologies
  if (any(duplicated(query_out[,dup_rows]))) {
    cat("Multiple Default chronologies. . .\n")
    
    if (any(query_out$IsDefault) & !all(is.na(query_out$IsDefault))) {
      # Pull the default, but there isn't always a default. . . 
      query_out <- subset(query_out, IsDefault == TRUE)
    }
    
    # There's a hierarchy to the date types:
    age_hier <- c('Calendar years BP', 'Calendar years AD/BC', 'Varve years BP', 
                  'Calibrated radiocarbon years BP', 'Radiocarbon years BP')
    
    model_levels <- match(query_out$AgeType, age_hier)
    
    if (any(max(model_levels) > model_levels)) {
      query_out <- query_out[match(query_out$AgeType, age_hier) == max(model_levels),]
    }
    
    if (any(duplicated(query_out[,dup_rows]))) {
      # If there's still duplicates, take the most recent:
      recent <- which.max(as.Date(query_out$DatePrepared))
      if (length(recent) > 0) {
        query_out <- subset(query_out, DatePrepared == query_out$DatePrepared[recent])
      } else {
        warning("Remaining chronologies have no assigned dates, can't distinguish between them.")
        output <- c(NA, NA)
        write.csv(output, 
                  paste0('dwc_test_output/empty_',dataset, '_test_', x$dataset.meta$dataset.type, '.csv'), 
                  row.names = FALSE)
        odbcCloseAll()
        return(NULL)
      }
    }
    
    if (any(duplicated(query_out[,dup_rows]))) {
      # I should have caught all the errors, but if I didn't . . . 
      stop("There remain multiple default chronologies with the same date of development & age type.")
    }
  }
  
  # Start of year/End of year refers to the dates of collection.  Should be NA if no collection date is
  # reported, otherwise, should be the specific date of collection.
  # This should tell us whether or not the collection year was a leap year.  Unfortunately, many records
  # do not have a collection date.  If this is the case, I'm not sure what we do. . . 
  eoy <- data.frame(startDayOfYear = ifelse(is.na(query_out$eventDate), NA, 
                                            as.numeric(strftime(query_out$eventDate, format = '%j'))),
                    endDayOfYear   = ifelse(is.na(query_out$eventDate), NA, 
                                            as.numeric(strftime(query_out$eventDate, format = '%j'))))
  
  query_out$sampleSizeUnit[query_out$sampleSizeUnit %in% 'NISP'] <- "Number of Identified Samples"
  query_out$sampleSizeUnit[query_out$sampleSizeUnit %in% 'MNI'] <- "Minimum Number of Individuals"
  
  # Find the appropriate age &cetera:
  ages <- read.csv('data/geolage.csv', stringsAsFactors = FALSE)
  age_bin <- data.frame(agePoint      = findInterval(query_out$Age/1e6, ages$End),
                        youngInterval = findInterval(query_out$AgeYounger/1e6, ages$End),
                        oldInterval   = findInterval(query_out$AgeYounger/1e6, ages$End))
  
  for (i in 1:nrow(age_bin)) {
    
    # Assign epochs & ages numerically using the interval.  For ages with younger & older limits
    # then go through & assign them to the age interval:
    if (is.na(age_bin$youngInterval[i] & age_bin$oldInterval[i] & !is.na(age_bin$agePoint[i]))) {
      age_bin$youngInterval[i] <- age_bin$oldInterval[i] <- age_bin$agePoint[i]
    }
    
    age_bin$earliestPeriodOrLowestSystem[i] <- ages$Period[age_bin$oldInterval[i]]
    age_bin$latestPeriodOrHighestSystem[i]  <- ages$Period[age_bin$youngInterval[i]]
    age_bin$earliestEpochOrLowestSeries[i]  <- ages$Epoch[age_bin$oldInterval[i]]
    age_bin$latestEpochOrHighestSeries[i]   <- ages$Epoch[age_bin$youngInterval[i]]
    age_bin$earliestAgeOrLowestStage[i]     <- ages$Age[age_bin$oldInterval[i]]
    age_bin$latestAgeOrHighestStage[i]      <- ages$Age[age_bin$youngInterval[i]]
    
  }
  
  pubs <- RODBC::sqlQuery(con, paste0("SELECT STUFF((SELECT '|' + pubs.Citation ", 
                               "FROM NDB.DatasetPublications AS dsp INNER JOIN ", 
                               "NDB.Publications AS pubs ON ",
                               "dsp.PublicationID = pubs.PublicationID ", 
                               "WHERE (dsp.DatasetID = ",dataset,") ",
                               "FOR XML PATH('')),1,1,'') AS associatedReferences"),
                   stringsAsFactors = FALSE)[[1]]
  
  if (is.na(pubs)) {
    # If there are no publications associated with the record, generate a
    # "data" citation, pointing to the raw dataset:
    pubs <- gsub('..', '.', paste0(ifelse(is.na(query_out$ContactName), "Anon.", query_out$ContactName),
                                   ". ", 
                                   format(as.POSIXct(query_out$`gbif:year`), "%Y"), ". ",
                                   query_out$siteName, " ",
                                   gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", query_out$samplingProtocol, perl = TRUE), 
                                   ' Dataset. Neotoma Paleoecological Database. Dataset: ', dataset,
                                   ' URL: apps.neotomadb.org/explorer/?datasetid=', dataset),
                 fixed = TRUE)
  }
  
  output <-  data.frame("dcterms:type"         = query_out$`dcterms:type`,
                        "basisOfRecord"        = "FossilSpecimen",
                        "dcterms:modified"     = as.Date(query_out$`dcterms:modified`),
                        "dcterms:language"     = "en-US",
                        "dcterms:license"      = "http://creativecommons.org/licenses/by/4.0/deed.en_US",
                        "dcterms:rightsHolder" = query_out$ContactName,
                        "dcterms:accessRights" = "public",
                        "dcterms:bibliographicCitation" = pubs,
                        "gbif:year"            = format(as.POSIXct(query_out$`gbif:year`), "%Y"),
                        "dcterms:references"   = paste0("http://apps.neotomadb.org/explorer/?datasetid=", dataset),
                        locationID             = paste0("http://apps.neotomadb.org/explorer/?siteids=", query_out$SiteID),
                        locality               = query_out$SiteName,
                        locationRemarks        = query_out$SiteDescription,
                        collectionID           = paste0("Neotoma Analysis Unit ", query_out$AnalysisUnitID),
                        datasetID              = paste0("http://api.neotomadb.org/v1/data/datasets/", dataset),
                        #institutionCode       = NA, # I can't find the link here. . .
                        datasetName            = paste0(query_out$SiteName, " ", 
                                                        query_out$samplingProtocol, ' dataset'),
                        dynamicProperties      = paste0('"{"estimatedSampleAge":', query_out$Age,',',
                                                        '"latestSampleAge":', query_out$AgeOlder,',',
                                                        '"earliestSampleAge":', query_out$AgeYounger,',',
                                                        '"sampleAgeType":', query_out$AgeType,',',
                                                        '"sampleAgeDatum":','}"'),
                        occurrenceID           = paste0('Neotoma_occ_', 
                                                        query_out$AnalysisUnitID, '-', 
                                                        query_out$TaxonID),
                        recordedBy             = query_out$ContactName,
                        "dwc:preparations"     = ifelse(query_out$Element_Full == "", NA, query_out$Element_Full),
                        occurrenceStatus       = "present",
                        associatedReferences   = ifelse(regexpr("Neotoma Paleoecological", pubs) > -1, NA, pubs),
                        eventID                = paste0("AnalysisUnit_",query_out$eventID),
                        parentEventID          = paste0("CollectionUnit_",query_out$parentEventID),
                        eventDate              = format(as.POSIXct(query_out$eventDate), '%Y-%m-%d'),
                        startDayOfYear         = eoy$startDayOfYear,
                        endDayOfYear           = eoy$endDayOfYear,
                        year                   = format(as.POSIXct(query_out$eventDate), '%Y'),
                        month                  = format(as.POSIXct(query_out$eventDate), '%m'),
                        day                    = format(as.POSIXct(query_out$eventDate), '%d'),
                        samplingProtocol       = paste0("Neotoma ", query_out$samplingProtocol, " dataset"),
                        sampleSizeValue        = query_out$sampleSizeValue,
                        sampleSizeUnit         = query_out$sampleSizeUnit,
                        eventRemarks           = query_out$eventRemarks,
                        minimumElevationInMeters = query_out$Altitude,
                        maximumElevationInMeters = query_out$Altitude,
                        decimalLatitude        = mean(c(query_out$latN,
                                                        query_out$latS)),
                        decimalLongitude       = mean(c(query_out$lonE,
                                                        query_out$lonW)),
                        geodeticDatum          = "EPSG:4326",
                        coordinatePrecision    = max(c(abs(query_out$lonE - query_out$lonW),
                                                       abs(query_out$latS - query_out$latN))),
                        footprintWKT           = paste0("POLYGON ((", 
                                                        query_out$lonE, " ", 
                                                        query_out$latN, ", ",
                                                        query_out$lonW, " ", 
                                                        query_out$latN, ", ",
                                                        query_out$lonW, " ", 
                                                        query_out$latS, ", ",
                                                        query_out$lonE, " ", 
                                                        query_out$latS, ", ",
                                                        query_out$lonE, ", ",
                                                        query_out$latN, "))"),
                        footprintSRS           = paste0('GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",',
                                                        'SPHEROID["WGS_1984",6378137,298.257223563]],',
                                                        'PRIMEM["Greenwich",0],UNIT["Degree",',
                                                        '0.0174532925199433]]'),
                        georeferencedBy        = query_out$ContactName,
                        earliestEonOrLowestEonothem  = "Phanerozoic",
                        latestEonOrHighestEonothem   = "Phanerozoic",
                        earliestEraOrLowestErathem   = "Cenozoic",
                        latestEraOrHighestErathem    = "Cenozoic",
                        earliestPeriodOrLowestSystem = age_bin$earliestPeriodOrLowestSystem,
                        latestPeriodOrHighestSystem  = age_bin$latestPeriodOrHighestSystem,
                        earliestEpochOrLowestSeries  = age_bin$earliestEpochOrLowestSeries,
                        latestEpochOrHighestSeries   = age_bin$latestEpochOrHighestSeries,
                        earliestAgeOrLowestStage     = age_bin$earliestAgeOrLowestStage,
                        latestAgeOrHighestStage      = age_bin$latestAgeOrHighestStage,
                        #lithostratigraphicTerms = ,
                        identificationQualifier = NA,
                        taxonID                      = paste0("Neotoma_taxon - ", query_out$TaxonID),
                        identifiedBy                 = query_out$ContactName,
                        scientificName               = query_out$scientificName,
                        scientificNameAuthorship     = query_out$scientificNameAuthorship,
                        stringsAsFactors = FALSE)
  
  if (length(grep("?",   query_out$scientificName, fixed = TRUE)) > 0) {
    output$identificationQualifier[grep("?",   query_out$scientificName, fixed = TRUE)] <- "?"  
  }
  
  if (length(grep("cf.",   query_out$scientificName, fixed = TRUE)) > 0) {
    output$identificationQualifier[grep("cf.", query_out$scientificName, fixed = TRUE)] <- "cf."
  }
  
  if (length(grep("type",   query_out$scientificName, fixed = TRUE)) > 0) {
    output$identificationQualifier[grep("-type", query_out$scientificName, fixed = TRUE)] <- "type"
  }
  
  if (length(grep("aff.",   query_out$scientificName, fixed = TRUE)) > 0) {
    output$identificationQualifier[grep("aff.",   query_out$scientificName, fixed = TRUE)] <- "aff."
  }
  
  if (length(grep("undiff.",   query_out$scientificName, fixed = TRUE)) > 0) {
    output$identificationQualifier[grep("undiff.",   query_out$scientificName, fixed = TRUE)] <- "undiff."
  }
  
  # Post hoc modifications. This uses the output and then adds content:
  # This is for concatenated strings, like "Cooccurs with"
  
  for (i in 1:nrow(output)) {
    # Add the cooccurrance data, parsing all taxa (or occurrence IDs) present in the
    # analysis unit (except the current sample)
    cooccurrance <- output$scientificName[which(output$collectionID == output$collectionID[i])]
    cooccurranceID <- output$occurrenceID[which(output$collectionID == output$collectionID[i])]
    
    # except the current sample:
    cooccurrance <- cooccurrance[!cooccurrance %in% output$scientificName[i]]
    cooccurranceID <- cooccurranceID[!cooccurranceID %in% output$occurrenceID[i]]
    
    if (length(cooccurrance) > 0) {
      output$associatedTaxa[i] <- paste0("cooccurrs with:", cooccurrance, collapse = " | ")
      output$associatedOccurrences[i] <- paste0("cooccurrs with:", cooccurranceID, collapse = " | ")
    } else {
      output$associatedTaxa[i] <- ""
      output$associatedOccurrences[i] <- ""
    }
    
  }
  
  for (i in 1:nrow(output)) {
    # assign samplingEffort:
    samp_eff <- sum(output$sampleSizeValue[which(output$collectionID == output$collectionID[i] & 
                                                   output$sampleSizeUnit == output$sampleSizeUnit[i])])
    output$samplingEffort[i] <- samp_eff
  }
  
  if (length(unique(query_out$SiteID)) == 1) {
    # Match the geopolitical data:
    geopol <- RODBC::sqlQuery(con, 
                       paste0("SELECT * FROM NDB.SiteGeoPolitical AS sgp INNER JOIN NDB.GeoPoliticalUnits as gpl ON sgp.GeoPoliticalID = gpl.GeoPoliticalID WHERE (sgp.SiteID =", unique(query_out$SiteID),")"),
                       stringsAsFactors = FALSE)
    
    if ("country" %in% geopol$GeoPoliticalUnit) {
      output$country <- geopol$GeoPoliticalName[which(geopol$GeoPoliticalUnit == "country")]
    }
    
    if (any(regexpr("county", geopol$GeoPoliticalUnit) > -1)) {
      output$county <- geopol$GeoPoliticalName[which(regexpr("county", geopol$GeoPoliticalUnit) > -1)]
    }
    
    if (any(regexpr("(^province)|(^territory)|(state \\()|(^state$)", geopol$GeoPoliticalUnit) > -1)) {
      output$stateProvince <- geopol$GeoPoliticalName[which(regexpr("(^province)|(^territory)|(state \\()|(^state$)", geopol$GeoPoliticalUnit) > -1)]
    }  
  } else {
    
    for (i in 1:nrow(output)) {
      
      # Match the geopolitical data:
      geopol <- sqlQuery(con, 
                         paste0("SELECT * FROM NDB.SiteGeoPolitical AS sgp INNER JOIN NDB.GeoPoliticalUnits as gpl ON sgp.GeoPoliticalID = gpl.GeoPoliticalID WHERE (sgp.SiteID =", query_out$SiteID[i],")"),
                         stringsAsFactors = FALSE)
      
      if ("country" %in% geopol$GeoPoliticalUnit) {
        output$country[i] <- geopol$GeoPoliticalName[which(geopol$GeoPoliticalUnit == "country")]
      }
      
      if (any(regexpr("county", geopol$GeoPoliticalUnit)) > -1) {
        output$county[i] <- geopol$GeoPoliticalName[which(regexpr("county", geopol$GeoPoliticalUnit) > -1)]
      }
      
      if (any(which(regexpr("(^province)|(^territory)|(state \\()|(^state$)", geopol$GeoPoliticalUnit) > -1))) {
        output$stateProvince[i] <- geopol$GeoPoliticalName[which(regexpr("(^province)|(^territory)|(state \\()|(^state$)", geopol$GeoPoliticalUnit) > -1)]
      }
    }
  }
  write.csv(output,
            paste0('dwc_test_output/', x$dataset.meta$dataset.id, '_test_', x$dataset.meta$dataset.type, '.csv'), 
            row.names = FALSE, na = "")
  odbcCloseAll()
}

ds_ids <-  sapply(datasets, function(x)x$dataset.meta$dataset.id)

# I don't have as many records locally as on the server.
ds_types <- sapply(datasets, function(x)x$dataset.meta$dataset.type)

failure <- NA

tests <- sapply(unique(ds_types), function(x)sample(which(ds_types %in% x), 30, replace = TRUE)) %>% as.numeric %>% unique

for (i in tests) {
  if (datasets[[1]]$dataset.meta$dataset.id < 19924) {
    dd <- try(test_dwc_export(datasets[[i]]))
  
    if ('try-error' %in% class(dd)) {
      failure <- na.omit(c(failure, i))
    }
  }
}

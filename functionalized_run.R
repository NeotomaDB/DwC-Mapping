library(RODBC, quietly = TRUE, verbose = FALSE)
library(neotoma)

datasets <- get_dataset()

test_dwc_export <- function(x){
  
  dataset_id <- x$dataset.meta$dataset.id
  con <- odbcDriverConnect('driver={SQL Server};server=SIMONGORING-PC\\SQLEXPRESS;database=Neotoma;trusted_connection=true')
  
  # Do this by dataset (I think this makes more sense)
  dataset <- dataset_id
  
  # For each dataset, get all the analysis units:
  query_out <- sqlQuery(con, 
                        query = paste0("SELECT        'dataset' AS [dcterms:type], ds.RecDateCreated AS [gbif:year], ds.RecDateModified AS [dcterms:modified], ds.DatasetID, smp.AnalysisUnitID, cu.CollDate AS eventDate, cnt.ContactName, 
                                       data.Value AS sampleSizeValue, au.AnalysisUnitID AS eventID, cu.CollectionUnitID AS parentEventID, taxa_1.TaxonID, taxa_1.TaxonName AS scientificName, taxa_1.Author AS scientificNameAuthorship, 
                                       dst.DatasetType AS samplingProtocol, cu.Notes AS eventRemarks, varu.VariableUnits AS sampleSizeUnit, sts.SiteID AS siteid, sts.Altitude AS altitude, sts.SiteDescription AS locationRemarks, 
                                       sts.SiteName AS siteName, sts.SiteDescription AS siteDescription, sts.SiteName AS Expr1, sts.LongitudeEast AS lonE, sts.LongitudeWest AS lonW, sts.LatitudeNorth AS latN, sts.LatitudeSouth AS latS, 
                                       sage.Age AS age, sage.AgeYounger AS ageYounger, sage.AgeOlder AS ageOlder
                                       FROM            NDB.Datasets AS ds INNER JOIN
                                       NDB.Samples AS smp ON smp.DatasetID = ds.DatasetID INNER JOIN
                                       NDB.CollectionUnits AS cu ON cu.CollectionUnitID = ds.CollectionUnitID INNER JOIN
                                       NDB.DatasetPIs AS dpi ON dpi.DatasetID = ds.DatasetID INNER JOIN
                                       NDB.Contacts AS cnt ON cnt.ContactID = dpi.ContactID INNER JOIN
                                       NDB.AnalysisUnits AS au ON au.AnalysisUnitID = smp.AnalysisUnitID INNER JOIN
                                       NDB.Data AS data ON smp.SampleID = data.SampleID INNER JOIN
                                       NDB.Variables AS vari ON data.VariableID = vari.VariableID INNER JOIN
                                       NDB.VariableUnits AS varu ON vari.VariableUnitsID = varu.VariableUnitsID INNER JOIN
                                       (SELECT        TaxonID, TaxonCode, TaxonName, Author, Valid, HigherTaxonID, Extinct, TaxaGroupID, PublicationID, ValidatorID, ValidateDate, Notes, RecDateCreated, RecDateModified
                                       FROM            NDB.Taxa AS Taxa
                                       WHERE        (TaxaGroupID <> 'LAB')) AS taxa_1 ON vari.TaxonID = taxa_1.TaxonID INNER JOIN
                                       NDB.DatasetTypes AS dst ON ds.DatasetTypeID = dst.DatasetTypeID INNER JOIN
                                       NDB.Sites AS sts ON cu.SiteID = sts.SiteID INNER JOIN
                                       NDB.SampleAges AS sage ON smp.SampleID = sage.SampleID
                                       WHERE        (ds.DatasetID = ",dataset,")"), stringsAsFactors = FALSE)
  
  # This should tell us whether or not the collection year was a leap year.  Unfortunately, many records
  # do not have a collection date.  If this is the case, I'm not sure what we do. . . 
  eoy <- data.frame(startDayOfYear = ifelse(is.na(query_out$eventDate), NA, 1),
                    endDayOfYear   = ifelse(as.numeric(format(as.POSIXct(query_out$eventDate), '%Y')) %% 100 %% 4 == 0, 366, 365))
  
  query_out$sampleSizeUnit[query_out$sampleSizeUnit %in% 'NISP'] <- "Number of Identified Samples"
  query_out$sampleSizeUnit[query_out$sampleSizeUnit %in% 'MNI'] <- "Minimum Number of Individuals"
  
  # Find the appropriate age &cetera:
  ages <- read.csv('data/geolage.csv', stringsAsFactors = FALSE)
  age_bin <- data.frame(agePoint      = findInterval(query_out$age/1e6, ages$End),
                        youngInterval = findInterval(query_out$ageYounger, ages$End),
                        oldInterval   = findInterval(query_out$ageYounger, ages$End))
  
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
  
  pubs <- sqlQuery(con, paste0("SELECT STUFF((SELECT '|' + pubs.Citation ", 
                               "FROM NDB.DatasetPublications AS dsp INNER JOIN ", 
                               "NDB.Publications AS pubs ON ",
                               "dsp.PublicationID = pubs.PublicationID ", 
                               "WHERE (dsp.DatasetID = ",dataset,") ",
                               "FOR XML PATH('')),1,1,'') AS associatedReferences"),
                   stringsAsFactors = FALSE)[[1]]
  
  if (is.na(pubs)) {
    # If there are no publications associated with the record, generate a
    # "data" citation, pointing to the raw dataset:
    pubs <- gsub('..', '.', paste0(query_out$ContactName, ". ", 
                                   format(as.POSIXct(query_out$`gbif:year`), "%Y"), ". ",
                                   query_out$siteName, " ", query_out$samplingProtocol, 
                                   ' dataset. Neotoma Paleoecological Database. Dataset:', dataset),
                 fixed = TRUE)
  }
  
  output <-  data.frame("dcterms:type"         = query_out$`dcterms:type`,
                        "dcterms:modified"     = as.Date(query_out$`dcterms:modified`),
                        "dcterms:language"     = "English",
                        "dcterms:license"      = "http://creativecommons.org/licenses/by/4.0/deed.en_US",
                        "dcterms:rightsHolder" = query_out$ContactName,
                        "dcterms:bibliographicCitation" = pubs,
                        "gbif:year"            = format(as.POSIXct(query_out$`gbif:year`), "%Y"),
                        #"dcterms:references"  = NA,  # link to external references
                        locationID             = paste0("Neotoma Site: ", query_out$siteid),
                        locality               = query_out$siteName,
                        locationRemarks        = query_out$siteDescription,
                        collectionID           = paste0("Neotoma Analysis Unit ", query_out$AnalysisUnitID),
                        datasetID              = paste0("http://api.neotomadb.org/v1/data/datasets?datasetids=", dataset),
                        #institutionCode       = NA, # I can't find the link here. . .
                        datasetName            = paste0(query_out$siteName, " ", 
                                                        query_out$samplingProtocol, ' dataset'),
                        occurrenceID           = paste0('Neotoma_occ_', 
                                                        query_out$AnalysisUnitID, '-', 
                                                        query_out$TaxonID),
                        recordedBy             = query_out$ContactName,
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
                        minimumElevationInMeters = query_out$altitude,
                        maximumElevationInMeters = query_out$altitude,
                        decimalLatitude        = mean(c(query_out$latN,
                                                        query_out$latS)),
                        decimalLongitude       = mean(c(query_out$lonE,
                                                        query_out$lonW)),
                        geodeticDatum          = "EPSG:4326",
                        coordinatePrecision    = max(c(abs(query_out$lonE - query_out$lonW),
                                                       abs(query_out$latS - query_out$latN))),
                        footprintWKT           = paste0("POLYGON ((", query_out$lonE, " ", 
                                                        query_out$latN, ", ",
                                                        query_out$lonW, " ", 
                                                        query_out$latN, ", ",
                                                        query_out$lonW, " ", 
                                                        query_out$latS, ", ",
                                                        query_out$lonE, " ", 
                                                        query_out$latS, "))"),
                        footprintSRS           = "EPSG:4326",
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
                        #identificationQualifier = ,
                        taxonID                      = paste0("Neotoma_taxon - ", query_out$TaxonID),
                        identifiedBy                 = query_out$ContactName,
                        scientificName               = query_out$scientificName,
                        scientificNameAuthorship     = query_out$scientificNameAuthorship,
                        stringsAsFactors = FALSE)
  
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
    
    output$associatedTaxa[i] <- paste0("cooccurrs with:", cooccurrance, collapse = "|")
    output$associatedOccurrences[i] <- paste0("cooccurrs with:", cooccurranceID, collapse = "|")
    
  }
  
  for (i in 1:nrow(output)) {
    # assign samplingEffort:
    samp_eff <- sum(output$sampleSizeValue[which(output$collectionID == output$collectionID[i] & 
                                                   output$sampleSizeUnit == output$sampleSizeUnit[i])])
    output$samplingEffort[i] <- samp_eff
  }
  
  for (i in 1:nrow(output)) {
    # Match the geopolitical data:
    geopol <- sqlQuery(con, 
                       paste0("SELECT * FROM NDB.SiteGeoPolitical AS sgp INNER JOIN NDB.GeoPoliticalUnits as gpl ON sgp.GeoPoliticalID = gpl.GeoPoliticalID WHERE (sgp.SiteID =", query_out$siteid[i],")"),
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
  
  write.csv(output, 
            paste0('dwc_test_output/',dataset_id, '_test_', x$dataset.meta$dataset.type, '.csv'), 
            row.names = FALSE)
  odbcCloseAll()
}

for(i in tests){try(test_dwc_export(datasets[[i]]))}

SELECT    'PhysicalObject' AS [dcterms:type], 
         ds.RecDateCreated AS [dcterms:date], 
        ds.RecDateModified AS [dcterms:modified], 
              ds.DatasetID, 
        smp.AnalysisUnitID, 
              smp.SampleID,
               cu.CollDate AS eventDate, 
                data.Value AS sampleSizeValue, 
         au.AnalysisUnitID AS eventID, 
       cu.CollectionUnitID AS parentEventID, 
              taxa.TaxonID, 
            taxa.TaxonName AS scientificName, 
          taxa.TaxaGroupID,
               taxa.Author AS scientificNameAuthorship, 
         eltyp.ElementType, 
             elpor.Portion, 
            elsym.Symmetry, 
            elmat.Maturity,
           dst.DatasetType AS samplingProtocol, 
                  cu.Notes AS eventRemarks, 
        varu.VariableUnits AS sampleSizeUnit, 
                sts.SiteID, 
              sts.Altitude, 
       sts.SiteDescription AS locationRemarks, 
              sts.SiteName, 
       sts.SiteDescription, 
              sts.SiteName AS Expr1, 
         sts.LongitudeEast AS lonE, 
         sts.LongitudeWest AS lonW, 
         sts.LatitudeNorth AS latN, 
         sts.LatitudeSouth AS latS, 
                  sage.Age, 
           sage.AgeYounger, 
             sage.AgeOlder, 
              atyp.AgeType, 
           chron.IsDefault, 
        chron.DatePrepared
FROM          NDB.Datasets AS ds INNER JOIN
               NDB.Samples AS smp   ON smp.DatasetID = ds.DatasetID INNER JOIN
       NDB.CollectionUnits AS cu    ON cu.CollectionUnitID = ds.CollectionUnitID LEFT OUTER JOIN
         NDB.AnalysisUnits AS au    ON au.AnalysisUnitID = smp.AnalysisUnitID INNER JOIN
                  NDB.Data AS data  ON smp.SampleID = data.SampleID INNER JOIN
             NDB.Variables AS vari  ON data.VariableID = vari.VariableID LEFT OUTER JOIN
      NDB.VariableElements AS varel ON vari.VariableElementID = varel.VariableElementID  LEFT OUTER JOIN
          NDB.ElementTypes AS eltyp ON varel.ElementTypeID = eltyp.ElementTypeID  LEFT OUTER JOIN
       NDB.ElementPortions as elpor ON varel.PortionID = elpor.PortionID  LEFT OUTER JOIN
     NDB.ElementSymmetries as elsym ON varel.SymmetryID = elsym.SymmetryID LEFT OUTER JOIN
     NDB.ElementMaturities as elmat ON varel.MaturityID = elmat.MaturityID LEFT OUTER JOIN
         NDB.VariableUnits AS varu  ON vari.VariableUnitsID = varu.VariableUnitsID INNER JOIN
                  NDB.Taxa AS taxa  ON vari.TaxonID = taxa.TaxonID INNER JOIN
          NDB.DatasetTypes AS dst   ON ds.DatasetTypeID = dst.DatasetTypeID INNER JOIN
                 NDB.Sites AS sts   ON cu.SiteID = sts.SiteID LEFT OUTER JOIN
            NDB.SampleAges AS sage  ON smp.SampleID = sage.SampleID               LEFT OUTER JOIN
          NDB.Chronologies AS chron ON sage.ChronologyID = chron.ChronologyID LEFT OUTER JOIN
              NDB.AgeTypes AS atyp  ON chron.AgeTypeID = atyp.AgeTypeID
WHERE 
     ds.DatasetID = ?
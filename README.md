# Mapping Neotoma against the DarwinCore Schema

<img src="c4pcdw.jpg" width="100">

This repository tracks the development of efforts to map Neotoma dataset records against the [DarwinCore](http://rs.tdwg.org/dwc/) schema to facilitate greater data discovery, reuse and sustainability of records archived within the Neotoma Paleoecological Database.  This project is part of the EarthCube Integrative Activities proposal between Neotoma and the Paleobiological Database, and is one step along the path to upload Neotoma records to [BISON](http://bison.usgs.ornl.gov/) and [GBIF](http://www.gbif.org/).

Initial work on this project was made possible through collaboration as part of the Cyber4Paleo Community Development Workshop in Boulder, CO, July, 2016.  Much of this work is archived as part of the Cyber4Paleo [GitHub organization](http://github.com/cyber4paleo) and [GitHub pages](cyber4paleo.github.io).

## Contributors

*We welcome contributions from any individual, whether code, documentation, or issue tracking.  All participants are expected to follow the [code of conduct](https://github.com/neotomadb/DwC-Mapping/blob/master/code_of_conduct.md) for this project.*

* [Simon Goring](http://simongoring.github.io) - Assistant Scientist, University of Wisconsin
* [Jack Williams](https://github.com/iceageecologist) - Professor, University of Wisconsin
* [Mark Uhen](http://github.com/markuhen)
* [Michael McLennan](https://github.com/mmcclenn)
* [John Wieczorek](http://github.com/tucotuco)

## Description

Mapping the Neotoma Database structure onto DarwinCore standards is relatively complex.  While some of the data structure maps easily, the content of the database, and the conceptual structure of the paleoecological records is not consistently equivalent to the semantic structure of the DarwinCore schema.  The `Rmd` has some simple relationships described in the markdown portion of the document, based on a cross-walk started by Michael McLennan, and extended by Jack Williams and Mark Uhen at the Cyber4Paleo Community Development Workshop.  Simon Goring developed this `Rmd` and implemented the actual conversion of the database structure to the csv file output.

The database itself is available as a SQL Server snapshot from the Neotoma Paleoecological Database's website [here](http://www.neotomadb.org/snapshots), or on [fighsare.org]() at the Neotoma Database Snapshot project [here](https://figshare.com/authors/Data_Backup_Neotoma_Paleoecological_Database/2741398).

# Multi-Study Movebank Locations

MoveApps

Github repository: github.com/movestore/multistudy_movebank_locations

## Description
Download movement tracks from several Movebank studies at once and combine them into one data set. The same settings (sensor types, species, time range, outliers, thinning, track definition, attribute minimisation) are applied to every selected study, and all animals of each study (optionally restricted to selected species) are downloaded. The studies are stacked into a single move2 object, ordered by track and time; locations without coordinates, duplicated location-time records and attributes containing only NAs are removed.

## Documentation
This App allows the direct download of animal movement data from more than one study stored on [Movebank](www.movebank.org) for which you have access [permissions](https://www.movebank.org/cms/movebank-content/permissions-and-sharing). It is the multi-study counterpart of the [Movebank (move2)](https://github.com/movestore/Movebank-Loc-move2) App: instead of selecting one study and a set of animals, you select one or more studies, and all animals of these studies are downloaded. Those data can be the start of workflows that then filter, visualise and/or analyse them.

**Step 1. Movebank Login.** Provide your Movebank account credentials, or select an account for which you have already provided credentials. Account information will be saved within MoveApps, however these credentials are *not* passed on when you share MoveApps workflows.

**Step 2. Studies.** Select one or more studies. For each study, the number of animals, the number of locations (events), sensor types, license, and the date of the first and most recent location are provided. In some cases, you will be asked to read and agree to a license agreement set by the data owner before proceeding. If you receive a message "No data are available for download" for a study, this may be because you do not have access, because there are no data in the study, or because the data in the study have not been associated with animals. If you are a Data Manager for the study, you can [add data](https://www.movebank.org/cms/movebank-content/add-data) or [deployments](https://www.movebank.org/cms/movebank-content/upload-qc#add_deployments) for the study in Movebank.

**Step 3. Species.** All species contained in the selected studies are listed. Select one, several or all of them; additionally, the option "Include tracks without associated species name" can be selected to also retain tracks for which no species is stored in Movebank. At least one entry has to be selected before proceeding.

**Step 4. Options.** Choose the sensor types, time range, data resolution, track definition and the other options described under *Settings* below. All options are applied to all selected studies.

**Step 5. Overview.** Review your selections. To make changes, select "Back", or select "Finish" to confirm and proceed. This summary is also helpful to remind you of your selections later: select the "i" button of this App and then "Settings" to see this overview or make changes at any time.

**Combining the studies.** The studies are downloaded one after the other and stacked into one move2 object. Then the track definition chosen by the user is applied to the combined data set, followed by the removal of duplicated location-time records and the optional thinning. If tracks are defined by animal ID and the same animal ID occurs in more than one study, the study ID is appended to the track ID of these animals (e.g. `Ronja_21231406`) to keep them separate; the log lists the affected IDs. If the App receives input data from a previous App, the downloaded data are appended to them.

:warning: For large or many studies, data transfer might take a long time. Use the options to reduce the size of the request to only the necessary data. After running the App, you can "Pin" your workflow to this App so that it will not need to repeat the download when rerunning subsequent Apps in the workflow.

:warning: This App repeatedly retries data download (for up to 30 min) in case Movebank is down. If Movebank cannot be reached within that time, the App stops with an error and returns no data, also if some studies had already been downloaded, so that no incomplete data set is passed on to the workflow. Any other problem (e.g. no data in the selected time range, no access) concerns only the affected study, which is skipped with an error message in the log while the remaining studies are downloaded.

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. It downloads any location data stored in Movebank studies that the user has access to.

#### Required data properties
The App does not require input data; it is usually the first App of a workflow. If input data are provided, they must be of type `move2::move2_loc` and use the same track definition (see setting *Attribute that will define and identify the tracks*) as this App, typically because they come from another Movebank App.

### Input type
none or `move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
`citation_metadata.csv`: table with one row per attribute and one column per study containing, where available in the study, the study ID, study name, taxon IDs, principal investigator, contact person, citation, license terms, license type, and the date of download.

### Settings 
All settings are provided by the Movebank data source interface of MoveApps (there are no settings in the `appspec.json`). The same values are used for the download of all selected studies.

`Movebank Login` (username, password): Movebank account credentials. Only studies for which this account has download access can be downloaded.

`Studies` (study): One or more Movebank study IDs. All animals of each study are downloaded.

`Sensor Selection` (select_sensors): Location sensor types to include (at least 1 sensor must be selected). The same sensor types are requested from all studies; a study that does not contain any of the selected sensor types yields no data and is skipped. (*Tip:* Different accuracy or sampling rates between sensor types can affect appropriate settings for subsequent Apps. You can create separate workflows or workflow instances to run sensor-specific analysis.)

`Species` (keep_species): The species (scientific names as stored in Movebank, attribute `taxon_canonical_name`) to retain; one, several or all of the species contained in the selected studies can be selected. The option `Include tracks without associated species name` (`no_species`) also retains tracks for which no species is stored in Movebank. At least one entry must be selected. After the download of each study, tracks of any other species are dropped; if none of the tracks of a study belongs to the selected species, the study is skipped with an error message in the log. At the end of the log, the number of individuals per species (and without species, if that option was selected) contained in the downloaded data is reported.

`Start Date` and `End Date` (timestamp_start, timestamp_end): Optional start and/or end timestamp to restrict the download to a specific time range. If, for a study, the start date lies after its last deployed location or the end date before its first one, the App logs an error stating the study's date, skips this study and continues with the next one. Default: not set (full time range).

`Data Resolution` (thin, thin_numb, thin_unit): By default (`thin` = `FALSE`), data will be accessed in full resolution. Alternatively, you can select to request data that is thinned to one location per specified interval (e.g. one per 6 hours), retaining the first record of each interval. Intervals are aligned to the clock (e.g. 00:00, 06:00, 12:00, 18:00), so the first location of each track usually falls into an incomplete interval and is therefore removed; a track consisting of a single location is kept as is. Note that no additional locations are interpolated at possible gaps. Default interval: `6 hours`.

`Attribute that will define and identify the tracks` (trackid): The attribute by which the tracks are defined. Options: (a) Animal ID (`indv`, individual local identifier), (b) Deployment ID (`deploy`, either user defined or a number generated by Movebank) or (c) the combination of animal ID and deployment ID separated by an underscore (`indv_deploy`). Default: `indv`. All Movebank Apps in a workflow must use the same option; otherwise the data cannot be combined and the App stops with an error.

`Argument Minimisation` (minarg): To reduce dataset size, you can choose to restrict the downloaded event attributes to a minimum set: animal ID, tag ID, deployment ID, sensor type, timestamp, latitude and longitude (plus [visible](http://vocab.nerc.ac.uk/collection/MVB/current/MVB000209/) when outliers are excluded). Track-level attributes such as species, sex or deployment details are always downloaded in full, regardless of this option. Consider whether you will need additional event information (e.g. altitude, speed, temperature) for subsequent steps of your analysis. Default: `FALSE`.

`Include Outliers` (incl_outliers): Include records flagged as outliers in Movebank. We strongly recommend that you leave this unchecked. Only select it when you are familiar with the data, for example, if you want to ignore filtering steps taken in Movebank to apply your own filtering methods in subsequent steps of your workflow. We recommend flagging outliers in Movebank ([see instructions](https://www.movebank.org/cms/movebank-content/deployments-and-outliers#mark_outliers)). Default: `FALSE`.

`Download most recent data` (lastXdays): Download the data of a certain number of days before NOW. This option only makes sense for data with live feed into Movebank. If this option is chosen, `Start Date` and `End Date` will be ignored. The reference time ("now") is fixed once, when the App starts, so that all studies are downloaded for exactly the same time window even if the downloads take a while. Default: not set.

`Fast data reduction profiles` (event_reduc): Movebank provides fast, reduced data download options: `EURING_01` downloads the full tracks in a resolution of 1 location per day, `EURING_03` downloads the complete tracks of the last 30 days (last 30 days of each track, independently of when it ended). Note that `Start Date`, `End Date` and `Download most recent data` will be ignored when a profile is selected. Default: not set.

### Changes in output data
The output contains the locations of all selected studies. The following modifications are applied to the downloaded data:

- The track ID column is the attribute chosen with `trackid`: `individual_local_identifier`, `deployment_id` or the new attribute `individual_name_deployment_id` (animal ID and deployment ID joined by an underscore). Depending on this choice, `deployment_id` or `individual_local_identifier` is an event attribute (if it varies within a track) or a track attribute.
- Track IDs are converted to syntactically valid names (`make.names()`): characters other than letters, numbers, dot and underscore are replaced by a dot, and IDs starting with a number get an `X` prepended (e.g. `742` becomes `X742`). IDs that only differ in such characters are kept distinct.
- If tracks are defined by animal ID and the same animal ID occurs in more than one study, the study ID is appended to the track ID of these animals (e.g. `Ronja_21231406`).
- If a species selection is set, only tracks whose `taxon_canonical_name` is among the selected species (or has no species, if `no_species` is selected) are retained.
- Locations are ordered by track and time. Locations with empty coordinates or a missing latitude or longitude are removed.
- Duplicated location-time records (same track ID and timestamp) are not allowed; of such duplicates, the record with the least number of attributes containing NA is retained, the first one if still duplicated.
- If a track attribute has several values for one track (e.g. several tags of one animal when tracks are defined by animal ID), these values are joined into one comma-separated character string.
- Event and track attributes that only contain NAs are removed.
- If thinning is selected, only the first location per interval is retained (see *Settings*).
- If input data are provided, the downloaded data are appended to them (`move2::mt_stack()`); track IDs that already exist in the input data are renamed with a numeric suffix. If the input data use a different coordinate reference system, the downloaded data are re-projected to match.

### Errors and null handling
**Studies:** A study for which no data can be downloaded is skipped with an error message in the log, and the App continues with the remaining studies; a warning at the end of the log lists how many studies returned data. Animals without data meeting the selection criteria (sensor type, time range, outlier setting) are simply absent from the returned data set; no warning is given for them. If none of the selected studies returns data, if no study or no sensor type is selected, or if Movebank could not be reached for 30 minutes, the App returns NULL, which stops the workflow.

**Common error** *"Study ...: No data are available in the selected time range (...) for the selected sensors"*, *"Your start timestamp is set after the last deployed location of the study"* / *"Your end timestamp is set before the first deployment location of the study"*: for this study the combination of sensors and time range yields no locations. The study is skipped; the other studies are still downloaded. Check the first/last location dates of the study and adjust the `Start Date`/`End Date` or the `Download most recent data` setting.

**Setting `keep_species`** *"Study ...: None of the tracks belongs to the selected species"*: the study contains no track of the selected species; it is skipped, the other studies are still downloaded. Select the species of the study in step 3, or `Include tracks without associated species name` if the tracks of the study have no species assigned.

**Common error** *"Study ...: download failed, this study is skipped"*: Movebank refused the request for this study (e.g. no download access, license not accepted, sensor type not present in the study). The message contains the reason given by Movebank.

**Common error** *"Movebank could not be reached while downloading study ..."*: Movebank did not respond for 30 minutes. The App returns NULL; try again later.

**Common error** *"The downloaded studies cannot be combined into one data set"*: an attribute has different data types in different studies, so the studies cannot be stacked. This should not happen with regular Movebank attributes; the message contains the affected attribute. Download the studies with separate Movebank Apps.

**Input data** *"The downloaded data cannot be combined with the input data: the track ID column differs"*: several Movebank Apps in the workflow use different settings for the `Attribute that will define and identify the tracks`. Select the same option in all of them.

:warning: This App does not check for records with timestamps that are in the future. Such obviously wrong timestamps are retained, and may cause errors or unexpected results in later steps of your workflow. They can be flagged as outliers in Movebank by a Data Manager for the study ([see instructions](https://www.movebank.org/cms/movebank-content/deployments-and-outliers#mark_outliers)) or filtered in subsequent steps of your workflow, e.g., with the [Remove Outliers App](https://github.com/movestore/RemoveOutliers). Such timestamps can represent erroneous values provided by the tag or be caused by incorrectly defining the field format when data were imported to Movebank (a Data Manager for the study can review and reimport data if needed following [these instructions](https://www.movebank.org/cms/movebank-content/upload-qc#fix_incorrectly_mapped_values)).

### Technical details
- The download relies on `move2::movebank_download_study()`; sensor types, study information and study attributes (needed for the event reduction profiles) are retrieved with `move2::movebank_retrieve()` and `move2::movebank_download_study_info()`. Every Movebank request is wrapped in a retry loop (up to 30 min with increasing waiting times) that only gives up early for errors that concern the request itself (no data, no access, license not accepted, invalid sensor type).
- Each study is downloaded separately with identical arguments, filtered to the selected species (`move2::filter_track_data()` on `taxon_canonical_name`) and put on `deployment_id` as track ID (one track per deployment). This keeps the attribute types identical across studies, so that they can be stacked with `move2::mt_stack()`. Only then is the track definition chosen by the user applied to the combined data set (`move2::mt_set_track_id()`), followed by the removal of duplicates (`move2::mt_filter_unique()`) and the optional thinning (`move2::mt_filter_per_interval()`).
- The time window for `Download most recent data` is computed once at the start of the App run, in UTC, so that all studies share the same window regardless of how long the previous downloads took. Timestamps are handled in UTC throughout.
- Data are downloaded and returned in geographic coordinates (EPSG:4326, longitude/latitude); they are only re-projected if input data with another coordinate reference system are provided.
- Runtime and memory scale with the total number of locations of all selected studies; the studies are downloaded sequentially.

### References
none

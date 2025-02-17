# cfpb.api
A R package that allows users to query Consumer Financial Protection Bureau public complaint data. This repository was adapted and expanded from Rob Carnell's code here (https://github.com/bertcarnell/cfpb.api)

There are five main ways to use this package:

  - read in all public complaints
  - form a query to return aggregate counts of complaints
  - query a specific complaint based on the complaint id
  - form a query to return a batch of raw complaint data
  - form a query to return longitudinal complaint trends
  
## Read in all public complaints

To read in all public complaints, use call `get_all_complaints()`. This function downloads a temporary file and reads it into the workspace. Users should be aware that this is a relatively large file. As of this writing (January 2024), there are around 4.5 million complaints, and the CFPB has been handling an increasing volume of complaints over time, surpassing 100k per month since the spring of 2023.

## Form a query to return aggregate counts of complaints

There are a set of functions to return complaints aggregated to either the state (`query_states()`), product (`query_product()`) or issue (`query_issue()`). These functions will return a dataframe providing counts of complaints that meet the search criteria, aggregated to the relevant unit.

## Query a specific complaint based on the complaint id

If you have a particular complaint you would like to retrieve, you can call `query_complaint_id()` and get the full record for that complaint.

## Form a query to return a batch of raw complaint data

The `query_complaints()` function will return a dataframe containing all complaint records that match the search criteria.

## Form a query to return longitudinal complaint trends

This `query_trends()` function will return a dataframe of of complaint counts over time that match the search criteria. The trend interval options are year (default), quarter, month, week, or day.

## Helper functions

There is currently one primary helper function. If you wish to narrow complaints by company, `query_companies()` allows you to find the proper name for a company as listed in the database.

Values for some other search parameters (e.g. `company_response`) can be found in the help page for the relevant function. Some search parameters have many values (e.g. `company_public_response`) and will ultimately have helper functions of their own. Until then, we advise pulling a sample of complaints and examining the value options in the raw data, or finding the value options using the dashboard at https://www.consumerfinance.gov/data-research/consumer-complaints/search.

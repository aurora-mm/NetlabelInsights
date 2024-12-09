# Netlabel Insights: Enough Records

The Netlabel Insights dashboard for [Enough Records](https://archive.org/details/enough_records) provides an overview of download data from [the Internet Archive](https://archive.org) across three historical periods. It allows users to explore the distribution of artist downloads and generate a hypothetical compilation using [stratified sampling](https://en.wikipedia.org/wiki/Stratified_sampling).

![](screenshot.png?raw=true)

# Features

The dashboard displays data on the artists with the highest mean download counts over specific time periods in the label’s history. It also shows the distribution of download counts for albums during these periods, as well as the trend in download counts with each subsequent album release.

The **Compilation Maker** generates a list of 10-14 artists for a hypothetical Enough Records compilation, based on historical data and stratified sampling. The compilation would feature artists whose albums have received fewer than 500 downloads, between 500 and 2000 downloads, and more than 2000 downloads. This ensures a mix of emerging and established artists from all time periods. Adjusting the Number of Artists slider generates a new compilation suggestion, drawn from Enough Records artist pool.

The app retrieves and processes data from the Internet Archive. Data is cached locally to improve performance and minimize repeated API calls. Artist name normalization resolves variations in artist naming (e.g., "Julien Palomo & Fabien Robbe" and "Fabien Robbe & Julien Palomo").

# Dependencies

`shinythemes`, `tidyverse`, `cowplot`, `internetarchive`

# Installation

To run the app directly from GitHub, run `shiny::runGitHub("NetlabelInsights", "aurora-mm", subdir = "EnoughRecords/")` from your [R](https://www.r-project.org) environment.

# Author

Linn Friberg

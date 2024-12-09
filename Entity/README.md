# Netlabel Insights: Entity

The Netlabel Insights dashboard for [Entity](https://archive.org/details/entity) provides an overview of download data from [the Internet Archive](https://archive.org). It allows users to explore the distribution of artist downloads and generate a hypothetical compilation using [stratified sampling](https://en.wikipedia.org/wiki/Stratified_sampling).

![](screenshot.png?raw=true)

# Features

The dashboard displays data on the artists with the highest mean download counts in the label’s history. It also shows the distribution of download counts for albums, as well as the trend in download counts with each subsequent album release.

The **Compilation Maker** generates a list of 10-14 artists for a hypothetical Entity compilation, based on historical data and stratified sampling. The compilation would feature artists whose albums have received fewer than 2000 downloads, between 2000 and 7000 downloads, and more than 7000 downloads. This ensures a mix of emerging and established artists. Adjusting the Number of Artists slider generates a new compilation suggestion, drawn from Entity artist pool.

The app retrieves and processes data from the Internet Archive. Data is cached locally to improve performance and minimize repeated API calls. Artist name normalization resolves variations in artist naming.

# Dependencies

`shinythemes`, `tidyverse`, `cowplot`, `internetarchive`

# Installation

To run the app directly from GitHub, run `shiny::runGitHub("NetlabelInsights", "aurora-mm", subdir = "Entity/")` from your [R](https://www.r-project.org) environment.

# Author

Linn Friberg

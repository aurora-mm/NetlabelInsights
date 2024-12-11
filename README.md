# Overview

This repository contains a set of four interactive [R Shiny](https://shiny.posit.co) dashboards designed to explore and visualize download data from various netlabels hosted on [the Internet Archive](https://archive.org). Each dashboard provides insights into historical download patterns, artist distributions, and trends specific to a given netlabel. The dashboards also enable users to generate hypothetical compilations based on [stratified sampling](https://en.wikipedia.org/wiki/Stratified_sampling) of the data.

# Available Dashboards

[Enough Records](https://github.com/aurora-mm/NetlabelInsights/tree/main/EnoughRecords)

[Entity](https://github.com/aurora-mm/NetlabelInsights/tree/main/Entity)

[Mahorka](https://github.com/aurora-mm/NetlabelInsights/tree/main/Mahorka)

[Treetrunk Records](https://github.com/aurora-mm/NetlabelInsights/tree/main/TreetrunkRecords)

Each dashboard focuses on a specific netlabel and offers insights into the corresponding statistics over time.

# Key Features

* Each dashboard shows artist download data across different historical periods, allowing users to view trends and distributions of album downloads.
* **Compilation Maker** generates a hypothetical compilation of artists, ensuring a mix of emerging and established artists based on historical download data.

# Dependencies

`shinythemes`, `tidyverse`, `cowplot`, `internetarchive`

# Installation

To run any of the apps directly from GitHub, use the following command in your [R](https://www.r-project.org) environment:

`shiny::runGitHub("NetlabelInsights", "aurora-mm", subdir = "EnoughRecords/")  # For the Enough Records dashboard`
`shiny::runGitHub("NetlabelInsights", "aurora-mm", subdir = "Entity/")  # For the Entity dashboard`
`shiny::runGitHub("NetlabelInsights", "aurora-mm", subdir = "Mahorka/")  # For the Mahorka dashboard`
`shiny::runGitHub("NetlabelInsights", "aurora-mm", subdir = "TreetrunkRecords/")  # For the Treetrunk Records dashboard`

# Author

Linn Friberg





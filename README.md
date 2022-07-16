# Treetrunk Records Insights

The **Treetrunk Records Insights** dashboard presents data on artists with the highest mean download counts during specific time periods in the history of [Treetrunk Records](https://archive.org/search.php?query=collection%3Atreetrunk&sort=-publicdate). It also shows [^1] the distribution of download counts for albums during these time periods, as well as how download counts change with each successive album release over time. The dataset includes all albums released on Treetrunk Records through June 6th, 2022. [^2]

The **Compilation Maker** provides a selection of 10-14 artists for a hypothetical Treetrunk Records compilation based on historical data and [stratified sampling](https://www.scribbr.com/methodology/stratified-sampling/). The proposed compilation would include artists whose albums have received less than 1000 downloads, between 1000 and 10000 downloads, and more than 10000 downloads, respectively. As a result, both emerging and established artists from all three time periods would be provided with an opportunity to be featured on the compilation. When the slider position (Number of artists) is changed, a new suggestion is formed based on the existing artist pool of Treetrunk Records.

The app is deployed [here](https://linfri.shinyapps.io/ttri/).

[^1]: Mobile devices don't display the graphs correctly, this will be fixed in the future version.
[^2]: Compilation albums were excluded from the dataset. Missing values in Creator field were imputed with the help of titles.

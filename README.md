# BC Prioritization Tool for Caribou

> Prioritizing Zones for Caribou Habitat Restoration in British Columbia

App to support the report by [Dickie, M. and Serrouya, R.](./app/www/BC-Restoration-Prioritization-Report-Final.pdf)

## Run App

Run the app from terminal as `R -q -e "shiny::runApp('app')"` or from R as `shiny::runApp('app')`.

## Deploy

See the deployed app at [biodiversity-pathways.shinyapps.io/bcprioritytool](https://biodiversity-pathways.shinyapps.io/bcprioritytool/).

```R
account = "biodiversity-pathways"
rsconnect::setAccountInfo(name = account,
  token = "xxxxxx",
  secret = "xxxxxx")

rsconnect::deployApp("app", 
  appName = "bcprioritytool",
  appTitle = "BC Prioritization Tool",
  account = account,
  forceUpdate = TRUE)
```

## License

MIT (c) 2022 Peter Solymos and [Analythium](https://analythium.io?ref=gh-bcpriority)

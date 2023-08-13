rsconnect::deployApp(appName = "climate-app", 
                     appFiles = c("app.R", 
                                  "data/combine_data.R", "data/cvl_dat.RData", "data/cvl_data.RDS", "data/cvl_data_geo.RDS",
                                  "www/about_work.html", "www/style.css", "www/message-handler.js", "www/cville_climate_update.html",
                                  "www/bivariate_legend_static.svg", "www/bivariate_legend.svg", "www/bivariate_legend.png", "www/ec_climate_logo.png"),
                     account = "virginiaequitycenter") 

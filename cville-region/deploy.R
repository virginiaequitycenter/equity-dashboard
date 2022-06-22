rsconnect::deployApp(appName = "cville_equity_atlas", 
                     appFiles = c("app.R", 
                                  "BAed_003.html", "BAed_003b.html",
                                  "alb_homesales_school.html", "alb_homesales_schoolb.html",
                                  "web_profile.html", "web_profileb.html",
                                  "equity_links.html", "equity_links.Rmd",
                                  "www/app_data_2021.Rdata", 
                                  "www/three-line-bw.png", "UVAEQUITYCENTER.jpeg",
                                  "www/tags_about.R", "www/tags_commpas.R", "www/tags_equity.R"),
                     account = "virginiaequitycenter") 

# rsconnect::deployApp(appName = "cville_region", 
#                      appFiles = c("app.R", 
#                                   "BAed_003.html", "BAed_003b.html",
#                                   "alb_homesales_school.html", "alb_homesales_schoolb.html",
#                                   "web_profile.html", "web_profileb.html",
#                                   "equity_links.html", "equity_links.Rmd",
#                                   "www/app_data_2021.Rdata", 
#                                   "www/three-line-bw.png", "UVAEQUITYCENTER.jpeg",
#                                   "www/tags_about.R", "www/tags_commpas.R", "www/tags_equity.R"),
#                      account = "commpaslab") 


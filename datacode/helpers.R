# Text for helpers()

helpers <- list(counties = c("Select one or more localities to view. The selected localities appear on the maps and as observations in the provided graphs."), 
                
               geo = c("View indicators at the county, census tract, or the block group level. Some indicators are only available at the tract or county level.", "",
           	"<b>County:</b> The counties and cities within the region.", "",
           	"<b>Census Tract:</b> A census tract is an area roughly equivalent to a neighborhood generally encompassing between 2,500 to 8,000 people. They are designed to be relatively consistent with respect to population characteristics, economic status, and living conditions.", "",
           	"<b>Block Group:</b> A block group is a cluster of adjacent census blocks within a census tract. Block groups generally contain between 600 and 3,000 people. This is the smallest geographical unit for which the census publishes sample data."), 
               
               instructions = c("Make selections in the sidebar to view demographic, economic and social data on maps, and graphs on their relationship and difference in tabs in the navigation bar at the top of the page.
                    Variables include data related to Health, Housing, People, Youth & Education, Jobs, Wages & Income, and various Indices.",
             "There are 30+ equity indicators currently available in the following categories:<br /><br />",
               "<b>Health:</b> Health insurance rates and life expectancy.<br /><br />", "",
               "<b>Housing:</b> Number and rates of vacant housing, home ownership rates, percent rent burdened.<br /><br />", "",
               "<b>Indices:</b> Measures derived from combined indicators including human development index, gini inequality index, and measures of residential dissimilarity.<br /><br />", "",
               "<b>Jobs, Wages & Income:</b> Median household income and personal earnings, poverty and unemployment rates.<br /><br />", "",
               "<b>People:</b> Basic demographics including total population, racial and ethnic populations, populations by age group, and disability.<br /><br />", "",
               "<b>Youth & Education:</b> Educational attainment, school enrollment, child poverty rates.<br /><br />"),
               
               indicator = c("Select an indicator to view in the <b>First Indicator Map</b> tab in the left-side panel. The selected indicator will also be shown in the <b>Relationship</b> tab, as the <b>x-axis</b> of the graph, to show the correlation with the second indicator.<br /><br />Some equity indicators are not available at all geographic levels, see Available Geographic Levels below the selector."), 
               
               indicator2 = c("Select an equity indicator to view in the <b>Second Indicator Map</b> tab in the left-side panel. The selected indicator will also be shown in the <b>Relationship</b> tab, as the <b>y-axis</b> of the graph, to show the correlation with the first indicator. Available selections are the same as for the first equity indicator.<br /><br />Some equity indicators are not available at all geographic levels, see Available Geographic Levels below the selector."), 
               
               time = c("<b>Under Development.</b> Some indicators are available for multiple years. Select the year you wish to view. Currently, over time data is only present for total population and population by race or ethnicity."), 
               
               map_geo = c("Select a base map. The detailed map will show roads and other features more clearly, but viewers may find the detail distracting."),
               
               distribution = c("The distribution, or histogram, shows how often each value of an indicator occurs in the selected data (defined by locality and geography level). The distribution quickly shows the minimum and maximum values as well as the range of the most common values of an indicator."),
               
               correlation = c("The correlation, or scatterplot, shows the relationship between the first and second equity indicators across geographic areas. This helps us see how two indicators relate to one another.", "",
                         	"To identify possible correlation between two indicators, look at the graph and ask: As the value of the first indicator increases, does the value of the second indicator notably increase or decrease?", "",
                         	"It can also be useful to identify geographic areas (counties, census Tracts, or block groups) that have extreme values on both measures. That is, are their geographic areas in the very bottom left corner of the graph, or in the top right?"),
               
               race = c("<b>Under Development</b>. The comparison by race is intended to show, where available, how an indicator like household income differs by racial and ethnic populations. Currently only available when Median Household Income or Life Expectancy at Birth is selected as the primary indicator."))

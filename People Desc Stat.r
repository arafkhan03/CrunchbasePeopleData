colMeans(is.na(people))
format(colMeans(is.na(investors)), scientific = FALSE)

# Descriptive Stat for People

# 1.0 Library Loading, Data Prep & Summary
library(skimr)
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)

        ## Summary
dim(people)
skim(people)

        ## Only Keep Columns that have NAs Less than 75%
people_NA <- as.data.frame(
        round(colMeans(is.na(people)), 2)
)
people_WNA <- people[, people_NA$`round(colMeans(is.na(people)), 2)` < 0.75]

        ## Summary
skim(people_WNA)






# 2.0 Number of People by Job Title

        ## Standard Title List
title_list <- data.frame("Title" = c("CEO", "CFO", "CIO", "COO", "CMO", "CTO", "CRO", "CPO",
                                     "President",
                                     "Founder", "Co-Founder",
                                     "Partner", "Vice President",
                                     "Director", "Venture", "Chairman",
                                     "Chief Executive Officer", "Chief Operating Officer",
                                     "Chief Operations Officer", "Chief Information Officer",
                                     "Chief Marketing Officer", "Chief Finance Officer",
                                     "Chief Financial Officer", 
                                     "Chief People Officer", "Chief Revenue Officer",
                                     "VP"))

        ## Finding Closest Matches in the Given Titles According to Standard Titles

                ### Using stringdist package
library(stringdist)

                ### Conversion to Lower Cases
a <- as.character(tolower(people_WNA$`Primary Job Title`))
b <- as.character(tolower(title_list$Title))

                ### Matrix Using DL Method to Find the Match Number for Substrings
                ### Heavy Computation
distmatrix <- stringdist::stringdistmatrix(a, b,
                             useNames = TRUE, method = "dl")

                ### Make Sure to Have a Proper Matrix with Standard Titles in Columns,
                ### People Dataset's Titles in the Rows, and Match Numbers in the Cells
                ### And Convert it to a DF
dim(distmatrix)
distmatrixdf <- data.frame(distmatrix)

                ### Populate with Closest Titles
distmatrix_closest_std_title <- apply(distmatrixdf,1,
            function(x)names(distmatrixdf)[sort(head(order(x,decreasing=FALSE),1))])

                ### Covert to DF
distmatrixdf_closest_std_title <- as.data.frame(distmatrix_closest_std_title)

                ### Get Min Match Value for Each Row
min_match_value <- apply(distmatrix, 1, min)
min_match_value_df <- data.frame(min_match_value)

                ### Final People Dataset with Closest Job Title Match and Min Match Value
                ### Keeping a RData as this takes time!
people_WNA_Jobtl_Score <- cbind(people_WNA, distmatrixdf_closest_std_title, 
                                min_match_value_df)
saveRDS(people_WNA_Jobtl_Score, file="people_wna_jobtl_score.rds")

        ## Subset the Dataset by Taking Necessary Variables
people_working <- people_WNA_Jobtl_Score[,c(1,3,4,6,9,12,13:16)]

        ## Subset the Dataset by Taking Min Match Value < 10
people_working_less10match <- people_working[people_working$min_match_value < 11,]
people_working_less10match <- people_working_less10match[!is.na(people_working_less10match$min_match_value),]
skim(people_working_less10match)

        ## Summarize by Closest Title
closest_title_count_people <- people_working_less10match %>%
        dplyr::group_by(distmatrix_closest_std_title, .drop = TRUE) %>%
        dplyr::summarize(Count_by_People = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Group the Similar Titles
similar_title_CEO <- c(1, 4)
similar_title_CFO <- c(2, 5, 6)
similar_title_CIO <- c(13, 7)
similar_title_CMO <- c(14, 8)
similar_title_COO <- c(16, 9, 10)
similar_title_CPO <- c(17, 11)
similar_title_CRO <- c(18, 12)
similar_title_VP <- c(26, 25)

closest_title_count_people[similar_title_CEO[1], 2] <- 
        colSums(closest_title_count_people[similar_title_CEO, 2])

closest_title_count_people[similar_title_CFO[1], 2] <- 
        colSums(closest_title_count_people[similar_title_CFO, 2])

closest_title_count_people[similar_title_CIO[1], 2] <- 
        colSums(closest_title_count_people[similar_title_CIO, 2])

closest_title_count_people[similar_title_CMO[1], 2] <- 
        colSums(closest_title_count_people[similar_title_CMO, 2])

closest_title_count_people[similar_title_COO[1], 2] <- 
        colSums(closest_title_count_people[similar_title_COO, 2])

closest_title_count_people[similar_title_CPO[1], 2] <- 
        colSums(closest_title_count_people[similar_title_CPO, 2])

closest_title_count_people[similar_title_CRO[1], 2] <- 
        colSums(closest_title_count_people[similar_title_CRO, 2])

closest_title_count_people[similar_title_VP[1], 2] <- 
        colSums(closest_title_count_people[similar_title_VP, 2])

closest_title_count_people <- closest_title_count_people[c(1:2, 13:14, 16:18, 26),]

        ## Make the Titles Upper Case
closest_title_count_people$distmatrix_closest_std_title <-
        toupper(closest_title_count_people$distmatrix_closest_std_title)

        ## Percentage of People by Closest Titles
closest_title_count_people$Percentage <- as.numeric(format(closest_title_count_people$Count_by_People/
                                                           sum(closest_title_count_people$Count_by_People), 
                                                   scientific = FALSE, digits = 2))

        ## Arrange the DF by Count of People
closest_title_count_people <- arrange(closest_title_count_people, 
                                      desc(closest_title_count_people$Count_by_People))
sum(closest_title_count_people$Count_by_People)
sum(closest_title_count_people$Percentage)

        ## Plot Number of People by Title
ylim_people_title <- c(0, 1.1*max(closest_title_count_people$Count_by_People))
xx_people_title <- barplot(closest_title_count_people$Count_by_People, xaxt = 'n', xlab = '', width = 0.85,
                   ylim = ylim_people_title, main = "Number of People by Most Frequent & Closely Matched Job Titles", 
                   ylab = "Frequency")
text(x = xx_people_title, y = closest_title_count_people$Count_by_People, 
     label = closest_title_count_people$Count_by_People, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_people_title, labels=closest_title_count_people$distmatrix_closest_std_title, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 2.0 Number of People by Company Associated with
length(unique(people_working$`Primary Organization`))

        ## Summarize No. of People by Company Associated with
companyworking_count_people <- people_working %>%
        dplyr::group_by(`Primary Organization`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Companyworking = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Arrange the DF by Count of People
companyworking_count_people <- arrange(companyworking_count_people, 
                                      desc(companyworking_count_people$Count_by_Companyworking))

        ## Percentage of People by Company Associated with
companyworking_count_people$Percentage <- as.numeric(format(companyworking_count_people$Count_by_Companyworking/
                                                                    sum(companyworking_count_people$Count_by_Companyworking), 
                                                            scientific = FALSE, digits = 2))

        ## Take Top 30
companyworking_count_people <- companyworking_count_people[1:30,]
sum(companyworking_count_people$Percentage)

        ## Plot Number of Companies by Company Associated with
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

                ### Generate the layout. This function return a dataframe with one line per bubble. 
                ### It gives its center (x and y) and its radius, proportional of the value
companyworking_packing <- circleProgressiveLayout(companyworking_count_people$Count_by_Companyworking, 
                                   sizetype='area')

                ### We can add these packing information to the initial data frame
data_companyworking_circlepack <- cbind(companyworking_count_people, companyworking_packing)

                ### The next step is to go from one center + a radius to the coordinates of a circle that
                ### is drawn by a multitude of straight lines.
dat.gg_companyworking_circle <- circleLayoutVertices(companyworking_packing, npoints=50)

                ## Make the plot
HQ_Companyworking_Circleplot <- ggplot() + 
        
        ### Make the bubbles
        geom_polygon(data = dat.gg_companyworking_circle, aes(x, y, group = id, 
                                                       fill=as.factor(id)), 
                     colour = "black", alpha = 0.6) +
        
        ### Add text in the center of each bubble + control its size
        geom_text(data = data_companyworking_circlepack, aes(x, y, size=Count_by_Companyworking, 
                                                      label = `Primary Organization`)) +
        ggtitle("Distribution of People by Top 30 Companies Associated with") +
        
        ### General theme:
        scale_size_continuous(range = c(1,4)) +
        theme_void() + 
        theme(legend.position="none") +
        coord_equal()

HQ_Companyworking_Circleplot






# 3.0 Count of People by Gender

        ## Summarize No. of People by Gender
gender_count_people <- people_working %>%
        dplyr::group_by(Gender, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Gender = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of All Companies by Company Type
gender_count_people$Percentage <- as.numeric(format(gender_count_people$Count_by_Gender/
                                                           sum(gender_count_people$Count_by_Gender), 
                                                   scientific = FALSE, digits = 2))

        ## Arrange the DF by Count of People
gender_count_people <- arrange(gender_count_people, 
                                       desc(gender_count_people$Count_by_Gender))

        ## Write the Table
write.table(gender_count_people, file = "/Users/araf03/Desktop/MS Thesis/Tables/People/People by Gender.txt",
            sep = ",")






# 4.0 Count of People by School Attended

length(unique(people_working$`Schools Attended`))

        ## Summarize No. of People by Gender
school_count_people <- people_working %>%
        dplyr::group_by(`Schools Attended`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_School = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Arrange the DF by Count of People
school_count_people <- arrange(school_count_people, 
                               desc(school_count_people$Count_by_School))

        ## Percentage of People by Companyworking
school_count_people$Percentage <- as.numeric(format(school_count_people$Count_by_School/
                                                                    sum(school_count_people$Count_by_School), 
                                                            scientific = FALSE, digits = 2))

        ## Take Top 20
school_count_people <- school_count_people[1:20,]
sum(school_count_people$Percentage)

        ## Plot Number of Companies by HQ's Country
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

                ### Generate the layout. This function return a dataframe with one line per bubble. 
                ### It gives its center (x and y) and its radius, proportional of the value
school_packing <- circleProgressiveLayout(school_count_people$Count_by_School, 
                                                  sizetype='area')

                ### We can add these packing information to the initial data frame
data_school_circlepack <- cbind(school_count_people, school_packing)

                ### The next step is to go from one center + a radius to the coordinates of a circle that
                ### is drawn by a multitude of straight lines.
dat.gg_school_circle <- circleLayoutVertices(school_packing, npoints=50)

        ## Make the plot
HQ_School_Circleplot <- ggplot() + 
        
                ### Make the bubbles
        geom_polygon(data = dat.gg_school_circle, aes(x, y, group = id, 
                                                              fill=as.factor(id)), 
                     colour = "black", alpha = 0.6) +
        
                ### Add text in the center of each bubble + control its size
        geom_text(data = data_school_circlepack, aes(x, y, size=Count_by_School, 
                                                             label = `Schools Attended`)) +
        ggtitle("Distribution of People by Top 20 Schools Attended") +
        
                ### General theme:
        scale_size_continuous(range = c(1,4)) +
        theme_void() + 
        theme(legend.position="none") +
        coord_equal()

HQ_School_Circleplot






# 5.0 Number of People by Orgs Funded

summary(people_working$`Number of Founded Organizations`)

        ## Converting Continuous Variable to Categorical
people_working$num_funded_org_cat <- 
        cut(people_working$`Number of Founded Organizations`, 
            breaks = c(0, 1, 2, 3, 4, 5, 7, 9, 11, 15, 19, 23, 27))

        ## Number of People by Group of Number of Funded Orgs
fundedorg_number_count_people <- people_working %>%
        dplyr::group_by(num_funded_org_cat, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Fundedorg = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of People by Orgs Funded
fundedorg_number_count_people$Percentage <- as.numeric(format(fundedorg_number_count_people$Count_by_Fundedorg/
                                                                   sum(fundedorg_number_count_people$Count_by_Fundedorg), 
                                                           scientific = FALSE, digits = 2))

        ## Plot Number of People by Orgs Funded
ylim_people_fundedorg <- c(0, 1.1*max(fundedorg_number_count_people$Count_by_Fundedorg))
xx_people_fundedorg <- barplot(fundedorg_number_count_people$Count_by_Fundedorg, xaxt = 'n', xlab = '', width = 0.85,
                           ylim = ylim_people_fundedorg, main = "Number of People by Organizations Funded", 
                           ylab = "Frequency")
text(x = xx_people_fundedorg, y = fundedorg_number_count_people$Count_by_Fundedorg, 
     label = fundedorg_number_count_people$Count_by_Fundedorg, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_people_fundedorg, labels=fundedorg_number_count_people$num_funded_org_cat, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)



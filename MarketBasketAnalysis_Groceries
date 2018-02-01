###############################################################
#load the packages for association rules and R sample datasets#
###############################################################

library(arules)
library(datasets)
library(arulesViz)
library(visNetwork)
library(igraph)
#load and examine the data

data(Groceries)
summary(Groceries)
inspect(head(Groceries))

#Alternatively, load local data from working directory
setwd(choose.dir()) 
Groceries <- read.transactions("groceries.csv",format=c("basket"),sep=",", rm.duplicates=TRUE)

#plot the relative frequency of top 30 grocery item in the dataset
itemFrequencyPlot(Groceries,topN=169,type="relative")

####################################################
#generate rules with minimum support and confidence#
####################################################
rules <- apriori(Groceries,parameter=list(supp = 0.001, conf=0.6,ext=TRUE)) 

#specify the format of output
options(digits=2)

#summarize all the discovered rules
summary(rules)

#sort the rules based on lift, then inspect the top 10.
rules <- sort(rules,by="lift",decreasing=TRUE)
subrules <-rules[1:10]
inspect(subrules)

##########################
#visualize rules-two ways#
##########################
#1.use arulesviz
ig <- plot( subrules, 
            method="graph", 
            measure='support',
            control=list(type="items"),
            interactive=TRUE)

#2.using igrah and visNetwork
ig_df <- get.data.frame( ig, what = "both" )
#fix lift vector in the igraph dataframe 
lift_vec<-append(ig_df$vertices$lift[is.na(ig_df$vertices$lift)],
                 na.omit(ig_df$vertices$lift))

visNetwork(
  nodes = data.frame(
    id = ig_df$vertices$name,
    title = ifelse(ig_df$vertices$label== "",round(lift_vec,2), 
                   ig_df$vertices$label),
    ig_df$vertices),
  edges = ig_df$edges,
  main='Top 10 Discovered Rules'
) %>%
  visEdges( arrows='to' ) %>%
  visOptions( highlightNearest = TRUE )%>% 
  visNodes(shadow = TRUE,
           shape=ifelse(ig_df$vertices$label == "","circle","ellipse"))%>%
  visInteraction(hover = TRUE)


#specify specific items to filter results
# e.g. we only want to include rules where beer appears on rhs or butter/sugar on lhs:
subrules2<-subset(rules, subset = (lhs %in% c("butter","sugar")|rhs %pin% "beer") & lift >5.0)
inspect(subrules2)

#Write out the output
#convert to a data.frame
rules.df<-as(rules,"data.frame")
#write to csv
write.csv(rules.df, file = "C://Users/ying.li/Desktop/Basket Analysis/OutputRules.csv")

#Output dataframe Option 2.
recommends= data.frame(lhs = labels(lhs(subrules)),
                       rhs = labels(rhs(subrules)),
                       round(subrules@quality,2))

#Write out the output
#write to csv

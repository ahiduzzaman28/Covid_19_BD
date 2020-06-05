library(tabulizer)
library(tidyverse)

#Extracting Table From PDF

data=
    extract_tables("https://www.iedcr.gov.bd/website/images/files/nCoV/Case_dist_3_June_upload.pdf",
                   output = "data.frame",method = "decide")
data

##Covid_19 positive cases in top 10 districts/cities in Bangladesh

#wrangling using dplyr

district=data[[1]]
table1=district%>%top_n(n=10,wt=Total)%>%select(District.City,Total)
table1
table1=table1%>%mutate(District.City=recode(District.City,"Coxâ€™s bazar"="Cox's Bazar"))

#Visualization using ggplot2

table1%>%ggplot(aes(Total,reorder(District.City,Total),fill=District.City))+geom_bar(stat="identity")+
    theme_minimal()+
    scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73","#293352",
                                                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#52854C"))+
    labs(x="Total Covid19 positive cases",y="Districts/CIties",title = "Covid19 positive cases in top 10 Districts/Cities in Bangladesh")


##Covid_19 poaitive cases Division wise

#wrangling with dplyr

division=data[[1]]
table2=division%>%select(Division,Division.1)
table2=table2%>%mutate(Division=ifelse(Division.1>10000,"Dhaka City",Division))
table2=table2%>%mutate(Division=ifelse(Division.1==7391,"Dhaka",Division))
table2=table2%>%filter(Division.1>0)
table2

#visualization with ggplot

table2%>%ggplot(aes(Division.1,reorder(Division,Division.1),fill=Division))+geom_bar(stat="identity")+
    theme_minimal()+
    scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73","#777777","#293352",
                                 "#F0E442", "#0072B2", "#D55E00"))+
    labs(x="Total Covid19 positive cases",y="Divisions",title = "Covid19 positive cases in Divisions of Bangladesh")


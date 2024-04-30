##This code will take two pairs of ancestry and population data and combine them to make a violin plot showing variation in ancestry proportions for a subset of populations  
install.packages("dplyr")
library(dplyr)

##Download the data from github

#this file contains the ancestry proportions for Pop1 individuals and reference data
Pop1_Admix <- read.table("https://raw.githubusercontent.com/stacy-edington/5202Tutorial/main/Pop1_Admix.txt", 
                   header = FALSE)  

##this file contains the population IDs for Pop1 individuals and reference data 
Pop1_IDS = read.table("https://raw.githubusercontent.com/stacy-edington/5202Tutorial/main/Pop1_IDs.txt", col.names = FALSE)

##combine the ancestry proportions file with the population IDs file using cbind 
Pop1 = cbind(Pop1_IDS, Pop1_Admix) #make sure that your ID file is listed first so that it will be the first column in your new datatable

colnames(Pop1)[1] <- "Population" ##this will change the first column name to Populations in your datatable. this is necessary for future steps 
colnames(Pop1)[3] <- "KHS" ##this will change the second column name in the Pop1 data to "European" in your datatable.  
colnames(Pop1)[4] <- "Euro" ##this will change the third column name in the Pop1 data to "KHS" in your datatable. 

##this file contains the population IDs for Pops2-7 individuals and reference data 
Pop2_7_IDs = read.table("https://raw.githubusercontent.com/stacy-edington/5202Tutorial/main/Pop2_7_IDs.txt", col.names = FALSE)  

#this file contains the ancestry proportions for Pops2-7 individuals and reference data
Pop2_7_Admix <- read.table("https://raw.githubusercontent.com/stacy-edington/5202Tutorial/main/Pop2_7_Admix.txt", 
                        header = FALSE)  

##combine the Pops2-7 ID and Admix files using cbind 
Pops2_7 = cbind(Pop2_7_IDs, Pop2_7_Admix)

##rename the first column of the new Pops2-7 datatable to be Population like the Pop1 dataset previously. 
colnames(Pops2_7)[1] <- "Population"
colnames(Pops2_7)[3] <- "KHS"   
colnames(Pops2_7)[4] <- "Euro"  


##this next step is going to subset the data to only focus on a certain set of populations and a certain column.
##these two functions below are going to look at only the Population and KHS columns in each dataframe. 
Pop1_Euro = Pop1 %>% 
  filter(Population == "Pop1") %>% 
  select(Population, Euro)

Pops2_7_Euro = Pops2_7 %>% 
  filter(Population == "Pop2" | Population == "Pop7" | Population == "Pop6" | Population == "Pop3" | Population == "Pop5" | Population == "Pop4") %>% 
  select(Population, Euro)

##combine the new subsetted data to make one large dataset for the populations and ancestry column of interest. 
combined_data_Euro <- rbind(Pop1_Euro, Pops2_7_Euro)

##once the data has been combined and you have a dataset including two columns "Population" and "European" then we plot based on ancestry proportions 

##this will plot a box plot for the variation in European ancestry across populations 
devtools::install_github("BlakeRMills/MoMAColors") ##this line of code downloads a certain color palette from github
library(MoMAColors) #this is loading the color palette once it is downloaded
library(ggplot2)
pal <- moma.colors("Warhol") #this sets up what color palette we would like to use for the plots
combined_data_KHS$Population <- factor(combined_data_Euro$Population, levels = c("Pop1", "Pop2", "Pop3", "Pop4", "Pop5", "Pop6", "Pop7"))
p1 = ggplot(combined_data_Euro, aes(x=Population, y=Euro, fill=Population)) + 
  geom_boxplot(width=0.1, fill="white")+ 
  labs(title="Variation in European Ancestry Proportions",x="Pop", y = "Proportion European Ancestry")+
  scale_fill_manual(values=pal)+
  theme_classic()
print(p1)
ggsave("Euro_BoxPlot.png", plot = p1, width = 6, height = 4, units = "in")


##this will plot a violin plot for the variation in European ancestry across populations 
combined_data_Euro$Population <- factor(combined_data_Euro$Population, levels = c("Pop1", "Pop2", "Pop3", "Pop4", "Pop5", "Pop6", "Pop7"))
p2 = ggplot(combined_data_Euro, aes(x=Population, y=Euro, fill=Population)) + 
  geom_violin(trim=FALSE)+
  labs(title="Variation in European Ancestry Proportions",x="Pop", y = "Proportion European Ancestry")+
  scale_fill_manual(values=pal)+
  theme_classic()
print(p2)
ggsave("Euro_Violin.png", plot = p2, width = 6, height = 4, units = "in")


##this will plot a violin plot for the variation in European ancestry across populations 
combined_data_Euro$Population <- factor(combined_data_Euro$Population, levels = c("Pop1", "Pop2", "Pop3", "Pop4", "Pop5", "Pop6", "Pop7"))
p3 = ggplot(combined_data_Euro, aes(x=Population, y=Euro, fill=Population)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+ #this adds boxplots inside our violins for more understanding of the variation
  labs(title="Variation in European Ancestry Proportions",x="Pop", y = "Proportion European Ancestry")+
  scale_fill_manual(values=pal)+
  theme_classic()
print(p3)
ggsave("Euro_Violin_Box.png", plot = p3, width = 6, height = 4, units = "in")










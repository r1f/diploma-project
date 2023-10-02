# library

library("xlsx")
library(readr)
library(tidyverse)
library(outliers)
library(ggplot2)
library(car)
library(ggpubr)
library(ggstatsplot)
library(TeachingDemos)
library(writexl)


# file

output <- read_delim("output.txt", delim = "\t", 
                     escape_double = FALSE, col_types = cols(`Experimental spectrum, time` = col_double(), 
                                                             `Background spectrum, time` = col_double(), 
                                                             `Pa-234m_Activity, Bq/kg` = col_double(), 
                                                             `Bi-214_Activity, Bq/kg` = col_double()), 
                     na = "null", trim_ws = TRUE)
View(output)

data <- data.frame(output)

cols <- ncol(data)
colStart <- grep("^Comments$", colnames(data)) + 1 

replaceNA <- function(data, cols)
{
  for (colValue in cols)
  {
    for (rowValue in 1:nrow(data)) 
    {
      if (is.na(data[rowValue, colValue]))
      {
        #data[rowValue,colValue] = data[rowValue, colValue + 3]
      }
    }
  }
  return(data)
}

some <- replaceNA(data, grep("Activity", colnames(data)))

functionMeanAndSd <- function(data, cols, colStart, mean = TRUE)
{
  if (mean == TRUE)
  {
    for(col in colStart:cols) 
    {
      name <- paste(colnames(data[col]),"mean")
      data[as.character(name)] <- NULL
    }
    
    for(id in 1:data[nrow(data),"ID"])
    {
      range <- which(sapply(data[,"ID"], function(x) id %in% x)) 
      for(col in colStart:cols) 
      {
        name <- paste(colnames(data[col]),"mean")
        data[max(range),as.character(name)] <- mean(data[min(range):max(range),col])
      }
    }
    return(data)
  }
  else
  {
    for(col in colStart:cols) 
    {
      name <- paste(colnames(data[col]),"sd")
      data[as.character(name)] <- NULL
    }
    
    for(id in 1:data[nrow(data),"ID"])
    {
      range <- which(sapply(data[,"ID"], function(x) id %in% x)) 
      for(col in colStart:cols) 
      {
        name <- paste(colnames(data[col]),"sd")
        data[max(range),as.character(name)] <- sd(data[min(range):max(range),col])
      }
    }
    return(data)
  }
}

#some <- functionMeanAndSd(some, cols, colStart, TRUE)
#some <- functionMeanAndSd(some, cols, colStart, FALSE)

df <- function(data, cols, colStart)
{
  id <- 1:data[nrow(data),"ID"]
  df <- data.frame(id)
  
  for (col in colStart:cols)
  {
    arrayMean <- NULL
    arraySD <- NULL
    for(id in 1:data[nrow(data),"ID"])
    {
      range <- which(sapply(data[,"ID"], function(x) id %in% x))
      
      arrayMean <- append(arrayMean, mean(data[min(range):max(range),col]))
      
      arraySD <- append(arraySD, sd(data[min(range):max(range),col]))
    }
    name <- paste(colnames(data[col]),"mean")
    df[as.character(name)] <- arrayMean
    
    name <- paste(colnames(data[col]),"sd")
    df[as.character(name)] <- arraySD
    
  }
  return (df)
}

df <- df(some, cols, colStart)


displayAllActivity <- function(data)
{
  nuclideCol <- grep("Activity", colnames(data))
  
  for (index in nuclideCol) 
  {
    df_reshaped <- data.frame(x = data[,1],                           
                              y = data[,index],
                              group = data[,grep("Position", colnames(data))])
    
      
    print(ggplot(df_reshaped, aes(x, y, col = group)) + geom_point() + scale_x_continuous(breaks = data[,1]) +
            theme_bw() +
            theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
                  axis.text.y = element_text(colour = "grey20", size = 12),
                  strip.text = element_text(face = "italic"),
                  text = element_text(size = 16)) + ggtitle(colnames(data[index])) + ylab("Bq/kg") + xlab(colnames(data[1])) + geom_line() + facet_wrap(vars(group)))
  }
}


displayAllActivity(some)


displayActivityByID <- function(data)
{
  nuclideCol <- grep("Activity", colnames(data))
  
  for (index in nuclideCol)
  {
    for (id in 1:max(data$ID))
    {
      df_reshaped <- data.frame(x = as.numeric(rownames(data[data$ID == id, 0])),                           
                                y = data[data$ID == id, index],
                                group = data$Position[data$ID == id])
      
      xName <- paste(c(colnames(data[index]), "( Folder ID =", id, ")"), collapse = " ")
      
      print(ggplot(df_reshaped, aes(x, y, col = group)) + geom_point() + scale_x_continuous(breaks = as.numeric(rownames(data[data$ID == id, 0]))) +
        theme_bw() + geom_line() + xlab("") + ylab("Activity Bq/Kg") + ggtitle(xName) +
        theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
              axis.text.y = element_text(colour = "grey20", size = 12),
              strip.text = element_text(face = "italic"),
              text = element_text(size = 16)))
      
      colName <- paste(c(id, colnames(data[index])), collapse = "_")
      
      fileName <- paste(c(colName,"png"), collapse = ".")
      
      ggsave(fileName, width = 19, height = 10)
      
    }
  }
}


displayActivityByID(some)


displayActivityByIDAndPosition <- function(data)
{
  nuclideCol <- grep("Activity", colnames(data))
  
  for (id in 1:max(data$ID))
  {
    df <- data.frame(
      x = rep(1:length(as.numeric(rownames(some[some$ID == id, 0]))), length(grep("Activity", colnames(some)))),
      
      y = c(
        data[data$ID == id, nuclideCol[1]],
        data[data$ID == id, nuclideCol[2]],
        data[data$ID == id, nuclideCol[3]], 
        data[data$ID == id, nuclideCol[4]]
        ),
      
      group = rep(data$Position[data$ID == id], length(grep("Activity", colnames(data)))),
      
      nuclide = c(
        rep(colnames(data[nuclideCol[1]]) ,length(rownames(data[data$ID == id, 0]))),
        rep(colnames(data[nuclideCol[2]]) ,length(rownames(data[data$ID == id, 0]))),
        rep(colnames(data[nuclideCol[3]]) ,length(rownames(data[data$ID == id, 0]))),
        rep(colnames(data[nuclideCol[4]]) ,length(rownames(data[data$ID == id, 0])))
        )
    )
    
    xName <- c(unique(data$FolderName))
    
    print(ggplot(df, aes(x, y, col = nuclide)) + geom_point() + scale_x_continuous(breaks = 1:length(as.numeric(rownames(data[data$ID == id, 0])))) +
      theme_bw() +
      geom_line() +
      theme(
        axis.text.x = element_text(colour = "grey20", size = 10, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16)
        ) + 
      facet_wrap(vars(group)) + 
      xlab("") +
      ylab("Activity Bq/Kg") +
      ggtitle(xName[id]))
    
    colName <- paste(c(id, "Activity"), collapse = "_")
    
    fileName <- paste(c(colName,"png"), collapse = ".")
    
    ggsave(fileName, width = 19, height = 10)
  }
}

#sd(output$`K-40_Activity, Bq/kg`[output$ID == 5])

displayActivityByIDAndPosition(some)

#

boxplot.with.outlier.label(some$K.40_Activity..Bq.kg[some$ID == 2] ~ some$Position[some$ID == 2], some$K.40_Activity..Bq.kg[some$ID == 2])
#

nDigits <- function(x)
{
  if (trunc(x) != 0) nchar( trunc( abs(x) ) ) else nchar( trunc( abs(x) ) ) - 1
}


getActivityMean <- function(data)
{
  nuclideCol <- grep("Activity", colnames(data))
  
  detectionDevice <- unique(data$Detection.device.SN)
  
  xName <- c(unique(data$FolderName))
  
  dataFrame <- data.frame()
  
  nuclideNames <- colnames(some[seq(20, ncol(some), 4)])
  
  for (device in detectionDevice) 
  {
    for (id in 1:max(data$ID)) 
    {
      arrayMean <- NULL
      for (nuclide in nuclideCol) 
      {
        if(length(data[data$ID == id & data$Detection.device.SN == device, nuclide]) == 0)
        {
          meanValue <- NA
        }
        else if(is.na(mean(data[data$ID == id & data$Detection.device.SN == device, nuclide + 1])) == TRUE)
        {
          meanValue <- "< MDA"
        }
        else
        {
          meanValue <- mean(data[data$ID == id & data$Detection.device.SN == device, nuclide])
          
          errorValue <- (mean(data[data$ID == id & data$Detection.device.SN == device, nuclide + 1]) * mean(data[data$ID == id & data$Detection.device.SN == device, nuclide]))/100
          
          #sdValue <- sd(data[data$ID == id & data$Detection.device.SN == device, nuclide])
          
          meanValue <- paste(signif(as.numeric(meanValue), nDigits(as.numeric(meanValue)) + 1), "(", signif(as.numeric(errorValue), nDigits(as.numeric(errorValue)) + 1), ")")
        }
        
        arrayMean <- append(arrayMean, meanValue)
      }
      dataFrame <- rbind(dataFrame, c(xName[id], as.character(device), arrayMean))
    }
  }
  
  colnames(dataFrame) <- c("Folder Name","Detection_Device", nuclideNames)
  
  write_xlsx(dataFrame, "output.xlsx")
  
    for (id in 1:max(data$ID))
    {
      arraySd <- NULL
      for (nuclide in nuclideCol)
      {
        if(length(data[data$ID == id, nuclide]) == 0)
        {
          sdValue <- NA
        }
        else if(is.na(mean(data[data$ID == id, nuclide + 1])) == TRUE)
        {
          sdValue <- "-"
        }
        else
        {
          sdValue <- sd(data[data$ID == id, nuclide])
          sdValue <- as.numeric(sdValue)
          sdValue <- signif(sdValue, nDigits(sdValue) + 2)
        }
        arraySd <- append(arraySd, sdValue)
      }
      dataFrame <- rbind(dataFrame, c(xName[id], as.character(device), arraySd))
    }

  colnames(dataFrame) <- c("Folder path","Detection_Device", nuclideNames)

  write_xlsx(dataFrame, "output.xlsx")

  
}

getActivityMean(some)

output[output$`Detection device SN`]

text8500<-some[some$Detection.device.SN==8500,]

text8500Down<-some[some$Detection.device.SN==8500 & some$Position == "Down",]

text8500Top<-some[some$Detection.device.SN==8500 & some$Position == "Top",]

text8500Center<-some[some$Detection.device.SN==8500 & some$Position == "Center",]

text8501 <-some[some$Detection.device.SN==8501,] 


write.table(text8500, file = "8500_All_Positions.txt", row.names = FALSE, quote = FALSE, sep = "\t")

write.table(text8500Down, file = "8500_Down.txt", row.names = FALSE, quote = FALSE, sep = "\t")

write.table(text8500Top, file = "8500_Top.txt", row.names = FALSE, quote = FALSE, sep = "\t")

write.table(text8500Center, file = "8500_Center.txt", row.names = FALSE, quote = FALSE, sep = "\t")


# experiments


ggplot(df, aes(x = x, y = value, color = variable)) +
  geom_line()

boxplot.with.outlier.label(some$K.40_Activity..Bq.kg[some$ID == 2] ~ some$Position[some$ID == 2], some$K.40_Activity..Bq.kg[some$ID == 2])


df_reshaped <- data.frame(x = as.numeric(rownames(some[some$ID == 1, 0])),                           
                          y = some$K.40_Activity..Bq.kg[some$ID == 2],
                          group = some$Position[some$ID == 2])

df <- data.frame(
  x = rep(as.numeric(rownames(some[some$ID == 2, 0])), length(grep("Activity", colnames(some)))),
  y = c(some$K.40_Activity..Bq.kg[some$ID == 2],some$Bi.214_Activity..Bq.kg[some$ID == 2], some$Pa.234m_Activity..Bq.kg[some$ID == 2], some$Th.232_Activity..Bq.kg[some$ID == 2]),
  group = rep(some$Position[some$ID == 2], length(grep("Activity", colnames(some)))),
  nuclide = c(  rep(colnames(some[18]) ,length(rownames(some[some$ID == 2, 0]))), rep(colnames(some[22]) ,length(rownames(some[some$ID == 2, 0]))), rep(colnames(some[26]) ,length(rownames(some[some$ID == 2, 0]))), rep(colnames(some[30]) ,length(rownames(some[some$ID == 2, 0])))    )
)

#ggplot(df_reshaped, aes(y, colour=group)) + geom_density()

ggplot(df, aes(x, y, col = nuclide)) + geom_point() + scale_x_continuous(breaks = as.numeric(rownames(some[some$ID == 2, 0]))) +
  theme_bw() + geom_line() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16)) + facet_wrap(vars(group))



ggbarplot(df_reshaped, x = "x", y = "y",
                fill = "group",               # change fill color by cyl
                color = "white",            # Set bar border colors to white
                palette = "jco",            # jco journal color palett. see ?ggpar
                sort.val = "asc",           # Sort the value in ascending order
                sort.by.groups = TRUE,      # Sort inside each group
                x.text.angle = 90           # Rotate vertically x axis texts
)


boxplot(df_reshaped$y~df_reshaped$group)

plot(some$Pa.234m_Activity..Bq.kg[some$ID == 10], type = "l")

df_reshaped$PosVal = df_reshaped$group

df_reshaped$PosVal[df_reshaped$PosVal == "Center"] = 0

df_reshaped$PosVal[df_reshaped$PosVal == "Down"] = -1

df_reshaped$PosVal[df_reshaped$PosVal == "Top"] = 1

typeof(df_reshaped$PosVal)

df_reshaped <- transform(df_reshaped, PosVal = as.numeric(PosVal)) 



df_reshaped <- data.frame(x = some$ID,                           
                          y = some$Th.232_Activity..Bq.kg,
                          group = some$Position)

ggplot(df_reshaped, aes(x, y, col = group)) + geom_point() + scale_x_continuous(breaks = df$id) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16)) + facet_wrap(vars(group))





# one-graph

df_reshaped <- data.frame(x = df$id,                           
                          y = c(df$`Pa.234m_Activity..Bq.kg mean`, df$`Pa.234m_Statistic.error... mean`, df$`Pa.234m_Complete.error..Bq.kg mean`,df$`Pa.234m_MDA..Bq.kg mean`),
                          group = c(rep("Pa.234m_Activity..Bq.kg mean", nrow(df)),
                                    rep("Pa.234m_Statistic.error... mean", nrow(df)),
                                    rep("Pa.234m_Complete.error..Bq.kg mean", nrow(df)),
                                    rep("Pa.234m_MDA..Bq.kg mean", nrow(df))))

ggplot(df_reshaped, aes(x, y, col = group)) +  geom_line() + geom_point()


df_reshaped <- data.frame(x = df$id,                           
                          y = c(unlist(df[seq(2, 32, 2)])),
                          group = c(rep(colnames(df[seq(2, 32, 2)]), nrow(df))))

ggplot(df_reshaped, aes(x, y, col = group)) +  geom_line() + geom_point() 

# multiple graph

displayNuclide <- function(df, nuclideCount, output = TRUE)
{
  if (output == TRUE)
  {
    startCol <- grep("^Comments$", colnames(df)) + 1
    
    queue <- seq(startCol, ncol(df), 4)
    
    for (index in 1:nuclideCount) 
    {
      currentQueue <- seq(queue[index], queue[index] + 3, 1)
      for (queueIndex in 1) 
      {
        df_reshaped <- data.frame(x = df$ID,                           
                                  y = c((df[1:nrow(df),currentQueue[1]]) , (df[1:nrow(df),currentQueue[2]]) , (df[1:nrow(df),currentQueue[3]]) , (df[1:nrow(df),currentQueue[4]])),
                                  group = c(rep(colnames(df[currentQueue[1]]), nrow(df)), rep(colnames(df[currentQueue[2]]), nrow(df)), rep(colnames(df[currentQueue[3]]), nrow(df)),rep(colnames(df[currentQueue[4]]), nrow(df))))
        
        print(ggplot(df_reshaped, aes(x, y, col = group)) + geom_point() + scale_x_continuous(breaks = df$ID) +
                theme_bw() +
                theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
                      axis.text.y = element_text(colour = "grey20", size = 12),
                      strip.text = element_text(face = "italic"),
                      text = element_text(size = 16)) + facet_wrap(vars(group)))
      }
    }
  }
  else
  {
    queue <- seq(2, nuclideCount * 8, 8)
    
    for (index in 1:length(queue)) 
    {
      currentQueue <- seq(queue[index], queue[index] + 6, 2)
      for (queueIndex in 1) 
      {
        df_reshaped <- data.frame(x = df$id,                           
                                  y = c((df[1:nrow(df),currentQueue[1]]) , (df[1:nrow(df),currentQueue[2]]) , (df[1:nrow(df),currentQueue[3]]) , (df[1:nrow(df),currentQueue[4]])),
                                  group = c(rep(colnames(df[currentQueue[1]]), nrow(df)), rep(colnames(df[currentQueue[2]]), nrow(df)), rep(colnames(df[currentQueue[3]]), nrow(df)),rep(colnames(df[currentQueue[4]]), nrow(df))))
        
        print(ggplot(df_reshaped, aes(x, y, col = group)) + geom_point() + scale_x_continuous(breaks = df$id) +
                theme_bw() +
                theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
                      axis.text.y = element_text(colour = "grey20", size = 12),
                      strip.text = element_text(face = "italic"),
                      text = element_text(size = 16)) + facet_wrap(vars(group)))
      }
    }
  }
}

displayNuclide(some, 2, TRUE)

df_reshaped <- data.frame(x = df$id,                           
                          y = c((df[1:nrow(df),2]) , (df[1:nrow(df),4]) , (df[1:nrow(df),6]) , (df[1:nrow(df),8])),
                          group = c(rep(colnames(df[2]), nrow(df)), rep(colnames(df[4]), nrow(df)), rep(colnames(df[6]), nrow(df)),rep(colnames(df[8]), nrow(df))))

ggplot(df_reshaped, aes(x, y, col = group)) + geom_point() + scale_x_continuous(breaks = df$id) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16)) + facet_wrap(vars(group))


# try section

dfas <- data.frame(subject <- c(rep(row.names(df[0]), 4)),
                credit <- c(rep(colnames(df[2]), nrow(df)), rep(colnames(df[4]), nrow(df)), rep(colnames(df[6]), nrow(df)),rep(colnames(df[8]), nrow(df))),
                value <- c((df[1:nrow(df),2]) , (df[1:nrow(df),4]) , (df[1:nrow(df),6]) , (df[1:nrow(df),8])))

ggplot(data = dfas, aes(x = "", y = value, fill = credit )) + 
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = ""), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme(legend.position='bottom') + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))


qqnorm(c(smth[1:nrow(smth),18]), xlab = df[1])
qqline(c(smth[1:nrow(smth),18]))

qplot(c(df[1:nrow(df),2]), c(df[1:nrow(df),1]))


# pie

df_reshaped <- data.frame(x = df$id,                           
                          y = c((df[1:nrow(df),2]) , (df[1:nrow(df),4]) , (df[1:nrow(df),6]) , (df[1:nrow(df),8])),
                          group = c(rep(df$id)))

ggplot(df_reshaped, aes("", y, col = group)) +
  geom_bar(stat="identity", width=1, color="white") + coord_polar("x", start=0)


daf = data.frame(x <- c(df[1:nrow(df),2] , df[1:nrow(df),4] , df[1:nrow(df),6] , df[1:nrow(df),8]),
                 labels<- c(rep(colnames(df[2]), nrow(df)), rep(colnames(df[4]), nrow(df)), rep(colnames(df[6]), nrow(df)),rep(colnames(df[8]), nrow(df))),
                  y<- c(row.names(df[0]),row.names(df[0]),row.names(df[0]),row.names(df[0])))

ggplot(daf, aes(x, "", fill=labels)) +geom_bar(width = 1, stat = "identity") +
  coord_polar("x", start=0)


pie(x = as.integer((df[1:nrow(df),2]) , (df[1:nrow(df),4]) , (df[1:nrow(df),6]) , (df[1:nrow(df),8])), labels = rep(rownames(df[0]), 4), col = rainbow(length(x)))
legend("topright", c(colnames(df[seq(2, 8, 2)])), cex = 0.8, fill = rainbow(length(x)))
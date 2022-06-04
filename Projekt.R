setwd('E:/Projekt grupowy')
getwd()
list.files()


populacja <- read.csv2("dictionary.csv",header = T,encoding = "UTF-8",sep=",")
zimowe <- read.csv2("winter.csv",header = T,encoding = "UTF-8",sep=",")
letnie <- read.csv2("summer.csv",header = T,encoding = "UTF-8",sep=",")


#NORMALIZOWANIE KODU ISO3


library(rvest)
url <- paste0("https://www.iban.com/country-codes")
path <- "/html/body/div[1]/div[2]/div/div/div/div/table"
out <- html_nodes(read_html(url),xpath=path)
(ISO <- html_table(out)) # to jest lista
isomodel <- ISO[[1]]


#usuwam niepotrzebne kolumny
isomodel <- isomodel[-2]
isomodel <- isomodel[-3]
isomodel



letnie$Country <- gsub("CHI","CHL",letnie$Country)
zimowe$Country <- gsub("CHI","CHL",zimowe$Country)
populacja$Code <- gsub("CHI","CHL",populacja$Code)
letnie$Country <- gsub("GER","DEU",letnie$Country)
zimowe$Country <- gsub("GER","DEU",zimowe$Country)
populacja$Code <- gsub("GER","DEU",populacja$Code)
letnie$Country <- gsub("GRE","GRC",letnie$Country)
zimowe$Country <- gsub("GRE","GRC",zimowe$Country)
populacja$Code <- gsub("GRE","GRC",populacja$Code)
letnie$Country <- gsub("DEN","DNK",letnie$Country)
populacja$Code <- gsub("DEN","DNK",populacja$Code)
zimowe$Country <- gsub("DEN","DNK",zimowe$Country)
letnie$Country <- gsub("SUI","CHE",letnie$Country)
zimowe$Country <- gsub("SUI","CHE",zimowe$Country)
populacja$Code <- gsub("SUI","CHE",populacja$Code)
letnie$Country <- gsub("NED","NLD",letnie$Country)
zimowe$Country <- gsub("NED","NLD",zimowe$Country)
populacja$Code <- gsub("NED","NLD",populacja$Code)
letnie$Country <- gsub("RSA","ZAF",letnie$Country)
zimowe$Country <- gsub("RSA","ZAF",zimowe$Country)
populacja$Code <- gsub("RSA","ZAF",populacja$Code)
letnie$Country <- gsub("POR","PRT",letnie$Country)
zimowe$Country <- gsub("POR","PRT",zimowe$Country)
populacja$Code <- gsub("POR","PRT",populacja$Code)
letnie$Country <- gsub("URU","URY",letnie$Country)
zimowe$Country <- gsub("URU","URY",zimowe$Country)
populacja$Code <- gsub("URU","URY",populacja$Code)
letnie$Country <- gsub("HAI","HTI",letnie$Country)
zimowe$Country <- gsub("HAI","HTI",zimowe$Country)
populacja$Code <- gsub("HAI","HTI",populacja$Code)
letnie$Country <- gsub("PHI","PHL",letnie$Country)
zimowe$Country <- gsub("PHI","PHL",zimowe$Country)
populacja$Code <- gsub("PHI","PHL",populacja$Code)
letnie$Country <- gsub("LAT","LVA",letnie$Country)
zimowe$Country <- gsub("LAT","LVA",zimowe$Country)
populacja$Code <- gsub("LAT","LVA",populacja$Code)
letnie$Country <- gsub("SRI","LKA",letnie$Country)
zimowe$Country <- gsub("SRI","LKA",zimowe$Country)
populacja$Code <- gsub("SRI","LKA",populacja$Code)
letnie$Country <- gsub("PUR","PRI",letnie$Country)
zimowe$Country <- gsub("PUR","PRI",zimowe$Country)
populacja$Code <- gsub("PUR","PRI",populacja$Code)
letnie$Country <- gsub("IRI","IRN",letnie$Country)
zimowe$Country <- gsub("IRI","IRN",zimowe$Country)
populacja$Code <- gsub("IRI","IRN",populacja$Code)
letnie$Country <- gsub("TRI","TTO",letnie$Country)
zimowe$Country <- gsub("TRI","TTO",zimowe$Country)
populacja$Code <- gsub("TRI","TTO",populacja$Code)
letnie$Country <- gsub("BUL","BGR",letnie$Country)
zimowe$Country <- gsub("BUL","BGR",zimowe$Country)
populacja$Code <- gsub("BUL","BGR",populacja$Code)
letnie$Country <- gsub("LIB","LBN",letnie$Country)
zimowe$Country <- gsub("LIB","LBN",zimowe$Country)
populacja$Code <- gsub("LIB","LBN",populacja$Code)
letnie$Country <- gsub("BAH","BHS",letnie$Country)
zimowe$Country <- gsub("BAH","BHS",zimowe$Country)
populacja$Code <- gsub("BAH","BHS",populacja$Code)
letnie$Country <- gsub("TPE","TWN",letnie$Country)
zimowe$Country <- gsub("TPE","TWN",zimowe$Country)
populacja$Code <- gsub("TPE","TWN",populacja$Code)
letnie$Country <- gsub("SIN","SGP",letnie$Country)
zimowe$Country <- gsub("SIN","SGP",zimowe$Country)
populacja$Code <- gsub("SIN","SGP",populacja$Code)
letnie$Country <- gsub("NGR","NGA",letnie$Country)
zimowe$Country <- gsub("NGR","NGA",zimowe$Country)
populacja$Code <- gsub("NGR","NGA",populacja$Code)
letnie$Country <- gsub("TPE","TWN",letnie$Country)
zimowe$Country <- gsub("TPE","TWN",zimowe$Country)
populacja$Code <- gsub("TPE","TWN",populacja$Code)
letnie$Country <- gsub("MGL","MNG",letnie$Country)
zimowe$Country <- gsub("MGL","MNG",zimowe$Country)
populacja$Code <- gsub("MGL","MNG",populacja$Code)
letnie$Country <- gsub("NIG","NER",letnie$Country)
zimowe$Country <- gsub("NIG","NER",zimowe$Country)
populacja$Code <- gsub("NIG","NER",populacja$Code)
letnie$Country <- gsub("BER","BMU",letnie$Country)
zimowe$Country <- gsub("BER","BMU",zimowe$Country)
populacja$Code <- gsub("BER","BMU",populacja$Code)
letnie$Country <- gsub("TZN","TZA",letnie$Country)
zimowe$Country <- gsub("TZN","TZA",zimowe$Country)
populacja$Code <- gsub("TZN","TZA",populacja$Code)
letnie$Country <- gsub("ZIM","ZWE",letnie$Country)
zimowe$Country <- gsub("ZIM","ZWE",zimowe$Country)
populacja$Code <- gsub("ZIM","ZWE",populacja$Code)
letnie$Country <- gsub("ZAM","ZMB",letnie$Country)
zimowe$Country <- gsub("ZAM","ZMB",zimowe$Country)
populacja$Code <- gsub("ZAM","ZMB",populacja$Code)
letnie$Country <- gsub("ALG","DZA",letnie$Country)
zimowe$Country <- gsub("ALG","DZA",zimowe$Country)
populacja$Code <- gsub("ALG","DZA",populacja$Code)
letnie$Country <- gsub("CRC","CRI",letnie$Country)
zimowe$Country <- gsub("CRC","CRI",zimowe$Country)
populacja$Code <- gsub("CRC","CRI",populacja$Code)
letnie$Country <- gsub("INA","IDN",letnie$Country)
zimowe$Country <- gsub("INA","IDN",zimowe$Country)
populacja$Code <- gsub("INA","IDN",populacja$Code)
letnie$Country <- gsub("ISV","VIR",letnie$Country)
zimowe$Country <- gsub("ISV","VIR",zimowe$Country)
populacja$Code <- gsub("ISV","VIR",populacja$Code)
letnie$Country <- gsub("MAS","MYS",letnie$Country)
zimowe$Country <- gsub("MAS","MYS",zimowe$Country)
populacja$Code <- gsub("MAS","MYS",populacja$Code)
letnie$Country <- gsub("CRO","HRV",letnie$Country)
zimowe$Country <- gsub("CRO","HRV",zimowe$Country)
populacja$Code <- gsub("CRO","HRV",populacja$Code)
letnie$Country <- gsub("SLO","SVN",letnie$Country)
zimowe$Country <- gsub("SLO","SVN",zimowe$Country)
populacja$Code <- gsub("SLO","SVN",populacja$Code)
letnie$Country <- gsub("TGA","TON",letnie$Country)
zimowe$Country <- gsub("TGA","TON",zimowe$Country)
populacja$Code <- gsub("TGA","TON",populacja$Code)
letnie$Country <- gsub("KSA","SAU",letnie$Country)
zimowe$Country <- gsub("KSA","SAU",zimowe$Country)
populacja$Code <- gsub("KSA","SAU",populacja$Code)
letnie$Country <- gsub("BAR","BRB",letnie$Country)
zimowe$Country <- gsub("BAR","BRB",zimowe$Country)
populacja$Code <- gsub("BAR","BRB",populacja$Code)
letnie$Country <- gsub("KSA","SAU",letnie$Country)
zimowe$Country <- gsub("KSA","SAU",zimowe$Country)
populacja$Code <- gsub("KSA","SAU",populacja$Code)
letnie$Country <- gsub("KUW","KWT",letnie$Country)
zimowe$Country <- gsub("KUW","KWT",zimowe$Country)
populacja$Code <- gsub("KUW","KWT",populacja$Code)
letnie$Country <- gsub("VIE","VNM",letnie$Country)
zimowe$Country <- gsub("VIE","VNM",zimowe$Country)
populacja$Code <- gsub("VIE","VNM",populacja$Code)
letnie$Country <- gsub("PAR","PRY",letnie$Country)
zimowe$Country <- gsub("PAR","PRY",zimowe$Country)
populacja$Code <- gsub("PAR","PRY",populacja$Code)
letnie$Country <- gsub("UAE","ARE",letnie$Country)
zimowe$Country <- gsub("UAE","ARE",zimowe$Country)
populacja$Code <- gsub("UAE","ARE",populacja$Code)
letnie$Country <- gsub("SUD","SDN",letnie$Country)
zimowe$Country <- gsub("SUD","SDN",zimowe$Country)
populacja$Code <- gsub("SUD","SDN",populacja$Code)
letnie$Country <- gsub("MRI","MUS",letnie$Country)
zimowe$Country <- gsub("MRI","MUS",zimowe$Country)
populacja$Code <- gsub("MRI","MUS",populacja$Code)
letnie$Country <- gsub("TOG","TGO",letnie$Country)
zimowe$Country <- gsub("TOG","TGO",zimowe$Country)
populacja$Code <- gsub("TOG","TGO",populacja$Code)
letnie$Country <- gsub("GUA","GTM",letnie$Country)
zimowe$Country <- gsub("GUA","GTM",zimowe$Country)
populacja$Code <- gsub("GUA","GTM",populacja$Code)
letnie$Country <- gsub("GRN","GRD",letnie$Country)
zimowe$Country <- gsub("GRN","GRD",zimowe$Country)
populacja$Code <- gsub("GRN","GRD",populacja$Code)
letnie$Country <- gsub("BOT","BWA",letnie$Country)
zimowe$Country <- gsub("BOT","BWA",zimowe$Country)
populacja$Code <- gsub("BOT","BWA",populacja$Code)




####### PRZEDSTAWIANIE LICZBY MEDALI DLA KA¯DEGO KRAJU NA PODSTAWIE MAPY ##########


igrzyska <- rbind(zimowe,letnie)
igrzyska <- igrzyska[order(igrzyska$Year),]

setdiff(igrzyska$Country,isomodel$`Alpha-3 code`)
length(setdiff(igrzyska$Country,isomodel$`Alpha-3 code`))

krajeMedale <- aggregate(igrzyska["Medal"],igrzyska["Country"],FUN=length)



#po przegladnieciu liczby medali na oficjalnych stronach zdecydowalem sie
#usunac pierwsza kolumne, poniewaz liczba medali nie miala przepisanego panstwa (komitetu)
krajeMedale <- krajeMedale[-1,]
krajeMedale

#nastepnym krokiem jest dodanie liczby medali do Rosji i Niemiec, zdobytych przez te pañstwa
#wczesniej pod innymi szyldami

krajeMedale$Country <- as.factor(krajeMedale$Country)
levels(krajeMedale$Country)

#NIEMCY GER = (GER+FRG+GDR+EUA)
#584(FRG) + 985(GDR) + 287(EUA)

krajeMedale$Country[33]
krajeMedale$Medal[33] <- krajeMedale$Medal[33] + 985 + 287 + 584

#ROSJA RUS =  (RUS+URS+RU1) +OKR, ale nie dotyczy nas, bo mamy dane do 2014
#2489(URS) + 17(RU1)

krajeMedale$Country[111]
krajeMedale$Medal[111] <- krajeMedale$Medal[111] + 2489 + 17




############## MAPA ŒWIATA I EUROPY WZGLEDEM MEDALI ####################


###### ŒWIAT ######


library(rworldmap)
library(RColorBrewer)
library(colorspace)


png("MedaleNaSwiecie.png",width=922,height=600)

krajeMedaleDM <- joinCountryData2Map(krajeMedale, 
                                     joinCode = "ISO3",
                                     nameJoinColumn = "Country",
                                     mapResolution = "coarse")


paletaMedalowa <- choose_palette()
mapa <- mapCountryData(krajeMedaleDM, 
               nameColumnToPlot="Medal", 
               mapTitle="Kraje œwiata wzglêdem liczby medali",
               catMethod = c(range(1:5),range(6:10),range(11:50),
                             range(51:100),range(101:500),range(501:1000),
                             range(1001:2500),range(2501:5000),range(5001:5238)),
               missingCountryCol = gray(.8),
               oceanCol = "lightblue",
               borderCol = "grey",
               colourPalette = paletaMedalowa(9),
               addLegend=F)


addMapLegendBoxes(
  cutVector=c("Brak danych/medali","1-5","6-10","11-50","51-100","101-500","501-1000","1001-2500",
              "2501-5000","5238") 
  ,colourVector = c("lightgrey","#DAFF47" ,"#E9CF00", "#EDA200", "#E4774D" ,
                    "#D24E71", "#B62485" ,"#91008D" ,"#5F008C" ,"#001889")
  ,x='bottomleft'
  ,title="Przedzia³y medali"
  ,pt.cex=2 
  ,col="black" 
  ,bg="white"
)


dev.off()


########## EUROPA ###########

png("MedaleWEuropie.png",width=922,height=600)

europaMedaleDM <- joinCountryData2Map(krajeMedale, 
                                     joinCode = "ISO3",
                                     nameJoinColumn = "Country",
                                     mapResolution = "high")


europa <- mapCountryData(europaMedaleDM, 
                       nameColumnToPlot="Medal", 
                       mapTitle="Kraje Europy wzglêdem liczby medali",
                       catMethod =  c(range(1:5),range(6:10),range(11:50),
                                      range(51:100),range(101:500),range(501:1000),
                                      range(1001:2500),range(2501:5000),range(5001:5238)),
                       missingCountryCol = gray(.8),
                       oceanCol = "lightblue",
                       mapRegion ="Europe",
                       borderCol = "grey",
                       colourPalette = paletaMedalowa(9),
                       addLegend=F)

addMapLegendBoxes(
  cutVector=c("Brak danych/medali","1-5","6-10","11-50","51-100","101-500","501-1000","1001-2500",
              "2501-3537") 
  ,colourVector = c("lightgrey","#DAFF47" ,"#E9CF00", "#EDA200", "#E4774D" ,
                    "#D24E71", "#B62485" ,"#91008D" ,"#5F008C" )
  ,x='bottomleft'
  ,title="Przedzia³y medali"
  ,pt.cex=2 
  ,col="black" 
  ,bg="white"
)

dev.off()


###########################################################################


############## MAPA ŒWIATA I EUROPY WZGLEDEM POPULACJI ####################

krajePopulacja <- data.frame(panstwo = populacja$Code,
                             populacja = populacja$Population)


png("PopulacjaNaSwiecie.png",width=922,height=600)

krajePopulacjaDM <- joinCountryData2Map(krajePopulacja, 
                                     joinCode = "ISO3",
                                     nameJoinColumn = "panstwo",
                                     mapResolution = "coarse")

paletaPopulacyjna <- choose_palette()
mapaPopulacja <- mapCountryData(krajePopulacjaDM, 
                       nameColumnToPlot="populacja", 
                       mapTitle="Kraje œwiata wzglêdem populacji",
                       catMethod = c(range(1:100000),
                                     range(100001:500000),
                                     range(500001:1000000),
                                     range(1000001:5000000),
                                     range(5000001:10000000),
                                     range(10000001:50000000),
                                     range(50000001:100000000),
                                     range(100000001:500000000),
                                     range(500000001:1000000000),
                                     range(1000000001:1400000000)),
                       missingCountryCol = gray(.8),
                       oceanCol = "lightblue",
                       borderCol = "grey",
                       colourPalette = paletaPopulacyjna(10),
                       addLegend=F)

addMapLegendBoxes(
  cutVector=c("Brak danych","1-100tys","100tys-500tys","500tys-1mln",
              "1mln-5mln","5mln-10mln","10mln-50mln",
              "50mln-100mln","100mln-500mln","500mln-1mld",
              "1mld-1,4mld") 
  ,colourVector = c("lightgrey","#DAFF47" ,"#E8D400", "#EDAC00" ,"#E8853A",
                    "#DB6063" ,"#C73B7B", "#AB1488", "#87008D",
                    "#58008B", "#001889")
  ,x='bottomleft'
  ,title="Liczba ludnoœci"
  ,pt.cex=2 
  ,col="black" 
  ,bg="white"
)

dev.off()

png("PopulacjaWEuropie.png",width=922,height=600)

europaPopulacjaDM <- joinCountryData2Map(krajePopulacja, 
                                      joinCode = "ISO3",
                                      nameJoinColumn = "panstwo",
                                      mapResolution = "high")


europa <- mapCountryData(europaPopulacjaDM, 
                         nameColumnToPlot="populacja", 
                         mapTitle="Kraje Europy wzglêdem populacji",
                         catMethod =  c(range(1:100000),
                                        range(100001:500000),
                                        range(500001:1000000),
                                        range(1000001:5000000),
                                        range(5000001:10000000),
                                        range(10000001:50000000),
                                        range(50000001:100000000),
                                        range(100000001:500000000),
                                        range(500000001:1000000000),
                                        range(1000000001:1400000000)),
                         missingCountryCol = gray(.8),
                         oceanCol = "lightblue",
                         mapRegion ="Europe",
                         borderCol = "grey",
                         colourPalette = paletaPopulacyjna(10),
                         addLegend=F)

addMapLegendBoxes(
  cutVector=c("Brak danych","1-100tys","100tys-500tys","500tys-1mln",
              "1mln-5mln","5mln-10mln","10mln-50mln",
              "50mln-100mln","100mln-145mln") 
  ,colourVector = c("lightgrey","#DAFF47" ,"#E8D400", "#EDAC00" ,"#E8853A",
                    "#DB6063" ,"#C73B7B", "#AB1488", "#87008D")
  ,x='bottomleft'
  ,title="Liczba ludnoœci"
  ,pt.cex=2 
  ,col="black" 
  ,bg="white"
)

dev.off()


############################################################################



### LICZBA UNIKALNYCH KRAJOW ZDOBYWAJACYCH MEDALE NA LETNICH IGRZYSKACH ####

unikalneKraje <- data.frame(rok=igrzyska$Year,
                            panstwo=igrzyska$Country)

podzialUnikalnychKrajow <- split(unikalneKraje, unikalneKraje$rok)

podzialUnikalnychKrajow[[1]][,2] #wszystkie panstwa medalowe w roku i=1
unique(podzialUnikalnychKrajow[[1]][,2]) #unikatowe
length(unique(podzialUnikalnychKrajow[[1]][,2]))

podzialUnikalnychKrajow[[3]][1][1,]

ramkaUnikalne <- data.frame(rok=podzialUnikalnychKrajow[[1]][1][1,],
                            liczba=length(unique(podzialUnikalnychKrajow[[1]][,2])))

for(i in 2:length(podzialUnikalnychKrajow)){
  if(as.numeric(podzialUnikalnychKrajow[[i]][1][1,])%%4==0){
  doDolaczenia <- c(podzialUnikalnychKrajow[[i]][1][1,],length(unique(podzialUnikalnychKrajow[[i]][,2])))
  ramkaUnikalne <- rbind(ramkaUnikalne,doDolaczenia)
}}


png("LiczbaKrajowZMedalemNaDanychIgrzyskach.png",width=922,height=600)

plot.new()

x <- plot(ramkaUnikalne$rok,ramkaUnikalne$liczba, xlab="Rok igrzysk",
     ylab="Liczba krajów z medalami", col="red",pch=18,bty="L",
     cex=2, main="Liczba krajów zdobywaj¹cych medale na poszczególnych letnich igrzyskach")

lines(ramkaUnikalne$rok,ramkaUnikalne$liczba,lwd=2,col="pink")

abline(v = seq(1896, 2014, 4),
       lty = 2, col = "lightgray")

abline(h = seq(0,90, 4),
       lty = 2, col = "gray")

points(ramkaUnikalne$rok,ramkaUnikalne$liczba, pch = 19, col = "red",cex=2)

dev.off()

#################################################################




############ MEDALE WZGLÊDEM P£CI NA KA¯DYCH IGRZYSKACH #########

medalePlec <- data.frame(rok=igrzyska$Year,
                         plec=igrzyska$Gender,
                         medal=igrzyska$Medal)

podzial <- split(medalePlec,medalePlec$rok)

podzial1 <- aggregate(podzial[[1]]['medal'],podzial[[1]]['plec'],FUN=length)

ramkaPlecDP <- data.frame(rok=podzial[[1]][1][1,],
                          kobiety=0,
                          mezczyzni=podzial1[2][1,])

  
  for (i in 2:length(podzial)){
  podzial1 <- aggregate(podzial[[i]]['medal'],podzial[[i]]['plec'],FUN=length)
  doDolaczenia <- c(podzial[[i]][1][1,],podzial1[2][2,],podzial1[2][1,])
  ramkaPlecDP <- rbind(ramkaPlecDP,doDolaczenia)
}


ramkaPlecDP
ramkaPlecDP <- ramkaPlecDP[-c(23,25,27,29,31,33),] #usuwam zimowe igrzyska


png("LiczbaKrajowDlaPoszczegolnejPlci.png",width=922,height=600)

plot.new()

plot(ramkaPlecDP$rok,ramkaPlecDP$mezczyzni, xlab="Rok igrzysk", type ="p",
          ylab="Liczba medali dla poszczególnej p³ci", col="green4",pch=18,bty="L",
          cex=2, main="Liczba medali dla poszczególnej p³ci na letnich igrzyskach",
     ylim=c(-50,1400))
lines(ramkaPlecDP$rok,ramkaPlecDP$mezczyzni,lwd=2,col="lightgreen")
points(ramkaPlecDP$rok,ramkaPlecDP$mezczyzni, pch = 18, col = "green4",cex=2)

lines(ramkaPlecDP$rok,ramkaPlecDP$kobiety,lwd=2,col="pink")
points(ramkaPlecDP$rok,ramkaPlecDP$kobiety, pch = 16, col = "red",cex=2)


abline(v = seq(1896, 2014, 4),
       lty = 2, col = "lightgray")

abline(h = seq(0,1500, 100),
       lty = 2, col = "gray")

legend("bottomright", legend = c("Mezczyzni","Kobiety"), 
        col = c("green4","red"), pch = c(18,16),cex=1.5)

dev.off()


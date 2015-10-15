# STAT 6306 HW5
##Name: Kevin Kyoo Ha Cha
##Let's Download and tidying GDP data first
##We will download and merge educational data later
required package: plyr, Hmisc


```r
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
gdp <- read.csv(url(fileURL), header=T)
names(gdp)
```

```
##  [1] "X"                           "Gross.domestic.product.2012"
##  [3] "X.1"                         "X.2"                        
##  [5] "X.3"                         "X.4"                        
##  [7] "X.5"                         "X.6"                        
##  [9] "X.7"                         "X.8"
```

```r
head(gdp)
```

```
##     X Gross.domestic.product.2012 X.1           X.2          X.3 X.4 X.5
## 1                                  NA                                 NA
## 2                                  NA               (millions of      NA
## 3                         Ranking  NA       Economy  US dollars)      NA
## 4                                  NA                                 NA
## 5 USA                           1  NA United States  16,244,600       NA
## 6 CHN                           2  NA         China   8,227,103       NA
##   X.6 X.7 X.8
## 1  NA  NA  NA
## 2  NA  NA  NA
## 3  NA  NA  NA
## 4  NA  NA  NA
## 5  NA  NA  NA
## 6  NA  NA  NA
```


```r
#remove first 4 rows in this csv file
gdp <-read.csv(url(fileURL),skip=4)#now it is better
head(gdp)
```

```
##     X X.1 X.2            X.3          X.4 X.5 X.6 X.7 X.8 X.9
## 1 USA   1  NA  United States  16,244,600       NA  NA  NA  NA
## 2 CHN   2  NA          China   8,227,103       NA  NA  NA  NA
## 3 JPN   3  NA          Japan   5,959,718       NA  NA  NA  NA
## 4 DEU   4  NA        Germany   3,428,131       NA  NA  NA  NA
## 5 FRA   5  NA         France   2,612,878       NA  NA  NA  NA
## 6 GBR   6  NA United Kingdom   2,471,784       NA  NA  NA  NA
```

##It looks little bit better, but we can do more tidying as naming varialbes rather than x1, x2 etc..
##We will determine how many NAs as well as excluding them
##We will also convert the data type from character to numeric for a variable, GDP


```r
#when you see the bottom of this csv file, there are paragraphs so we have problem
#so when r see these characters to goes
gdp <-read.csv(url(fileURL),skip=4, na.strings=c("..","Not available.",".. Not available."),stringsAsFactors=FALSE)
str(gdp)#it looks little better
```

```
## 'data.frame':	326 obs. of  10 variables:
##  $ X  : chr  "USA" "CHN" "JPN" "DEU" ...
##  $ X.1: chr  "1" "2" "3" "4" ...
##  $ X.2: logi  NA NA NA NA NA NA ...
##  $ X.3: chr  "United States" "China" "Japan" "Germany" ...
##  $ X.4: chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
##  $ X.5: chr  "" "" "" "" ...
##  $ X.6: logi  NA NA NA NA NA NA ...
##  $ X.7: logi  NA NA NA NA NA NA ...
##  $ X.8: logi  NA NA NA NA NA NA ...
##  $ X.9: logi  NA NA NA NA NA NA ...
```

```r
#let's get rid of NA
gdp <- dplyr::select(gdp, X,X.1,X.3,X.4)
str(gdp)
```

```
## 'data.frame':	326 obs. of  4 variables:
##  $ X  : chr  "USA" "CHN" "JPN" "DEU" ...
##  $ X.1: chr  "1" "2" "3" "4" ...
##  $ X.3: chr  "United States" "China" "Japan" "Germany" ...
##  $ X.4: chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
```

```r
#need to make dollar and ranking numeric now
names(gdp) <- c("CountryCode", "Ranking", "Economy", "GDP")
str(gdp) #it is better now.
```

```
## 'data.frame':	326 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : chr  "1" "2" "3" "4" ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
```

```r
gdp$Ranking <- as.numeric(gdp$Ranking)#we'll have problem so look at the last 40 rows
```

```
## Warning: NAs introduced by coercion
```

```r
tail(gdp,40)#so we have many NAs so which is a problem
```

```
##     CountryCode Ranking Economy GDP
## 287                  NA            
## 288                  NA            
## 289                  NA            
## 290                  NA            
## 291                  NA            
## 292                  NA            
## 293                  NA            
## 294                  NA            
## 295                  NA            
## 296                  NA            
## 297                  NA            
## 298                  NA            
## 299                  NA            
## 300                  NA            
## 301                  NA            
## 302                  NA            
## 303                  NA            
## 304                  NA            
## 305                  NA            
## 306                  NA            
## 307                  NA            
## 308                  NA            
## 309                  NA            
## 310                  NA            
## 311                  NA            
## 312                  NA            
## 313                  NA            
## 314                  NA            
## 315                  NA            
## 316                  NA            
## 317                  NA            
## 318                  NA            
## 319                  NA            
## 320                  NA            
## 321                  NA            
## 322                  NA            
## 323                  NA            
## 324                  NA            
## 325                  NA            
## 326                  NA
```

```r
sum(is.na(gdp$Ranking))
```

```
## [1] 136
```

```r
dim(gdp)
```

```
## [1] 326   4
```

```r
326-136
```

```
## [1] 190
```

```r
gdp[180:200,] #we see NAs starting from 191, the data is not meaningful, we will delete NAs
```

```
##     CountryCode Ranking                        Economy   GDP
## 180         VCT     180 St. Vincent and the Grenadines  713 
## 181         WSM     181                          Samoa  684 
## 182         COM     182                        Comoros  596 
## 183         DMA     183                       Dominica  480 
## 184         TON     184                          Tonga  472 
## 185         FSM     185          Micronesia, Fed. Sts.  326 
## 186         STP     186          São Tomé and Principe  263 
## 187         PLW     187                          Palau  228 
## 188         MHL     188               Marshall Islands  182 
## 189         KIR     189                       Kiribati  175 
## 190         TUV     190                         Tuvalu   40 
## 191                  NA                                     
## 192         ASM      NA                 American Samoa  <NA>
## 193         ADO      NA                        Andorra  <NA>
## 194         CYM      NA                 Cayman Islands  <NA>
## 195         CHI      NA                Channel Islands  <NA>
## 196         CUW      NA                        Curaçao  <NA>
## 197         DJI      NA                       Djibouti      
## 198         FRO      NA                 Faeroe Islands  <NA>
## 199         PYF      NA               French Polynesia  <NA>
## 200         GRL      NA                      Greenland  <NA>
```

```r
summary(gdp[191:326,]) #so we only have many many NAs, we confirm that we should delete NAs
```

```
##  CountryCode           Ranking      Economy              GDP           
##  Length:136         Min.   : NA   Length:136         Length:136        
##  Class :character   1st Qu.: NA   Class :character   Class :character  
##  Mode  :character   Median : NA   Mode  :character   Mode  :character  
##                     Mean   :NaN                                        
##                     3rd Qu.: NA                                        
##                     Max.   : NA                                        
##                     NA's   :136
```

```r
gdp <- gdp[1:190,]
str(gdp)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
```

```r
#then, now lets change ranking to numeric variable 
gdp$Ranking <- as.numeric(gdp$Ranking)
str(gdp)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
```

```r
tail(gdp, 40)
```

```
##     CountryCode Ranking                        Economy     GDP
## 151         MNE     151                     Montenegro  4,373 
## 152         MWI     152                         Malawi  4,264 
## 153         BRB     153                       Barbados  4,225 
## 154         MRT     154                     Mauritania  4,199 
## 155         FJI     155                           Fiji  3,908 
## 156         TGO     156                           Togo  3,814 
## 157         SLE     157                   Sierra Leone  3,796 
## 158         SWZ     158                      Swaziland  3,744 
## 159         ERI     159                        Eritrea  3,092 
## 160         GUY     160                         Guyana  2,851 
## 161         ABW     161                          Aruba  2,584 
## 162         BDI     162                        Burundi  2,472 
## 163         LSO     163                        Lesotho  2,448 
## 164         MDV     164                       Maldives  2,222 
## 165         CAF     165       Central African Republic  2,184 
## 166         CPV     166                     Cape Verde  1,827 
## 167         BTN     167                         Bhutan  1,780 
## 168         LBR     168                        Liberia  1,734 
## 169         BLZ     169                         Belize  1,493 
## 170         TMP     170                    Timor-Leste  1,293 
## 171         LCA     171                      St. Lucia  1,239 
## 172         ATG     172            Antigua and Barbuda  1,134 
## 173         SYC     173                     Seychelles  1,129 
## 174         SLB     174                Solomon Islands  1,008 
## 175         GMB     175                    Gambia, The    917 
## 176         GNB     176                  Guinea-Bissau    822 
## 177         VUT     177                        Vanuatu    787 
## 178         GRD     178                        Grenada    767 
## 179         KNA     178            St. Kitts and Nevis    767 
## 180         VCT     180 St. Vincent and the Grenadines    713 
## 181         WSM     181                          Samoa    684 
## 182         COM     182                        Comoros    596 
## 183         DMA     183                       Dominica    480 
## 184         TON     184                          Tonga    472 
## 185         FSM     185          Micronesia, Fed. Sts.    326 
## 186         STP     186          São Tomé and Principe    263 
## 187         PLW     187                          Palau    228 
## 188         MHL     188               Marshall Islands    182 
## 189         KIR     189                       Kiribati    175 
## 190         TUV     190                         Tuvalu     40
```

```r
sum(is.na(gdp$Ranking)) #there are 136 NAs
```

```
## [1] 0
```

```r
dim(gdp)
```

```
## [1] 190   4
```

```r
326-136 #there are 190 rows WITHOUT  NAs
```

```
## [1] 190
```

```r
gdp[180:200,] #Just want to double check
```

```
##      CountryCode Ranking                        Economy   GDP
## 180          VCT     180 St. Vincent and the Grenadines  713 
## 181          WSM     181                          Samoa  684 
## 182          COM     182                        Comoros  596 
## 183          DMA     183                       Dominica  480 
## 184          TON     184                          Tonga  472 
## 185          FSM     185          Micronesia, Fed. Sts.  326 
## 186          STP     186          São Tomé and Principe  263 
## 187          PLW     187                          Palau  228 
## 188          MHL     188               Marshall Islands  182 
## 189          KIR     189                       Kiribati  175 
## 190          TUV     190                         Tuvalu   40 
## NA          <NA>      NA                           <NA>  <NA>
## NA.1        <NA>      NA                           <NA>  <NA>
## NA.2        <NA>      NA                           <NA>  <NA>
## NA.3        <NA>      NA                           <NA>  <NA>
## NA.4        <NA>      NA                           <NA>  <NA>
## NA.5        <NA>      NA                           <NA>  <NA>
## NA.6        <NA>      NA                           <NA>  <NA>
## NA.7        <NA>      NA                           <NA>  <NA>
## NA.8        <NA>      NA                           <NA>  <NA>
## NA.9        <NA>      NA                           <NA>  <NA>
```

```r
summary (gdp[191:326,]) #to make sure it is alll NAs
```

```
##  CountryCode           Ranking      Economy              GDP           
##  Length:136         Min.   : NA   Length:136         Length:136        
##  Class :character   1st Qu.: NA   Class :character   Class :character  
##  Mode  :character   Median : NA   Mode  :character   Mode  :character  
##                     Mean   :NaN                                        
##                     3rd Qu.: NA                                        
##                     Max.   : NA                                        
##                     NA's   :136
```

```r
gdp <- gdp[1:190,] #set gdp data without NAs, this is the same as above steps
str(gdp)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
```

```r
gdp2 <- gdp
gdp2$GDP <- gsub("," , "" ,gdp2$GDP)
gdp2$GDP <- as.numeric(gdp2$GDP)
str(gdp2)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : num  16244600 8227103 5959718 3428131 2612878 ...
```

```r
#we had all NAs because of the commas on numbers (i.e. $65,000), we neet to remove them, try it on your test dataset gdp2
#we also convert from character to numeric on GDP data
gdp$GDP <- gsub("," , "" ,gdp$GDP)
str(gdp)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : chr  " 16244600 " " 8227103 " " 5959718 " " 3428131 " ...
```

```r
gdp$GDP <- as.numeric(gdp$GDP)
str(gdp)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : num  16244600 8227103 5959718 3428131 2612878 ...
```

```r
#we can use either gdp or gdp2 data because I tried something differnt before, but
#changed back to make both gdp and gdp2 are identical 
#######################################################################################
```

##Answering HW Questions..
##Question1

```r
#download and tidying educational data 
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
edu <- read.csv(url(fileURL))
names(edu)
```

```
##  [1] "CountryCode"                                      
##  [2] "Long.Name"                                        
##  [3] "Income.Group"                                     
##  [4] "Region"                                           
##  [5] "Lending.category"                                 
##  [6] "Other.groups"                                     
##  [7] "Currency.Unit"                                    
##  [8] "Latest.population.census"                         
##  [9] "Latest.household.survey"                          
## [10] "Special.Notes"                                    
## [11] "National.accounts.base.year"                      
## [12] "National.accounts.reference.year"                 
## [13] "System.of.National.Accounts"                      
## [14] "SNA.price.valuation"                              
## [15] "Alternative.conversion.factor"                    
## [16] "PPP.survey.year"                                  
## [17] "Balance.of.Payments.Manual.in.use"                
## [18] "External.debt.Reporting.status"                   
## [19] "System.of.trade"                                  
## [20] "Government.Accounting.concept"                    
## [21] "IMF.data.dissemination.standard"                  
## [22] "Source.of.most.recent.Income.and.expenditure.data"
## [23] "Vital.registration.complete"                      
## [24] "Latest.agricultural.census"                       
## [25] "Latest.industrial.data"                           
## [26] "Latest.trade.data"                                
## [27] "Latest.water.withdrawal.data"                     
## [28] "X2.alpha.code"                                    
## [29] "WB.2.code"                                        
## [30] "Table.Name"                                       
## [31] "Short.Name"
```

```r
head(edu)
```

```
##   CountryCode                    Long.Name         Income.Group
## 1         ABW                        Aruba High income: nonOECD
## 2         ADO      Principality of Andorra High income: nonOECD
## 3         AFG Islamic State of Afghanistan           Low income
## 4         AGO  People's Republic of Angola  Lower middle income
## 5         ALB          Republic of Albania  Upper middle income
## 6         ARE         United Arab Emirates High income: nonOECD
##                       Region Lending.category Other.groups  Currency.Unit
## 1  Latin America & Caribbean                                Aruban florin
## 2      Europe & Central Asia                                         Euro
## 3                 South Asia              IDA         HIPC Afghan afghani
## 4         Sub-Saharan Africa              IDA              Angolan kwanza
## 5      Europe & Central Asia             IBRD                Albanian lek
## 6 Middle East & North Africa                                U.A.E. dirham
##   Latest.population.census  Latest.household.survey
## 1                     2000                         
## 2           Register based                         
## 3                     1979               MICS, 2003
## 4                     1970 MICS, 2001, MIS, 2006/07
## 5                     2001               MICS, 2005
## 6                     2005                         
##                                                                 Special.Notes
## 1                                                                            
## 2                                                                            
## 3 Fiscal year end: March 20; reporting period for national accounts data: FY.
## 4                                                                            
## 5                                                                            
## 6                                                                            
##   National.accounts.base.year National.accounts.reference.year
## 1                        1995                               NA
## 2                                                           NA
## 3                   2002/2003                               NA
## 4                        1997                               NA
## 5                                                         1996
## 6                        1995                               NA
##   System.of.National.Accounts SNA.price.valuation
## 1                          NA                    
## 2                          NA                    
## 3                          NA                 VAB
## 4                          NA                 VAP
## 5                        1993                 VAB
## 6                          NA                 VAB
##   Alternative.conversion.factor PPP.survey.year
## 1                                            NA
## 2                                            NA
## 3                                            NA
## 4                       1991-96            2005
## 5                                          2005
## 6                                            NA
##   Balance.of.Payments.Manual.in.use External.debt.Reporting.status
## 1                                                                 
## 2                                                                 
## 3                                                           Actual
## 4                              BPM5                         Actual
## 5                              BPM5                         Actual
## 6                              BPM4                               
##   System.of.trade Government.Accounting.concept
## 1         Special                              
## 2         General                              
## 3         General                  Consolidated
## 4         Special                              
## 5         General                  Consolidated
## 6         General                  Consolidated
##   IMF.data.dissemination.standard
## 1                                
## 2                                
## 3                            GDDS
## 4                            GDDS
## 5                            GDDS
## 6                            GDDS
##   Source.of.most.recent.Income.and.expenditure.data
## 1                                                  
## 2                                                  
## 3                                                  
## 4                                         IHS, 2000
## 5                                        LSMS, 2005
## 6                                                  
##   Vital.registration.complete Latest.agricultural.census
## 1                                                       
## 2                         Yes                           
## 3                                                       
## 4                                                1964-65
## 5                         Yes                       1998
## 6                                                   1998
##   Latest.industrial.data Latest.trade.data Latest.water.withdrawal.data
## 1                     NA              2008                           NA
## 2                     NA              2006                           NA
## 3                     NA              2008                         2000
## 4                     NA              1991                         2000
## 5                   2005              2008                         2000
## 6                     NA              2008                         2005
##   X2.alpha.code WB.2.code           Table.Name           Short.Name
## 1            AW        AW                Aruba                Aruba
## 2            AD        AD              Andorra              Andorra
## 3            AF        AF          Afghanistan          Afghanistan
## 4            AO        AO               Angola               Angola
## 5            AL        AL              Albania              Albania
## 6            AE        AE United Arab Emirates United Arab Emirates
```

```r
summary(edu)
```

```
##   CountryCode                   Long.Name                 Income.Group
##  ABW    :  1   American Samoa        :  1                       :24   
##  ADO    :  1   Antigua and Barbuda   :  1   High income: nonOECD:37   
##  AFG    :  1   Arab Republic of Egypt:  1   High income: OECD   :30   
##  AGO    :  1   Argentine Republic    :  1   Low income          :40   
##  ALB    :  1   Aruba                 :  1   Lower middle income :56   
##  ARE    :  1   Barbados              :  1   Upper middle income :47   
##  (Other):228   (Other)               :228                             
##                         Region   Lending.category    Other.groups
##  Europe & Central Asia     :57        :92                  :178  
##  Sub-Saharan Africa        :47   Blend:16         Euro area: 16  
##  Latin America & Caribbean :38   IBRD :63         HIPC     : 40  
##  East Asia & Pacific       :36   IDA  :63                        
##                            :24                                   
##  Middle East & North Africa:21                                   
##  (Other)                   :11                                   
##                Currency.Unit Latest.population.census
##                       : 24   2001   :46              
##  Euro                 : 20   2000   :35              
##  CFA franc            : 14          :26              
##  U.S. dollar          : 13   2002   :21              
##  East Caribbean dollar:  6   2006   :15              
##  Australian dollar    :  3   2004   :12              
##  (Other)              :154   (Other):79              
##  Latest.household.survey
##            :112         
##  MICS, 2006: 23         
##  MICS, 2000:  8         
##  DHS, 2005 :  7         
##  DHS, 2007 :  6         
##  MICS, 2005:  6         
##  (Other)   : 72         
##                                                                      Special.Notes
##                                                                             :143  
##  Fiscal year end: June 30; reporting period for national accounts data: FY. :  7  
##  Fiscal year end: March 31; reporting period for national accounts data: CY.:  7  
##  Fiscal year end: June 30; reporting period for national accounts data: CY. :  6  
##  Fiscal year end: March 31; reporting period for national accounts data: FY.:  3  
##  Fiscal year end: March 20; reporting period for national accounts data: FY.:  2  
##  (Other)                                                                    : 66  
##  National.accounts.base.year National.accounts.reference.year
##         :68                  Min.   :1987                    
##  2000   :42                  1st Qu.:1996                    
##  1990   :16                  Median :2000                    
##  1995   :10                  Mean   :1999                    
##  1994   : 7                  3rd Qu.:2002                    
##  1997   : 6                  Max.   :2007                    
##  (Other):85                  NA's   :197                     
##  System.of.National.Accounts SNA.price.valuation
##  Min.   :1993                   : 40            
##  1st Qu.:1993                VAB:158            
##  Median :1993                VAP: 36            
##  Mean   :1993                                   
##  3rd Qu.:1993                                   
##  Max.   :1993                                   
##  NA's   :149                                    
##  Alternative.conversion.factor PPP.survey.year
##         :187                   Min.   :2005   
##  1990-95:  8                   1st Qu.:2005   
##  1987-95:  5                   Median :2005   
##  1993   :  3                   Mean   :2005   
##  1991   :  2                   3rd Qu.:2005   
##  1992-95:  2                   Max.   :2005   
##  (Other): 27                   NA's   :89     
##  Balance.of.Payments.Manual.in.use External.debt.Reporting.status
##      : 63                                     :106               
##  BPM4:  9                          Actual     : 93               
##  BPM5:162                          Estimate   : 13               
##                                    Preliminary: 22               
##                                                                  
##                                                                  
##                                                                  
##  System.of.trade Government.Accounting.concept
##         : 51                 : 87             
##  General:111     Budgetary   : 36             
##  Special: 72     Consolidated:111             
##                                               
##                                               
##                                               
##                                               
##  IMF.data.dissemination.standard
##      :71                        
##  GDDS:95                        
##  SDDS:68                        
##                                 
##                                 
##                                 
##                                 
##  Source.of.most.recent.Income.and.expenditure.data
##             : 93                                  
##  IHS, 2007  : 10                                  
##  IHS, 2000  :  9                                  
##  ES/BS, 2005:  6                                  
##  IHS, 2006  :  6                                  
##  ES/BS, 2004:  5                                  
##  (Other)    :105                                  
##  Vital.registration.complete Latest.agricultural.census
##     :131                              :100             
##  Yes:103                     2001     : 14             
##                              2000     : 12             
##                              1999-2000: 11             
##                              2002     :  8             
##                              2003     :  8             
##                              (Other)  : 81             
##  Latest.industrial.data Latest.trade.data Latest.water.withdrawal.data
##  Min.   :1995           Min.   :1975      Min.   :1990                
##  1st Qu.:2002           1st Qu.:2007      1st Qu.:2000                
##  Median :2004           Median :2008      Median :2000                
##  Mean   :2003           Mean   :2007      Mean   :2001                
##  3rd Qu.:2005           3rd Qu.:2008      3rd Qu.:2000                
##  Max.   :2006           Max.   :2008      Max.   :2006                
##  NA's   :139            NA's   :46        NA's   :82                  
##  X2.alpha.code   WB.2.code            Table.Name           Short.Name 
##         : 26          : 25   Afghanistan   :  1   Afghanistan   :  1  
##  AD     :  1   AD     :  1   Albania       :  1   Albania       :  1  
##  AE     :  1   AE     :  1   Algeria       :  1   Algeria       :  1  
##  AF     :  1   AF     :  1   American Samoa:  1   American Samoa:  1  
##  AG     :  1   AG     :  1   Andorra       :  1   Andorra       :  1  
##  (Other):203   (Other):204   Angola        :  1   Angola        :  1  
##  NA's   :  1   NA's   :  1   (Other)       :228   (Other)       :228
```

```r
#Merge data by country codes
matchedData <- merge(gdp2, edu, by.x = "CountryCode", by.y = "CountryCode")
head(matchedData)
```

```
##   CountryCode Ranking              Economy    GDP
## 1         ABW     161                Aruba   2584
## 2         AFG     105          Afghanistan  20497
## 3         AGO      60               Angola 114147
## 4         ALB     125              Albania  12648
## 5         ARE      32 United Arab Emirates 348595
## 6         ARG      26            Argentina 475502
##                      Long.Name         Income.Group
## 1                        Aruba High income: nonOECD
## 2 Islamic State of Afghanistan           Low income
## 3  People's Republic of Angola  Lower middle income
## 4          Republic of Albania  Upper middle income
## 5         United Arab Emirates High income: nonOECD
## 6           Argentine Republic  Upper middle income
##                       Region Lending.category Other.groups  Currency.Unit
## 1  Latin America & Caribbean                                Aruban florin
## 2                 South Asia              IDA         HIPC Afghan afghani
## 3         Sub-Saharan Africa              IDA              Angolan kwanza
## 4      Europe & Central Asia             IBRD                Albanian lek
## 5 Middle East & North Africa                                U.A.E. dirham
## 6  Latin America & Caribbean             IBRD              Argentine peso
##   Latest.population.census  Latest.household.survey
## 1                     2000                         
## 2                     1979               MICS, 2003
## 3                     1970 MICS, 2001, MIS, 2006/07
## 4                     2001               MICS, 2005
## 5                     2005                         
## 6                     2001                         
##                                                                 Special.Notes
## 1                                                                            
## 2 Fiscal year end: March 20; reporting period for national accounts data: FY.
## 3                                                                            
## 4                                                                            
## 5                                                                            
## 6                                                                            
##   National.accounts.base.year National.accounts.reference.year
## 1                        1995                               NA
## 2                   2002/2003                               NA
## 3                        1997                               NA
## 4                                                         1996
## 5                        1995                               NA
## 6                        1993                               NA
##   System.of.National.Accounts SNA.price.valuation
## 1                          NA                    
## 2                          NA                 VAB
## 3                          NA                 VAP
## 4                        1993                 VAB
## 5                          NA                 VAB
## 6                        1993                 VAB
##   Alternative.conversion.factor PPP.survey.year
## 1                                            NA
## 2                                            NA
## 3                       1991-96            2005
## 4                                          2005
## 5                                            NA
## 6                       1971-84            2005
##   Balance.of.Payments.Manual.in.use External.debt.Reporting.status
## 1                                                                 
## 2                                                           Actual
## 3                              BPM5                         Actual
## 4                              BPM5                         Actual
## 5                              BPM4                               
## 6                              BPM5                         Actual
##   System.of.trade Government.Accounting.concept
## 1         Special                              
## 2         General                  Consolidated
## 3         Special                              
## 4         General                  Consolidated
## 5         General                  Consolidated
## 6         Special                  Consolidated
##   IMF.data.dissemination.standard
## 1                                
## 2                            GDDS
## 3                            GDDS
## 4                            GDDS
## 5                            GDDS
## 6                            SDDS
##   Source.of.most.recent.Income.and.expenditure.data
## 1                                                  
## 2                                                  
## 3                                         IHS, 2000
## 4                                        LSMS, 2005
## 5                                                  
## 6                                         IHS, 2006
##   Vital.registration.complete Latest.agricultural.census
## 1                                                       
## 2                                                       
## 3                                                1964-65
## 4                         Yes                       1998
## 5                                                   1998
## 6                         Yes                       2002
##   Latest.industrial.data Latest.trade.data Latest.water.withdrawal.data
## 1                     NA              2008                           NA
## 2                     NA              2008                         2000
## 3                     NA              1991                         2000
## 4                   2005              2008                         2000
## 5                     NA              2008                         2005
## 6                   2001              2008                         2000
##   X2.alpha.code WB.2.code           Table.Name           Short.Name
## 1            AW        AW                Aruba                Aruba
## 2            AF        AF          Afghanistan          Afghanistan
## 3            AO        AO               Angola               Angola
## 4            AL        AL              Albania              Albania
## 5            AE        AE United Arab Emirates United Arab Emirates
## 6            AR        AR            Argentina            Argentina
```

```r
#Number of matched countries
dim(matchedData)[1]
```

```
## [1] 189
```

There are 189 matched IDs

##Question 2

```r
#Sort the data by GDP so United States is last
require(plyr)
```

```
## Loading required package: plyr
```

```r
arrange(matchedData,desc(Ranking))[13,1:4]
```

```
##    CountryCode Ranking             Economy GDP
## 13         KNA     178 St. Kitts and Nevis 767
```

The 13th country is KNA with GDP 767.

##Question 3

```r
#Subset "High income: OECD" and calculate the mean GDP Ranking
mean(subset(matchedData, Income.Group %in% "High income: OECD", select = c(Ranking))$Ranking)
```

```
## [1] 32.96667
```

```r
#Subset "High income: nonOECD" and calculate the mean GDP Ranking
mean(subset(matchedData, Income.Group %in% "High income: nonOECD", select = c(Ranking))$Ranking)
```

```
## [1] 91.91304
```

Average GDP ranking for "High income: OECD" is 32.96667

Average GDP ranking for "High Income: nonOECD" is 91.91304

##Question 4

```r
require(Hmisc)
```

```
## Loading required package: Hmisc
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## Loading required package: ggplot2
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:plyr':
## 
##     is.discrete, summarize
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
#Cut Ranks into 5 groups and store as factor variable
matchedData$Rank.Groups = cut2(matchedData$Rank, g = 5)
#Build a table of Income Groups across Rank Groups
table(matchedData$Income.Group, matchedData$Rank.Groups)
```

```
##                       
##                        [  1, 39) [ 39, 77) [ 77,115) [115,154) [154,190]
##                                0         0         0         0         0
##   High income: nonOECD         4         5         8         5         1
##   High income: OECD           18        10         1         1         0
##   Low income                   0         1         9        16        11
##   Lower middle income          5        13        12         8        16
##   Upper middle income         11         9         8         8         9
```

There are 5 countries with lower middle income but among the 38 nations with highest GDP

##Question 5
There are 136 NAs determined by sum(is.na(gdp$Ranking)), then I calculate the number of rows without NAs and it is 190 rows (326-136). I use gdp[180:200,] to see how the data looks like and I see NAs from line 191 to the end.

I used summary(gdp[191:326,]) to see how many NAs in order to confirm my findings above.
I renamed the data, gdp, containing only values without NAs using gdp <- gdp[1:190,]
See from line 24 to 56 to see how we tidying the gdp data.








```


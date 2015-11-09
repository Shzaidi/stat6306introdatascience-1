# STAT6306 CaseStudy2
Kevin Kyoo Ha Cha  
Nov 10, 2015  
## Case Study II: Using Data Science to Define Data Science
## Due Date: November 5, 2015

##Introduction
In this case study, We will use R web scraping and text mining technique to explore data scientist jobs using cybercoders.com. We are interested in finding a set of skills that each job post expects and want. Furthermore, we will also extract the information about salaries, location, and dateposted as well as drawing several word clouds for the skills for each job list.

### The Code
 We will make a separate function for each step, which should make the functions easier to read, test, maintain, and adjust as the format of the web pages changes. The function `cy.getFreeFormWords()` below fetches the lists of free-form text in the HTML document. The function then decomposes the text into the words in each element, using spaces and punctuation characters to separate them. This is done by calling the `asWords()` function. One of the arguements for `asWords()` is a list of "stop words", which are small words that are present in a large number of English sentences. We don't want to include these words in our list of post words. Finally, a call to `removeStopWords()` removes all stop words from the post, so that we have only the words that carry meaning for the job seeker (well, almost).




```r
require(XML)
```

```
## Loading required package: XML
```

```r
require(RCurl)
```

```
## Loading required package: RCurl
## Loading required package: bitops
```

```r
require(wordcloud)
```

```
## Loading required package: wordcloud
## Loading required package: RColorBrewer
```

```r
StopWords = readLines("http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop")

asWords = function(txt, stopWords = StopWords, stem = FALSE)
{
  words = unlist(strsplit(txt, '[[:space:]!.,;#:()/"]+'))
  words = words[words != ""]
  if(stem && require(Rlibstemmer))
     words = wordStem(words)
  i = tolower(words) %in% tolower(stopWords)
  words[!i]
}

removeStopWords = function(x, stopWords = StopWords) 
     {
         if(is.character(x))
             setdiff(x, stopWords)
         else if(is.list(x))
             lapply(x, removeStopWords, stopWords)
         else
             x
     }

cy.getFreeFormWords = function(doc, stopWords = StopWords)
     {
         nodes = getNodeSet(doc, "//div[@class='job-details']/
                                 div[@data-section]")
         if(length(nodes) == 0) 
             nodes = getNodeSet(doc, "//div[@class='job-details']//p")
         
         if(length(nodes) == 0) 
             warning("did not find any nodes for the free form text in ",
                     docName(doc))
         
         words = lapply(nodes,
                        function(x)
                            strsplit(xmlValue(x), 
                                     "[[:space:][:punct:]]+"))
         
         removeStopWords(words, stopWords)
     }
```

### Question 1: Implement the following functions. Use the code we explored to extract the date posted, skill sets and salary and location information from the parsed HTML document.


```r
cy.getSkillList = function(doc)
{
  lis = getNodeSet(doc, "//div[@class = 'skills-section']//
                         li[@class = 'skill-item']//
                         span[@class = 'skill-name']")

  sapply(lis, xmlValue)
}

cy.getDatePosted = function(doc)
  { xmlValue(getNodeSet(doc, 
                     "//div[@class = 'job-details']//
                        div[@class='posted']/
                        span/following-sibling::text()")[[1]],
    trim = TRUE) 
}

cy.getLocationSalary = function(doc)
{
  ans = xpathSApply(doc, "//div[@class = 'job-info-main'][1]/div", xmlValue)
  names(ans) = c("location", "salary")
  ans
}

# cy.getSkillList(cydoc)
# cy.getLocationSalary(cydoc)
```

The function `cy.ReadPost()` given below reads each job post. This function implements three other functions: `cy.getFreeFormWords()`, `cy.getSkillList()`, and `cy.getLocationSalary()`.


```r
cy.readPost = function(u, stopWords = StopWords, doc = htmlParse(u))
  {
    ans = list(words = cy.getFreeFormWords(doc, stopWords),
         datePosted = cy.getDatePosted(doc),
         skills = cy.getSkillList(doc))
    o = cy.getLocationSalary(doc)
    ans[names(o)] = o
    ans
}
# cyFuns = list(readPost = function(u, stopWords = StopWords, doc=htmlParse(u)))
```
### Reading posts programmatically
The function `cy.ReadPost()` allows us to read a single post from CyberCoders.com in a very general format. All we need is the URL for the post. Now, let's see about obtaining the URLs using a computer program.


```r
# Obtain URLs for job posts
txt = getForm("http://www.cybercoders.com/search/", searchterms = '"Data Scientist"',
              searchlocation = "",  newsearch = "true", sorttype = "")
# Parse the links
doc = htmlParse(txt, asText = TRUE)
links = getNodeSet(doc, "//div[@class = 'job-title']/a/@href")
# Save the links in the vector joblinks
joblinks <- getRelativeURL(as.character(links), "http://www.cybercoders.com/search/")
# Read the posts
posts <- lapply(joblinks,cy.readPost)

cy.getPostLinks = function(doc, baseURL = "http://www.cybercoders.com/search/") 
  {
    if(is.character(doc)) doc = htmlParse(doc)
    links = getNodeSet(doc, "//div[@class = 'job-title']/a/@href") 
    getRelativeURL(as.character(links), baseURL)
}

cy.readPagePosts = function(doc, links = cy.getPostLinks(doc, baseURL),
baseURL = "http://www.cybercoders.com/search/")
  {
    if(is.character(doc)) doc = htmlParse(doc)
    lapply(links, cy.readPost)
 }

## Testing the function with the parsed version of the first page of results in object doc
posts = cy.readPagePosts(doc)
sapply(posts,`[[`, "salary")
```

```
##           /data-scientist-job-214172           /data-scientist-job-200988 
##            "Full-time $100k - $160k"            "Full-time $100k - $160k" 
##           /data-scientist-job-184567           /data-scientist-job-230442 
##            "Full-time $130k - $160k"            "Full-time $140k - $160k" 
##           /data-scientist-job-234161           /data-scientist-job-223862 
## "Full-time Compensation Unspecified" "Full-time Compensation Unspecified" 
##           /data-scientist-job-230135           /data-scientist-job-231278 
## "Full-time Compensation Unspecified"             "Full-time $50k - $110k" 
##           /data-scientist-job-235549           /data-scientist-job-232102 
##            "Full-time $120k - $150k"            "Full-time $100k - $160k" 
##           /data-scientist-job-223543           /data-scientist-job-193909 
##            "Full-time $100k - $150k"            "Full-time $110k - $150k" 
##           /data-scientist-job-199692           /data-scientist-job-221652 
## "Full-time Compensation Unspecified"             "Full-time $75k - $150k" 
##           /data-scientist-job-224228           /data-scientist-job-227142 
##            "Full-time $100k - $150k"            "Full-time $100k - $130k" 
##           /data-scientist-job-227433           /data-scientist-job-227439 
##             "Full-time $75k - $100k"            "Full-time $120k - $140k" 
##           /data-scientist-job-227443           /data-scientist-job-233385 
##             "Full-time $90k - $110k"            "Full-time $110k - $150k"
```

```r
summary(sapply(posts, function(x) length(unlist(x$words))))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    66.0    99.0   126.5   143.4   168.8   326.0
```

**Question:** Test the `cy.getFreeFromWords()` function on several different posts.

The following code chunk pulls it all together. The function `cy.getNextPageLink()` retrieves each page from CyberCoders and calls the other functions to parse each post in order to obtain information such as salary, skills, and location.


```r
# Test of concept
# getNodeSet(doc, "//a[@rel='next']/@href")[[1]]
## A function to get all pages
cy.getNextPageLink = function(doc, baseURL = docName(doc))
{
  if(is.na(baseURL))
     baseURL = "http://www.cybercoders.com/"
  link = getNodeSet(doc, "//li[@class = 'lnk-next pager-item ']/a/@href")
  if(length(link) == 0)
    return(character())
    link2 <- gsub("./", "search/",link[[1]])
 getRelativeURL(link2, baseURL)
}

# Test the above function
tmp = cy.getNextPageLink(doc, "http://www.cybercoders.com")
```

Now we have all we need to retrieve all job posts on Cyber Coders for a given search query. The following function puts it all together into a function that we can call with a search string for a job of interest. The function submits the initial query and then reads the posts from each result page.

```r
cyberCoders =
function(query)
{
   txt = getForm("http://www.cybercoders.com/search/",
                  searchterms = query,  searchlocation = "",
                  newsearch = "true",  sorttype = "")
   doc = htmlParse(txt)

   posts = list()
   while(TRUE) {
       posts = c(posts, cy.readPagePosts(doc))
       nextPage = cy.getNextPageLink(doc)
       if(length(nextPage) == 0)
          break

       nextPage = getURLContent(nextPage)
       doc = htmlParse(nextPage, asText = TRUE)
   }
   invisible(posts)
}
```

The function cyberCoders is called below with the skill "Data Scientist". Then, we sort the skills and obtain all skills that are mentioned more than twice in the list.


```r
dataSciPosts = cyberCoders("Data Scientist")
tt = sort(table(unlist(lapply(dataSciPosts, `[[`, "skills"))),
           decreasing = TRUE)
tt[tt >= 2]
```

```
## 
##                              Machine Learning 
##                                            97 
##                                        Python 
##                                            88 
##                                   Data Mining 
##                                            72 
##                                             R 
##                                            69 
##                                        Hadoop 
##                                            56 
##                                           SQL 
##                                            38 
##                                          Java 
##                                            26 
##                                        Matlab 
##                                            22 
##                                           SAS 
##                                            19 
##                                      Big Data 
##                                            18 
##                                         Scala 
##                                            17 
##                                    Algorithms 
##                                            15 
##                                         Linux 
##                                            15 
##                                  Apache Spark 
##                                            14 
##                          Predictive Analytics 
##                                            10 
##                                         SPARK 
##                                             9 
##                                    Statistics 
##                                             9 
##                                           C++ 
##                                             8 
##                                 Data Modeling 
##                                             8 
##                                  Data Science 
##                                             8 
##                                Data Scientist 
##                                             8 
##                                     Mapreduce 
##                                             7 
##                                         NoSQL 
##                                             7 
##                          Statistical Analysis 
##                                             7 
##                          Statistical Modeling 
##                                             7 
##                                           AWS 
##                                             6 
##                            Big Data analytics 
##                                             6 
##                                 Data Analysis 
##                                             6 
##                   Natural Language Processing 
##                                             6 
##                           Predictive Modeling 
##                                             6 
##                            Data Visualization 
##                                             5 
##                                 Deep Learning 
##                                             5 
##                                         C/C++ 
##                                             4 
##                                          Hive 
##                                             4 
##                                       Tableau 
##                                             4 
##                         Algorithm Development 
##                                             3 
##                           Applied Mathematics 
##                                             3 
##                            Bayesian Inference 
##                                             3 
##                           Big Data Statistics 
##                                             3 
##                               Computer Vision 
##                                             3 
##                                 elasticsearch 
##                                             3 
##                                         Excel 
##                                             3 
##                                           Git 
##                                             3 
##                                          HDFS 
##                                             3 
##                   Machine Learning algorithms 
##                                             3 
##                            Mobile Advertising 
##                                             3 
##                                         MySQL 
##                                             3 
##                                           NLP 
##                                             3 
##                                Pandas & NumPy 
##                                             3 
##                                 R programming 
##                                             3 
##                                          SPSS 
##                                             3 
##                                         Agile 
##                                             2 
##                                            AI 
##                                             2 
##                            Applied statistics 
##                                             2 
##                                Bioinformatics 
##                                             2 
##                                 Clinical Data 
##                                             2 
##                 Complex Quantitative Analysis 
##                                             2 
##               Consumer Segmentation Reporting 
##                                             2 
##                            Context Extraction 
##                                             2 
##                                Data Analytics 
##                                             2 
##                             Data Manipulation 
##                                             2 
##                                     data sets 
##                                             2 
##                               data structures 
##                                             2 
##                              Data Warehousing 
##                                             2 
##                                           ETL 
##                                             2 
##                   Excel Pivot Tables & Macros 
##                                             2 
## Exp with Click stream data(raw web analytics) 
##                                             2 
##                           Experimental Design 
##                                             2 
##                           Geo-Spatial Analyst 
##                                             2 
##                Healthcare/biomedical industry 
##                                             2 
##                                        HiveQL 
##                                             2 
##                               Large Data Sets 
##                                             2 
##                                        Mahout 
##                                             2 
##                                    Map/Reduce 
##                                             2 
##                                       mongodb 
##                                             2 
##                                           OOP 
##                                             2 
##                                          Perl 
##                                             2 
##                                           PHP 
##                                             2 
##                                    PostgreSQL 
##                                             2 
##                           Predictive Analysis 
##                                             2 
##                         Predictive Analystics 
##                                             2 
##                    Product Analyst experience 
##                                             2 
##                                   Programming 
##                                             2 
##                                      Python/R 
##                                             2 
##                               Python/R/Matlab 
##                                             2 
##                                      R/Python 
##                                             2 
##                        Recommendation Engines 
##                                             2 
##                           Regression analysis 
##                                             2 
##                                      SAS/SPSS 
##                                             2 
##                                  scikit.learn 
##                                             2 
##                                     Scripting 
##                                             2 
##                  SQL and Relational Databases 
##                                             2 
##                                    SQL Server 
##                                             2 
##                            StartUp Experience 
##                                             2 
##                           Statistical Methods 
##                                             2 
##                        statistical techniques 
##                                             2 
##                    Strong Econometric mindset 
##                                             2 
##                Strong Math/Physics background 
##                                             2 
##                                          Unix 
##                                             2 
##                                    Unix/Linux 
##                                             2 
##                                           VBA 
##                                             2 
##                          Virtual Environments 
##                                             2
```

###Word Cloud for all skills.

The purpose to perform this task is to visualize every skills listed and see how we will plan to categorize similar skills into one group.

The max.words option sets maximum number of words to be plotted. In this case we are having maximum of 50 and least frequent skills dropped.


```r
#The option, max.words=50, sets maximum number of words to be plotted. In this case we are having maximum of 50 and least frequent skills dropped.
wordcloud(names(tt),tt,max.words = 50,colors=brewer.pal(8, "Dark2"))
```

```
## Warning in wordcloud(names(tt), tt, max.words = 50, colors =
## brewer.pal(8, : Machine Learning could not be fit on page. It will not be
## plotted.
```

![](CaseStudy2_Nov5_files/figure-html/unnamed-chunk-1-1.png) 

**Notice that we have a warning sign because the size of Machine Learning is too large to fit in one page**

###Word Cloud after Categorize

We now have the skill keywords that are mentioned more than twice in the list. We can visualize these skills using wordcloud. As we see it, we can discern similar skills are repeated several times such as Python, Python scientific stack, Python/Pandas, etc.

We will clean up the skills list using regular expressions. We also take into consideration the frequencies of skills menthoed because there are 5 or more skills that are mentioned more than 30 times.  

For the computer programming skills, we notice that several of them are stacked together. We decided only to extract the first skill along since each unique skill is on the first place on each stack. For instance, we extract R from R/Python/Matlab. We can see another stacked such as Python/R/Jave, etc. Therefore, we do not really have to worry about any left out. 

We now combine similar skills into several categories:

1) any statistical skills as "Statistics", 

2) any SQL programming skills as "SQL"

3) any Mathematical Skills as "Mathematics", this include Applied Mathematics

4) any skills related to Big Data as "Big Data"

5) any Bayesian inference skills as "Bayesian"

6) any Machine learning skills as "Machine Learning"

7) any Python programming skills as "Python", this include Python scientific stack and Python/Pandas as well.

8) any R programming skills as "R"

9) any Java, C/C++, Hadoop, Unix, Matlap, and SAS programming skills as "Java", "C/C++", "Hadoop", "Unix/Linux", "Matlap", and "SAS"

10) any Data Mining skills as "Data Mining"

11) skills that can develop and handle algorithm as "Algorithm"

12) any data analytic skills as "Data Analytics"

13) any predictable skills as "Predictive Analytics" 

14) any regression skills as "Regression Analysis", this include linear and logistic regression

15) we also include any skills related to data science and visualization as "Data Science" and "Data Visualization" because these two are very important in our Data Science Class!


```r
i = grepl("^Statist+|statist+",names(tt))
if(any(i)) names(tt)[i] = "Statistics"


j = grepl("+SQL+",names(tt))
if(any(j)) names(tt)[j] = "SQL"

k = grepl("+Math+", names(tt))
if(any(k)) names(tt)[k] = "Mathematics"

l = grepl("^Big Data+", names(tt))
if(any(l)) names(tt)[l] = "Big Data"

m = grepl("^Bayesian+", names(tt))
if(any(m)) names(tt)[m] = "Bayesian"

n= grepl("+Machine Learning+| +machine learning +", names(tt))
if(any(n)) names(tt)[n] = "ML"
#The size of "Machine Learning" is too large to put in wordcloud, so we change it as "ML"

o= grepl("^Python+", names(tt))
if(any(o)) names(tt)[o] = "Python"

p= grepl("^R+", names(tt))
if(any(p)) names(tt)[p] = "R"

q= grepl("^Java+", names(tt))
if(any(q)) names(tt)[q] = "Java"

r= grepl("^C +", names(tt))
if(any(r)) names(tt)[r] = "C/C++"

s= grepl("+Data Mining+", names(tt))
if(any(s)) names(tt)[s] = "Data Mining"

t= grepl("^Hadoop+", names(tt))
if(any(t)) names(tt)[t] = "Hadoop"

u= grepl("^Matlap+", names(tt))
if(any(u)) names(tt)[u] = "Matlap"

v= grepl("^SAS+", names(tt))
if(any(v)) names(tt)[v] = "SAS"

w= grepl("+Algorithm+", names(tt))
if(any(w)) names(tt)[w] = "Algorithm"

x= grepl("^Data Ana+", names(tt))
if(any(x)) names(tt)[x] = "Data Analytics"

y= grepl("Data Mining+", names(tt))
if(any(y)) names(tt)[y] = "Data Mining"

z= grepl("Predict+", names(tt))
if(any(z)) names(tt)[z] = "Predictive Analytics"

ii= grepl("+Regression+ | +regression+ | +Linear+", names(tt))
if(any(ii)) names(tt)[ii] = "Regression Analysis"

jj= grepl("^Unix+", names(tt))
if(any(jj)) names(tt)[jj] = "Unix/Linux"

kk= grepl("^Data Visu+", names(tt))
if(any(kk)) names(tt)[kk] = "Data Visualization"

ll= grepl("^Data Sci+", names(tt))
if(any(ll)) names(tt)[ll] = "Data Science"

tt <- tapply(tt,names(tt),sum)

length(tt)
```

```
## [1] 223
```

```r
head(tt)
```

```
##              .NET             Adobe             Agile Agricultural Data 
##                 1                 1                 2                 1 
##                AI              Ajax 
##                 2                 1
```

```r
tt[tt >= 2]
```

```
##                                         Agile 
##                                             2 
##                                            AI 
##                                             2 
##                                     Algorithm 
##                                            20 
##                                  Apache Spark 
##                                            14 
##                                           AWS 
##                                             6 
##                                      Bayesian 
##                                             5 
##                                      Big Data 
##                                            30 
##                                Bioinformatics 
##                                             2 
##                                         C/C++ 
##                                             5 
##                                           C++ 
##                                             8 
##                                 Clinical Data 
##                                             2 
##                 Complex Quantitative Analysis 
##                                             2 
##                               Computer Vision 
##                                             3 
##               Consumer Segmentation Reporting 
##                                             2 
##                            Context Extraction 
##                                             2 
##                                Data Analytics 
##                                            10 
##                             Data Manipulation 
##                                             2 
##                                   Data Mining 
##                                            74 
##                                 Data Modeling 
##                                             8 
##                                  Data Science 
##                                            16 
##                                     data sets 
##                                             2 
##                               data structures 
##                                             2 
##                            Data Visualization 
##                                             5 
##                              Data Warehousing 
##                                             2 
##                                 Deep Learning 
##                                             5 
##                                 elasticsearch 
##                                             3 
##                                           ETL 
##                                             2 
##                                         Excel 
##                                             3 
##                   Excel Pivot Tables & Macros 
##                                             2 
## Exp with Click stream data(raw web analytics) 
##                                             2 
##                           Experimental Design 
##                                             2 
##                           Geo-Spatial Analyst 
##                                             2 
##                                           Git 
##                                             3 
##                                        Hadoop 
##                                            57 
##                                          HDFS 
##                                             3 
##                Healthcare/biomedical industry 
##                                             2 
##                                          Hive 
##                                             4 
##                                        HiveQL 
##                                             2 
##                                          Java 
##                                            31 
##                               Large Data Sets 
##                                             2 
##                                         Linux 
##                                            15 
##                                        Mahout 
##                                             2 
##                                    Map/Reduce 
##                                             2 
##                                     Mapreduce 
##                                             7 
##                                   Mathematics 
##                                             6 
##                                        Matlab 
##                                            22 
##                                            ML 
##                                           106 
##                            Mobile Advertising 
##                                             3 
##                                       mongodb 
##                                             2 
##                   Natural Language Processing 
##                                             6 
##                                           NLP 
##                                             3 
##                                           OOP 
##                                             2 
##                                Pandas & NumPy 
##                                             3 
##                                          Perl 
##                                             2 
##                                           PHP 
##                                             2 
##                          Predictive Analytics 
##                                            21 
##                    Product Analyst experience 
##                                             2 
##                                   Programming 
##                                             2 
##                                        Python 
##                                            95 
##                                             R 
##                                            87 
##                                           SAS 
##                                            23 
##                                         Scala 
##                                            17 
##                                  scikit.learn 
##                                             2 
##                                     Scripting 
##                                             2 
##                                         SPARK 
##                                             9 
##                                          SPSS 
##                                             3 
##                                           SQL 
##                                            59 
##                            StartUp Experience 
##                                             2 
##                                    Statistics 
##                                            34 
##                    Strong Econometric mindset 
##                                             2 
##                                       Tableau 
##                                             4 
##                                    Unix/Linux 
##                                             5 
##                                           VBA 
##                                             2 
##                          Virtual Environments 
##                                             2
```

```r
wordcloud(names(tt),tt,min.freq = 2,random.order=T,
colors=brewer.pal(8, "Dark2"),rot.per=0.35)
```

![](CaseStudy2_Nov5_files/figure-html/unnamed-chunk-2-1.png) 

**Note: Machine Learning = ML**

After we categorize similar skills, this wordcloud looks more meaningful since we only see some kind of distinctive skills. In this wordcloud we are having skills presenting at least twice showing and least frequent skills dropped (obviously skills with frequency = 1). 


##Conclusion

We use R web scraping and text mining technique to explore data scientist jobs using cybercoders.com. We extract a set of skills that each job post expects and wants as well as information about salaries, location, and dateposted. Furthermore,  we discover that the function `cy.getNextPageLink()` retrieves each page from CyberCoders and calls the other functions to parse each post in order to obtain information such as salary, skills, and location.

We also write a code which puts it all together into one function that we can call with a search string for a job of interest. Then, we submits the initial query and then reads the posts from each result page. Finally, the function cyberCoders is called with the skill "Data Scientist". After that, we sort and obtain all skills that are mentioned more than twice in the list.

Next, we visualize these skills using wordcloud after we categorize similar skills into a specific group using R regular expression technique, so we easily find out popular skills that lots of employers want. 

Finally, we could produce wordcloud showing popular skills that most employers want. It can be an handy tool because it highlights the most commonly cited words in a text using a quick visualization.


##Appendix

In this case study 2, the original task was that we had to explore at least two job websites. We tried on Monster.com as well as Kaggle.com, but non of them really worked. Moreover, the R codes that the author provided from the textbook did not work well, so we learned that R code can be outdated/updated to something else very quickly. 

When working on cybercoder.com, we had a problem on moving to another page to extract all necessary information. Below is original function which cause a problem to move onto next pages.

cy.getNextPageLink =
function(doc, baseURL = docName(doc))
{
  if(is.na(baseURL))
     baseURL = "http://www.cybercoders.com/search/"
  link = getNodeSet(doc, "//a[@rel='next']/@href")
  if(length(link) == 0)
     return(character())

  getRelativeURL(link[[1]], baseURL)
}


tmp = cy.getNextPageLink(doc, "http://www.cybercoders.com/search/")

Below function is updated one, so it actually worked which searched necessary things from the next pages.

We use @class = 'lnk-next pager-item instead of @rel='next' on setting link because this is updated HTML command for moving next page in cybercoder.com.

cy.getNextPageLink = function(doc, baseURL = docName(doc))
{
  if(is.na(baseURL))
     baseURL = "http://www.cybercoders.com/"
  link = getNodeSet(doc, "//li[@class = 'lnk-next pager-item ']/a/@href")
  if(length(link) == 0)
    return(character())
    link2 <- gsub("./", "search/",link[[1]])
 getRelativeURL(link2, baseURL)
}

tmp = cy.getNextPageLink(doc, "http://www.cybercoders.com")

We also had some problem to plot wordcloud since too many skills presented at first, so it made us a hard time to perform quick visualization. Moreover, we mistakenly call table(names(tt)) function to have original count of each skill while categorizing similar skills into one. Since names function extracts unique skill name, so table function shows each skill counted as 1.

We add tapply(tt,names(tt),sum) instead of tt <- table(names(tt)) in order to solve the problem. This preserve the original counts as well as categorizing similar skills into groups.

Since wordcloud function with no additional options does not make the output pretty, we add colors option to distinguish each category and use rot.per option to decide the percentage of words to be rotated 90 degrees in wordcloud.

Lastly, the size of "Machine Learning" is too large to put in wordcloud, so we change it as "ML"

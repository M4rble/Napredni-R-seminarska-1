# ui

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(shinyWidgets)
shinyUI(fluidPage(#theme = shinytheme("flatly"),
  includeCSS("www/tema.css"),
                  
                  titlePanel("NV Investments"),
 
                  navbarPage( "NV Investments - people will eNVy you your investments",
                              
                             navbarMenu("Overview", icon = icon("eye"),
                                        tabPanel("How to use the app?", 
                                                 h2("How to use the app?"),
                                                 mainPanel(fluidRow(
                                                   column(12,
                                                          p("Aplikacija je namenjena tistim, ki jih zanima, kako bi lahko s pomočjo investicij v različna finančna sredstva ustvarili čim bolj optimalen portfelj."),
                                                          p("Aplikacija je sestavljena iz treh delov:"),
                                                          h3("1. Overview"),
                                                          p("Prvi zavihek, imenovan 'Overview', nam omogoča vpogled v navodila za uporabo aplikacije, opis finančnih sredstev, ki jih lahko uporabite v portfelju, in opis metode za izračun optimalnih deležev posameznih sredstev v portfelju."),
                                                          h3("2. Financial assets analysis"),
                                                          p("Drugi zavihek, imenovan 'Financial assets analysis', je razdeljen na štiri podrazdelke: Indices, Commodities, Bonds, Cryptocurrencies. S klikom na posamezni podrazdelek pridobimo izris gibanja vrednosti posameznih finančnih sredstev. Pri tem lahko spreminjamo posamezno sredstvo, vrsto cene (Opening, Closing, Highest, Lowest), frekvenco nabora podatkov (Daily, Weekly, Monthly, Quarterly, Yearly) in želeno obdobje opazovanja. V kolikor želimo dodatne informacije, ki nam izboljšajo razumevanje podatkov, lahko izberemo sledeče indikatorje: Moving Average, RSI ali MACD. Ob izbiri posameznega indikatorja se nam odprejo možnosti za izračun njegovih vrednosti. Drugi razdelek je torej namenjen statistični analizi finančnih sredstev, ki jih lahko vključimo v svoj portfelj."),
                                                          h3("3. Portfolio optimization"),
                                                          p("Tretji razdelek – 'Portfolio optimization' pa je glavni del aplikacije NV Investments, ki posamezniku omogoča izračun optimalnih uteži za dva različna portfelja, enega z najmanjšim tveganjem in drugega z največjim Sharpejevim razmerjem. Uporabnik najprej izbere število naključno generiranih portfeljev in vnese netvegano obrestno mero. Nato izbere poljubna finančna sredstva, ki jih želi imeti v svojem portfelju. Aplikacijo omogoča tudi izbiro vseh sredstev hkrati s pritiskom na gumb Select All. Po izbiri željenih finančnih sredstev mora uporabnik pritisniti 'Calculate optimal weights', počakati nekaj trenutkov in aplikacija mu izriše tri grafe. Prvi graf predstavlja optimalne deleže izbranih sredstev v porfelju z najmanjšo varianco (tveganjem), drugi graf pa z največjim Sharpejevim razmerjem. Zadnji graf uporaniku prikaže vrednosti tveganja, donosov in Sharpovega razmerja za vse zgenerirane porfelje. Rdeča toča predstavlja portfelj z najmanjšo varianco, oranžna točka pa portfelj z največjim Sharpejevim razmerjem."))
                                                 ))),
                                        tabPanel("Data description", h2("Data description"),
                                                 mainPanel(fluidRow(
                                                   column(12,
                                                          h3("Finančni indeksi"),
                                                          p(" 1.	S&P 500: S&P 500 je finančni indeks, ki sledi spremembam na ameriškem delniškem trgu in vključuje 500 največjih ameriških javnih podjetij glede na njihovo tržno kapitalizacijo. Indeks je široko spremljan kazalnik za ocenjevanje uspešnosti ameriškega trga in se uporablja kot referenca za številne naložbene strategije."),
            p("2.	Nasdaq: Nasdaq je finančni indeks, ki se osredotoča na tehnološka in rastna podjetja na ameriškem trgu. Indeks vključuje več kot 3000 podjetij, med njimi pa so številna znana tehnološka podjetja, kot so Apple, Microsoft, Amazon in Facebook. Nasdaq je znan po svoji visoki volatilnosti in se šteje za ključni indeks za ocenjevanje uspešnosti tehnološkega sektorja."),
            p("3.	Dow Jones: Dow Jones je najstarejši finančni indeks na svetu in se uporablja za sledenje uspešnosti 30 največjih ameriških podjetij. Indeks je znan po svojem visokem vplivu na globalne trge in vključuje podjetja iz različnih sektorjev, kot so finančne storitve, zdravstvo, energija in proizvodnja."),
            p("4.	DAX: DAX je glavni finančni indeks nemškega trga in sledi uspešnosti 30 največjih nemških podjetij glede na tržno kapitalizacijo. Indeks je znan po svoji vlogi kot ključnem kazalniku za ocenjevanje nemškega gospodarstva, saj vključuje podjetja iz različnih sektorjev, kot so avtomobilska industrija, kemična industrija, bančništvo in tehnologija."),
            p("5.	STOXX Europe 50: STOXX Europe 50 je finančni indeks, ki sledi uspešnosti 50 največjih podjetij v 18 evropskih državah. Indeks vključuje podjetja iz različnih sektorjev, kot so finance, farmacija, telekomunikacije, nafta in plin ter se uporablja kot ključni kazalnik za ocenjevanje uspešnosti evropskega trga."),
                                                          h3("Surovine"),
            p(" 1.Srebro: Srebro je plemenita kovina, ki se uporablja v različnih industrijah, vključno z elektroniko, fotografsko industrijo, medicino in nakitom. Srebro je tudi priljubljena surovina za trgovanje na finančnih trgih, saj se njegova cena običajno giblje v skladu z gospodarskimi in geopolitičnimi dejavniki ter povpraševanjem po industrijski uporabi."),
            p("2.	Zlato: Zlato je plemenita kovina, ki ima bogato zgodovino kot sredstvo za shranjevanje vrednosti. Zlato se uporablja tudi v nakitu, elektroniki in nekaterih industrijskih aplikacijah. Kot surovina za trgovanje je zlato priljubljena izbira med vlagatelji, saj se pogosto uporablja kot zaščita pred inflacijo, nestabilnostjo na finančnih trgih in valutnimi nihajnostmi."),
            p("3.	Pšenica: Pšenica je ena najpomembnejših žitnih pridelkov in se uporablja predvsem v prehrambni industriji za proizvodnjo kruha, testenin in drugih žitnih izdelkov. Cena pšenice na finančnih trgih je običajno odvisna od različnih dejavnikov, kot so ponudba in povpraševanje na svetovnem trgu, podnebni pogoji, politični dogodki in gibanja valut."),
            p("4.	Zemeljski plin: Zemeljski plin je fosilno gorivo, ki se uporablja predvsem za ogrevanje, proizvodnjo električne energije in kot surovina v kemični industriji. Cene zemeljskega plina na finančnih trgih so običajno odvisne od faktorjev, kot so ponudba in povpraševanje, podnebni pogoji, energetska politika ter geopolitične napetosti."),
            p("5.	Olje: Nafta je ena najpomembnejših surovin v svetovnem gospodarstvu, saj se uporablja za proizvodnjo energije, goriva za prevozna sredstva in različne kemične izdelke. Cene nafte na finančnih trgih so odvisne od številnih dejavnikov, kot so globalna ponudba in povpraševanje, geopolitične napetosti, politika proizvajalcev nafte ter gospodarske in okoljske razmere."),
                                                          h3("Obveznice"),
            p(" Ameriške zakladniške obveznice, pogosto imenovane tudi \"treasury bonds\" ali \"T-bonds\", so dolgoročne obveznice, ki jih izdaja ameriška vlada za financiranje svojega dolga. Obstajajo različni ročnosti T-obveznic, vključno s 5, 10 in 30 letnimi obveznicami."),
            p("•	5-letne T-obveznice: Gre za obveznice z ročnostjo 5 let, kar pomeni, da se zapadejo in izplačajo po 5 letih od datuma izdaje. Te obveznice običajno ponujajo nižji donos v primerjavi z daljšimi ročnostmi, vendar so manj občutljive na spremembe obrestnih mer."),
            p("•	10-letne T-obveznice: To so obveznice z ročnostjo 10 let, ki se zapadejo in izplačajo po 10 letih od datuma izdaje. 10-letne T-obveznice so priljubljene med vlagatelji, saj ponujajo zmerno donosnost in so običajno manj tvegane od krajših ročnosti."),
            p("•	30-letne T-obveznice: Gre za obveznice z najdaljšo ročnostjo med ameriškimi zakladniškimi obveznicami, saj se zapadejo in izplačajo po 30 letih od datuma izdaje. 30-letne T-obveznice običajno ponujajo najvišji donos, vendar so tudi bolj občutljive na spremembe obrestnih mer in tveganja inflacije."),
            p("Ameriške zakladniške obveznice so običajno veljajo za relativno varne naložbe, saj so podprte s kreditno boniteto ameriške vlade. Donosnost T-obveznic je odvisna od številnih dejavnikov, vključno z obrestnimi merami, inflacijo, gospodarskimi in geopolitičnimi razmerami ter povpraševanjem na trgu."),
                                                          h3("Kriptovalute"),
            p("Kriptovalute so digitalna sredstva, ki uporabljajo kriptografske tehnike za zagotavljanje varnosti, transparentnosti in decentralizacije. Kriptovalute med katerimi lahko uporabnik izbira so:"),
            p("•	Bitcoin: Je najstarejša in najbolj znana kriptovaluta, uvedena leta 2009. Temelji na tehnologiji blockchain in se uporablja kot digitalni denar za izvajanje transakcij in shranjevanje vrednosti. Bitcoin je znan po svoji omejeni ponudbi, saj je skupno število bitcoinov, ki jih lahko obstajajo, omejeno na 21 milijonov."),
            p("•	Ethereum: Je platforma za decentralizirane aplikacije (DApps) in kriptovaluta, ki je bila uvedena leta 2015. Ethereum omogoča izvajanje pametnih pogodb, ki so samodejno izvršljive kode, kar omogoča razvoj in izvajanje različnih blockchain aplikacij."),
            p("•	XRP: Je kriptovaluta, ki jo uporablja Ripple, podjetje za plačilne protokole. XRP se uporablja za olajšanje mednarodnih plačil in prenosov v realnem času. Med drugim je znan po svoji hitrosti in nizkih stroških transakcij."),
            p("•	Solana: Je odprtokodna blockchain platforma, ki omogoča hitre transakcije in visoko zmogljivost. Solana se uporablja za razvoj decentraliziranih aplikacij (DApps), NFT-jev (nepremičninskih žetonov) in drugih kripto projektov."),
            p("•	Dogecoin: Je kriptovaluta, ki je bila prvotno ustvarjena kot šala, vendar je pridobila veliko priljubljenost. Dogecoin temelji na odprtokodni tehnologiji Litecoin in je znan po svoji prepoznavni podobi Shibe Inu psa. Uporablja se za transakcije in shranjevanje vrednosti, vendar nima določenega maksimalnega števila kovancev, kar ga ločuje od drugih kriptovalut."),
            p("Kriptovalute so znane po svoji visoki volatilnosti, saj so cene na trgih kriptovalut znane po hitrih spremembah. Vlaganje v kriptovalute je visoko tvegano in zahteva previdnost ter temeljito raziskavo preden se odločite za vlaganje vanje."),
                                                          h3("Finančni indikatorji"),
                                                         p("Indikatorji so orodja, ki se uporabljajo v tehnični analizi finančnih trgov za analizo cenovnih gibanj in identifikacijo vzorcev ter trendov. Nekateri izmed najpogosteje uporabljenih indikatorjev vključujejo:"),
            p("1.	Povprečje premikov (Moving Average): Je enostaven indikator, ki prikazuje povprečje cenovnih vrednosti za določeno obdobje. Uporablja se za identifikacijo trendov in prepoznavanje presečišč med povprečjem in trenutno ceno kot morebitne signale za nakup ali prodajo."),
            p("2.	RSI (Relative Strength Index) je tehnični indikator, ki se uporablja za merjenje hitrosti in obsega sprememb cenovnih gibanj. RSI se giblje med 0 in 100 ter lahko služi kot orodje za identifikacijo prekomerne kupljenosti ali prodanosti vrednostnega papirja. Konvencionalno se šteje, da je RSI nad 70 kot prekomerno kupljen, medtem ko se RSI pod 30 šteje za prekomerno prodan."),
            p("Ko RSI preseže 70, se lahko šteje, da je vrednostni papir prekomerno kupljen, kar lahko nakazuje na možnost možnega trenda obrata navzdol. To bi lahko bil signal za morebitno prodajo, saj se pričakuje, da bo cena vrednostnega papirja padla."),
            p("Na drugi strani pa se RSI pod 30 šteje za prekomerno prodan, kar lahko nakazuje na možnost možnega trenda obrata navzgor. To bi lahko bil signal za morebitno nakupno priložnost, saj se pričakuje, da bo cena vrednostnega papirja zrasla."),
            p("3.	Konvergenca in divergenca povprečij (Moving Average Convergence Divergence - MACD): Je trendni indikator, ki uporablja razlike med dvema eksponentno ponderiranima povprečjema za identifikacijo spreminjajočega se trenda. MACD prikazuje presečišče med signali in histogramom, kar lahko služi kot signal za nakup ali prodajo."),
            p("Ti indikatorji so le nekaj primerov iz širokega nabora razpoložljivih tehničnih indikatorjev, ki se uporabljajo za analizo finančnih trgov. Pomembno je razumeti, da indikatorji niso zagotovilo za uspešno trgovanje, ampak so le orodja za pomoč pri sprejemanju odločitev na podlagi analize preteklih cenovnih gibanj. Prav tako je ključno upoštevati druge dejavnike, kot so temeljna analiza, upravljanje tveganj in lastniška strategija, pri sprejemanju investicijskih odločitev.")
                                                 )))),
                                        tabPanel("Optimisation model description",
                                                 h2("Optimisation model description"),
                                                 mainPanel(fluidRow(
                                                   column(12,
                                                          p("Optimizacija portfelja sledi naslednjim korakom:"),
                                                          p("1.	Na začetku funkcija določi seznam finančnih instrumentov, za katere želi optimizirati portfelj. Ta seznam je podan s pomočjo simbolov, ki predstavljajo posamezne instrumente na finančnih trgih (npr. \"^GSPC\" za S&P 500 indeks ali \"GC=F\" za zlato)."),
                                                        
                                                          p("2.	Nato funkcija pridobi zgodovinske podatke za te instrumente, kot so cene delnic, indeksov ali surovin, za določeno obdobje, ki je privzeto zadnjih 20 let. To stori s pomočjo funkcije tq_get(), ki uporablja knjižnico tidyquant."),
                                                        
                                                          p("3.	Naslednji korak je izračun dnevnih donosov za vsak instrument na podlagi pridobljenih zgodovinskih podatkov. Uporablja se funkcija tq_transmute(), ki uporablja knjižnico tidyquant, za izračun logaritemskih donosov (returnov) na dnevni ravni."),
                                                      
                                                          p("4.	Funkcija nato izračuna povprečne donose za vsak instrument ter kovariančno matriko donosov za vse instrumente. Povprečni donosi se izračunajo s funkcijo colMeans(), medtem ko se kovariančna matrika izračuna s funkcijo cov()."),
                                                          p("5.	Po izračunu povprečnih donosov in kovariančne matrike, funkcija začne optimizacijo portfelja z generiranjem num_port naključnih portfeljev. Število naključnih portfeljev je podano kot argument funkcije."),
                                                          p("6.	Za vsak naključno generiran portfelj, funkcija izračuna njegov letni donos, tveganje (standardni odklon) ter Sharpejevo razmerje. Donos, tveganje in Sharpejevo razmerje se izračunajo za vsak portfelj na podlagi naključno določenih uteži za vsak instrument v portfelju."),
                                                          p("7.	Funkcija nato shrani rezultate v tabelo, ki vsebuje povprečne donose, tveganja in Sharpejeva razmerja za vse naključno generirane portfelje."),
                                                          p("8.	Prav tako funkcija izriše grafe, ki prikazujejo uteži za portfelj z najmanjšo varianco in najvišjim Sharpejevim razmerjem. Grafi so narisani s pomočjo knjižnice ggplot2."),
                                                          p("9.	Na koncu funkcija vrne rezultate, kot so povprečni donosi, tveganja in Sharpejeva razmerja za vse naključno generirane portfelje, ter utež")
                                                 )))
            
                             )),
                             navbarMenu("Financial instruments analysis", icon=icon("chart-line"),
                                        
    ################################# Indices #############################################################                                    
                                        
                                        tabPanel("Indices", 
                                                 h2("Indices"), 
                                                 
                                                 sidebarPanel(class = "sidebar",
                                                   
                                                   selectInput("index", label = "Choose index:",
                                                               choices = c("S&P500",
                                                                           "NASDAQ",
                                                                           "DowJones Index",
                                                                           "STOXX Europe 50 Index",
                                                                           "DAX")),
                                                 hr(),
                                                   
                                                 radioButtons("price", label = "Choose what price to show:",
                                                              choices = c("Opening",
                                                                          "Closing",
                                                                          "Highest",
                                                                          "Lowest"),
                                                              inline = TRUE),
                                                 
                                                 radioButtons("frequency", label = "Choose frequency:",
                                                              choices = c("daily",
                                                                          "weekly",
                                                                          "monthly",
                                                                          "quarterly",
                                                                          "yearly"),
                                                              inline = TRUE),
                                                 
                                                 dateRangeInput("datum", label = "Choose date range:",
                                                                start = Sys.Date() - 365,
                                                                end = Sys.Date(),
                                                                format="dd-mm-yyyy",
                                                                max = Sys.Date()
                                                 ),
                                                 hr(),
                                                 
                                                 checkboxGroupInput("indikatorji", label = "Add indicator:",
                                                                    choices = c("Moving average",
                                                                               "RSI",
                                                                               "MACD"),
                                                                    inline = TRUE),
                                                 conditionalPanel(
                                                   condition = "input.indikatorji.indexOf('Moving average') > -1",
                                                   radioButtons("ma_period", label = "Choose moving average period:",
                                                                choices = c("10 units",
                                                                            "20 units",
                                                                            "50 units",
                                                                            "100 units",
                                                                            "200 units"),
                                                                selected = "10 units",
                                                                inline = TRUE)
                                                 ),
                                                 
                                                 
                                                 conditionalPanel(
                                                   condition = "input.indikatorji.indexOf('RSI') > -1",
                                                   sliderInput("rsi_period", label = "Choose RSI period:",
                                                                min = 3, max = 21,
                                                                value = 14)
                                                 ),
                                                 
                                                 conditionalPanel(
                                                   condition = "input.indikatorji.indexOf('MACD') > -1",
                                                   numericInput("nFast", label = "Input short-term MACD period:",
                                                                value = 12, min = 1, max = 18),
                                                   numericInput("nSlow", label = "Input long-term MACD period:",
                                                                value = 26, min = 19, max = 52),
                                                   numericInput("nSig", label = "Input period for moving average smoothing:",
                                                                value = 9, min = 1, max = 24)
                                                 )
                                                 ),
                                                 
                                                 mainPanel(
                                                   plotOutput("plotIndex"))),
                                                 #plotlyOutput("plotIndex1"),
                                                 #plotlyOutput("plotIndex2"),
                                                 #plotlyOutput("plotIndex3"))),
                                        
    #############################Commodities######################################################                                    
                                        
                                        tabPanel("Commodities", h2("Commodities"),
                                                 sidebarPanel(class = "sidebar",
                                                   
                                                   selectInput("commodities", label = "Choose commodity:",
                                                               choices = c("crude oil",
                                                                           "gold",
                                                                           "silver",
                                                                           "natural gas",
                                                                           "wheat")),
                                                   hr(),
                                                   
                                                   radioButtons("price_c", label = "Choose what price to show:",
                                                                choices = c("Opening",
                                                                            "Closing",
                                                                            "Highest",
                                                                            "Lowest"),
                                                                inline = TRUE),
                                                   
                                                   radioButtons("frequency_c", label = "Choose frequency:",
                                                                choices = c("daily",
                                                                            "weekly",
                                                                            "monthly",
                                                                            "quarterly",
                                                                            "yearly"),
                                                                inline = TRUE),
                                                   
                                                   dateRangeInput("datum_c", label = "Choose date range:",
                                                                  start = Sys.Date() - 365,
                                                                  end = Sys.Date(),
                                                                  format="dd-mm-yyyy",
                                                                  max = Sys.Date()
                                                   ),
                                                   hr(),
                                                   
                                                   checkboxGroupInput("indikatorji_c", label = "Add indicator:",
                                                                      choices = c("Moving average",
                                                                                  "RSI",
                                                                                  "MACD"),
                                                                      inline = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_c.indexOf('Moving average') > -1",
                                                     radioButtons("ma_period_c", label = "Choose moving average period:",
                                                                  choices = c("10 units",
                                                                              "20 units",
                                                                              "50 units",
                                                                              "100 units",
                                                                              "200 units"),
                                                                  selected = "10 units",
                                                                  inline = TRUE)
                                                   ),
                                                   
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_c.indexOf('RSI') > -1",
                                                     sliderInput("rsi_period_c", label = "Choose RSI period:",
                                                                 min = 3, max = 21,
                                                                 value = 14)
                                                   ),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_c.indexOf('MACD') > -1",
                                                     numericInput("nFast_c", label = "Input short-term MACD period:",
                                                                  value = 12, min = 1, max = 18),
                                                     numericInput("nSlow_c", label = "Input long-term MACD period:",
                                                                  value = 26, min = 19, max = 52),
                                                     numericInput("nSig_c", label = "Input period for moving average smoothing:",
                                                                  value = 9, min = 1, max = 24)
                                                   )
                                                 ),
                                                 
                                                 
                                                 plotOutput("plotCommodities")),
    
    ############################## Bonds ################################################################
    
                                        tabPanel("Bonds", h2("Bonds"),
                                                 sidebarPanel(class = "sidebar",
                                                   
                                                   selectInput("bonds", label = "Choose bond:",
                                                               choices = c("US 5-Year Treasury Bond Yield",
                                                                           "US 10-Year Treasury Bond Yield",
                                                                           "US 30-Year Treasury Bond Yield")),
                                                   hr(),
                                                   
                                                   radioButtons("price_b", label = "Choose what price to show:",
                                                                choices = c("Opening",
                                                                            "Closing",
                                                                            "Highest",
                                                                            "Lowest"),
                                                                inline = TRUE),
                                                   
                                                   radioButtons("frequency_b", label = "Choose frequency:",
                                                                choices = c("daily",
                                                                            "weekly",
                                                                            "monthly",
                                                                            "quarterly",
                                                                            "yearly"),
                                                                inline = TRUE),
                                                   
                                                   dateRangeInput("datum_b", label = "Choose date range:",
                                                                  start = Sys.Date() - 365,
                                                                  end = Sys.Date(),
                                                                  format="dd-mm-yyyy",
                                                                  max = Sys.Date()
                                                   ),
                                                   hr(),
                                                   
                                                   checkboxGroupInput("indikatorji_b", label = "Add indicator:",
                                                                      choices = c("Moving average",
                                                                                  "RSI",
                                                                                  "MACD"),
                                                                      inline = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_b.indexOf('Moving average') > -1",
                                                     radioButtons("ma_period_b", label = "Choose moving average period:",
                                                                  choices = c("10 units",
                                                                              "20 units",
                                                                              "50 units",
                                                                              "100 units",
                                                                              "200 units"),
                                                                  selected = "10 units",
                                                                  inline = TRUE)
                                                   ),
                                                   
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_b.indexOf('RSI') > -1",
                                                     sliderInput("rsi_period_b", label = "Choose RSI period:",
                                                                 min = 3, max = 21,
                                                                 value = 14)
                                                   ),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_b.indexOf('MACD') > -1",
                                                     numericInput("nFast_b", label = "Input short-term MACD period:",
                                                                  value = 12, min = 1, max = 18),
                                                     numericInput("nSlow_b", label = "Input long-term MACD period:",
                                                                  value = 26, min = 19, max = 52),
                                                     numericInput("nSig_b", label = "Input period for moving average smoothing:",
                                                                  value = 9, min = 1, max = 24)
                                                   )
                                                 ),
                                                 
                                                 
                                                 plotOutput("plotBonds")),
    
    ############################## Cryptocurrencies ################################################################
                                          tabPanel("Cryptocurrencies", h2("Cryptocurrencies"),
                                                 sidebarPanel(
                                                   class = "sidebar",
                                                   selectInput("cryptocurrencies", label = "Choose cryptocurrency:",
                                                               choices = c("Bitcoin",
                                                                           "Ethereum",
                                                                           "XRP",
                                                                           "Solana",
                                                                           "Dogecoin")),
                                                   hr(),
                                                   
                                                   radioButtons("price_crypto", label = "Choose what price to show:",
                                                                choices = c("Opening",
                                                                            "Closing",
                                                                            "Highest",
                                                                            "Lowest"),
                                                                inline = TRUE),
                                                   
                                                   radioButtons("frequency_crypto", label = "Choose frequency:",
                                                                choices = c("daily",
                                                                            "weekly",
                                                                            "monthly",
                                                                            "quarterly",
                                                                            "yearly"),
                                                                inline = TRUE),
                                                   
                                                   dateRangeInput("datum_crypto", label = "Choose date range:",
                                                                  start = Sys.Date() - 365,
                                                                  end = Sys.Date(),
                                                                  format="dd-mm-yyyy",
                                                                  max = Sys.Date()
                                                   ),
                                                   hr(),
                                                   
                                                   checkboxGroupInput("indikatorji_crypto", label = "Add indicator:",
                                                                      choices = c("Moving average",
                                                                                  "RSI",
                                                                                  "MACD"),
                                                                      inline = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_crypto.indexOf('Moving average') > -1",
                                                     radioButtons("ma_period_crypto", label = "Choose moving average period:",
                                                                  choices = c("10 units",
                                                                              "20 units",
                                                                              "50 units",
                                                                              "100 units",
                                                                              "200 units"),
                                                                  selected = "10 units",
                                                                  inline = TRUE)
                                                   ),
                                                   
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_crypto.indexOf('RSI') > -1",
                                                     sliderInput("rsi_period_crypto", label = "Choose RSI period:",
                                                                 min = 3, max = 21,
                                                                 value = 14)
                                                   ),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.indikatorji_crypto.indexOf('MACD') > -1",
                                                     numericInput("nFast_crypto", label = "Input short-term MACD period:",
                                                                  value = 12, min = 1, max = 18),
                                                     numericInput("nSlow_crypto", label = "Input long-term MACD period:",
                                                                  value = 26, min = 19, max = 52),
                                                     numericInput("nSig_crypto", label = "Input period for moving average smoothing:",
                                                                  value = 9, min = 1, max = 24)
                                                   )
                                                 ),
                                                 
                                                 
                                                 plotOutput("plotCryptocurrencies"))),
                              navbarMenu("Portfolio optimisation", icon = icon ("money-bill-trend-up"),
                                        tabPanel("Optimal portfolio", h2("Optimal portfolio"),
                                                 sidebarPanel(class = "sidebar",
                                                   numericInput("num_port", 
                                                                label = "Input the number of portfolios to be randomly generated:",
                                                                value = 5000, min = 1000, max = 1000000, step = 1000),
                                                   numericInput("rfr", label = "Input risk-free-rate (in %)",
                                                                value = 0, min = 0, max = 10, step=0.01),
                                                   hr(),
                                                   checkboxGroupInput("assets", "Select Assets:", choices = c("S&P500",
                                                                                                       "NASDAQ",
                                                                                                       "DowJones Index",
                                                                                                       "STOXX Europe 50 Index",
                                                                                                       "DAX",
                                                                                                       "crude oil",
                                                                                                       "gold",
                                                                                                       "silver",
                                                                                                       "natural gas",
                                                                                                       "wheat",
                                                                                                       "US 5-Year Treasury Bond Yield",
                                                                                                       "US 10-Year Treasury Bond Yield",
                                                                                                       "US 30-Year Treasury Bond Yield",
                                                                                                       "Bitcoin",
                                                                                                       "Ethereum",
                                                                                                       "XRP",
                                                                                                       "Solana",
                                                                                                       "Dogecoin")),
                                                   actionButton("selectall", "Select All"),
                                                   hr(),
                                                   actionButton("calculate", "Calculate optimal weights", icon("refresh"))),
                                                  mainPanel(withSpinner(
                                                   plotlyOutput("optimal_weights1"), type = 4, size=0.7),
                                                   plotlyOutput("optimal_weights2"),
                                                   plotlyOutput("optimal_weights3"))
                                                 )
                                    
                             )
      


)
)
)



#sidebarLayout(
#  sidebarPanel(
#    numericInput("mean", "Mean", min = -2, max = 2, value = 0, step = 0.1),
#    sliderInput("sd", "Standard deviation", min = 0, max = 2, value = 1, step = 0.1,
#                animate=TRUE),
#    checkboxInput("plotMean", "Plot vertical line at mean?", value = FALSE),
#    hr(),
#    selectInput("curveColor", "Choose color of curve:",
#                choices = c("black", "red", "blue"), selected = "black"),
#    hr(),
#    textInput("plotTitle", "Specify title plot", value="A plot"),
#    textInput("verticalLabel", "Specify vertical label", value="probability density"),
#    hr(),
#    submitButton("Update View", icon("refresh"))
#  ),
# ui

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(shinyWidgets)
shinyUI(fluidPage(#theme = shinytheme("flatly"),
  includeCSS("www/tema.css"),
                  
                  titlePanel("NV Investments"),
 
                  navbarPage( "others will eNVy you your investments",
                              
                             navbarMenu("Overview", icon = icon("eye"),
                                        tabPanel("How to use the app?", 
                                                 h2("How to use the app?"),
                                                 mainPanel(fluidRow(
                                                   column(12,
                                                          p("Aplikacija je namenjena vsem, ki jih zanima, kako porazdeliti svoja
                                                            denarna sredstva, da bi z investicijami v različna finančna sredstva ustvarili
                                                            čim bolj optimalen portfelj."),
                                                          p("Aplikacija je sestavljena iz treh delov:"),
                                                          h3("1. Overview"),
                                                          p("Prvi zavihek, imenovan 'Overview', nam omogoča vpogled v navodila za uporabo aplikacije,
                                                            opis finančnih sredstev, ki jih lahko uporabite v portfelju in opis metode za izračun
                                                            optimalnih deležev posameznih sredstev v portfelju."),
                                                          h3("2. Financial assets analysis"),
                                                          p("Drugi zavihek, imenovan 'Financial assets analysis', je razdeljen na štiri podrazdelke:
                                                            Indices, Commodities, Bonds in Cryptocurrencies. S klikom na posamezni podrazdelek pridobimo
                                                            izris gibanja vrednosti posameznih finančnih sredstev. Pri tem lahko spreminjamo posamezno
                                                            sredstvo, vrsto cene (Opening, Closing, Highest, Lowest), frekvenco nabora podatkov
                                                            (Daily, Weekly, Monthly, Quarterly, Yearly) in želeno obdobje opazovanja. Če želimo prikazati dodatne
                                                            informacije, ki nam izboljšajo razumevanje podatkov, lahko izberemo sledeče indikatorje: 
                                                            Moving Average, RSI ali MACD. Ob izbiri posameznega indikatorja se nam odprejo možnosti za
                                                            izračun njegovih vrednosti. Drugi razdelek je torej namenjen statistični analizi finančnih
                                                            sredstev, ki jih lahko vključimo v svoj portfelj."),
                                                          h3("3. Portfolio optimization"),
                                                          p("Tretji razdelek – 'Portfolio optimization' je glavni del aplikacije NV Investments, 
                                                            ki posamezniku omogoča izračun optimalnih uteži za dva različna portfelja, enega z 
                                                            najmanjšim tveganjem in drugega z največjim Sharpejevim razmerjem. Uporabnik najprej izbere 
                                                            število naključno generiranih portfeljev in vnese netvegano obrestno mero. Nato izbere 
                                                            poljubna finančna sredstva, v katera želi investirati. Aplikacija omogoča tudi 
                                                            izbiro vseh sredstev hkrati s pritiskom na gumb Select All. Po izbiri željenih finančnih 
                                                            sredstev mora uporabnik pritisniti 'Calculate optimal weights', počakati nekaj trenutkov, da mu
                                                            algoritem preračuna optimalne uteži in 
                                                            aplikacija mu izriše tri grafe. Prvi graf predstavlja optimalne deleže izbranih sredstev v 
                                                            porfelju z najmanjšo varianco (tveganjem), drugi graf pa z največjim Sharpejevim razmerjem. 
                                                            Zadnji graf uporaniku prikaže vrednosti tveganja, donosov in Sharpovega razmerja za vse
                                                            naključno generirane porfelje. Rdeča toča predstavlja portfelj z najmanjšo varianco,
                                                            oranžna točka pa portfelj z največjim Sharpejevim razmerjem."))
                                                 ))),
                                        tabPanel("Data description", h2("Data description"),
                                                 mainPanel(fluidRow(
                                                   column(12,
                                                          h3("Finančni indeksi"),
            p("Finančni indeksi predstavljajo agregatne vrednosti skupine delnic, obveznic ali drugih finančnih instrumentov, ki služijo
              kot referenčna točka za merjenje uspeha naložb in učinkovitosti trgov."),
            strong("S&P 500"),
            p("S&P 500 je finančni indeks, ki sledi spremembam na ameriškem delniškem trgu in vključuje 500 največjih ameriških javnih
              podjetij glede na njihovo tržno kapitalizacijo. Indeks je eden najpomembnejših kazalnikov za ocenjevanje uspešnosti ameriškega
              trga in se uporablja kot referenca za številne naložbene strategije."),
            strong("NASDAQ"),
            p("NASDAQ je finančni indeks, ki se osredotoča na tehnološka in informacijska podjetja na ameriškem trgu. Indeks vključuje več kot
              3000 podjetij, med njimi pa so številna znana tehnološka podjetja, kot so Apple, Microsoft, Amazon in Facebook. NASDAQ je
              znan po visoki volatilnosti in se uporablja kot ključni indeks za ocenjevanje uspešnosti tehnološkega sektorja."),
            strong("Dow Jones"),
            p("Dow Jones je najstarejši finančni indeks na svetu in se uporablja za sledenje uspešnosti 30 največjih ameriških podjetij.
              Indeks je znan po svojem visokem vplivu na globalne trge in vključuje podjetja iz različnih sektorjev, kot so finančne
              storitve, zdravstvo, energija in proizvodnja."),
            strong("DAX"),
            p("DAX je glavni finančni indeks nemškega trga in sledi uspešnosti 30 največjih nemških podjetij glede na tržno
              kapitalizacijo. Indeks se najpogosteje uporablja za ocenjevanje stanja nemškega gospodarstva, saj vključuje
              podjetja iz različnih sektorjev, kot so avtomobilska industrija, kemična industrija, bančništvo in tehnologija."),
            strong("STOXX Europe 50"),
            p("STOXX Europe 50 je finančni indeks, ki sledi uspešnosti 50 največjih podjetij v 18 evropskih državah. Indeks vključuje
              podjetja iz različnih sektorjev, kot so finance, farmacija, telekomunikacije, nafta in plin ter se uporablja kot ključni
              kazalnik za ocenjevanje uspešnosti evropskega trga."),
                                                          h3("Surovine"),
            p("Surovine predstavljajo ključen del vlagateljskega portfelja, saj omogočajo razpršitev tveganja in zagotavljajo zaščito pred inflacijo."),
            strong("Nafta"),
            p("Nafta je ena najpomembnejših surovin v svetovnem gospodarstvu, saj se uporablja za proizvodnjo energije, goriva za
              prevozna sredstva in različne kemične izdelke. Cene nafte na finančnih trgih so odvisne od številnih dejavnikov, kot so 
              globalna ponudba in povpraševanje, geopolitične napetosti, politika proizvajalcev nafte ter gospodarske in okoljske razmere."),
            strong("Zlato"),
            p("Zlato je plemenita kovina, ki ima bogato zgodovino kot sredstvo za shranjevanje vrednosti. Zlato se uporablja tudi v nakitu, 
              elektroniki in nekaterih industrijskih aplikacijah. Kot surovina za trgovanje je zlato priljubljena izbira med vlagatelji, saj se 
              pogosto uporablja kot zaščita pred inflacijo, nestabilnostjo na finančnih trgih in nihanji vrednosti valut."),
            strong("Srebro"),
            p("Srebro je plemenita kovina, ki se uporablja v različnih industrijah, vključno z elektroniko, fotografsko industrijo, medicino in
              nakitom. Srebro je tudi priljubljena surovina za trgovanje na finančnih trgih, saj se njegova cena običajno giblje v skladu z
              gospodarskimi in geopolitičnimi dejavniki ter povpraševanjem po industrijski uporabi."),
            strong("Zemeljski plin"),
            p("Zemeljski plin je fosilno gorivo, ki se uporablja predvsem za ogrevanje, proizvodnjo električne energije in kot surovina v kemični
              industriji. Cene zemeljskega plina na finančnih trgih so običajno odvisne od faktorjev, kot so ponudba in povpraševanje, podnebni
              pogoji, energetska politika ter geopolitične napetosti."),
            strong("Pšenica"),
            p("Pšenica je ena najpomembnejših žitnih pridelkov in se uporablja predvsem v prehrambni industriji za proizvodnjo kruha, testenin in
              drugih žitnih izdelkov. Cena pšenice na finančnih trgih je običajno odvisna od različnih dejavnikov, kot so ponudba in povpraševanje
              na svetovnem trgu, podnebni pogoji, politični dogodki in gibanje vrednosti valut."),
           
                                                          h3("Obveznice"),
            p(" Ameriške zakladniške obveznice, pogosto imenovane tudi \"treasury bonds\" ali \"T-bonds\", so dolgoročne obveznice, ki jih
              izdaja ameriška vlada za financiranje svojega dolga. Obstajajo različni ročnosti T-obveznic, vključno s 5, 10 in 30 letnimi obveznicami."),
            strong("• 5-letne T-obveznice"),
            p("Gre za obveznice z ročnostjo 5 let, kar pomeni, da zapadejo in se izplačajo po 5 letih od datuma izdaje. 
              Te obveznice običajno ponujajo nižji donos v primerjavi z daljšimi ročnostmi, vendar so manj občutljive na spremembe obrestnih mer."),
            strong("•	10-letne T-obveznice"),
            p("To so obveznice z ročnostjo 10 let, ki zapadejo in se izplačajo po 10 letih od datuma izdaje. 10-letne T-obveznice so priljubljene 
              med vlagatelji, saj ponujajo zmerno donosnost in so običajno manj tvegane od krajših ročnosti."),
            strong("•	30-letne T-obveznice"),
            p("Gre za obveznice z najdaljšo ročnostjo med ameriškimi zakladniškimi obveznicami, saj zapadejo in se izplačajo po 30 letih
              od datuma izdaje. 30-letne T-obveznice običajno ponujajo najvišji donos, vendar so tudi bolj občutljive na spremembe
              obrestnih mer in tveganja inflacije."),
            p("Ameriške zakladniške obveznice običajno veljajo za relativno varne naložbe, saj so podprte s kreditno boniteto ameriške
              vlade. Donosnost T-obveznic je odvisna od številnih dejavnikov, vključno z obrestnimi merami, inflacijo, 
              gospodarskimi in geopolitičnimi razmerami ter povpraševanjem na trgu."),
                                                          h3("Kriptovalute"),
            p("Kriptovalute so digitalna sredstva, ki uporabljajo kriptografske tehnike za zagotavljanje varnosti, transparentnosti in
              decentralizacije."),
            strong("Bitcoin"),
            p("Bitcoin je najstarejša in najbolj znana kriptovaluta, uvedena leta 2009. Temelji na tehnologiji blockchain in se 
              uporablja kot digitalni denar za izvajanje transakcij in shranjevanje vrednosti. Bitcoin je znan po svoji omejeni ponudbi,
              saj je skupno število bitcoinov, ki lahko obstajajo, omejeno na 21 milijonov."),
            strong("Ethereum"),
            p("Ethereum je platforma za decentralizirane aplikacije (DApps) in kriptovaluta, ki je bila uvedena leta 2015. Ethereum 
              omogoča izvajanje pametnih pogodb, torej izvajanje avtomatiziranih transakcij brez posrednikov, kar omogoča razvoj
              in izvajanje različnih blockchain aplikacij."),
            strong("XRP"),
            p("XRP je kriptovaluta, ki jo uporablja Ripple, podjetje za plačilne protokole. XRP se uporablja za olajšanje mednarodnih 
              plačil in prenosov v realnem času. Med drugim je znan po svoji hitrosti in nizkih stroških transakcij."),
            strong("Solana"),
            p("Solana je odprtokodna blockchain platforma, ki omogoča hitre transakcije in visoko zmogljivost. Solana se uporablja za
              razvoj decentraliziranih aplikacij (DApps), NFT-jev (nepremičninskih žetonov) in drugih kripto projektov."),
            strong("Dogecoin"),
            p("Dogecoin je kriptovaluta, ki je bila prvotno ustvarjena kot šala, vendar je pridobila veliko priljubljenost. Dogecoin 
              temelji na odprtokodni tehnologiji Litecoin in je znan po svoji prepoznavni podobi Shibe Inu psa. 
              Uporablja se za transakcije in shranjevanje vrednosti, vendar nima določenega maksimalnega števila kovancev, 
              kar ga ločuje od drugih kriptovalut."),
            p("Kriptovalute so znane po svoji visoki volatilnosti, cene na trgih kriptovalut so znane po hitrih spremembah.
              Vlaganje v kriptovalute je visoko tvegano in zahteva previdnost ter temeljito raziskavo pred odločitvijo za vlaganje."),
                                                          h3("Finančni indikatorji"),
            p("Indikatorji so orodja, ki se uporabljajo v tehnični analizi finančnih trgov za analizo cenovnih gibanj in 
              identifikacijo vzorcev ter trendov. Uporabnik lahko izbira med naslednjimi indikatorji:"),
            strong("Moving average"),
            p("Drseče povprečje je tehnični indikator, ki se uporablja za analizo časovnih vrst podatkov, zlasti cenovnih podatkov.
            Gre za enostavno matematično orodje, ki izračuna povprečje cen v določenem časovnem obdobju, s čimer zgladi cene in
            omogoča boljše razumevanje trendov in vzorcev na trgu. Drseča povprečja se pogosto uporabljajo v finančni analizi in 
            trgovanju ter so lahko različnih vrst, kot so preprosto drseče povprečje (SMA), eksponentno drseče povprečje (EMA) ali
            tehtano drseče povprečje (WMA). V naši aplikaciji je na voljo le SMA."),
            p("Drseča povprečja lahko uporabimo za identifikacijo trendov, kjer naraščajoče drseče povprečje nakazuje naraščajoč trend,
              medtem ko padajoče drseče povprečje nakazuje padajoč trend. Prav tako se uporabljajo za prepoznavanje podpore in upora,
              saj se tržne cene pogosto gibljejo okoli drsečih povprečij."),
            strong("RSI"),
            p("RSI je tehnični indikator, ki se uporablja za oceno zagona cen ter za prepoznavanje prekupljenih in preprodanih sredstev
            na trgu. RSI je oscilator, ki meri hitrost in spremembe cenovnih gibanj, z vrednostmi, ki se gibljejo med 0 in 100. Izračuna 
            se na podlagi povprečnega dobička in povprečne izgube v določenem časovnem obdobju (običajno 14 dni). Običajno se 
            uporabljajo pragovi 30 in 70 za identifikacijo preprodanih in prekupljenih sredstev. Ko RSI pade pod 30 pomeni, da je trg
            preprodan, kar lahko nakazuje na potencialni obrat navzgor. Nasprotno pa, ko RSI preseže 70 pomeni, da je trg prenasičen, kar
            lahko nakazuje na potencialni obrat navzdol."),
            p("Poleg prekupljenih in preprodanih signalov lahko RSI uporabimo tudi za iskanje divergenc med cenovnimi gibanji in
              indikatorjem, kar lahko nakazuje na potencialne preobrate v trendu. RSI se pogosto uporablja v kombinaciji z drugimi
              tehničnimi indikatorji za boljše razumevanje tržnih razmer in izboljšanje natančnosti signalov."),
            strong("MACD"),
            p("Konvergenca in divergenca povprečij (Moving Average Convergence Divergence) je priljubljen tehnični indikator, ki ga
            uporabljajo trgovci in vlagatelji za prepoznavanje trendov in potencialnih preobratov na finančnih trgih. Deluje z 
            analizo razlike med dvema drsečima povprečjema cen - običajno 12-dnevnim eksponentnim drsečim povprečjem (EMA) in 
            26-dnevnim EMA. MACD se izračuna tako, da se od 12-dnevnega EMA odšteje 26-dnevni EMA. Rezultat predstavlja MACD linijo, 
            ki se uporablja za prepoznavanje sprememb  v trendu in zagonu. Poleg MACD linije se pogosto izračuna tudi 9-dnevni EMA MACD
            linije, ki se imenuje signalna linija. Signalna linija se uporablja za generiranje prodajnih in nakupnih signalov."),
            p("Trgovci in vlagatelji uporabljajo MACD za prepoznavanje križanj med MACD in signalno linijo. Kadar MACD linija prečka 
            signalno linijo navzgor, se to šteje za nakupni signal, ki kaže na povečanje zagona in možen začetek ali nadaljevanj
            naraščajočega trenda. Nasprotno pa velja, kadar MACD linija prečka signalno linijo navzdol, kar se šteje za prodajni signal,
            ki nakazuje na oslabitev zagona in možen začetek ali nadaljevanje padajočega trenda."),
            p("Navedenih je le nekaj indikatorjev iz širokega nabora razpoložljivih tehničnih indikatorjev, ki se uporabljajo za 
              analizo finančnih trgov. Pomembno je razumeti, da indikatorji niso zagotovilo za uspešno trgovanje, ampak so 
              le orodja za pomoč pri sprejemanju odločitev na podlagi analize preteklih cenovnih gibanj. Prav tako je pri sprejemanju
              investicijskih odločitev ključno upoštevati druge dejavnike, kot so temeljna analiza, upravljanje tveganj 
              in lastniška strategija.")
                                                 )))),
                                        tabPanel("Optimization model description",
                                                 h2("Optimization model description"),
                                                 mainPanel(fluidRow(
                                                   column(12,
            p("Optimizacija portfelja sledi naslednjim korakom:"),
            p("1.	Najprej izberemo seznam finančnih sredstev, za katere želimo optimizirati portfelj. Vnesemo tudi število naključnih
              portfeljev, ki naj jih algoritem zgenerira in vrednost netvegane obrestne mere."),
            p("2.	Algoritem nato iz interneta pridobi zgodovinske dnevne cene za vsa izbrana sredstva (indeksi, surovine, obveznice in kriptovalute)
               za zadnjih 20 let. Če sredstvo obstaja manj kot 20 let (recimo kriptovalute), potem pridobi vse zgodovinske cene od 
              takrat ko je nastalo."),
            p("3.	Sledi izračun dnevnih donosov za vsako sredstvo na podlagi pridobljenih zgodovinskih podatkov. Izračunajo se zvezni, logaritemski
              donosi na dnevni ravni."),
            p("4.	Model nato za vsako sredstvo izračuna povprečne donose ter kovariančno matriko donosov."),
            p("5.	V tem koraku se začne optimizacija portfelja. Algoritem generira vneseno število naključnih portfeljev. To pomeni, da za
              vsak portfelj naključno generira vrednosti uteži (med 0 in 1, seštejejo se v 1). Za vsak naključno generiran portfelj, algoritem
              izračuna njegov letni donos, tveganje (standardni odklon) ter Sharpejevo razmerje na podlagi naključno določenih uteži."),
            p("6.	Na koncu algoritem poišče portfelja z najmanjšo varianco in najvišjim Sharpejevim razmerjem, ter izriše grafa uteži. Posebej 
              izriše tudi graf rezultatov s povprečni donosi, tveganjem in Sharpejevim razmerjem za vse naključno generirane portfelje."),
                                                 )))
            
                             )),
                             navbarMenu("Financial assets analysis", icon=icon("chart-line"),
                                        
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
                                                   checkboxGroupInput("ma_period", label = "Choose moving average periods:",
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
                                                 plotlyOutput("plotIndex1"),
                                                 plotlyOutput("plotIndex2"),
                                                 plotlyOutput("plotIndex3"))),
                                        
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
                                                     checkboxGroupInput("ma_period_c", label = "Choose moving average periods:",
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
                                                 
                                                 mainPanel(
                                                   plotlyOutput("plotCommodities1"),
                                                   plotlyOutput("plotCommodities2"),
                                                   plotlyOutput("plotCommodities3"))),
    
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
                                                     checkboxGroupInput("ma_period_b", label = "Choose moving average periods:",
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
                                                 
                                                 
                                                 mainPanel(
                                                   plotlyOutput("plotBonds1"),
                                                   plotlyOutput("plotBonds2"),
                                                   plotlyOutput("plotBonds3"))),
    
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
                                                     checkboxGroupInput("ma_period_crypto", label = "Choose moving average periods:",
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
                                                 
                                                 mainPanel(
                                                   plotlyOutput("plotCryptocurrencies1"),
                                                   plotlyOutput("plotCryptocurrencies2"),
                                                   plotlyOutput("plotCryptocurrencies3")))),
    
####################################PORTFOLIO OPTIMIZATION#####################################################################
    
                              navbarMenu("Portfolio optimization", icon = icon ("money-bill-trend-up"),
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

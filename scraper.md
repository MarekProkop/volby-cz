Stahování historických dat z Volby.cz
================
Marek Prokop
2021-10-13

# Úvod

Je po parlamentních volbách 2021 a hodně lidí teď počítá z výsledků
různé statistiky. Protože je docela zajímavé porovnat právě skončené
volby s minulými (např. preferenční hlasy), stáhl jsem data všech
historických voleb do PSP ČR do jednoho datasetu. Pokud chcete jen ten
dataset, je ve složce *data.*

Zároveň jsem ale zdokumentoval celý postup, jak jsem data stáhl. Udělal
jsem to jednak proto, že by se to někomu mohlo hodit jako univerzální
příklad získávání podobných dat z webu, a jednak proto, že to třeba
nemám napsané úplně nejlépe a někdo mi doporučí lepší řešení, opraví
chyby apod.

Český statistický úřad publikuje data k volbám na webu
[Volby.cz](https://www.volby.cz/) jednak v HTML na webových stránkách a
jednak v XML. XML je na zpracování praktičtější, ale v tomto formátu
nejsou zpracované všechny volby. Připravím tedy řešení jen pro formát
HTML.

# Balíčky a parametry

Nejprve načtu balíčky, které budu potřebovat a definuji si URL úvodní
stránky s rozcestníkem na volby a jejich roky.

``` r
library(rvest, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.2     v dplyr   1.0.6
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## Warning: package 'stringr' was built under R version 4.1.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter()         masks stats::filter()
    ## x readr::guess_encoding() masks rvest::guess_encoding()
    ## x dplyr::lag()            masks stats::lag()

``` r
library(janitor, warn.conflicts = FALSE)
library(memoise)
```

``` r
topIndexHtmlUrl <- "https://volby.cz/"
```

# Hlavní index voleb a roků

Na stránce <https://volby.cz/> je hlavní rozcestník na výsledky voleb ve
formátu HTML. Jde o tabulku, která má v prvním sloupci název voleb a v
druhém sloupci odkazy na hlavní stránky daných voleb v jednotlivých
letech. Tabulka nemá žádné hlavičky, skládá se jen z prvků `tr`, `td` a
`a`. Její CSS class je `home`.

Tuto tabulku chci stáhnout do data framu se sloupci:

  - volby: název voleb
  - rok: číslo roku
  - url: URL hlavní stránky daných voleb pro daný rok

## Postup

I když chci stáhnout jen jednu tabulku, raději si na to napíšu funkci,
kterou jde volat opakovaně s různým vstupním URL.

Balíčkem *rvest* stáhnu úvodní stránku a z ní si uložím tabulku `home`.
Parsování je trochu složitější, protože tabulka není moc dobře
strukturovaná. Událám to funkcemi z balíčku *purrr* postupně po řádcích
a roky z druhého sloupce uložím do seznamu (*list*). Ten pak rozdělím na
dva sloupce funkcí *unest\_wider* z balíčku *tidyr*.

``` r
getTopIndex <- function(indexUrl) {
  indexHtml <- indexUrl %>% 
    read_html(encoding = "utf-8") %>% 
    html_element("table.home")
  
  indexHtml %>% html_nodes("tr") %>% 
    map_dfr(function(x) {
      tds <- x %>% html_nodes("td")
      electionName = tds[1] %>% html_text2()
      tibble(
        volby = electionName,
        roky = tds[2] %>% html_elements("a") %>% 
          map(function(y) {
            list(
              rok = y %>% html_text(),
              url = y %>% html_attr("href") %>% url_absolute(indexUrl)
            )
          })
      )
    }) %>% 
    unnest_wider(col = roky)
}
```

## Výsledek

Funkci aplikuji na URL indexu výsledků v HTML.

``` r
topIndexHtml <- getTopIndex(topIndexHtmlUrl)
```

A dostanu výsledek:

``` r
topIndexHtml %>% glimpse()
```

    ## Rows: 55
    ## Columns: 3
    ## $ volby <chr> "Prezident republiky", "Prezident republiky", "Poslanecká sněmov~
    ## $ rok   <chr> "2013", "2018", "1996", "1998", "2002", "2006", "2010", "2013", ~
    ## $ url   <chr> "https://volby.cz/pls/prez2013/pe", "https://volby.cz/pls/prez20~

Z takto vytvořeného data framu můžu vybírat konkrétní volby a všechny
jejich roky. Teď mě např. zajímají volby do Poslanecké sněmovny PČR:

``` r
topIndexHtml %>% 
  filter(volby == "Poslanecká sněmovna Parlamentu ČR")
```

    ## # A tibble: 8 x 3
    ##   volby                             rok   url                                   
    ##   <chr>                             <chr> <chr>                                 
    ## 1 Poslanecká sněmovna Parlamentu ČR 1996  https://volby.cz/pls/ps1996/u0        
    ## 2 Poslanecká sněmovna Parlamentu ČR 1998  https://volby.cz/pls/ps1998/u0        
    ## 3 Poslanecká sněmovna Parlamentu ČR 2002  https://volby.cz/pls/ps2002/psm       
    ## 4 Poslanecká sněmovna Parlamentu ČR 2006  https://volby.cz/pls/ps2006/ps?xjazyk~
    ## 5 Poslanecká sněmovna Parlamentu ČR 2010  https://volby.cz/pls/ps2010/ps?xjazyk~
    ## 6 Poslanecká sněmovna Parlamentu ČR 2013  https://volby.cz/pls/ps2013/ps?xjazyk~
    ## 7 Poslanecká sněmovna Parlamentu ČR 2017  https://volby.cz/cz/ps2017.htm        
    ## 8 Poslanecká sněmovna Parlamentu ČR 2021  https://volby.cz/pls/ps2021/ps?xjazyk~

# Jmenné seznamy k volbám do Poslanecké sněmovny

Chci stáhnout všechny jmenné seznamy ke všem volbám do Poslanecké
sněmovny a uspořádat je do jednoho data framu, ve kterém budou řádky
odpovídat jednotlivým kandidátům a sloupce všem údajům, které k nim
alespoň v některém roce v datech jsou.

Tento úkol komplikuje několik věcí:

1.  Finální data pro každý rok nejsou na jedné stránce. Je potřeba
    stáhnout data ze stánek za jednotlivé kraje, kterých je 14.
2.  Cesta ke stránce s daty kraje je od úvodní stránky docela dlouhá (4
    až 5 kliků) a není vždy stejná. Konkrétně v roce 2017 má jeden kok
    navíc.
3.  Struktura tabulek s finálními daty není jednotná. Jak uvidíte
    později, v různých letech se používají celkem tři různé sady
    sloupců.

## Postup

Protože nechci data z webu během testování postupu příliš často
opakovaně stahovat, všechny stahovací funkce ošetřím funkcí *memoise*,
která na pozadí zajistí kešování výsledků. Opakované volání téže funkce
se stejnými parametry tedy data reálně stahovat z webu, ale vezme je z
keše.

### Nalezení koncového URL

Nejprve se potřebuju dostat na stránku, která už přímo odkazuje na
tabulky se jmennými seznamy za jednotlivé kraje. To je např. takto
stránka: <https://www.volby.cz/pls/ps2021/ps11?xjazyk=CZ&xv=1&xt=1>

Cesta k ní jde popsat funkcí:

``` r
navigateToAllCandidates <- function(year) {
  topIndexHtml %>% 
  filter(
    volby == "Poslanecká sněmovna Parlamentu ČR",
    rok == year
  ) %>% 
  pull(url) %>% 
  session() %>% 
  {if (year == "2017") session_follow_link(., css = "ul li a") else . } %>% 
  session_follow_link("Jmenné seznamy") %>% 
  session_follow_link(css = "table a")
}
m_navigateToAllCandidates <- memoise(navigateToAllCandidates)
```

Funkce funguje správně na všech letech včetně roku 2017, pro který je
tam jeden přidaný krok:

``` r
m_navigateToAllCandidates("2021")
```

    ## Navigating to ps1?xjazyk=CZ

    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## <session> https://volby.cz/pls/ps2021/ps11?xjazyk=CZ&xv=1&xt=1
    ##   Status: 200
    ##   Type:   text/html; charset=UTF-8
    ##   Size:   41732

``` r
m_navigateToAllCandidates("2017")
```

    ## Navigating to /pls/ps2017nss/ps?xjazyk=CZ

    ## Navigating to ps1?xjazyk=CZ

    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## <session> https://volby.cz/pls/ps2017nss/ps11?xjazyk=CZ&xv=1&xt=1
    ##   Status: 200
    ##   Type:   text/html; charset=UTF-8
    ##   Size:   55623

### Jmenné seznamy podle krajů

Výše uvedeným postupem se dostanu na všechny stránky tohoto typu:
<https://www.volby.cz/pls/ps2021/ps11?xjazyk=CZ&xv=1&xt=1>. Ty obsahují
tabulku, která má v první řádku odkazy na jmenné seznamy jednotlivých
krajů, což jsou ta data, která chci stahovat.

#### Zjištění struktury

Potíž je, že tahle data vypadají v různých letech různě. V roce 2021
takto:
<https://www.volby.cz/pls/ps2021/ps111?xjazyk=CZ&xkraj=1&xstrana=0&xv=1&xt=1>,
ale v např. roce 1996 takto:
<https://www.volby.cz/pls/ps1996/u321?xpl=0&xtr=1&xvstrana=00&xkraj=31>.

Proto si nejprve ověřím, kolik verzí vlastně je a jak struktura tabulky
(tj. názvy sloupcú) v jednotlivých letech vypadá.

``` r
getStructure <- function(year) {
  tibble(
    rok = year,
    col_name = m_navigateToAllCandidates(year) %>% 
      session_follow_link(css = "table a") %>% 
      html_element("table") %>% 
      html_table() %>% 
      clean_names() %>% 
      names()
  ) %>% 
    mutate(col_num = row_number(), .after = 1)
}

structure <- topIndexHtml %>% 
  filter(volby == "Poslanecká sněmovna Parlamentu ČR") %>% 
  pull(rok) %>% 
  map_dfr(getStructure)
```

    ## Navigating to u3

    ## Navigating to u32?xpl=0&xtr=1

    ## Navigating to u321?xpl=0&xtr=1&xvstrana=00&xkraj=31

    ## Navigating to u3

    ## Navigating to u32?xpl=0&xtr=1

    ## Navigating to u321?xpl=0&xtr=1&xvstrana=00&xkraj=31

    ## Navigating to ps1?xjazyk=CZ

    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## Navigating to ps111?xjazyk=CZ&xv=1&xt=1&xkstrana=0&xkraj=1

    ## Navigating to ps1?xjazyk=CZ

    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## Navigating to ps111?xjazyk=CZ&xkraj=1&xstrana=0&xv=1&xt=1

    ## Navigating to ps1?xjazyk=CZ

    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## Navigating to ps111?xjazyk=CZ&xkraj=1&xstrana=0&xv=1&xt=1

    ## Navigating to ps1?xjazyk=CZ

    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## Navigating to ps111?xjazyk=CZ&xkraj=1&xstrana=0&xv=1&xt=1
    ## Navigating to ps111?xjazyk=CZ&xkraj=1&xstrana=0&xv=1&xt=1
    ## Navigating to ps111?xjazyk=CZ&xkraj=1&xstrana=0&xv=1&xt=1

A rozdíly ve struktuře si přehledně zobrazím:

``` r
structure %>% 
  pivot_wider(names_from = col_num, values_from = col_name) %>% 
  group_by(across(-rok)) %>% 
  summarise(roky = str_c(rok, collapse = ", "), .groups = "drop") %>% 
  relocate(roky, .before = 1)
```

    ## # A tibble: 3 x 12
    ##   roky    `1`     `2`     `3`    `4`   `5`   `6`   `7`   `8`   `9`   `10`  `11` 
    ##   <chr>   <chr>   <chr>   <chr>  <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    ## 1 2006, ~ kandid~ kandid~ kandi~ kand~ kand~ navr~ poli~ pred~ pred~ mand~ pora~
    ## 2 1996, ~ kandid~ kandid~ kandi~ kand~ vek   pred~ pred~ man_~ pora~ <NA>  <NA> 
    ## 3 2002    kandid~ kandid~ por_c~ kand~ kand~ vek   navr~ poli~ pred~ pred~ <NA>

Z výsledku je vidět, že tabulky osbahují maxmálně 12 sloupců a existují
tři různé verze:

1.  pro roky 2006, 2010, 2013, 2017, 2021,
2.  pro roky 1996, 1998 a
3.  pro rok 2002

Do finálních dat chci dostat těchto 11 sloupců:

1)  pořadové číslo strany: 1
2)  název strany: 2
3)  číslo kandidáta: 3
4)  jméno kandidáta: 2002: 4 + 5, ostatní: 4
5)  věk: 2002: 6, ostatní: 5
6)  navrhující strana: 2002: 7; 1996, 1998: NA; ostatní: 6
7)  politická příslušnost: 2002: 8; 1996, 1998: NA; ostatní: 7
8)  přednostní hlasy absolutně: 2002: 9; 1996, 1998: 6; ostatní: 8
9)  přednostní hlasy %: 2002: 10; 1996, 1998: 7; ostatní: 9
10) mandát: 1996, 1998: 8; 2002: NA; ostatní: 10
11) pořadí: 1996, 1998: 9; 2002: NA; ostatní: 11

#### Načtení dat

Zpracování jenoho roku a kraje voleb rozvětvím pro odlišnou strukturu v
různých letech a bude vypadat takto:

``` r
loadRegionData <- function(year, region, pageUrl) {
  df <- read_html(pageUrl) %>% 
    html_element("table") %>% 
    html_table() %>% 
    clean_names() %>% 
    slice(-1)
  
  if (as.numeric(year) >= 2006) {
    df <- df %>% 
      transmute(
        strana_cislo = as.integer(.[[1]]),
        strana_nazev = .[[2]],
        kandidat_cislo = as.integer(.[[3]]),
        kandidat = .[[4]],
        kandidat_vek = as.integer(.[[5]]),
        navrhujici_strana = .[[6]],
        prislusnost = .[[7]],
        prednostni_hlasy_abs = as.numeric(str_replace_all(.[[8]], "[^0-9]", "")),
        prednostni_hlasy_pct = as.numeric(str_replace(.[[9]], ",", ".")),
        mandat = .[[10]],
        poradi = as.integer(.[[11]])
      )
  } else if (as.numeric(year) == 2002) {
    df <- df %>% 
      transmute(
        strana_cislo = as.integer(.[[1]]),
        strana_nazev = .[[2]],
        kandidat_cislo = as.integer(.[[3]]),
        kandidat = paste(.[[4]], .[[5]]),
        kandidat_vek = as.integer(.[[6]]),
        navrhujici_strana = .[[7]],
        prislusnost = .[[8]],
        prednostni_hlasy_abs = as.numeric(str_replace_all(.[[9]], "[^0-9]", "")),
        prednostni_hlasy_pct = as.numeric(str_replace(.[[10]], ",", ".")),
        mandat = NA,
        poradi = NA
      )
  } else { # roky 1996 a 1998
    df <- df %>% 
      transmute(
        strana_cislo = as.integer(.[[1]]),
        strana_nazev = .[[2]],
        kandidat_cislo = as.integer(.[[3]]),
        kandidat = .[[4]],
        kandidat_vek = as.integer(.[[5]]),
        navrhujici_strana = NA,
        prislusnost = NA,
        prednostni_hlasy_abs = as.numeric(str_replace_all(.[[6]], "[^0-9]", "")),
        prednostni_hlasy_pct = as.numeric(str_replace(.[[7]], ",", ".")),
        mandat = .[[8]],
        poradi = as.integer(.[[9]])
      )
  }
  
  df %>% 
    mutate(rok = year, kraj = region, .before = 1)
}
m_loadRegionData <- memoise(loadRegionData)

m_loadRegionData("1996", "Hlavní město Praha", "https://www.volby.cz/pls/ps1996/u321?xpl=0&xtr=1&xvstrana=00&xkraj=31") %>% 
  slice_head(n = 10)
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## # A tibble: 10 x 13
    ##    rok   kraj    strana_cislo strana_nazev kandidat_cislo kandidat  kandidat_vek
    ##    <chr> <chr>          <int> <chr>                 <int> <chr>            <int>
    ##  1 1996  Hlavní~            7 DEU                      12 Abraham ~           64
    ##  2 1996  Hlavní~           13 ČMUS                      3 Adamčík ~           48
    ##  3 1996  Hlavní~            7 DEU                      24 Adamec L~           47
    ##  4 1996  Hlavní~           15 KSČM                     23 Ambrož P~           37
    ##  5 1996  Hlavní~            9 SČK                      35 Antoš Př~           74
    ##  6 1996  Hlavní~            1 SD-LSNS                  33 Babáček ~           51
    ##  7 1996  Hlavní~           12 SDL                      10 Bača Jar~           40
    ##  8 1996  Hlavní~           12 SDL                      32 Baláž Lu~           50
    ##  9 1996  Hlavní~            5 NEZ                       8 Barcháne~           47
    ## 10 1996  Hlavní~           13 ČMUS                     11 Bartoš L~           47
    ## # ... with 6 more variables: navrhujici_strana <lgl>, prislusnost <lgl>,
    ## #   prednostni_hlasy_abs <dbl>, prednostni_hlasy_pct <dbl>, mandat <chr>,
    ## #   poradi <int>

Zpracování všech regionů jedno celého roku bude vypadat takto:

``` r
loadAllRegions <- function(year) {
  page <- navigateToAllCandidates(year)
  
  regionUrls <- page %>% 
    html_elements("table tr:nth-child(2) a") %>% 
    html_attr("href") %>% 
    url_absolute(page$url)

  regionNames <- page %>% 
    html_elements("table tr:nth-child(1) th") %>% 
    html_text2() %>% 
    str_replace_all(fixed("-\n"), "") %>% 
    str_replace_all(fixed("\n"), " ") %>%
    .[1:length(regionUrls) + 2]
  
  map2_dfr(.x = regionNames, .y = regionUrls, .f = ~ m_loadRegionData(year, .x, .y))

}
m_loadAllRegions <- memoise(loadAllRegions)

m_loadAllRegions("2002") %>% 
  slice_sample(n = 10)
```

    ## Navigating to ps1?xjazyk=CZ

    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## # A tibble: 10 x 13
    ##    rok   kraj    strana_cislo strana_nazev kandidat_cislo kandidat  kandidat_vek
    ##    <chr> <chr>          <int> <chr>                 <int> <chr>            <int>
    ##  1 2002  Zlínsk~            6 VPB                       2 "Klimt R~           61
    ##  2 2002  Olomou~           23 KSČM                      8 "Surma A~           34
    ##  3 2002  Hlavní~           25 KOALICE                  34 "Polecha~           54
    ##  4 2002  Hlavní~            2 DL                       24 "Rozgony~           64
    ##  5 2002  Ústeck~            5 ODA                       2 "Bažant ~           51
    ##  6 2002  Vysoči~           25 KOALICE                   9 "Žáková ~           33
    ##  7 2002  Vysoči~           23 KSČM                     16 "Havlíče~           56
    ##  8 2002  Zlínsk~            3 ČSSD                     13 "Vykydal~           46
    ##  9 2002  Středo~            2 DL                       21 "Hoplíče~           27
    ## 10 2002  Olomou~           26 SŽJ                      20 "Ducháčo~           24
    ## # ... with 6 more variables: navrhujici_strana <chr>, prislusnost <chr>,
    ## #   prednostni_hlasy_abs <dbl>, prednostni_hlasy_pct <dbl>, mandat <lgl>,
    ## #   poradi <lgl>

A nyní konečně mohu načíst a zpracovat úplně všechna data.

``` r
allData <- topIndexHtml %>% 
  filter(volby == "Poslanecká sněmovna Parlamentu ČR") %>% 
  pull(rok) %>% 
  map_dfr(m_loadAllRegions)
```

    ## Navigating to u3

    ## Navigating to u32?xpl=0&xtr=1

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Navigating to u3
    ## Navigating to u32?xpl=0&xtr=1

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Navigating to ps1?xjazyk=CZ

    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Navigating to ps1?xjazyk=CZ
    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Navigating to ps1?xjazyk=CZ
    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Navigating to /pls/ps2017nss/ps?xjazyk=CZ

    ## Navigating to ps1?xjazyk=CZ

    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Navigating to ps1?xjazyk=CZ
    ## Navigating to ps11?xjazyk=CZ&xv=1&xt=1

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
    
    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
allData %>% 
  slice_sample(n = 10)
```

    ## # A tibble: 10 x 13
    ##    rok   kraj    strana_cislo strana_nazev kandidat_cislo kandidat  kandidat_vek
    ##    <chr> <chr>          <int> <chr>                 <int> <chr>            <int>
    ##  1 1996  Jihoče~           17 "SPR-RSČ"                 9 "Vaňata ~           73
    ##  2 2010  Plzeňs~            7 "\"KČ\""                 18 "Šmídová~           79
    ##  3 2021  Jihomo~           22 "Moravané"                1 "Valášek~           33
    ##  4 1998  Severo~            9 "KSČM"                   53 "Halbich~           60
    ##  5 1998  Východ~            9 "KSČM"                   33 "Plodík ~           42
    ##  6 2006  Hlavní~           18 "SZ"                      2 "Jacques~           35
    ##  7 2002  Středo~           27 "PB"                     13 "Brzoboh~           76
    ##  8 2013  Libere~            5 "HLVZHŮRU"                7 "Jirotka~           29
    ##  9 2002  Moravs~           14 "ROI"                    29 "Gábor G~           46
    ## 10 2017  Jihomo~           22 "DV 2016"                 2 "Reichma~           62
    ## # ... with 6 more variables: navrhujici_strana <chr>, prislusnost <chr>,
    ## #   prednostni_hlasy_abs <dbl>, prednostni_hlasy_pct <dbl>, mandat <chr>,
    ## #   poradi <int>

#### Kontrola dat

Ještě pár kontrolami ověřím, zda jsou data pravděpodobně v pořádku
načtená.

``` r
allData %>% count(rok)
```

    ## # A tibble: 8 x 2
    ##   rok       n
    ##   <chr> <int>
    ## 1 1996   4492
    ## 2 1998   3631
    ## 3 2002   6068
    ## 4 2006   4985
    ## 5 2010   5022
    ## 6 2013   5899
    ## 7 2017   7524
    ## 8 2021   5242

``` r
allData %>% count(kraj)
```

    ## # A tibble: 20 x 2
    ##    kraj                     n
    ##    <chr>                <int>
    ##  1 Hlavní město Praha    3945
    ##  2 Jihočeský kraj        2667
    ##  3 Jihomoravský kraj     5292
    ##  4 Karlovarský kraj      1330
    ##  5 kraj Praha            1041
    ##  6 Kraj Vysočina         1585
    ##  7 Královéhradecký kraj  1927
    ##  8 Liberecký kraj        1688
    ##  9 Moravskoslezský kraj  3670
    ## 10 Olomoucký kraj        2394
    ## 11 Pardubický kraj       1888
    ## 12 Plzeňský kraj         2019
    ## 13 Severočeský kraj       945
    ## 14 Severomoravský kraj   1475
    ## 15 Středočeský kraj      4280
    ## 16 Ústecký kraj          2578
    ## 17 Východočeský kraj      868
    ## 18 Vysočina               358
    ## 19 Západočeský kraj       694
    ## 20 Zlínský kraj          2219

``` r
allData %>% count(strana_nazev)
```

    ## # A tibble: 124 x 2
    ##    strana_nazev     n
    ##    <chr>        <int>
    ##  1 "\"KČ\""       103
    ##  2 "4 VIZE"        28
    ##  3 "A2000"          8
    ##  4 "ANEO"          11
    ##  5 "ANO"          685
    ##  6 "ANO 2011"     341
    ##  7 "ANS"          208
    ##  8 "APB"          343
    ##  9 "AZSD"          26
    ## 10 "BPI"          273
    ## # ... with 114 more rows

#### Uložení dat do CSV

Úplně nakonec uložím celý dataset do CSV souboru a je hoto.

``` r
allData %>% write_csv("data/psp-all-data.csv")
```

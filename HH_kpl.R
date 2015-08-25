
#########################################################################################
#                                                                                       #
#                                 Förberedelser lokalt                                  #
#                                                                                       #
#########################################################################################

is.inca <- function(){
    unname(Sys.info()["nodename"] == "EXT-R27-PROD")
}

if (!is.inca()) {
    setwd("~/Documents/huvud_hals/koll_pa_laget")
    options(register = "Huvud- och halscancer")
    df <- read.csv("incadata.csv")
}

#########################################################################################
#                                                                                       #
#                                     Förberedelser                                     #
#                                                                                       #
#########################################################################################

library(jsonlite)
if (!is.inca()) library(infuser)

names(df) <- tolower(names(df))


#########################################################################################
#                                                                                       #
#                                    Hjälpfunktioner                                    #
#                                                                                       #
#########################################################################################


# Skapa ledtid
# Negativa tider samt tider länre än ett år exkluderas
lt <- function(from, to){
    lt <- as.numeric(to - from)
    lt[!(lt %in% 0:365)] <- NA
    lt
}


# är första elementet det minsta av två
first_first <- function(x){
    if (all(is.na(x))) NA else if (is.na(x[1])) FALSE else x[1] == min(x, na.rm = TRUE)
}

# Funktion för att hitta relevant årtal. Nytt år tas först fr o m 1 juli
current_year <- function(date = Sys.Date()) {
    date <- as.Date(date)
    year <- as.numeric(format(date, format = "%Y"))
    month <- as.numeric(format(date, format = "%Y"))
    if (month <= 6 ) {
        year <- year - 1
    }
    year
}

#########################################################################################
#                                                                                       #
#                                 Korrekta datumformat                                  #
#                                                                                       #
#########################################################################################

## Gör alla faktorvariabler till character
faktorer <- sapply(df, is.factor)
df[faktorer] <- lapply(df[faktorer], as.character)

## Datum ska vara datum
datum_variabler <- c("a_remdat",
                     "a_diadat",
                     "a_cytdat",
                     "a_cytpad",
                     "a_besok",
                     "a_besldat",
                     "b_op1dat",
                     "b_op2dat",
                     "b_brachystart",
                     "b_stralstart",
                     "b_medstart"
                )
df[datum_variabler] <- lapply(df[datum_variabler],
                             function(x) as.Date(x, format = "%Y-%m-%d")
                       )

## Boolska variabler ska vara boolska
df <- transform(df,
        klinikbehorighet = as.logical(klinikbehorighet),
        regionbehorighet = as.logical(regionbehorighet)
)


#########################################################################################
#                                                                                       #
#                          Dataset med alla indikatorvariabler                          #
#                                                                                       #
#########################################################################################


df <-
    dplyr::mutate(df,

        ## Hjälpvariabler
        # Datum för första icke kirurgiska behandling
        b_behstart = pmin(b_brachystart, b_stralstart, b_medstart, na.rm = TRUE),
        # Datum för första kirurgiska behandling
        opdat = pmin(b_op1dat, b_op2dat, na.rm = TRUE),
        # Datum för förata behandling (oavsett kir eller icke kir)
        behandlingsstart = pmin(b_behstart, opdat, na.rm = TRUE),
        # ledtid för indikator nr 3, 4, 5
        lt3 = lt(a_besldat, behandlingsstart),
        # Multidiciplinär konferens (läpp exkuderas)
        a_multkonf_beskrivning = ifelse(a_multkonf_beskrivning != "" &
                                        a_icd10_gruppnamn  !=  "1 Läpp",
                                        a_multkonf_beskrivning, NA),

        ## Årtalsvariabler
        ar_remiss = format(a_remdat, format = "%Y"),
        ar_behandlingsstart = format(behandlingsstart, format = "%Y"),
        ar_beslut = format(a_besldat, format = "%Y"),
        ar_besok  = format(a_besok, format = "%Y"),
        ar_cytdat = format(a_cytdat, format = "%Y"),
        ar_cytpad = format(a_cytpad, format = "%Y")
        )

df <- dplyr::mutate(df,

        ## Indikatorvariabler
        ind1  = lt(a_remdat, behandlingsstart)              <= 40,
        ind2  = lt(a_besok, behandlingsstart)               <= 35,
        ind3  = lt3                                         <= 15,
        ind4  = ifelse(apply(data.frame(df$b_behstart, df$opdat),
                    1, first_first), lt3, NA)               <= 20,
        ind5  = ifelse(apply(data.frame(df$opdat, df$b_behstart),
                    1, first_first), lt3, NA)               <= 12,
        ind6 =  lt(a_remdat, a_besldat)                     <= 25,
        ind7 =  lt(a_remdat, a_besok)                       <= 5,
        ind8 =  lt(a_besok, a_cytdat)                       <= 3,
        ind9 =  lt(a_cytdat, a_cytpad)                      <= 3,
        ind10 = a_multkonf_beskrivning                      == "Ja"
    )



#########################################################################################
#                                                                                       #
#                       Definition av variabler till HTML-skript                        #
#                                                                                       #
#########################################################################################

indikator <- function(ind, ar, name, l1 = 50, l2 = 80,
                      description = ""){

    # Indikator. kan fallet klassas som "i år"
    iar <- ar == current_year()

    ## Plocka fram historiska klinikdata
    h <- data.frame(ind = ind[df$klinikbehorighet], ar = ar[df$klinikbehorighet])
    h <- aggregate(ind ~ ar, function(x) mean(x, na.rm = TRUE), data = h)
    h <- round(h[h$ar %in% (current_year() - 4):(current_year() - 1), "ind"] * 100 )
    h[h %in% c(NA, NaN)] <- 0
    historiska_ar <- h


    ## Hjälpfunktioner för att beräkna täljare och nämnare baserat på nivå
    num <- function(behorighet = TRUE){
        sum(ind[behorighet & iar], na.rm = TRUE)
    }
    den <- function(behorighet = TRUE){
        sum(!is.na(ind[behorighet & iar]))
    }

    ## Konstruera objekt med all info för ledtiden
    data.frame(
        name        = name,
        klinnum     = num(df$klinikbehorighet),
        klinden     = den(df$klinikbehorighet),
        regnum      = num(df$regionbehorighet),
        regden      = den(df$regionbehorighet),
        riknum      = num(),
        rikden      = den(),
        history1    = historiska_ar[1],
        history2    = historiska_ar[2],
        history3    = historiska_ar[3],
        history4    = historiska_ar[4],
        l1          = l1,
        l2          = l2,
        description = description
    )
}

indikatordefenitioner <-
    rbind(

        indikator(df$ind1, ar = df$ar_behandlingsstart,
                  name = "Remissankomst till behandlingsstart",
                  l1 = 50, l2 = 80,
                  description = paste0("Andel patienter där antalet (vecko)dagar ",
                        "från remissankomst (enligt blankett 1) till behandlingsstart ",
                        "(enligt blankett 2 a eller 2 b) uppgår till maximalt 40 dagar ",
                        "(negativa ledtider samt ledtider längre än ett år exkluderade).")
        ),

        indikator(df$ind2, ar = df$ar_behandlingsstart,
                  name = "Första besök på ÖNH-klinik till behandlingsstart ",
                  l1 = 50, l2 = 80,
                  description = paste0("Andel patienter där antalet (vecko)dagar ",
                        "från första besök på utredande ÖNH-mottagning ",
                        "(enligt blankett 1) till behandlingsstart ",
                        "(enligt blankett 2 a eller 2 b) uppgår till maximalt 35 ",
                        "dagar (negativa ledtider samt ledtider längre än ett år ",
                        "exkluderade).")
        ),

        indikator(df$ind3, ar = df$ar_behandlingsstart,
                  name = "Behandlingsbeslut till behandlingsstart ",
                  l1 = 50, l2 = 80,
                  description = paste0("Andel patienter där antalet (vecko)dagar ",
                        "från behandlingsbeslut (enligt blankett 1) till ",
                        "behandlingsstart (enligt blankett 2 a eller 2 b) uppgår ",
                        "till maximalt 15 dagar (negativa ledtider samt ledtider ",
                        "längre än ett år exkluderade).")
        ),

        indikator(df$ind4, ar = df$ar_behandlingsstart,
                  name = "Behandlingsbeslut till onkologisk behandlingsstart ",
                  l1 = 50, l2 = 80,
                  description = paste0("Andel patienter där antalet (vecko)dagar ",
                        "från behandlingsbeslut (enligt blankett 1) till onkologisk ",
                        "behandling (enligt blankett 2 b) uppgår till maximalt 20 ",
                        "dagar (enbart fall där extern strålbehandling, ",
                        "brachyterapi eller medecinsk tumörbehandling ges som ",
                        "första eller enda behandling; negativa ledtider samt ",
                        "ledtider längre än ett år exkluderade).")
        ),

        indikator(df$ind5, ar = df$ar_behandlingsstart,
                  name = "Behandlingsbeslut till kirurgi",
                  l1 = 50, l2 = 80,
                  description = paste0("Andel patienter där antalet (vecko)dagar ",
                        "från behandlingsbeslut (enligt blankett 1) till kirurgi ",
                        "(enligt blankett 2 a) uppgår till maximalt 12 dagar ",
                        "(enbart fall där kirurgi ges som första eller enda ",
                        "behandling; negativa ledtider samt ledtider längre än ",
                        "ett år exkluderade).")
        ),

        indikator(df$ind6, ar = df$ar_beslut,
                  name = "Remissankomst till behandlingsbeslut ",
                  l1 = 50, l2 = 80,
                  description = paste0("Andel patienter där antalet (vecko)dagar ",
                        "från remissankomst (enligt blankett 1) till ",
                        "behandlingsbeslut (enligt blankett 1) uppgår till ",
                        "maximalt 25 dagar (negativa ledtider samt ledtider ",
                        "längre än ett år exkluderade).")
        ),

        indikator(df$ind7, ar = df$ar_besok,
                  name = "Remissankomst till första besök",
                  l1 = 50, l2 = 80,
                  description = paste("Andel patienter där antalet (vecko)dagar ",
                        "från remissankomst (enligt blankett 1) till  första ",
                        "besök på utredande ÖNH-mottagning (enligt blankett 1) ",
                        "uppgår till maximalt 5 dagar (negativa ledtider samt ",
                        "ledtider längre än ett år exkluderade).")
        ),

        indikator(df$ind8, ar = df$ar_cytdat,
                  name = "Första besök till cytologi/biopsi ",
                  l1 = 50, l2 = 80,
                  description = paste0("Andel patienter där antalet (vecko)dagar ",
                        "från första besök på utredande ÖNH-mottagning ",
                        "(enligt blankett 1) till provtagningsdatum för ",
                        "cytologi/px (enligt blankett 1) uppgår till maximalt 3 ",
                        "dagar (negativa ledtider samt ledtider längre än ett år ",
                        "exkluderade).")
        ),

        indikator(df$ind9, ar = df$ar_cytpad,
                  name = "Biopsi till Cyt/PAD svar ",
                  l1 = 50, l2 = 80,
                  description = paste0("Andel patienter där antalet (vecko)dagar ",
                        "från provtagningsdatum för cytologi/px (enligt blankett 1) ",
                        "till provsvarsdatum för cytologi/PAD  (enligt blankett 1) ",
                        "uppgår till maximalt 3 dagar (negativa ledtider samt ",
                        "ledtider längre än ett år exkluderade).")
        ),

        indikator(df$ind10, ar = df$ar_beslut,
                  name = "Andel beslut på multidisciplinär konferens ",
                  l1 = 90, l2 = 95,
                  description = paste0("Andel patienter för vilka ",
                        "behandlingsbeslut tagits vid multidiciplinär konferens ",
                        "(enligt blankett 1). (Patienter med läppcancer exkluderade.) ")
        )
    )

# Enligt önskemål ändras ordningen på indikatorerna:
indikatordefenitioner <- indikatordefenitioner[c(7, 8, 9, 2, 6, 3, 4, 5, 1, 10), ]



#########################################################################################
#                                                                                       #
#                          Numeriska uppgifter till sidhuvudet                          #
#                                                                                       #
#########################################################################################

df_sidhuvud <-
    subset(df,
           klinikbehorighet,
           c(ar_remiss, klinikbehorighet, ar_beslut, ar_behandlingsstart))


# Antal fall nuvarande år för angiven variabel
ant <- function(var){
    var <- df_sidhuvud[[var]]
    nrow(df_sidhuvud[!is.na(var) & var == current_year(), ])
}

## Uppgifterna i sidhuvudet
klin                           <- paste0("\"", unique(df$userposname), "\"")
ant_inkomna_remisser           <- ant("ar_remiss")
ant_fattade_behandlingsbeslut  <- ant("ar_beslut")
ant_paborjade_behandlingar     <- ant("ar_behandlingsstart")





##########################################################################################
#                                                                                        #
#                   Spara ner allt till textfil med namnn output.html                    #
#                                                                                        #
##########################################################################################


#################################### Ändra inget här ####################################
public_files <- "D:/R-Scripts/Väst/oc5buer/huvud-_och_halscancer/kpl/"

if (!is.inca()) {
    # På egen dator genererar vi del1 och del2 för att kunna lägga över i INCAs public files
    # (där ska sökvägen vara relativ)
    writeLines(infuse("del1.html"), "del1-inca.txt")
    writeLines(infuse("del2.html"), "del2-inca.txt")
    writeLines(infuse("kpl_klinik_region_riket-template.js", url = public_files), "kpl_klinik_region_riket.js")
    del1 <- infuse("del1.html", url = public_files)
    del2 <- infuse("del2.html", url = public_files)
} else{

    # läs in del 1 och 2 från INCA:s public files om vi befinner oss i INCA
    del1 <- readChar(paste0(public_files, "del1-inca.txt"), 1e5)
    del2 <- readChar(paste0(public_files, "del2-inca.txt"), 1e8)
}


dfjson <- toJSON(indikatordefenitioner)
mitten <- paste0("\t\t var ser =",     dfjson,
                 "\n\t\t var klin = ", klin,
                 "\n\t\t var diag =",  ant_inkomna_remisser,
                 "\n\t\t var beh=",    ant_fattade_behandlingsbeslut,
                 "\n\t\t var prim =",  ant_paborjade_behandlingar,
                 "\n\t\t var year =",    current_year()
          )

## Här måste vi vara försiktiga med encodings. Olika delar får olika encodings!!!
cat(del1, file = file('output.html', encoding = ''))
cat("\n\t<script>\n", file = 'output.html', append = TRUE)
cat(mitten, file = file('output.html', 'at', encoding = 'UTF-8'), append = TRUE)
cat("\n\t</Script>\n", file = file('output.html', 'at', encoding = 'UTF-8'), append = TRUE)
cat(del2, file = file('output.html', 'at', encoding = ''), append = TRUE)



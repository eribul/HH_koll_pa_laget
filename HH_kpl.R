


#########################################################################################
#                                                                                       #
#                                 Förberedelser lokalt                                  #
#                                                                                       #
#########################################################################################

setwd("~/Documents/huvud_hals/koll_pa_laget")
options(register = "Huvud- och halscancer")
df <- read.csv("incadata.csv")


#########################################################################################
#                                                                                       #
#                                     Förberedelser                                     #
#                                                                                       #
#########################################################################################


library(dplyr)
library(jsonlite)

names(df) <- tolower(names(df))

NUVARANDE_AR <- 2014


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
df <- df %>% mutate(
    klinikbehorighet = as.logical(klinikbehorighet),
    regionbehorighet = as.logical(regionbehorighet)
)



#########################################################################################
#                                                                                       #
#                          Numeriska uppgifter till sidhuvudet                          #
#                                                                                       #
#########################################################################################


## Uppgifterna i sidhuvudet
klin   <- paste0("\"", unique(df$userposname), "\"")
diag   <- 999
beh    <- 999
prim   <- 999
år     <- NUVARANDE_AR




#########################################################################################
#                                                                                       #
#                          Dataset med alla indikatorvariabler                          #
#                                                                                       #
#########################################################################################


df <- df %>%
    mutate(

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
        ar_behandlingsstart = format(behandlingsstart, format = "%Y"),
        ar_beslut = format(a_besldat, format = "%Y"),
        ar_besok  = format(a_besok, format = "%Y"),
        ar_cytdat = format(a_cytdat, format = "%Y"),
        ar_cytpad = format(a_cytpad, format = "%Y"),

        ## Indikatorvariabler
        ind1  = lt(a_remdat, behandlingsstart)              <= 40,
        ind2  = lt(a_besok, behandlingsstart)               <= 35,
        ind3  = lt3                                         <= 15,
        ind4  = ifelse(apply(data.frame(b_behstart, opdat),
                    1, first_first), lt3, NA)               <= 20,
        ind5  = ifelse(apply(data.frame(opdat, b_behstart),
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

    # Indikator. kan fallet klassas som "iår"
    iar <- ar == NUVARANDE_AR

    ## Plocka fram historiska klinikdata
    historiska_ar <- data_frame(ind = ind[df$klinikbehorighet], ar = ar[df$klinikbehorighet]) %>%
        group_by(ar) %>%
        summarise(andel = mean(ind, na.rm = TRUE)) %>%
        filter(ar %in% (NUVARANDE_AR - 4):(NUVARANDE_AR - 1)) %>%
        select(andel) %>%
        unlist() %>%
        unname() %>%
        `*`(100) %>%
        round() %>%
        ifelse(!is.nan(.) & !is.na(.) , ., 0)

    ## Hjälpfunktioner för att beräkna täljare och nämnare baserat på nivå
    num <- function(behorighet = TRUE){
        sum(ind[behorighet & iar], na.rm = TRUE)
    }
    den <- function(behorighet = TRUE){
        sum(!is.na(ind[behorighet & iar]))
    }

    ## Konstruera objekt med all info för ledtiden
    data_frame(
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

        # indikator 1
        indikator(df$ind1, ar = df$ar_behandlingsstart,
                  name = "Remissankomst till behandlingsstart",
                  l1 = 50, l2 = 80,
                  description = ""
        ),

        # indikator 2
        indikator(df$ind2, ar = df$ar_behandlingsstart,
                  name = "Första besök på ÖNH-klinik till behandlingsstart ",
                  l1 = 50, l2 = 80,
                  description = ""
        ),

        # indikator 3
        indikator(df$ind3, ar = df$ar_behandlingsstart,
                  name = "Behandlingsbeslut till behandlingsstart ",
                  l1 = 50, l2 = 80,
                  description = ""
        ),

        # indikator 4
        indikator(df$ind4, ar = df$ar_behandlingsstart,
                  name = "Behandlingsbeslut till onkologisk behandlingsstart ",
                  l1 = 50, l2 = 80),

        # indikator 5
        indikator(df$ind5, ar = df$ar_behandlingsstart,
                  name = "Behandlingsbeslut till kirurgi",
                  l1 = 50, l2 = 80,
                  description = ""
        ),

        # indikator 6
        indikator(df$ind6, ar = df$ar_beslut,
                  name = "Remissankomst till behandlingsbeslut ",
                  l1 = 50, l2 = 80),

        # indikator 7
        indikator(df$ind7, ar = df$ar_besok,
                  name = "Remissankomst till första besök",
                  l1 = 50, l2 = 80,
                  description = ""
        ),

        # indikator 8
        indikator(df$ind8, ar = df$ar_cytdat,
                  name = "Första besök till cytologi/biopsi ",
                  l1 = 50, l2 = 80,
                  description = ""
        ),

        # indikator 9
        indikator(df$ind9, ar = df$ar_cytpad,
                  name = "Biopsi till Cyt/PAD svar ",
                  l1 = 50, l2 = 80,
                  description = ""
        ),

        # indikator 10
        indikator(df$ind10, ar = df$ar_beslut,
                  name = "Andel beslut på multidisciplinär konferens ",
                  l1 = 95, l2 = 98,
                  description = ""
        )
    )









#################################### Ändra inget här ####################################


fileName <- "del1.html"
del1 <- readChar(fileName, file.info(fileName)$size)
fileName <- "del2.html"
del2 <- readChar(fileName, file.info(fileName)$size)

dfjson <- toJSON(indikatordefenitioner)
mitten <- paste0("\t\t var ser =", dfjson, "\n\t\t var klin = ", klin, "\n\t\t var diag =", diag, "\n\t\t var beh=", beh, "\n\t\t var prim =", prim,"\n\t\t var år =", år )

final = paste0(del1,"\n\t<script>\n",mitten,"\n\t</Script>\n",del2)

writeLines(final,"output.html")

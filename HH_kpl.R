


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

# Även befintliga ledtider ska ligga i intervallet 0-365 dagar
limit_zdiff <- function(x){
    ifelse(x %in% 0:365, x, NA)
}


# Hitta vilken behandling (kir/ickekir som gavs först)
# x är data frame där första kolumnen är kirurgisk datum, andra icke kir datum
onk_beh_first <- function(x){
    !all(is.na(x)) && x[2] < x[1]
}
kir_beh_first <- function(x){
    !all(is.na(x)) && x[1] <= x[2]
}

# Identifiera de poster som tillhör inloggad enhet i INCA
# sjh = variabelnamn på den sjukhuskod som identifierar sjukhuset
# klk = variabelnamn på den klinikkod som identifierar kliniken
sjh_klk <- function(df, sjh, klk){
    df$userparentunitcode == sjh & df$userunitcode == klk
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

        ## Kan den inloggade kliniken knytas till behandling etc?
        behandlande_klinik =
            sjh_klk(., a_behsjh1, a_behkli1) |
            sjh_klk(., a_behsjh2, a_bekli2),
        beslutande_klinik = sjh_klk(., a_beslsjh, a_beslkli),
        utredande_klinik = sjh_klk(., autrsjh, autrklk),
        klinik =
            sjh_klk(., a_anmsjh, a_anmkli) |
            sjh_klk(., b_anmsjh, b_anmkli) |
            sjh_klk(., b_onk_inrappsjh, b_onk_inrappklk) |
            sjh_klk(., u_uppfsjh, b_uppfkli) |
            behandlande_klinik |
            beslutande_klinik |
            utredande_klinik,
        region = region_namn == userregionname,

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
        ind4  = ifelse(apply(data.frame(opdat, b_behstart),
                    1, onk_beh_first), lt3, NA)             <= 20,
        ind5  = ifelse(apply(data.frame(opdat, b_behstart),
                    1, kir_beh_first), lt3, NA)             <= 12,
        ind6 = limit_zdiff(zdiff_behbeslut_remiss)          <= 25,
        ind7 = limit_zdiff(zdiff_önh_remiss)                <= 5,
        ind8 = limit_zdiff(zdiff_px_önh)                    <= 3,
        ind9 = limit_zdiff(zdiff_cytpad_px)                 <= 3,
        ind10 = a_multkonf_beskrivning                      == "Ja"
    )



#########################################################################################
#                                                                                       #
#                       Definition av variabler till HTML-skript                        #
#                                                                                       #
#########################################################################################

indikator <- function(ind, klinikindikator = df$klinik,
                      ar, region = df$region,
                      name, l1 = 50, l2 = 80,
                      description = "",
                      nuvarande_ar = NUVARANDE_AR){

    ## Plocka fram historiska klinikdata
    historiska_ar <- data_frame(ind = ind[klinikindikator], ar = ar[klinikindikator]) %>%
        group_by(ar) %>%
        summarise(andel = mean(ind, na.rm = TRUE)) %>%
        filter(ar %in% (nuvarande_ar - 4):(nuvarande_ar - 1)) %>%
        select(andel) %>%
        unlist() %>%
        unname() %>%
        `*`(100) %>%
        round() %>%
        ifelse(!is.nan(.) & !is.na(.) , ., 0)

    ## Konstruera objekt med all info för ledtiden
    data_frame(
        name        = name,
        klinnum     = sum(ind[klinikindikator & ar == nuvarande_ar], na.rm = TRUE),
        klinden     = sum(!is.na(ind[klinikindikator & ar == nuvarande_ar])),
        regnum      = sum(ind[region & ar == nuvarande_ar], na.rm = TRUE),
        regden      = sum(!is.na(ind[region & ar == nuvarande_ar])),
        riknum      = sum(ind[ar == nuvarande_ar], na.rm = TRUE),
        rikden      = sum(!is.na(ind[ar == nuvarande_ar])),
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
                  l1 = 50, l2 = 80),

        # indikator 2
        indikator(df$ind2, ar = df$ar_behandlingsstart,
                  name = "Första besök på ÖNH-klinik till behandlingsstart ",
                  l1 = 50, l2 = 80),

        # indikator 3
        indikator(df$ind3, ar = df$ar_behandlingsstart,
                  name = "Behandlingsbeslut till behandlingsstart ",
                  l1 = 50, l2 = 80),

        # indikator 4
        indikator(df$ind4, ar = df$ar_behandlingsstart,
                  name = "Behandlingsbeslut till onkologisk behandlingsstart ",
                  l1 = 50, l2 = 80),

        # indikator 5
        indikator(df$ind5, ar = df$ar_behandlingsstart,
                  name = "Behandlingsbeslut till kirurgi",
                  l1 = 50, l2 = 80),

        # indikator 6
        indikator(df$ind6, ar = df$ar_beslut,
                  name = "Remissankomst till behandlingsbeslut ",
                  l1 = 50, l2 = 80),

        # indikator 7
        indikator(df$ind7, ar = df$ar_besok,
                  name = "Remissankomst till första besök",
                  l1 = 50, l2 = 80),

        # indikator 8
        indikator(df$ind8, ar = df$ar_cytdat,
                  name = "Första besök till cytologi/biopsi ",
                  l1 = 50, l2 = 80),

        # indikator 9
        indikator(df$ind9, ar = df$ar_cytpad,
                  name = "Biopsi till Cyt/PAD svar ",
                  l1 = 50, l2 = 80),

        # indikator 10
        indikator(df$ind10, ar = df$ar_beslut,
                  name = "Andel beslut på multidisciplinär konferens ",
                  l1 = 95, l2 = 98)
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

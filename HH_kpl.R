
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
library(dplyr)
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
# eller enligt parameterval om sådant finns
current_year <- function(date = Sys.Date()) {

    # Använd värden från parameterval om sådana finns,
    # använd annars innevarande år fr o m juli, annars föregående år
    if (exists("param") && !is.null(param$year_from) && param$year_from != "1900") {
        from_year <- as.numeric(param$year_from)
        to_year  <- as.numeric(param$year_to)
        if (from_year > to_year) {
            fd <- file( "output.html", "w", encoding = "UTF-8" )
            write(paste('<strong> <font color="red"> Du har valt ett ogiltigt',
                        'årsintervall! Förmodligen bör du byta plats på de båda',
                        'årtalen! </forn></strong>', file = fd )
            )
            q()
        }
    } else {
        date <- as.Date(date)
        year <- as.numeric(format(date, format = "%Y"))
        month <- as.numeric(format(date, format = "%Y"))
        if (month <= 6 ) {
            to_year <- year - 1
        } else{
            to_year <- year
        }
        from_year <- to_year
    }

    # Ett antal olika årsangivelser i retur
    list(from_year        = from_year,
         to_year          = to_year,
         hist_years       = if (from_year == to_year) seq.int(to_year - 4, to_year - 1, 1)
                            else seq.int(to_year - 3, to_year, 1),
         years_num        = seq.int(from_year, to_year, 1),
         years_label      = if (from_year == to_year) from_year
                            else paste0("'", from_year, " - ", to_year, "'"),
         hist_years_label = if (from_year == to_year)
                                paste0("'", to_year - 4, " - ", to_year - 1, "'")
                            else paste0("'", to_year - 3, " - ", to_year, "'")

    )
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
datum_variabler <- c("a_rappdatanm",
                     "b_rappdatbeh",
                     "b_onk_inrappdat",
                     "a_remdat",
                     "a_diadat",
                     "a_cytdat",
                     "a_cytpad",
                     "a_besok",
                     "a_besldat",
                     "b_op1dat",
                     "b_op2dat",
                     "b_brachystart",
                     "b_stralstart",
                     "b_medstart",
                     "u_rappdatuppf"
                )
df[datum_variabler] <- lapply(df[datum_variabler],
                             function(x) as.Date(x, format = "%Y-%m-%d")
                       )



##########################################################################################
#                                                                                        #
#                             Klinik- och regiontillhörigeht                             #
#                                                                                        #
##########################################################################################
# Jämför alltid sjukhus
# Jämför dessutom klinik om klk != NULL och klinik relevant enl param
compare_unit <- function(sjh, klk = NULL) {
    compare_sjh <- as.numeric(df$userparentunitcode) == suppressWarnings(as.numeric(df[[sjh]]))
    x <-
        if (!is.null(klk)) {
            compare_sjh & as.numeric(df$userunitcode) == suppressWarnings(as.numeric(df[[klk]]))
        } else{
            compare_sjh
        }
    x[is.na(x)] <- FALSE
    x
}

# enhetstillhörighet styrd av parameterval
df$klinikbehorighet <-
        switch(param$belongs_to_unit,
          "min klinik registrerat anmälningsblanketten" =
              compare_unit("a_anmsjh", "a_anmkli"),
          "mitt sjukhus fattat behandlingsbeslut" =
              compare_unit("a_beslsjh"),
          "min klinik opererat" =
              compare_unit("b_op1sjh", "b_op1kli") |
              compare_unit("b_op2sjh", "b_op2kli") |
              compare_unit("b_ophogersjh", "b_ophogerkli") |
              compare_unit("b_opvanstersjh", "b_opvansterkli"),
          "min klinik utfört onkologisk behandling" =
              compare_unit("b_brachysjh", "b_brachykli") |
              compare_unit("b_stralsjh", "b_stralkli") |
              compare_unit("b_medsjh", "b_medkli"),
          "min klinik bidragit till INCA-rapportering" =
              as.logical(df$registerpostbehorighet)
        )
df$klinikbehorighet[is.na(df$klinikbehorighet)] <- FALSE
df$regionbehorighet <- as.logical(df$regionbehorighet)

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
    iar <- ar %in% current_year()$years_num

    ## Plocka fram historiska klinikdata
    h <- data.frame(ind = ind[df$klinikbehorighet], ar = ar[df$klinikbehorighet])
    h <- h %>%
        group_by(ar) %>%
        summarise(ind = mean(ind, na.rm = TRUE) * 100) %>%
        filter(ar %in% current_year()$hist_years) %>%
        .$ind
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


# Antal fall nuvarande år för angiven variabel
ant <- function(x, var){
    var <- as.Date(x[[var]])
    var <- as.numeric(format(var, format = "%Y"))
    nrow(x[!is.na(var) & var %in% current_year()$years_num, ])
}

not_blank <- function(x) {
    !is.na(x) & as.character(x) != ""
}


############################# Aktuell klinik ##############################
klin      <- paste0("\"", "Total inrapporteringsaktivitet ", current_year()$years_label, " för: <br>",
                    unique(df$userposname), "\"")



################ Antal inrapporterade anmälningsblanketter ################

ant_blk1  <- df[compare_unit("a_anmsjh", "a_anmkli"), ] %>%
    ant("a_rappdatanm")



################# Antal inrapporterade behandlingsblanketter ##################

# Blanketten ska kunna knytas till anmälande enhet av kirurgi bland de enheter
# man valt att inkludera
ant_blk2 <- df[compare_unit("b_anmsjh", "b_anmkli"), ] %>%

    # Det finns två rappdatum, räcker att ett av dem är ifyllt
    mutate(b_beh_rappdat_kombinerat = ifelse(is.na(b_onk_inrappdat),
                                     as.character(b_rappdatbeh),
                                     as.character(b_onk_inrappdat))) %>%

    # Blanketten kan knytas till inrapportörens klinik oavsett om det är
    # kir eller onk
    mutate(b_onk_inrappklk2 = ifelse(is.na(b_onk_inrappklk),
                                     as.character(b_anmkli),
                                     as.character(b_onk_inrappklk)),
           b_onk_inrappsjh2 = ifelse(is.na(b_onk_inrappsjh),
                                     as.character(b_anmsjh),
                                     as.character(b_onk_inrappsjh))) %>%
    slice(which(compare_unit("b_onk_inrappsjh2", "b_onk_inrappklk2"))) %>%

    ant("b_beh_rappdat_kombinerat")



################# Antal inrapporterade uppföljningsblanketter #################

ant_blk3 <-  df[compare_unit("u_uppfsjh", "b_uppfkli"), ] %>%
    ant("u_rappdatuppf")



##########################################################################################
#                                                                                        #
#                             Text som beskriver gjort uravl                             #
#                                                                                        #
##########################################################################################

min_enhet <- if (grepl("sjukhus", param$belongs_to_unit, TRUE)) {
    unlist(strsplit(unique(df$userposname), " - "))[2]
} else {
    paste(unlist(strsplit(unique(df$userposname), " - "))[2:3], collapse = " - ")
}

unit_based_on <- paste(unlist(strsplit(param$belongs_to_unit, " ", fixed = TRUE))[-(1:2)], collapse = " ")

urvals_label <- paste0("\"", tolower(gsub("[[:digit:]][[:space:]]", "",
                                          paste(param$diagnos, collapse = ", "))),
                       "<br> där ", min_enhet, " ", unit_based_on, "\"")


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
    writeLines(infuse("kpl_klinik_region_riket-template.js"), "kpl_klinik_region_riket.js")
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
                 "\n\t\t var diag =",  ant_blk1,
                 "\n\t\t var beh=",    ant_blk2,
                 "\n\t\t var prim =",  ant_blk3,
                 "\n\t\t var urval =", urvals_label,
                 "\n\t\t var year =",  current_year()$years_label,
                 "\n\t\t var hist_years_label =",  current_year()$hist_years_label
          )

## Här måste vi vara försiktiga med encodings. Olika delar får olika encodings!!!
cat(del1, file = file('output.html', encoding = ''))
cat("\n\t<script>\n", file = 'output.html', append = TRUE)
cat(mitten, file = file('output.html', 'at', encoding = 'UTF-8'), append = TRUE)
cat("\n\t</Script>\n", file = file('output.html', 'at', encoding = 'UTF-8'), append = TRUE)
cat(del2, file = file('output.html', 'at', encoding = ''), append = TRUE)



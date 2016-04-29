
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
    df <- read.csv("testdata/incadata.csv")
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

## Gör alla faktorvariabler till character
faktorer <- sapply(df, is.factor)
df[faktorer] <- lapply(df[faktorer], as.character)


#########################################################################################
#                                                                                       #
#                                    Hjälpfunktioner                                    #
#                                                                                       #
#########################################################################################

# Datum och årtal
as.date         <- function(x)    as.Date(x, format = "%Y-%m-%d")
as.year         <- function(x)    as.numeric(format(as.date(x), format = "%Y"))
as.month        <- function(x)    as.numeric(format(as.date(x), format = "%m"))
year_interval   <- function(xs) {
    if (all(is.na(xs))) {
      "'(historik saknas)'"
    } else if (length(na.omit(unique(xs))) == 1) {
      paste0("'", na.omit(unique(xs)), "'")
    } else paste0("'", paste(range(xs, na.rm = TRUE), collapse = " - "), "'")
}
in_current_year <- function(x)    !is.na(x) & as.year(x) %in% current_year()$years_num


# Skapa ledtid
# Negativa tider samt tider längre än ett år exkluderas
lt <- function(from, to){
    lt <- as.date(to) - as.date(from)
    lt[!(lt %in% 0:365)] <- NA
    lt
}


# är första elementet det minsta av två
first_first <- function(x){
    if (all(is.na(x))) NA else if (is.na(x[1])) FALSE else x[1] == min(x, na.rm = TRUE)
}

# Skriv ut felmeddeladen om årtal omkastade
stop_if_wrong_date_order <- function(from_year, to_year) {
    if (from_year > to_year) {
        write(paste('<strong> <font color="red"> Du har valt ett ogiltigt',
                    'årsintervall! Förmodligen bör du byta plats på de båda',
                    'årtalen! </forn></strong>'),
              file = file( "output.html", "w", encoding = "UTF-8"))
        q()
    }
}


# Funktion för att hitta relevant årtal. Nytt år tas först fr o m 1 juli
# eller enligt parameterval om sådant finns
#' @param first_historic_year anger första möjliga året att ta historiska data för
current_year <- function(date = Sys.Date(), first_historic_year = 2008) {

    # Använd värden från parameterval om sådana finns,
    # använd annars innevarande år fr o m juli, annars föregående år
    if (exists("param") && !is.null(param$year_from) && param$year_from != "1900") {
        from_year <- as.numeric(param$year_from)
        to_year   <- as.numeric(param$year_to)
        stop_if_wrong_date_order(from_year, to_year)

    # Om inga parameterval gjorts väljs innevarande år fr o m 1 juli,
    # dessförinnan väljs föregående år.
    } else
        from_year <- to_year <- if (as.month(date) <= 4) as.year(date) - 1 else as.year(date)

    # Historiska år
    hist_years <-
        if (from_year == to_year)
            seq.int(to_year - 4, to_year - 1, 1)
        else seq.int(to_year - 3, to_year, 1)

    # Historiska år "trunkeras" om de inkluderar för tidiga år.
    # Vi vill dock behålla längden på vektorn så tar inte bort dem helt
    if (!(is.null(first_historic_year)))
        hist_years[hist_years < first_historic_year] <- NA


    # Ett antal olika årsangivelser i retur
    list(from_year        = from_year,
         to_year          = to_year,
         hist_years       = hist_years,
         years_num        = seq.int(from_year, to_year, 1),
         years_label      = if (from_year == to_year) from_year
                            else year_interval(seq.int(from_year, to_year)),
         hist_years_label = year_interval(hist_years)
    )
}



##########################################################################################
#                                                                                        #
#                             Klinik- och regiontillhörigeht                             #
#                                                                                        #
##########################################################################################
# Jämför alltid sjukhus
# Jämför dessutom klinik om klk != NULL och klinik relevant enl param
compare_unit <- function(sjh, klk = NULL) {
    cmp <- function(env_code, unit){
        if (is.null(unit)) TRUE
        else (!is.na(df[[unit]]) & as.numeric(df[[env_code]]) == suppressWarnings(as.numeric(df[[unit]])))
    }
    cmp("userparentunitcode", sjh) & cmp("userunitcode", klk)
}

# enhetstillhörighet styrd av parameterval
df$klinikbehorighet <-
        switch(param$belongs_to_unit,
          "min klinik registrerat anmälningsblanketten" =
              compare_unit("a_anmsjh",       "a_anmkli"),
          "mitt sjukhus fattat behandlingsbeslut" =
              compare_unit("a_beslsjh"),
          "min klinik opererat" =
              compare_unit("b_op1sjh",       "b_op1kli") |
              compare_unit("b_op2sjh",       "b_op2kli") |
              compare_unit("b_ophogersjh",   "b_ophogerkli") |
              compare_unit("b_opvanstersjh", "b_opvansterkli"),
          "min klinik utfört onkologisk behandling" =
              compare_unit("b_brachysjh",    "b_brachykli") |
              compare_unit("b_stralsjh",     "b_stralkli") |
              compare_unit("b_medsjh",       "b_medkli"),
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

pmin2 <- function(...) pmin(..., na.rm = TRUE)

df <-
    dplyr::mutate(df,

        ## Hjälpvariabler
        # Datum för första icke kirurgiska behandling
        b_behstart             = pmin2(b_brachystart, b_stralstart, b_medstart),
        # Datum för första kirurgiska behandling
        opdat                  = pmin2(b_op1dat, b_op2dat),
        # Datum för förata behandling (oavsett kir eller icke kir)
        behandlingsstart       = pmin2(b_behstart, opdat),
        # ledtid för indikator nr 3, 4, 5
        lt3                    = lt(a_besldat, behandlingsstart),
        # Multidiciplinär konferens (läpp exkuderas)
        a_multkonf_beskrivning = ifelse(a_multkonf_beskrivning != "" &
                                        a_icd10_gruppnamn  !=  "1 Läpp",
                                        a_multkonf_beskrivning, NA),

        ## Årtalsvariabler
        ar_remiss              = as.year(a_remdat),
        ar_behandlingsstart    = as.year(behandlingsstart),
        ar_beslut              = as.year(a_besldat),
        ar_besok               = as.year(a_besok),
        ar_cytdat              = as.year(a_cytdat),
        ar_cytpad              = as.year(a_cytpad)
    )


df <-
    dplyr::mutate(df,

        ## Indikatorvariabler
        ind1  = lt(a_remdat, behandlingsstart)                    <= 40,
        ind2  = lt(a_besok,  behandlingsstart)                    <= 35,
        ind3  = lt3                                               <= 15,
        ind4  = ifelse(apply(data.frame(df$b_behstart, df$opdat),
                    1, first_first), lt3, NA)                     <= 20,
        ind5  = ifelse(apply(data.frame(df$opdat, df$b_behstart),
                    1, first_first), lt3, NA)                     <= 12,
        ind6  = lt(a_remdat, a_besldat)                           <= 25,
        ind7  = lt(a_remdat, a_besok)                             <= 5,
        ind8  = lt(a_besok,  a_cytdat)                            <= 3,
        ind9  = lt(a_cytdat, a_cytpad)                            <= 3,
        ind10 = a_multkonf_beskrivning                            == "Ja"
    )



#########################################################################################
#                                                                                       #
#                       Definition av variabler till HTML-skript                        #
#                                                                                       #
#########################################################################################

indikator <- function(ind, ar, name, l1 = 50, l2 = 80, description = ""){

    # Indikator. kan fallet klassas som "i år"
    iar <- ar %in% current_year()$years_num

    ## Plocka fram historiska klinikdata
    h <- data.frame(ind = ind[df$klinikbehorighet], ar = ar[df$klinikbehorighet])
    h <- h %>%
        group_by(ar) %>%
        summarise(ind = mean(ind, na.rm = TRUE) * 100) %>%
        filter(ar %in% current_year()$hist_years) %>%
        .$ind
    historiska_ar <- h


    ## Hjälpfunktioner för att beräkna täljare och nämnare baserat på nivå
    num <- function(behorighet = TRUE) sum(       ind[behorighet & iar], na.rm = TRUE)
    den <- function(behorighet = TRUE) sum(!is.na(ind[behorighet & iar]))

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
ant       <- function(x, var) nrow(x[in_current_year(x[[var]]), ])
not_blank <- function(x) !is.na(x) & as.character(x) != ""


############################# Aktuell klinik ##############################
klin      <- paste0("\"", "Total inrapporteringsaktivitet ",
                    current_year()$years_label, " för: <br>",
                    unique(df$userposname), "\"")



################ Antal inrapporterade anmälningsblanketter ################

ant_blk1  <- df[compare_unit("a_anmsjh", "a_anmkli"), ] %>%
    ant("a_rappdatanm")



################# Antal inrapporterade behandlingsblanketter ##################

# Blanketten kan avse antingen kirurgi eller onkologisk behandling
ant_blk2 <- df %>%
    filter(userparentunitcode == b_onk_inrappsjh | userparentunitcode == b_anmsjh,
           userunitcode       == b_onk_inrappklk | userunitcode       == b_anmkli,
           in_current_year(b_rappdatbeh) | in_current_year(b_onk_inrappdat)) %>%
    nrow()


################# Antal inrapporterade uppföljningsblanketter #################

ant_blk3 <-  df[compare_unit("u_uppfsjh", "b_uppfkli"), ] %>%
    ant("u_rappdatuppf")



##########################################################################################
#                                                                                        #
#                             Text som beskriver gjort uravl                             #
#                                                                                        #
##########################################################################################

split   <- function(x, delim = " ") unlist(strsplit(unique(x), delim, fixed = TRUE))
combine <- function(x, delim = " ") paste(x, collapse = delim)

index         <- if (grepl("sjukhus", param$belongs_to_unit, TRUE)) 2 else 2:3
min_enhet     <- split(df$userposname, " - ")[index] %>% combine(" - ")
unit_based_on <- split(param$belongs_to_unit)[-(1:2)] %>% combine()
urvals_label  <- paste0("\"", tolower(gsub("[[:digit:]][[:space:]]", "",
                                          paste(param$diagnos, collapse = ", "))),
                       "<br> där ", min_enhet, " ", unit_based_on, "\"")


##########################################################################################
#                                                                                        #
#                   Spara ner allt till textfil med namnn output.html                    #
#                                                                                        #
##########################################################################################


#################################### Ändra inget här ####################################

if (!is.inca()) {
    # På egen dator genererar vi del1 och del2 för att kunna lägga över i INCAs public files
    writeLines(infuse("templates/del1.html"), "files_to_copy_to_servers/del1-inca.txt")
    writeLines(infuse("templates/del2.html"), "files_to_copy_to_servers/del2-inca.txt")
    writeLines(infuse("templates/kpl_klinik_region_riket-template.js"), "files_to_copy_to_servers/kpl_klinik_region_riket.js")
} else{
    # läs in del 1 och 2 från INCA:s public files om vi befinner oss i INCA
    public_files <- "D:/R-Scripts/Väst/oc5buer/huvud-_och_halscancer/kpl/"
    del1 <- readChar(paste0(public_files, "del1-inca.txt"), 1e5)
    del2 <- readChar(paste0(public_files, "del2-inca.txt"), 1e8)

    mitten <- paste0("\t\t var ser =",                 toJSON(indikatordefenitioner),
                     "\n\t\t var klin = ",             klin,
                     "\n\t\t var diag =",              ant_blk1,
                     "\n\t\t var beh=",                ant_blk2,
                     "\n\t\t var prim =",              ant_blk3,
                     "\n\t\t var urval =",             urvals_label,
                     "\n\t\t var year =",              current_year()$years_label,
                     "\n\t\t var hist_years_label =",  current_year()$hist_years_label
    )

    ## Här måste vi vara försiktiga med encodings. Olika delar får olika encodings!!!
    cat(del1,              file = file('output.html',       encoding = ''     )               )
    cat("\n\t<script>\n",  file =      'output.html',                            append = TRUE)
    cat(mitten,            file = file('output.html', 'at', encoding = 'UTF-8'), append = TRUE)
    cat("\n\t</Script>\n", file = file('output.html', 'at', encoding = 'UTF-8'), append = TRUE)
    cat(del2,              file = file('output.html', 'at', encoding = ''     ), append = TRUE)
}


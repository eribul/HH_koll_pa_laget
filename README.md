# Koll på läget för huvud/halsregistret i INCA

Dessa skript används för att generera mall i INCA för koll på läget (KPL) i INCA. 
Några av filerna ska synkas till R-servern, några till Public files och några filer utgör arbetsfiler för lokalt utvecklingsarbete.


## Synkas med R-servern
* `del1-inca.txt` (denna fil genereras lokalt från filen `del1.html` men versionshanteras inte i sig själv)
* `del2-inca.txt` (denna fil genereras lokalt från filen `del2.html` men versionshanteras inte i sig själv)
* `HH_kpl.R`

## Synkas med Public files
Avser filer under Public files > huvud/hals > kpl
* `kpl_klinik_region_riket.js`
* `SweHNCR_logga.png`

library(rvest)
library(magrittr)
library(stringr)
library(assertthat)
library(tidyr)
library(tabulizer)
library(dplyr)

extract_header <- function(pdftext) {
  hdr <- pdftext %>% str_locate("Übersicht Spieldaten")
  beg <- hdr[1, 2] + 1
  zsr <- pdftext %>% str_locate("Zuschauer")
  end <- zsr[1, 1] - 1
  tab <- pdftext %>% str_sub(beg, zsr) %>% str_split("\n") %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% select(1)
  names(tab) <- "V"
  
  tab <- tab %>% filter(nchar(str_trim(V)) > 0)

  extr <- tab %>% filter(startsWith(V, "Spielklasse BHV")) %$% V %>% str_match("Spielklasse BHV:\\s(.+)")
  liga <- extr[1, 2]
  
  extr <- tab %>% filter(startsWith(V, "Spiel/Datum")) %$% V %>% str_match("Spiel/Datum\\s(.+)\\s,\\sam\\s(\\d\\d.\\d\\d.\\d\\d)\\sum\\s(\\d\\d:\\d\\d)h")
  match <- extr[1, 2]
  datum <- as.Date(extr[1, 3], "%d.%m.%y")
  zeit <- as.difftime(extr[1, 4], "%H:%M")
  
  extr <- tab %>% filter(startsWith(V, "Heim - Gast")) %$% V %>% str_match("Heim - Gast\\s(.+)\\s-\\s(.+)")
  heim <- extr[1, 2]
  gast <- extr[1, 3]
  
  return(list(SPIELKLASSE = liga, MATCH = match, DATUM = datum, ZEIT = zeit, HEIM = heim, GAST = gast))
}

extract_match_log <- function(pdftext) {
  splv <- pdftext %>% str_locate("Spielverlauf")
  mlog <- pdftext %>% str_sub(splv[1, 2] + 1) %>% str_split("\n") %>%  
    as.data.frame(stringsAsFactors = FALSE)
  names(mlog) <- "V"
  mlog <- mlog %>% filter(nchar(str_trim(V)) > 0 & str_trim(V) != "Zeit Spielzeit Spielstand Aktion" &
                    str_starts(V, "\\d\\d:\\d\\d:\\d\\d"))
  
  mlog <- str_split(mlog$V, "\\s", 3, simplify = TRUE) %>% as.data.frame(stringsAsFactors = FALSE)
  names(mlog) <- c("UHRZEIT", "SPIELZEIT", "AKTION")
  mlog$UHRZEIT <- NULL
  mlog$SPIELZEIT <- as.difftime(mlog$SPIELZEIT, "%M:%S", units = "secs")
  
  act <- mlog$AKTION %>% 
    str_replace("7m, KEIN Tor(.+)", "7m-kein-Tor\\1") %>% 
    str_match("(((\\d+:\\d+\\s)?(.+)\\sdurch\\s(.+))|((.+)\\sfür\\s(.+)))\\s\\((\\d+),\\s(.+)\\)") %>% 
    as.data.frame(stringsAsFactors = FALSE)

  mlog$EREIGNIS <- if_else(is.na(act$V5), act$V8, act$V5)
  mlog$SPIELSTAND <- if_else(is.na(act$V5), NA_character_, act$V4)
  mlog$SPIELER <- if_else(is.na(act$V6), act$V9, act$V6)
  mlog$NUMMER <- act$V10
  mlog$MANNSCHAFT <- act$V11
  
  hg <- mlog$SPIELSTAND %>% str_match("(\\d+):(\\d+)") 
  mlog$HEIM_TORE <- as.integer(hg[,2])
  mlog$GAST_TORE <- as.integer(hg[,3])

  mlog %<>% mutate(EREIGNIS = if_else(str_starts(AKTION, "Auszeit"), "Auszeit", EREIGNIS),
                   MANNSCHAFT = if_else(str_starts(AKTION, "Auszeit"), 
                                        str_match(AKTION, "Auszeit\\s(.+)")[,2], MANNSCHAFT))
  
  mlog$AKTION <- NULL
  
  mlog
}

get_parts <- function(link) {
  pdftext <- extract_text(link)
  header_data <- pdftext %>%  extract_header()
  mlog <- pdftext %>% extract_match_log()
  
  return(list(HEADER = header_data, BODY = mlog))
}

is_wellformed <- function(part) {
  if (any(is.na(part$HEADER)) | any(is.null(part$HEADER))) {
    return(FALSE)
  }
  if (any(is.na(part$BODY$UHRZEIT))) {
    return(FALSE)
  }
  
  return(TRUE)
}

extract_game <- function(link) {
  parts <- get_parts(link)
  
  if(!is_wellformed(parts)) {
    warning(paste(link, " is not wellformed."), .call = FALSE)
    return(NULL)
  }

  tab <- parts$BODY
  tab$SPIELKLASSE <- parts$HEADER$SPIELKLASSE
  tab$MATCH <- parts$HEADER$MATCH
  tab$DATUM <- parts$HEADER$DATUM
  tab$BEGINN <- parts$HEADER$ZEIT
  tab$HEIM <- parts$HEADER$HEIM
  tab$GAST <- parts$HEADER$GAST
  
  tab %<>% mutate(IST_TOR = EREIGNIS == "Tor" | EREIGNIS == "7m-Tor")
  
  tab %<>% fill(HEIM_TORE) %>% fill(GAST_TORE) %>% 
                mutate(IST_HEIM_TOR = if_else(IST_TOR & HEIM_TORE > lag(HEIM_TORE, n = 1, default = 0, order_by = SPIELZEIT), TRUE, FALSE),
                  IST_GAST_TOR = IST_TOR & !IST_HEIM_TOR
                )
  
  hkn <- tab %>% filter(IST_HEIM_TOR) %$% MANNSCHAFT[[1]]
  
  gkn <- tab %>% filter(IST_GAST_TOR) %$% MANNSCHAFT[[1]]
  
  tab$HEIM_KURZ <- hkn
  tab$GAST_KURZ <- gkn
  
  tab 
}

url <- "http://spo.handball4all.de/Spielbetrieb/index.php?orgGrpID=35&score=46921&nm=3&all=1"

get_match_links <- function(source) {
  read_html(source) %>% 
  html_node(css = ".gametable") %>% 
  html_nodes(xpath = "./tr/td[last()]/a/@href") %>% 
  html_text()
}

links <- url %>% get_match_links()

all_result <- lapply(links, extract_game) %>% bind_rows()

tw_quote_7m <- all_result %>% filter(EREIGNIS %in% c("7m-kein-Tor", "7m-Tor")) %>% 
  select(MATCH, HEIM_KURZ, GAST_KURZ, EREIGNIS, SPIELER, IST_TOR, IST_HEIM_TOR, 
         IST_GAST_TOR, MANNSCHAFT) %>% 
  mutate(TORWART = if_else(MANNSCHAFT == HEIM_KURZ, GAST_KURZ, HEIM_KURZ),
         TREFFER = if_else(IST_TOR, 1, 0), 
         GEHALTEN = if_else(IST_TOR, 0, 1)) %>% 
  group_by(TORWART) %>% 
  summarise(T = sum(TREFFER), G = sum(GEHALTEN), N = T + G, Q = G/N * 100) %>% 
  arrange(desc(Q))
                                                                                                                                                                                                    
s_quote_7m <- all_result %>% filter(EREIGNIS %in% c("7m-kein-Tor", "7m-Tor")) %>% 
  select(MATCH, HEIM_KURZ, GAST_KURZ, EREIGNIS, SPIELER, IST_TOR, IST_HEIM_TOR, 
         IST_GAST_TOR, MANNSCHAFT) %>% 
  mutate(SCHUETZE = MANNSCHAFT, 
         TREFFER = if_else(IST_TOR, 1, 0), 
         GEHALTEN = if_else(IST_TOR, 0, 1)) %>% 
  group_by(SCHUETZE, SPIELER) %>% 
  summarise(T = sum(TREFFER), G = sum(GEHALTEN), N = T + G, Q = T/N * 100) %>% 
  filter(N > 5) %>% 
  arrange(desc(Q))

# Verteilung der Torwürfe auf Spielerinnen pro Verein
alle_ms <- all_result %>% distinct(MANNSCHAFT, SPIELER)
alle_mm <- all_result %>% distinct(MATCH, MANNSCHAFT)

mms_tab <- alle_mm %>% inner_join(alle_ms, by = "MANNSCHAFT") %>% na.omit()

treffer_vt <- mms_tab %>% left_join(all_result, by = c("MATCH", "MANNSCHAFT", "SPIELER")) %>% 
  group_by(MANNSCHAFT, SPIELER, MATCH) %>%
  mutate(IST_TOR = if_else(is.na(IST_TOR), FALSE, IST_TOR)) %>% 
  summarise(NT = sum(IST_TOR)) %>% 
  group_by(MANNSCHAFT, SPIELER) %>%
  summarize(MNT = mean(NT)) %>% 
  mutate(RANG = row_number(-MNT)) %>% 
  filter(RANG <= 10) %>% 
  arrange(MANNSCHAFT, RANG) %>% 
  pivot_wider(id_cols = MANNSCHAFT, names_from = RANG, values_from = c(MNT, SPIELER))
  

  
  
  
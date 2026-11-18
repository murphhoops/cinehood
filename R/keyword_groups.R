# Sorting TMDB keywords by what they describe, so the city is built from
# what films are actually about.
#
#   tone        mood words ("admiring", "bold", "suspenseful"). Kept out of the
#               map: TMDB added them in one batch to some films, mostly recent
#               ones, so they group films by who tagged them. Kept in the data
#               for the quiz, where mood is exactly what we want.
#   production  how a film was made, sourced or released ("sequel", "black
#               and white", "woman director"). Left out of the map.
#   light       true but generic: relationships, places, the decade a film is
#               set in. Counted at reduced weight so they inform but don't lead.

# TMDB's mood tags were added as one block of keyword IDs.
TONE_ID_RANGE <- c(325761L, 325857L)

# Mood words from before that block.
TONE_EXTRA <- c(
  "dramatic", "suspenseful", "intense", "hilarious", "absurd", "aggressive",
  "inspirational", "romantic", "playful", "nostalgic", "whimsical", "campy",
  "shocking", "philosophical", "lighthearted", "melancholy", "tense", "anxious",
  "depressing", "cautionary", "disturbed", "sentimental", "mysterious", "witty",
  "provocative", "thoughtful", "macabre", "candid", "reflective", "grim",
  "intimate", "somber", "critical", "frantic", "dreary", "loving", "introspective",
  "blunt", "mischievous", "sinister", "desperate", "bitter", "biting", "cynical",
  "joyous", "angry", "complex", "grand", "curious", "hostile", "malicious",
  "paranoid", "serious", "unassuming", "casual", "meditative", "understated",
  "clinical", "irreverent", "incredulous", "relaxed", "factual", "depressed",
  "speculative", "hard", "hopeless", "ironic", "detached", "lyrical", "direct",
  "vindictive", "pathetic", "egotistical", "reminiscent", "suspicious", "wonder",
  "calm", "wistful", "questioning", "zealous", "sadistic", "psychotic", "evil",
  "suspense", "mystery", "horror", "comedy", "crime", "adventure", "fantasy",
  "romance", "psychological", "surreal"
)

PRODUCTION <- c(
  "woman director", "duringcreditsstinger", "aftercreditsstinger",
  "sequel", "prequel", "remake", "live action remake", "reboot", "spin off",
  "black and white", "independent film", "partially lost film",
  "live action and animation", "cgi-live action hybrid", "computer animation",
  "east asian lead", "canuxploitation", "psychotronic", "video nasty",
  "marvel cinematic universe (mcu)", "dc extended universe (dceu)",
  "based on novel or book", "based on comic", "based on manga", "based on video game",
  "based on play or musical", "based on short story", "based on tv series",
  "based on young adult novel", "based on children's book", "based on memoir or autobiography",
  "based on movie", "based on short film", "based on toy", "based on cartoon",
  "based on graphic novel", "based on magazine, newspaper or article", "adaptation"
)

# Generic family and relationship words that aren't caught by the pattern below.
LIGHT_EXTRA <- c(
  "family", "friendship", "love", "marriage", "friends", "best friend", "best friends",
  "daughter", "father", "mother", "son", "brother", "sister", "sibling", "siblings",
  "parents", "wife", "husband", "girlfriend", "boyfriend", "couple", "family relationships",
  "family conflict", "family drama", "married couple", "group of friends", "lovers", "lover",
  # places
  "new york city", "california", "texas", "florida", "japan", "france", "italy",
  "england", "germany", "mexico", "china", "spain", "canada", "australia", "ireland",
  "scotland", "india", "africa", "europe", "usa", "america", "american", "small town",
  "las vegas", "hollywood", "new jersey", "massachusetts", "hong kong", "tokyo, japan"
)

classify_keyword <- function(keyword, keyword_id) {
  k <- tolower(keyword)
  dplyr::case_when(
    keyword_id >= TONE_ID_RANGE[1] & keyword_id <= TONE_ID_RANGE[2] ~ "tone",
    k %in% TONE_EXTRA ~ "tone",
    k %in% PRODUCTION ~ "production",
    grepl("relationship", k) ~ "light",
    k %in% LIGHT_EXTRA ~ "light",
    grepl(", ", k) ~ "light",                               # "paris, france"
    grepl("^(1[0-9]|20)[0-9]0s$", k) ~ "light",             # "1970s"
    grepl("^[0-9]+(st|nd|rd|th) century$", k) ~ "light",     # "19th century"
    TRUE ~ "content"
  )
}

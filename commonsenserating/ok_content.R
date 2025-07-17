# Load required libraries
library(stringdist)
library(dplyr)
library(readxl)

# Load the data
variables <- read.csv("joined - for sharing ALL 4.10.25.csv")
tv_titles <- read.csv("Content Matching/TV_Supreet - Full Tab.csv")
movie_titles <- read.csv("Content Matching/Movies_Supreet - Full Tab.csv")
yt_channels <- read_excel("CSM YT dataset all codes n1639.xlsx")

#variables <- variables[variables$study == "oklahoma", ]

variables <- variables[variables$study != "germany" & variables$study != "sweden" & (variables$study != "Denver" | variables$wave != 1), ]


# add to spreadsheet: 

# highest goood match: 0.25


# danny go

# first incorrect: 0.04761905

# combine variables 

variables[is.na(variables$video_favorites_1), ]$video_favorites_1 <- variables[is.na(variables$video_favorites_1), ]$fav_tv
variables[is.na(variables$video_favorites_2), ]$video_favorites_2 <- variables[is.na(variables$video_favorites_2), ]$fav_tv_2
variables[is.na(variables$video_favorites_3), ]$video_favorites_3 <- variables[is.na(variables$video_favorites_3), ]$fav_tv_3

variables$video_favorites_1 <- iconv(variables$video_favorites_1, from = "latin1", to = "UTF-8", sub = "")
variables$video_favorites_2 <- iconv(variables$video_favorites_2, from = "latin1", to = "UTF-8", sub = "")
variables$video_favorites_3 <- iconv(variables$video_favorites_3, from = "latin1", to = "UTF-8", sub = "")

# Prepare favorite TV show columns
fav_tv_1 <- data.frame(video_favorites_1 = tolower(variables$video_favorites_1)) %>% mutate(row_num = row_number())
fav_tv_2 <- data.frame(video_favorites_2 = tolower(variables$video_favorites_2)) %>% mutate(row_num = row_number())
fav_tv_3 <- data.frame(video_favorites_3 = tolower(variables$video_favorites_3)) %>% mutate(row_num = row_number())
masked_pid <- data.frame(masked_pid = variables$masked_pid) %>% mutate(row_num = row_number())
masked_family_id <- data.frame(masked_family_id = variables$masked_family_id) %>% mutate(row_num = row_number())
study <- data.frame(study = variables$study) %>% mutate(row_num = row_number())
wave <- data.frame(wave = variables$wave) %>% mutate(row_num = row_number())


# Combine favorite TV shows into one dataframe
combined_fav_tv <- fav_tv_1 %>%
  left_join(fav_tv_2, by = "row_num") %>%
  left_join(fav_tv_3, by = "row_num") %>%
  left_join(masked_pid, by = "row_num") %>%
  left_join(masked_family_id, by = "row_num") %>%
  left_join(study, by = "row_num") %>%
  left_join(wave, by = "row_num") %>%
  select(video_favorites_1, video_favorites_2, video_favorites_3, row_num, masked_pid, masked_family_id, study, wave)

# Clean combined_fav_tv to replace variations of NA with actual NA
clean_fav_tv <- function(column) {
  column <- tolower(column) # Convert to lowercase for case-insensitive matching
  column <- case_when(
    grepl("\\bna\\b|\\bn/a\\b|none|don't have one|dont have one|i don't have one|i dont have one|not applicable|nil", column, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ column
  )
  return(column)
}

combined_fav_tv <- combined_fav_tv %>%
  mutate(
    fav_tv_1 = clean_fav_tv(video_favorites_1),
    fav_tv_2 = clean_fav_tv(video_favorites_2),
    fav_tv_3 = clean_fav_tv(video_favorites_3)
  )

# Filter out rows where all fav_tv_1, fav_tv_2, and fav_tv_3 are NA
combined_fav_tv <- combined_fav_tv %>%
  filter(!(is.na(fav_tv_1) & is.na(fav_tv_2) & is.na(fav_tv_3)))

# Add column to check if the favorite TV show entries are the same
combined_fav_tv <- combined_fav_tv %>%
  mutate(all_same = fav_tv_1 == fav_tv_2 | fav_tv_1 == fav_tv_3 | fav_tv_2 == fav_tv_3)


# Correcting shows 
combined_fav_tv <- combined_fav_tv %>%
  mutate(fav_tv_1 = ifelse(grepl("disney", fav_tv_1, ignore.case = TRUE) & 
                             !grepl("channel|junior", fav_tv_1, ignore.case = TRUE),
                           "disney+", fav_tv_1))

combined_fav_tv <- combined_fav_tv %>%
  mutate(fav_tv_2 = ifelse(grepl("disney", fav_tv_2, ignore.case = TRUE) & 
                             !grepl("channel|junior", fav_tv_2, ignore.case = TRUE),
                           "disney+", fav_tv_2))

combined_fav_tv <- combined_fav_tv %>%
  mutate(fav_tv_3 = ifelse(grepl("disney", fav_tv_3, ignore.case = TRUE) & 
                             !grepl("channel|junior", fav_tv_3, ignore.case = TRUE),
                           "disney+", fav_tv_3))

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "moana and coco, two movies"),]$fav_tv_2 <- "coco"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "moana and coco, two movies"),]$fav_tv_3 <- NA 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "moana and coco, two movies"),]$fav_tv_1 <- "moana"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "pup pals, lion guard, paw patrol, bluey,"),]$fav_tv_2 <- "lion guard"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "pup pals, lion guard, paw patrol, bluey,"),]$fav_tv_3 <- "paw patrol" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "pup pals, lion guard, paw patrol, bluey,"),]$fav_tv_1 <- "pup pals"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "miniforce super dino power ,lego shows, boss baby, super monsters, power rangers"),]$fav_tv_2 <- "boss baby"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "miniforce super dino power ,lego shows, boss baby, super monsters, power rangers"),]$fav_tv_3 <- "super monsters" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "miniforce super dino power ,lego shows, boss baby, super monsters, power rangers"),]$fav_tv_1 <- "miniforce super dino power"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "mickey mouse,elmo"),]$fav_tv_3 <- "elmo" 

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "daniel tiger, clifford, bluye"),]$fav_tv_2 <- "clifford"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "daniel tiger, clifford, bluye"),]$fav_tv_3 <- "bluey" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "daniel tiger, clifford, bluye"),]$fav_tv_1 <- "daniel tiger"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "dino trux, wild kratts, sesame street"),]$fav_tv_2 <- "wild kratts"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "dino trux, wild kratts, sesame street"),]$fav_tv_3 <- "sesame street" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "dino trux, wild kratts, sesame street"),]$fav_tv_1 <- "dino trux"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "ryans family review, word party, boss baby"),]$fav_tv_2 <- "word party"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "ryans family review, word party, boss baby"),]$fav_tv_3 <- "boss baby" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "ryans family review, word party, boss baby"),]$fav_tv_1 <- "ryans family review"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "cocomelon, masha & bear,"),]$fav_tv_2 <- "masha & bear"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "cocomelon, masha & bear,"),]$fav_tv_1 <- "cocomelon"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "moana, sing, boss baby"),]$fav_tv_2 <- "sing"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "moana, sing, boss baby"),]$fav_tv_3 <- "boss baby" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "moana, sing, boss baby"),]$fav_tv_1 <- "moana"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "bluey, magic school bus, mr dimeo, dr binco"),]$fav_tv_1 <- "bluey"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "veggie tales, octonauts, cocomelon,"),]$fav_tv_2 <- "octonauts"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "veggie tales, octonauts, cocomelon,"),]$fav_tv_3 <- "cocomelon" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "veggie tales, octonauts, cocomelon,"),]$fav_tv_1 <- "veggie tales"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "scooby do, the last kids on earth, garfield"),]$fav_tv_2 <- "the last kids on earth"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "scooby do, the last kids on earth, garfield"),]$fav_tv_3 <- "garfield" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "scooby do, the last kids on earth, garfield"),]$fav_tv_1 <- "what's new, scooby-doo?"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "sunny bunnies, oddbods"),]$fav_tv_3 <- "oddbods" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "sunny bunnies, oddbods"),]$fav_tv_1 <- "sunny bunnies"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "bubble guppies, blues clues"),]$fav_tv_2 <- "blues clues"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "bubble guppies, blues clues"),]$fav_tv_1 <- "bubble guppies"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "octonauts, daniel tiger, wild krats, paw patrol, pj masks"),]$fav_tv_2 <- "wild krats"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "octonauts, daniel tiger, wild krats, paw patrol, pj masks"),]$fav_tv_3 <- "paw patrol" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "octonauts, daniel tiger, wild krats, paw patrol, pj masks"),]$fav_tv_1 <- "octonauts"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "puppy pals . mickey mouse club house  , baby boss"),]$fav_tv_1 <- "mickey mouse club house"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "baby bus, mila and morpho dino ranchers, monster jam, cocomelon"),]$fav_tv_2 <- "mila and morpho dino ranchers"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "baby bus, mila and morpho dino ranchers, monster jam, cocomelon"),]$fav_tv_3 <- "monster jam" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "baby bus, mila and morpho dino ranchers, monster jam, cocomelon"),]$fav_tv_1 <- "baby bus"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "barbie, masha and the bear, morphle, kongsuni, cocomelon, oddbods, word party"),]$fav_tv_2 <- "masha and the bear"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "barbie, masha and the bear, morphle, kongsuni, cocomelon, oddbods, word party"),]$fav_tv_3 <- "morphle" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "barbie, masha and the bear, morphle, kongsuni, cocomelon, oddbods, word party"),]$fav_tv_1 <- "barbie"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "beatrix potter, the world of peter rabbit and friends"),]$fav_tv_1 <- "peter rabbit"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "curious george, fixer upper, golf"),]$fav_tv_2 <- "fixer upper"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "curious george, fixer upper, golf"),]$fav_tv_3 <- "golf" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "curious george, fixer upper, golf"),]$fav_tv_1 <- "curious george"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "daniel tiger, tumble leaf, if you give a mouse a cookie, and cedarmont kids"),]$fav_tv_2 <- "tumble leaf"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "daniel tiger, tumble leaf, if you give a mouse a cookie, and cedarmont kids"),]$fav_tv_3 <- "cedarmont kids" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "daniel tiger, tumble leaf, if you give a mouse a cookie, and cedarmont kids"),]$fav_tv_1 <- "daniel tiger"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "ridley jones, charlie's colorform city, esme & roy"),]$fav_tv_3 <- "charlie's colorform city" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "ridley jones, charlie's colorform city, esme & roy"),]$fav_tv_2 <- "esme & roy" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "ridley jones, charlie's colorform city, esme & roy"),]$fav_tv_1 <- "ridley jones" 


combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "true & the rainbow kingdom,bubble guppies"),]$fav_tv_2 <- "bubble guppies"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "true & the rainbow kingdom,bubble guppies"),]$fav_tv_1 <- "true and the rainbow kingdom"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "a show about a family on kids youtube, frozen, trolls"),]$fav_tv_2 <- "frozen"
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "a show about a family on kids youtube, frozen, trolls"),]$fav_tv_3 <- "trolls" 
combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "a show about a family on kids youtube, frozen, trolls"),]$fav_tv_1 <- "youtube"

combined_fav_tv[which(combined_fav_tv$fav_tv_1 == "paw patrol, blippi"),]$fav_tv_3 <- "blippi" 

replacements <- list(
  "^cars.*" = "cars",
  "llama" = "llama llama",
  "youtube eller netflix" = "pok├⌐mon",
  "pokeman" = "pok├⌐mon",
  "pokemon" = "pok├⌐mon",
  "barney" = "barney & friends",
  "baby bum" = "little baby bum",
  "jurassic world" = "jurassic world: camp cretaceous",
  "magic school bus" = "the magic school bus",
  "netflix and pbs kids" = "pbs kids",
  "blipp" = "blippi",
  "paw patrol" = "paw patrol",
  "fancy nancy" = "fancy nancy ",
  "thomas" = "thomas & friends",
  "babybus" = "babybus - nursery rhymes",
  "nursery rhymes" = "babybus - nursery rhymes",
  "baby bus" = "babybus - nursery rhymes",
  "ryan" = "ryan's world",
  "roma" = "diana roma show",
  "diana" = "diana roma show",
  "rachel" = "ms. rachel",
  "wiggles" = "the wiggles",
  "peppa" = "peppa pig",
  "avengers" = "the avengers",
  "baby shark" = "baby shark's big show",
  "elmo" = "sesame street",
  "sesame" = "sesame street",
  "pajanimals" = "pajanimals",
  "ben" = "ben & holly's little kingdom",
  "madeline" = "madeline",
  "mickey(?!.*clubhouse)" = "mickey mouse",
  "wild" = "wild kratts",
  "how to train your dragon" = "your dragon",
  "spiderman" = "spider-man",
  "prestonplayz" = "prestonplayz",
  "boss" = "the boss baby",
  "frozen" = "frozen",
  "miraclous" = "miraculous: ladybug & cat noir, the movie",
  "furchester hotel" = "the furchester hotel",
  "bluie" = "bluey",
  "daniel" = "daniel tiger's neighborhood",
  "blaze" = "blaze and the monster machines",
  "dora" = "dora",
  "^spirit$" = "spirit: stallion of the cimarron", 
  "stinky" = "the stinky & dirty show", 
  "cory" = "go! go! cory carson",
  "littles" = "ms. rachel",
  "spidey" = "spider-man and his amazing friends",
  "spidy" = "spider-man and his amazing friends",
  "mouse a cookie" = "if you give a mouse a cookie",
  "baby einstein" = "baby einstein: my first signs",
  "moana" = "moana",
  "clubhouse" = "mickey mouse clubhouse",
  "pinkalicious" = "pinkalicious & peterrific",
  "simple" = "super simple songs - kids songs",
  "club house" = "mickey mouse clubhouse",
  "rogers" = "mister rogers' neighborhood",
  "sponge" = "spongebob squarepants",
  "sofia" = "sofia the first",
  "sophia" = "sofia the first",
  "pals" = "puppy dog pals",
  "rescue bots" = "transformers rescue bots",
  "coc" = "cocomelon",
  "riders" = "dragons: rescue riders",
  "true" = "true and the rainbow kingdom",
  "toy story" = "toy story (1995)",
  "dr. pol" = "the incredible dr. pol",
  "tangled" = "enredados (tangled)",
  "spirit the horse show" = "spirit: stallion of the cimarron",
  "callie" = "sheriff callie's wild west",
  "sing movies" = "sing",
  "young jedi" = "star wars: young jedi adventures",
  "bluey" = "bluey",
  "zig" = "zig & sharko",
  "mighty express (w/ brother)" = "mighty express", 
  "ross" = "chill with bob ross",
  "mr. rodgers" = "mister rogers' neighborhood",
  "jurassic" = "jurassic world: camp cretaceous",
  "axel" = "axel: the biggest little hero",
  "spong" = "spongebob squarepants",
  "hogi" = "Hogie the Globehopper",
  "lion king" = "the lion king",
  "barbie dream" = "barbie dreamhouse adventures",
  "dreamhouse" = "barbie dreamhouse adventures",
  "tayo" = "tayo the little bus",
  "dino dan" = "dino dan",
  "pokã©mon" = "pok├⌐mon",
  "minnie" = "mickey mouse",
  "hey bear" = "hey bear sensory",
  "mario" = "the super mario bros. movie",
  "pet plus cat" = "peg + cat",
  "clifford" = "clifford",
  "noddy" = "noddy, toyland detective", 
  "spirited away" = "el viaje de chihiro (spirited away)",
  "kid danger" = "the adventures of kid danger",
  "pink fong" = "pinkfong wonderstar",
  "pinkfong" = "pinkfong wonderstar",
  "cat in the hat" = "dr. seuss's the cat in the hat",
  "storybot" = "storybots super songs",
  "story bot" = "storybots super songs",
  "totoro" = "mi vecino totoro",
  "ruff" = "ruff-ruff, tweet & dave",
  "cleo" = "cleo & cuquin",
  "elementary" = "hero elementary",
  "word world" = "wordworld",
  "super hero adventures" = "marvel super hero adventures",
  "siwa" = "its jojo siwa",
  "pixar" = "pixar",
  "cookie monster" = "cookie monster's bake sale",
  "marvelous ladybug" = "miraculous: tales of ladybug & cat noir",
  "mouse and a cookie" = "if you give a mouse a christmas cookie",
  "skylanders" = "skylanders academy",
  "space jam" = "space jam: a new legacy",
  "elena" = "elena of avalor",
  "masha" = "masha and the bear",
  "nemo" = "buscando a nemo",
  "clues" = "blue's clues",
  "ninjago" = "the lego ninjago movie",
  "tots" = "t.o.t.s.",
  "pups" = "paw patrol: mighty pups",
  "grinch" = "dr. seuss' the grinch",
  "riverdance" = "riverdance: the animated adventure",
  "spiriy" = "spirit: stallion of the cimarron",
  "mona" = "moana",
  "busy town" = "busytown mysteries",
  "cry babies" = "cry babies magic tears",
  "transformers academy" = "transformers: rescue bots academy",
  "spirit rangers" = "spirit rangers - los guardaesp├¡ritus del bosque",
  "pat patrouille" = "paw patrol",
  "mighty trains" = "mighty express: mighty trains race",
  "black panther" = "pantera negra (black panther)",
  "santiago" = "santiago de los mares",
  "your dragon" = "how to train your dragon",
  "tayo" = "tayo the little bus",
  "babyfinn" = "bebefinn",
  "race to the edge" = "dragons: race to the edge",
  "she-ra" = "she-ra and the princesses of power",
  "baby estein" = "baby einstein: my first signs",
  "noddy" = "noddy, toyland detective",
  "peg" = "peg + cat",
  "aquanots" = "octonauts",
  "gabby" = "gabby's dollhouse",
  "mecha builders" = "sesame street mecha builders",
  "snowy day" = "the snowy day",
  "pac-man" = "pac-man and the ghostly adventures",
  "polar express" = "the polar express",
  "dino train" = "dinosaur train",
  "rescuebots" = "transformers rescue bots",
  "bluet" = "bluey",
  "young jedis" = "star wars: young jedi adventures",
  "super hero girls" = "dc super hero girls: hero of the year",
  "bugs bunny" = "bugs bunny: big top bunny",
  "ninja turtles" = "teenage mutant ninja turtles: out of the shadows",
  "caliuo" = "caillou",
  "zootropolis" = "zootopia",
  "gabba" = "yo gabba gabba!",
  "vlad" = "vlad and niki",
  "toy transformers" = "transformers",
  "hogi" = "hogie the globehopper",
  "kate and mimi" = "kate & mim-mim",
  "seasame" = "sesame street",
  "paw pottol" = "paw patrol",
  "barbie dolphin adventure movie" = "barbie dolphin magic",
  "wishenpoof, amazon" = "wishenpoof!",
  "motown" = "la magia de motown (motown magic)",
  "hot wheels" = "hot wheels: let's race",
  "pinkalicous" = "pinkalicious & peterrific",
  "mesha and the bear" = "masha and the bear",
  "letter factory" = "leapfrog letter factory adventures: the letter machine rescue team",
  "olaf shorts" = "olaf presents",
  "kick buttowski" = "kick buttowski: suburban daredevil",
  "doozers" = "doozers",
  "jack hartmann videos" = "jack hartmann kids music channel",
  "tiyo" = "tayo the little bus",
  "spirit (horse movie)" = "spirit: stallion of the cimarron",
  "mila" = "mila and morphle - cartoons and songs",
  "little pony" = "my little pony: the movie",
  "lego superheros" = "lego batman: the movie -- dc superheroes unite",
  "xavier" = "xavier riddle and the secret museum",
  "fantasia" = "fantasia", 
  "rodney cunningham 9th grade ninja" = "randy cunningham: 9th grade ninja",
  "lucky dog" = "lucky dog",
  "room on the broom" = "room on the broom",
  "coyote peterson" = "coyote peterson's wild field trip",
  "encanto" = "encanto",
  "cat and tin hat" = "dr. seuss's the cat in the hat",
  "zaboomafoo" = "zoboomafoo",
  "blueie" = "bluey",
  "maggie and the beast" = "maggie and the ferocious beast",
  "pokã¨mon" = "pok├⌐mon",
  "lion guard" = "the lion guard",
  "micky mouse club" = "mickey mouse clubhouse",
  "chip & potato" = "chip and potato",
  "zig" = "zig & sharko",
  "blue's clues" = "blue's clues",
  "lama" = "llama llama",
  "animal mechanicals and word party" = "animal mechanicals",
  "blimpie" = "blippi",
  "spider and his amazing friends" = "spider-man and his amazing friends",
  "youtube" = "youtube movies",
  "netflix" = "netflix jr."
)

# Vector of target columns
target_cols <- c("fav_tv_1", "fav_tv_2", "fav_tv_3")

# Apply replacements
for (col in target_cols) {
  for (pattern in names(replacements)) {
    replacement <- replacements[[pattern]]
    combined_fav_tv[[col]] <- ifelse(
      str_detect(combined_fav_tv[[col]], regex(pattern, ignore_case = TRUE)),
      replacement,
      combined_fav_tv[[col]]
    )
  }
}

# Prepare title lists
csm_tv_titles <- tolower(tv_titles$Title)
csm_movie_titles <- tolower(movie_titles$Title)
yt_creators <- tolower(unique(yt_channels$Creator))
csm_titles_combined <- c(csm_tv_titles, csm_movie_titles, yt_creators)

# Clean csm_titles_combined to remove NA and empty strings
csm_titles_combined <- csm_titles_combined[!is.na(csm_titles_combined) & csm_titles_combined != ""]

get_best_match <- function(fav_tv_column, csm_titles_combined) {
  # Clean a vector: lowercase, remove "the"/"movie", trim extra spaces
  clean_string <- function(x) {
    x %>%
      str_to_lower() %>%
      str_remove_all("\\b(the|movie)\\b") %>%
      str_squish()
  }
  
  # Remove NA values from input
  non_na_indices <- which(!is.na(fav_tv_column))
  fav_tv_non_na <- fav_tv_column[non_na_indices]
  
  # Clean both for comparison
  fav_tv_clean <- clean_string(fav_tv_non_na)
  csm_clean <- clean_string(csm_titles_combined)
  
  # Compute distances on cleaned strings
  distance_matrix <- stringdistmatrix(fav_tv_clean, csm_clean, method = "jw")
  
  # Find best match index and distance
  best_match_index <- apply(distance_matrix, 1, which.min)
  best_distance <- apply(distance_matrix, 1, min)
  
  # Pull the original (uncleaned) best match title
  best_match_title <- csm_titles_combined[best_match_index]
  
  # Fill full-length outputs
  full_best_match_title <- rep(NA, length(fav_tv_column))
  full_best_match_title[non_na_indices] <- best_match_title
  
  full_best_distance <- rep(NA, length(fav_tv_column))
  full_best_distance[non_na_indices] <- best_distance
  
  return(list(best_match_title = full_best_match_title,
              best_distance = full_best_distance))
}

# Apply the matching function for each favorite TV column
match_1 <- get_best_match(combined_fav_tv$fav_tv_1, csm_titles_combined)
combined_fav_tv$Best_match_1 <- match_1$best_match_title
combined_fav_tv$Distance_1 <- match_1$best_distance

match_2 <- get_best_match(combined_fav_tv$fav_tv_2, csm_titles_combined)
combined_fav_tv$Best_match_2 <- match_2$best_match_title
combined_fav_tv$Distance_2 <- match_2$best_distance

match_3 <- get_best_match(combined_fav_tv$fav_tv_3, csm_titles_combined)
combined_fav_tv$Best_match_3 <- match_3$best_match_title
combined_fav_tv$Distance_3 <- match_3$best_distance

combined_fav_tv <- combined_fav_tv %>%
  mutate(Category_1 = case_when(
    !is.na(Best_match_1) & Best_match_1 %in% csm_tv_titles ~ "TV",
    !is.na(Best_match_1) & Best_match_1 %in% csm_movie_titles ~ "Movie",
    !is.na(Best_match_1) & Best_match_1 %in% yt_creators ~ "YouTube",
    TRUE ~ NA_character_
  ),
  Category_2 = case_when(
    !is.na(Best_match_2) & Best_match_2 %in% csm_tv_titles ~ "TV",
    !is.na(Best_match_2) & Best_match_2 %in% csm_movie_titles ~ "Movie",
    !is.na(Best_match_2) & Best_match_2 %in% yt_creators ~ "YouTube",
    TRUE ~ NA_character_
  ),
  Category_3 = case_when(
    !is.na(Best_match_3) & Best_match_3 %in% csm_tv_titles ~ "TV",
    !is.na(Best_match_3) & Best_match_3 %in% csm_movie_titles ~ "Movie",
    !is.na(Best_match_3) & Best_match_3 %in% yt_creators ~ "YouTube",
    TRUE ~ NA_character_
  ))

# Function to add ratings for TV and Movies
add_ratings <- function(best_match_column, category_column, tv_titles, movie_titles) {
  tv_titles_lower <- tv_titles %>% mutate(Title = tolower(Title))
  movie_titles_lower <- movie_titles %>% mutate(Title = tolower(Title))
  
  ratings <- data.frame(
    Diverse.Representations.Rating = rep(NA, length(best_match_column)),
    Educational.Value.Rating = rep(NA, length(best_match_column)),
    Positive.Messages.Rating = rep(NA, length(best_match_column)),
    Positive.Role.Models.Rating = rep(NA, length(best_match_column)),
    Violence...Scariness.Rating = rep(NA, length(best_match_column))
  )
  
  for (i in seq_along(best_match_column)) {
    if (!is.na(best_match_column[i]) && !is.na(category_column[i]) && category_column[i] %in% c("TV", "Movie")) {
      if (category_column[i] == "TV") {
        match_row <- tv_titles_lower[tv_titles_lower$Title == best_match_column[i], ]
        if (nrow(match_row) > 0) {
          ratings[i, ] <- match_row[1, c("Diverse.Representations.Rating",
                                         "Educational.Value.Rating",
                                         "Positive.Messages.Rating",
                                         "Positive.Role.Models.Rating",
                                         "Violence...Scariness.Rating")]
        }
      } else if (category_column[i] == "Movie") {
        match_row <- movie_titles_lower[movie_titles_lower$Title == best_match_column[i], ]
        if (nrow(match_row) > 0) {
          ratings[i, ] <- match_row[1, c("Diverse.Representations.Rating",
                                         "Educational.Value.Rating",
                                         "Positive.Messages.Rating",
                                         "Positive.Role.Models.Rating",
                                         "Violence...Scariness.Rating")]
        }
      }
    }
  }
  return(ratings)
}

# Function to add ratings for YouTube
add_youtube_ratings <- function(best_match_column, category_column, yt_channels) {
  yt_channels_lower <- yt_channels %>% mutate(Creator = tolower(Creator))
  
  ratings <- data.frame(
    Scariness = rep(NA, length(best_match_column)),
    Educational_Value = rep(NA, length(best_match_column)),
    Positive_Role_Models_Representat = rep(NA, length(best_match_column))
  )
  
  for (i in seq_along(best_match_column)) {
    if (!is.na(best_match_column[i]) && !is.na(category_column[i]) && category_column[i] == "YouTube") {
      match_row <- yt_channels_lower[yt_channels_lower$Creator == best_match_column[i], ]
      if (nrow(match_row) > 0) {
        ratings[i, ] <- match_row[1, c("Scariness", "Educational_Value", "Positive_Role_Models_Representat")]
      }
    }
  }
  return(ratings)
}

# Add ratings for TV and Movies
ratings_1 <- add_ratings(combined_fav_tv$Best_match_1, combined_fav_tv$Category_1, tv_titles, movie_titles)
ratings_2 <- add_ratings(combined_fav_tv$Best_match_2, combined_fav_tv$Category_2, tv_titles, movie_titles)
ratings_3 <- add_ratings(combined_fav_tv$Best_match_3, combined_fav_tv$Category_3, tv_titles, movie_titles)

# Add ratings for YouTube
youtube_ratings_1 <- add_youtube_ratings(combined_fav_tv$Best_match_1, combined_fav_tv$Category_1, yt_channels)
youtube_ratings_2 <- add_youtube_ratings(combined_fav_tv$Best_match_2, combined_fav_tv$Category_2, yt_channels)
youtube_ratings_3 <- add_youtube_ratings(combined_fav_tv$Best_match_3, combined_fav_tv$Category_3, yt_channels)

# Combine all ratings into the main dataframe
combined_fav_tv <- cbind(
  combined_fav_tv,
  ratings_1 %>% rename_with(~ paste0(.x, "_1")),
  ratings_2 %>% rename_with(~ paste0(.x, "_2")),
  ratings_3 %>% rename_with(~ paste0(.x, "_3")),
  youtube_ratings_1 %>% rename_with(~ paste0(.x, "_YouTube_1")),
  youtube_ratings_2 %>% rename_with(~ paste0(.x, "_YouTube_2")),
  youtube_ratings_3 %>% rename_with(~ paste0(.x, "_YouTube_3"))
)

b_1 <- combined_fav_tv[, c("fav_tv_1", "Best_match_1", "Distance_1", "study", "wave")]
b_2 <- combined_fav_tv[, c("fav_tv_2", "Best_match_2", "Distance_2", "study", "wave")]
b_3 <- combined_fav_tv[, c("fav_tv_3", "Best_match_3", "Distance_3", "study", "wave")]

write.csv(b_1, "b_1.csv", row.names = FALSE)
write.csv(b_2, "b_2.csv", row.names = FALSE)
write.csv(b_3, "b_3.csv", row.names = FALSE)


# Save the final dataframe to CSV
write.csv(combined_fav_tv, "all_data_fav_ratings.csv", row.names = FALSE)



# to do: AVERAGE SCORES 





# scripts 
# 
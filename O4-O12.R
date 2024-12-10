# rens beskrivelse
# aæø ændres, da bogstaverne ellers helt fjernes i næste skridt, hvor alle tegn fjernes
bil_df$description <- gsub("æ", "ae", bil_df$description)
bil_df$description <- gsub("ø", "oe", bil_df$description)
bil_df$description <- gsub("å", "aa", bil_df$description)

bil_df$description <- gsub("Æ", "AE", bil_df$description)
bil_df$description <- gsub("Ø", "OE", bil_df$description)
bil_df$description <- gsub("Å", "AA", bil_df$description)

# Matcher alle tegn, der ikke er inden for ASCII-området (hexadecimal værdier \x01 til \x7F).
# ASCII-tegn omfatter bogstaver (A-Z, a-z), cifre (0-9), symboler og kontroltegn.
bil_df$description <- gsub("[^\x01-\x7F]", "", bil_df$description)
#print(hyundai$description)

# fjern newline (\n)
bil_df$description <- gsub("\n", ". ", bil_df$description)

# fjern "_"
bil_df$description <- gsub("_", "", bil_df$description)

# fjern \r
bil_df$description <- gsub("\r", "", bil_df$description)

# fjern unødigt mellemrum
bil_df$description <- gsub("\\s+", " ", bil_df$description)

# fjern unødigt punktummer
bil_df$description <- gsub("\\s*\\.\\s*", ".", bil_df$description)
bil_df$description <- gsub("\\.+", ". ", bil_df$description)

bil_df$description <- gsub("\\:\\.", ":", bil_df$description)

bil_df$description <- gsub("\\*", "", bil_df$description)

# ændre tilbage til æøå
bil_df$description <- gsub("ae", "æ", bil_df$description)
bil_df$description <- gsub("oe", "ø", bil_df$description)
bil_df$description <- gsub("aa", "å", bil_df$description)

bil_df$description <- gsub("AE", "Æ", bil_df$description)
bil_df$description <- gsub("OE", "Ø", bil_df$description)
bil_df$description <- gsub("AA", "Å", bil_df$description)

# Code written by Killian Murphy on 14/08/2025, trivial edits made from original

INFILE="reactions.dum"

cat("INFO: Reading file", INFILE, "\n")

# Read the raw GECKO mechanism into a character vector:
RAW_GECKO = readLines(paste0(VOC, "/", INFILE))

cat("INFO: Looking for split reactions", "\n")

# Look for lines that have a + at the end:
MATCHES = grepl("\\+\\s*$", RAW_GECKO)


while (  any(MATCHES) ) {
 
  # Get the line numbers of lines with a + at the end:
  INDICES = which(MATCHES)
  
  # Use our knowledge of where those lines are to add the following line to them,
  # then delete the following line:
  cat("INFO: Joining lines", "\n")
  
  RAW_GECKO[INDICES] = paste0(sub("\\s*\\+\\s*$", "\\ +", RAW_GECKO[INDICES]), RAW_GECKO[INDICES + 1])
  RAW_GECKO = RAW_GECKO[-(INDICES + 1)]
  
  # Look for lines that have a + at the end:
  MATCHES = grepl("\\+\\s*$", RAW_GECKO)
  
}

cat("INFO: No lines to join", "\n")





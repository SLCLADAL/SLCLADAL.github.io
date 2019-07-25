
# "Regular Expressions"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)
. 
[:digit:] Digits: 0 1 2 3 4 5 6 7 8 9
[:lower:] Lowercase characters: a–z
[:upper:] Uppercase characters: A–Z
[:alpha:] Alphabetic characters: a–z and A–Z
[:alnum:] Digits and alphabetic characters
[:punct:] Punctuation characters: . , ; etc.
[:graph:] Graphical characters: [:alnum:] and [:punct:]
[:blank:] Blank characters: Space and tab
[:space:] Space characters: Space, tab, newline, and other space characters
[:print:] Printable characters: [:alnum:], [:punct:] and [:space:]
? The preceding item is optional and will be matched at most once
* The preceding item will be matched zero or more times
+ The preceding item will be matched one or more times
{n} The preceding item is matched exactly n times
{n,} The preceding item is matched n or more times
{n,m} The preceding item is matched at least n times, but not more than m times
\w Word characters: [[:alnum:]_]
\W No word characters: [ˆ[:alnum:]_]
\s Space characters: [[:blank:]]
\S No space characters: [ˆ[:blank:]]
\d Digits: [[:digit:]]
\D No digits: [ˆ[:digit:]]
\b Word edge
\B No word edge
\< Word beginning
\> Word end

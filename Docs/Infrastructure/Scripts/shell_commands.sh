pwd #get full path
grep -A 1 -B 1 searchitem file.txt #Gets one line before and after searchitem
| #Take output of previous and pipe into next thing
cat sample.txt | cut -d' ' -f2 #open sample.txt, pipe it into cut delimited by space, get second column
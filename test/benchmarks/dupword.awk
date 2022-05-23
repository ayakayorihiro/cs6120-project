# ARGS: words.txt
# dupword -- find duplicate words in text
# Arnold Robbins, arnold@gnu.ai.mit.edu, Public Domain
# December 1991

{
    $0 = tolower($0)
    gsub(/[^A-Za-z0-9 \t]/, "");
    if ($1 == prev)
        print(NR ": duplicate " $1)
    for (i = 2; i <= NF; i++)
        if ($i == $(i-1))
            print(NR ": duplicate " $i)
    prev = $NF
}

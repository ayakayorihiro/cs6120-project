# ARGS: dupword_test.txt
# dupword -- find duplicate words in text
# Arnold Robbins, arnold@gnu.ai.mit.edu, Public Domain
# December 1991

{
    $0 = tolower($0)
    gsub(/[^A-Za-z0-9 \t]/, "");
    if ($1 == prev)
        print(FILENAME ":" FNR ": duplicate " $1 "\n")
    for (i = 2; i <= NF; i++)
        if ($i == $(i-1))
            printf(FILENAME ":" FNR ": duplicate " $i "\n")
    prev = $NF
}

# ARGS: numbers.tsv
BEGIN { getline; }
{ N[$1]++; SUM[$1]+=$2; }
END {
    OFS="\t"
    for (k in N) {
        print k, ((SUM[k])/N[k]);
    }
}

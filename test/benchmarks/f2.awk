# ARGS: tree_grm_est_head.csv

# from https://github.com/ezrosent/frawk/blob/master/info/performance.md
# This is presented as the less hardcore, non-parallel version.

function min(x,y) { return x<y?x:y; }
function max(x,y) { return x<y?y:x; }
function step_sum(x) { SUM += x; }
function step_stddev(x, k,  xa2) { xa2 = (x - A) * (x - A); A = A + (x-A)/k; Q=Q+((k-1)/k)*xa2; }
NR==1  { h2 = $5; h1 = $6; }
NR > 1 {
    # f2 is numeric, f1 is a string
    f2=$5+0; f2Len = length($5);
    f1=$6; f1Len = length($6);
    # sanity check
    print "f1: " f1
    print "f2: " f2 "\n"
    if (NR==2) {
        min1=max1=f1;
        min2=max2=f2;
        min1L=max1L=f1Len;
        min2L=max2L=f2Len;
    } else {
        min1 = min(min1, f1)
        min2 = min(min2, f2)
        min1L = min(min1L, f1Len)
        min2L = min(min2L, f2Len)
        max1 = max(max1, f1)
        max2 = max(max2, f2)
        max1L = max(max1L, f1Len)
        max2L = max(max2L, f2Len)
    }
    step_sum(f2);
    step_stddev(f2, NR-1);
}
END {
    N=NR-1 # account for header
    print "field",  "\t" "sum", "\t" "min", "\t" "max", "\t" "minlen",  "\t" "maxlen",  "\t" "mean",  "\t" "stddev"
    print h2,       "\t" SUM,   "\t" min2,  "\t" max2,  "\t" min2L,     "\t" max2L,     "\t" (SUM/N), "\t" sqrt(Q/(N-1))
    print h1,       "\t" "NA",  "\t" min1,  "\t" max1,  "\t" min1L,     "\t" max1L,     "\t" "NA",    "\t" "NA"
}
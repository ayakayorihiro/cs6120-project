# ARGS: Catch-22.txt

# split.awk -- do split in awk
# Arnold Robbins, arnold@gnu.ai.mit.edu, Public Domain
# May 1993
#
# Modified May 20, 2022

function chr(c)
{
    # force c to be numeric by adding 0
    return sprintf("%c", c + 0)
}

BEGIN{
    outfile = "part_"    # default
    count = 5

    i = 1
    if (ARGV[i] ~ /^-[0-9]+$/) {
        count = -ARGV[i]
        ARGV[i] = ""
        i++
    }
    # test argv in case reading from stdin instead of file
    if (i in ARGV)
        i++    # skip data file name
    if (i in ARGV) {
        outfile = ARGV[i]
        ARGV[i] = ""
    }

    part = 0
    part_char = chr(part + 97)
    out = (outfile part_char ".txt")
}
{
    if (++tcount > count) {
        close(out)
        if (part == 25) {
            printf("split: %s is too large to split\n",
                       FILENAME) > "/dev/stderr"
            exit 1
        }
        else
            part++
        part_char = chr(part + 97)
        out = (outfile part_char ".txt")
        tcount = 1
    }
    print > out
}

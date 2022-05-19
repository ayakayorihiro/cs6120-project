# BRAWN_NOTES: removed printf

# awksed.awk -- do s/foo/bar/g using just print
#    Thanks to Michael Brennan for the idea

# Arnold Robbins, arnold@gnu.ai.mit.edu, Public Domain
# August 1995

function usage()
{
    print "usage: awksed pat repl [files...]" > "/dev/stderr"
    exit 1
}

BEGIN {
    # validate arguments
    if (ARGC < 3)
        usage()

    RS = ARGV[1]
    ORS = ARGV[2]

    # don't use arguments as files
    ARGV[1] = ARGV[2] = ""
}

# look ma, no hands!
{
    if (RT == "")
        print $0
    else
        print
}

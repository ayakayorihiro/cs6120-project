# Print list of word frequencies
# Arnold Robbins, arnold@gnu.ai.mit.edu, Public Domain
# June 1992

{
    $0 = tolower($0)    # remove case distinctions
    gsub(/[^a-z0-9_ \t]/, "", $0)  # remove punctuation
    for (i = 1; i <= NF; i++)
        freq[$i]++
}

END {
    for (word in freq)
        print word "\t" freq[word]
}

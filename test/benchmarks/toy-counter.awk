# ARGS: toy-counter-input.txt
# Expects the file to have a number on the first line.
# Counts from 0 to that number, inclusive.

function count(limit)
{
#initialize a counter
x=0

do {
    print x;
    x+=1;
}
while(x<=limit)
};

BEGIN {
    getline limit
    count(limit)
}
# ARGS: Catch-22.txt
# I give this argument to make Turnt happy and then thrown it away. 
# TODO explore optional arguments or similar in Turnt

BEGIN {
#initialize a counter
x=0

do {
    print x;
    x+=1;
}
while(x<=10)
};

function max(x,y) { return x<y?y:x; }
"GS" == $8 { accum += (0.5*$1+0.5*max($4+0,$5+0))/1000.0 };
END { print accum; };

require 'rabbit/element'

module Rabbit
  module Parser
    module Ext
      module Entity
        TABLE = {
          # angle with down zig-zag arrow
          "angzarr" => "&#x0237C;",
          # circle, mid below
          "cirmid" => "&#x02AEF;",
          # left, curved, down arrow
          "cudarrl" => "&#x02938;",
          # right, curved, down arrow
          "cudarrr" => "&#x02935;",
          # /curvearrowleft A: left curved arrow
          "cularr" => "&#x021B6;",
          # curved left arrow with plus
          "cularrp" => "&#x0293D;",
          # /curvearrowright A: rt curved arrow
          "curarr" => "&#x021B7;",
          # curved right arrow with minus
          "curarrm" => "&#x0293C;",
          # down two-headed arrow
          "Darr" => "&#x021A1;",
          # /Downarrow A: down dbl arrow
          "dArr" => "&#x021D3;",
          # /downdownarrows A: two down arrows
          "ddarr" => "&#x021CA;",
          # right arrow with dotted stem
          "DDotrahd" => "&#x02911;",
          # down fish tail
          "dfisht" => "&#x0297F;",
          # down harpoon-left, down harpoon-right
          "dHar" => "&#x02965;",
          # /downharpoonleft A: dn harpoon-left
          "dharl" => "&#x021C3;",
          # /downharpoonright A: down harpoon-rt
          "dharr" => "&#x021C2;",
          # down arrow, up arrow
          "duarr" => "&#x021F5;",
          # down harp, up harp
          "duhar" => "&#x0296F;",
          # right long zig-zag arrow
          "dzigrarr" => "&#x027FF;",
          # equal, right arrow below
          "erarr" => "&#x02971;",
          # /Leftrightarrow A: l&r dbl arrow
          "hArr" => "&#x021D4;",
          # /leftrightarrow A: l&r arrow
          "harr" => "&#x02194;",
          # left and right arrow with a circle
          "harrcir" => "&#x02948;",
          # /leftrightsquigarrow A: l&r arr-wavy
          "harrw" => "&#x021AD;",
          # horizontal open arrow
          "hoarr" => "&#x021FF;",
          # image of
          "imof" => "&#x022B7;",
          # /Lleftarrow A: left triple arrow
          "lAarr" => "&#x021DA;",
          # /twoheadleftarrow A:
          "Larr" => "&#x0219E;",
          # left arrow-bar, filled square
          "larrbfs" => "&#x0291F;",
          # left arrow, filled square
          "larrfs" => "&#x0291D;",
          # /hookleftarrow A: left arrow-hooked
          "larrhk" => "&#x021A9;",
          # /looparrowleft A: left arrow-looped
          "larrlp" => "&#x021AB;",
          # left arrow, plus
          "larrpl" => "&#x02939;",
          # left arrow, similar
          "larrsim" => "&#x02973;",
          # /leftarrowtail A: left arrow-tailed
          "larrtl" => "&#x021A2;",
          # left double arrow-tail
          "lAtail" => "&#x0291B;",
          # left arrow-tail
          "latail" => "&#x02919;",
          # left doubly broken arrow
          "lBarr" => "&#x0290E;",
          # left broken arrow
          "lbarr" => "&#x0290C;",
          # left down curved arrow
          "ldca" => "&#x02936;",
          # left harpoon-down over right harpoon-down
          "ldrdhar" => "&#x02967;",
          # left-down-right-up harpoon
          "ldrushar" => "&#x0294B;",
          # left down angled arrow
          "ldsh" => "&#x021B2;",
          # left fish tail
          "lfisht" => "&#x0297C;",
          # left harpoon-up over left harpoon-down
          "lHar" => "&#x02962;",
          # /leftharpoondown A: l harpoon-down
          "lhard" => "&#x021BD;",
          # /leftharpoonup A: left harpoon-up
          "lharu" => "&#x021BC;",
          # left harpoon-up over long dash
          "lharul" => "&#x0296A;",
          # /leftleftarrows A: two left arrows
          "llarr" => "&#x021C7;",
          # left harpoon-down below long dash
          "llhard" => "&#x0296B;",
          # left open arrow
          "loarr" => "&#x021FD;",
          # /leftrightarrows A: l arr over r arr
          "lrarr" => "&#x021C6;",
          # /leftrightharpoons A: l harp over r
          "lrhar" => "&#x021CB;",
          # right harpoon-down below long dash
          "lrhard" => "&#x0296D;",
          # /Lsh A:
          "lsh" => "&#x021B0;",
          # left-up-right-down harpoon
          "lurdshar" => "&#x0294A;",
          # left harpoon-up over right harpoon-up
          "luruhar" => "&#x02966;",
          # twoheaded mapsto
          "Map" => "&#x02905;",
          # /mapsto A:
          "map" => "&#x021A6;",
          # mid, circle below
          "midcir" => "&#x02AF0;",
          # /multimap A:
          "mumap" => "&#x022B8;",
          # NE arrow-hooked
          "nearhk" => "&#x02924;",
          # NE pointing dbl arrow
          "neArr" => "&#x021D7;",
          # /nearrow A: NE pointing arrow
          "nearr" => "&#x02197;",
          # /toea A: NE & SE arrows
          "nesear" => "&#x02928;",
          # /nLeftrightarrow A: not l&r dbl arr
          "nhArr" => "&#x021CE;",
          # /nleftrightarrow A: not l&r arrow
          "nharr" => "&#x021AE;",
          # /nLeftarrow A: not implied by
          "nlArr" => "&#x021CD;",
          # /nleftarrow A: not left arrow
          "nlarr" => "&#x0219A;",
          # /nRightarrow A: not implies
          "nrArr" => "&#x021CF;",
          # /nrightarrow A: not right arrow
          "nrarr" => "&#x0219B;",
          # not right arrow-curved
          "nrarrc" => "&#x02933;&#x00338;",
          # not right arrow-wavy
          "nrarrw" => "&#x0219D;&#x00338;",
          # not, vert, left and right double arrow
          "nvHarr" => "&#x02904;",
          # not, vert, left double arrow
          "nvlArr" => "&#x02902;",
          # not, vert, right double arrow
          "nvrArr" => "&#x02903;",
          # NW arrow-hooked
          "nwarhk" => "&#x02923;",
          # NW pointing dbl arrow
          "nwArr" => "&#x021D6;",
          # /nwarrow A: NW pointing arrow
          "nwarr" => "&#x02196;",
          # NW & NE arrows
          "nwnear" => "&#x02927;",
          # /circlearrowleft A: l arr in circle
          "olarr" => "&#x021BA;",
          # /circlearrowright A: r arr in circle
          "orarr" => "&#x021BB;",
          # original of
          "origof" => "&#x022B6;",
          # /Rrightarrow A: right triple arrow
          "rAarr" => "&#x021DB;",
          # /twoheadrightarrow A:
          "Rarr" => "&#x021A0;",
          # approximate, right arrow above
          "rarrap" => "&#x02975;",
          # right arrow-bar, filled square
          "rarrbfs" => "&#x02920;",
          # right arrow-curved
          "rarrc" => "&#x02933;",
          # right arrow, filled square
          "rarrfs" => "&#x0291E;",
          # /hookrightarrow A: rt arrow-hooked
          "rarrhk" => "&#x021AA;",
          # /looparrowright A: rt arrow-looped
          "rarrlp" => "&#x021AC;",
          # right arrow, plus
          "rarrpl" => "&#x02945;",
          # right arrow, similar
          "rarrsim" => "&#x02974;",
          # right two-headed arrow with tail
          "Rarrtl" => "&#x02916;",
          # /rightarrowtail A: rt arrow-tailed
          "rarrtl" => "&#x021A3;",
          # /rightsquigarrow A: rt arrow-wavy
          "rarrw" => "&#x0219D;",
          # right double arrow-tail
          "rAtail" => "&#x0291C;",
          # right arrow-tail
          "ratail" => "&#x0291A;",
          # /drbkarow A: twoheaded right broken arrow
          "RBarr" => "&#x02910;",
          # /dbkarow A: right doubly broken arrow
          "rBarr" => "&#x0290F;",
          # /bkarow A: right broken arrow
          "rbarr" => "&#x0290D;",
          # right down curved arrow
          "rdca" => "&#x02937;",
          # right harpoon-down over left harpoon-down
          "rdldhar" => "&#x02969;",
          # right down angled arrow
          "rdsh" => "&#x021B3;",
          # right fish tail
          "rfisht" => "&#x0297D;",
          # right harpoon-up over right harpoon-down
          "rHar" => "&#x02964;",
          # /rightharpoondown A: rt harpoon-down
          "rhard" => "&#x021C1;",
          # /rightharpoonup A: rt harpoon-up
          "rharu" => "&#x021C0;",
          # right harpoon-up over long dash
          "rharul" => "&#x0296C;",
          # /rightleftarrows A: r arr over l arr
          "rlarr" => "&#x021C4;",
          # /rightleftharpoons A: r harp over l
          "rlhar" => "&#x021CC;",
          # right open arrow
          "roarr" => "&#x021FE;",
          # /rightrightarrows A: two rt arrows
          "rrarr" => "&#x021C9;",
          # /Rsh A:
          "rsh" => "&#x021B1;",
          # right harpoon-up over left harpoon-up
          "ruluhar" => "&#x02968;",
          # /hksearow A: SE arrow-hooken
          "searhk" => "&#x02925;",
          # SE pointing dbl arrow
          "seArr" => "&#x021D8;",
          # /searrow A: SE pointing arrow
          "searr" => "&#x02198;",
          # /tosa A: SE & SW arrows
          "seswar" => "&#x02929;",
          # similar, right arrow below
          "simrarr" => "&#x02972;",
          # short left arrow
          "slarr" => "&#x02190;",
          # short right arrow
          "srarr" => "&#x02192;",
          # /hkswarow A: SW arrow-hooked
          "swarhk" => "&#x02926;",
          # SW pointing dbl arrow
          "swArr" => "&#x021D9;",
          # /swarrow A: SW pointing arrow
          "swarr" => "&#x02199;",
          # SW & NW arrows
          "swnwar" => "&#x0292A;",
          # up two-headed arrow
          "Uarr" => "&#x0219F;",
          # /Uparrow A: up dbl arrow
          "uArr" => "&#x021D1;",
          # up two-headed arrow above circle
          "Uarrocir" => "&#x02949;",
          # up arrow, down arrow
          "udarr" => "&#x021C5;",
          # up harp, down harp
          "udhar" => "&#x0296E;",
          # up fish tail
          "ufisht" => "&#x0297E;",
          # up harpoon-left, up harpoon-right
          "uHar" => "&#x02963;",
          # /upharpoonleft A: up harpoon-left
          "uharl" => "&#x021BF;",
          # /upharpoonright /restriction A: up harp-r
          "uharr" => "&#x021BE;",
          # /upuparrows A: two up arrows
          "uuarr" => "&#x021C8;",
          # /Updownarrow A: up&down dbl arrow
          "vArr" => "&#x021D5;",
          # /updownarrow A: up&down arrow
          "varr" => "&#x02195;",
          # /Longleftrightarrow A: long l&r dbl arr
          "xhArr" => "&#x027FA;",
          # /longleftrightarrow A: long l&r arr
          "xharr" => "&#x027F7;",
          # /Longleftarrow A: long l dbl arrow
          "xlArr" => "&#x027F8;",
          # /longleftarrow A: long left arrow
          "xlarr" => "&#x027F5;",
          # /longmapsto A:
          "xmap" => "&#x027FC;",
          # /Longrightarrow A: long rt dbl arr
          "xrArr" => "&#x027F9;",
          # /longrightarrow A: long right arrow
          "xrarr" => "&#x027F6;",
          # right zig-zag arrow
          "zigrarr" => "&#x021DD;",
          # most positive
          "ac" => "&#x0223E;",
          # most positive, two lines below
          "acE" => "&#x0223E;&#x00333;",
          # /amalg B: amalgamation or coproduct
          "amalg" => "&#x02A3F;",
          # bar, vee
          "barvee" => "&#x022BD;",
          # /doublebarwedge B: log and, dbl bar above
          "Barwed" => "&#x02306;",
          # /barwedge B: logical and, bar above
          "barwed" => "&#x02305;",
          # reverse solidus in square
          "bsolb" => "&#x029C5;",
          # /Cap /doublecap B: dbl intersection
          "Cap" => "&#x022D2;",
          # intersection, and
          "capand" => "&#x02A44;",
          # intersection, bar, union
          "capbrcup" => "&#x02A49;",
          # intersection, intersection, joined
          "capcap" => "&#x02A4B;",
          # intersection above union
          "capcup" => "&#x02A47;",
          # intersection, with dot
          "capdot" => "&#x02A40;",
          # intersection, serifs
          "caps" => "&#x02229;&#x0FE00;",
          # closed intersection, serifs
          "ccaps" => "&#x02A4D;",
          # closed union, serifs
          "ccups" => "&#x02A4C;",
          # closed union, serifs, smash product
          "ccupssm" => "&#x02A50;",
          # /coprod L: coproduct operator
          "coprod" => "&#x02210;",
          # /Cup /doublecup B: dbl union
          "Cup" => "&#x022D3;",
          # union, bar, intersection
          "cupbrcap" => "&#x02A48;",
          # union above intersection
          "cupcap" => "&#x02A46;",
          # union, union, joined
          "cupcup" => "&#x02A4A;",
          # union, with dot
          "cupdot" => "&#x0228D;",
          # union, or
          "cupor" => "&#x02A45;",
          # union, serifs
          "cups" => "&#x0222A;&#x0FE00;",
          # /curlyvee B: curly logical or
          "cuvee" => "&#x022CE;",
          # /curlywedge B: curly logical and
          "cuwed" => "&#x022CF;",
          # /ddagger B: double dagger relation
          "Dagger" => "&#x02021;",
          # /dagger B: dagger relation
          "dagger" => "&#x02020;",
          # /diamond B: open diamond
          "diam" => "&#x022C4;",
          # /divideontimes B: division on times
          "divonx" => "&#x022C7;",
          # equal, plus
          "eplus" => "&#x02A71;",
          # hermitian conjugate matrix
          "hercon" => "&#x022B9;",
          # /intercal B: intercal
          "intcal" => "&#x022BA;",
          # /intprod
          "iprod" => "&#x02A3C;",
          # plus sign in left half circle
          "loplus" => "&#x02A2D;",
          # multiply sign in left half circle
          "lotimes" => "&#x02A34;",
          # /leftthreetimes B:
          "lthree" => "&#x022CB;",
          # /ltimes B: times sign, left closed
          "ltimes" => "&#x022C9;",
          # /ast B: asterisk
          "midast" => "&#x0002A;",
          # /boxminus B: minus sign in box
          "minusb" => "&#x0229F;",
          # /dotminus B: minus sign, dot above
          "minusd" => "&#x02238;",
          # minus sign, dot below
          "minusdu" => "&#x02A2A;",
          # bar, intersection
          "ncap" => "&#x02A43;",
          # bar, union
          "ncup" => "&#x02A42;",
          # /circledast B: asterisk in circle
          "oast" => "&#x0229B;",
          # /circledcirc B: small circle in circle
          "ocir" => "&#x0229A;",
          # /circleddash B: hyphen in circle
          "odash" => "&#x0229D;",
          # divide in circle
          "odiv" => "&#x02A38;",
          # /odot B: middle dot in circle
          "odot" => "&#x02299;",
          # dot, solidus, dot in circle
          "odsold" => "&#x029BC;",
          # filled circle in circle
          "ofcir" => "&#x029BF;",
          # greater-than in circle
          "ogt" => "&#x029C1;",
          # circle with horizontal bar
          "ohbar" => "&#x029B5;",
          # large circle in circle
          "olcir" => "&#x029BE;",
          # less-than in circle
          "olt" => "&#x029C0;",
          # vertical bar in circle
          "omid" => "&#x029B6;",
          # /ominus B: minus sign in circle
          "ominus" => "&#x02296;",
          # parallel in circle
          "opar" => "&#x029B7;",
          # perpendicular in circle
          "operp" => "&#x029B9;",
          # /oplus B: plus sign in circle
          "oplus" => "&#x02295;",
          # /oslash B: solidus in circle
          "osol" => "&#x02298;",
          # multiply sign in double circle
          "Otimes" => "&#x02A37;",
          # /otimes B: multiply sign in circle
          "otimes" => "&#x02297;",
          # multiply sign in circle, circumflex accent
          "otimesas" => "&#x02A36;",
          # circle with vertical bar
          "ovbar" => "&#x0233D;",
          # plus, circumflex accent above
          "plusacir" => "&#x02A23;",
          # /boxplus B: plus sign in box
          "plusb" => "&#x0229E;",
          # plus, small circle above
          "pluscir" => "&#x02A22;",
          # /dotplus B: plus sign, dot above
          "plusdo" => "&#x02214;",
          # plus sign, dot below
          "plusdu" => "&#x02A25;",
          # plus, equals
          "pluse" => "&#x02A72;",
          # plus, similar below
          "plussim" => "&#x02A26;",
          # plus, two; Nim-addition
          "plustwo" => "&#x02A27;",
          # /prod L: product operator
          "prod" => "&#x0220F;",
          # reverse most positive, line below
          "race" => "&#x029DA;",
          # plus sign in right half circle
          "roplus" => "&#x02A2E;",
          # multiply sign in right half circle
          "rotimes" => "&#x02A35;",
          # /rightthreetimes B:
          "rthree" => "&#x022CC;",
          # /rtimes B: times sign, right closed
          "rtimes" => "&#x022CA;",
          # /cdot B: small middle dot
          "sdot" => "&#x022C5;",
          # /dotsquare /boxdot B: small dot in box
          "sdotb" => "&#x022A1;",
          # /setminus B: reverse solidus
          "setmn" => "&#x02216;",
          # plus, similar above
          "simplus" => "&#x02A24;",
          # smash product
          "smashp" => "&#x02A33;",
          # solidus in square
          "solb" => "&#x029C4;",
          # /sqcap B: square intersection
          "sqcap" => "&#x02293;",
          # square intersection, serifs
          "sqcaps" => "&#x02293;&#x0FE00;",
          # /sqcup B: square union
          "sqcup" => "&#x02294;",
          # square union, serifs
          "sqcups" => "&#x02294;&#x0FE00;",
          # /smallsetminus B: sm reverse solidus
          "ssetmn" => "&#x02216;",
          # /star B: small star, filled
          "sstarf" => "&#x022C6;",
          # subset, with dot
          "subdot" => "&#x02ABD;",
          # /sum L: summation operator
          "sum" => "&#x02211;",
          # superset, with dot
          "supdot" => "&#x02ABE;",
          # /boxtimes B: multiply sign in box
          "timesb" => "&#x022A0;",
          # multiply sign, bar below
          "timesbar" => "&#x02A31;",
          # times, dot
          "timesd" => "&#x02A30;",
          # dot in triangle
          "tridot" => "&#x025EC;",
          # minus in triangle
          "triminus" => "&#x02A3A;",
          # plus in triangle
          "triplus" => "&#x02A39;",
          # triangle, serifs at bottom
          "trisb" => "&#x029CD;",
          # multiply in triangle
          "tritime" => "&#x02A3B;",
          # /uplus B: plus sign in union
          "uplus" => "&#x0228E;",
          # /veebar B: logical or, bar below
          "veebar" => "&#x022BB;",
          # wedge, bar below
          "wedbar" => "&#x02A5F;",
          # /wr B: wreath product
          "wreath" => "&#x02240;",
          # /bigcap L: intersection operator
          "xcap" => "&#x022C2;",
          # /bigcirc B: large circle
          "xcirc" => "&#x025EF;",
          # /bigcup L: union operator
          "xcup" => "&#x022C3;",
          # /bigtriangledown B: big dn tri, open
          "xdtri" => "&#x025BD;",
          # /bigodot L: circle dot operator
          "xodot" => "&#x02A00;",
          # /bigoplus L: circle plus operator
          "xoplus" => "&#x02A01;",
          # /bigotimes L: circle times operator
          "xotime" => "&#x02A02;",
          # /bigsqcup L: square union operator
          "xsqcup" => "&#x02A06;",
          # /biguplus L:
          "xuplus" => "&#x02A04;",
          # /bigtriangleup B: big up tri, open
          "xutri" => "&#x025B3;",
          # /bigvee L: logical and operator
          "xvee" => "&#x022C1;",
          # /bigwedge L: logical or operator
          "xwedge" => "&#x022C0;",
          # /llcorner O: lower left corner
          "dlcorn" => "&#x0231E;",
          # /lrcorner C: lower right corner
          "drcorn" => "&#x0231F;",
          # dbl left parenthesis, greater
          "gtlPar" => "&#x02995;",
          # left angle, dot
          "langd" => "&#x02991;",
          # left bracket, equal
          "lbrke" => "&#x0298B;",
          # left bracket, solidus bottom corner
          "lbrksld" => "&#x0298F;",
          # left bracket, solidus top corner
          "lbrkslu" => "&#x0298D;",
          # /lceil O: left ceiling
          "lceil" => "&#x02308;",
          # /lfloor O: left floor
          "lfloor" => "&#x0230A;",
          # /lmoustache
          "lmoust" => "&#x023B0;",
          # O: left parenthesis, lt
          "lparlt" => "&#x02993;",
          # dbl right parenthesis, less
          "ltrPar" => "&#x02996;",
          # right angle, dot
          "rangd" => "&#x02992;",
          # right bracket, equal
          "rbrke" => "&#x0298C;",
          # right bracket, solidus bottom corner
          "rbrksld" => "&#x0298E;",
          # right bracket, solidus top corner
          "rbrkslu" => "&#x02990;",
          # /rceil C: right ceiling
          "rceil" => "&#x02309;",
          # /rfloor C: right floor
          "rfloor" => "&#x0230B;",
          # /rmoustache
          "rmoust" => "&#x023B1;",
          # C: right paren, gt
          "rpargt" => "&#x02994;",
          # /ulcorner O: upper left corner
          "ulcorn" => "&#x0231C;",
          # /urcorner C: upper right corner
          "urcorn" => "&#x0231D;",
          # /gnapprox N: greater, not approximate
          "gnap" => "&#x02A8A;",
          # /gneqq N: greater, not dbl equals
          "gnE" => "&#x02269;",
          # /gneq N: greater, not equals
          "gne" => "&#x02A88;",
          # /gnsim N: greater, not similar
          "gnsim" => "&#x022E7;",
          # /gvertneqq N: gt, vert, not dbl eq
          "gvnE" => "&#x02269;&#x0FE00;",
          # /lnapprox N: less, not approximate
          "lnap" => "&#x02A89;",
          # /lneqq N: less, not double equals
          "lnE" => "&#x02268;",
          # /lneq N: less, not equals
          "lne" => "&#x02A87;",
          # /lnsim N: less, not similar
          "lnsim" => "&#x022E6;",
          # /lvertneqq N: less, vert, not dbl eq
          "lvnE" => "&#x02268;&#x0FE00;",
          # /napprox N: not approximate
          "nap" => "&#x02249;",
          # not approximately equal or equal to
          "napE" => "&#x02A70;&#x00338;",
          # not approximately identical to
          "napid" => "&#x0224B;&#x00338;",
          # /ncong N: not congruent with
          "ncong" => "&#x02247;",
          # not congruent, dot
          "ncongdot" => "&#x02A6D;&#x00338;",
          # /nequiv N: not identical with
          "nequiv" => "&#x02262;",
          # /ngeqq N: not greater, dbl equals
          "ngE" => "&#x02267;&#x00338;",
          # /ngeq N: not greater-than-or-equal
          "nge" => "&#x02271;",
          # /ngeqslant N: not gt-or-eq, slanted
          "nges" => "&#x02A7E;&#x00338;",
          # not triple greater than
          "nGg" => "&#x022D9;&#x00338;",
          # not greater, similar
          "ngsim" => "&#x02275;",
          # not, vert, much greater than
          "nGt" => "&#x0226B;&#x020D2;",
          # /ngtr N: not greater-than
          "ngt" => "&#x0226F;",
          # not much greater than, variant
          "nGtv" => "&#x0226B;&#x00338;",
          # /nleqq N: not less, dbl equals
          "nlE" => "&#x02266;&#x00338;",
          # /nleq N: not less-than-or-equal
          "nle" => "&#x02270;",
          # /nleqslant N: not less-or-eq, slant
          "nles" => "&#x02A7D;&#x00338;",
          # not triple less than
          "nLl" => "&#x022D8;&#x00338;",
          # not less, similar
          "nlsim" => "&#x02274;",
          # not, vert, much less than
          "nLt" => "&#x0226A;&#x020D2;",
          # /nless N: not less-than
          "nlt" => "&#x0226E;",
          # /ntriangleleft N: not left triangle
          "nltri" => "&#x022EA;",
          # /ntrianglelefteq N: not l tri, eq
          "nltrie" => "&#x022EC;",
          # not much less than, variant
          "nLtv" => "&#x0226A;&#x00338;",
          # /nmid
          "nmid" => "&#x02224;",
          # /nparallel N: not parallel
          "npar" => "&#x02226;",
          # /nprec N: not precedes
          "npr" => "&#x02280;",
          # not curly precedes, eq
          "nprcue" => "&#x022E0;",
          # /npreceq N: not precedes, equals
          "npre" => "&#x02AAF;&#x00338;",
          # /ntriangleright N: not rt triangle
          "nrtri" => "&#x022EB;",
          # /ntrianglerighteq N: not r tri, eq
          "nrtrie" => "&#x022ED;",
          # /nsucc N: not succeeds
          "nsc" => "&#x02281;",
          # not succeeds, curly eq
          "nsccue" => "&#x022E1;",
          # /nsucceq N: not succeeds, equals
          "nsce" => "&#x02AB0;&#x00338;",
          # /nsim N: not similar
          "nsim" => "&#x02241;",
          # /nsimeq N: not similar, equals
          "nsime" => "&#x02244;",
          # /nshortmid
          "nsmid" => "&#x02224;",
          # /nshortparallel N: not short par
          "nspar" => "&#x02226;",
          # not, square subset, equals
          "nsqsube" => "&#x022E2;",
          # not, square superset, equals
          "nsqsupe" => "&#x022E3;",
          # not subset
          "nsub" => "&#x02284;",
          # /nsubseteqq N: not subset, dbl eq
          "nsubE" => "&#x02AC5;&#x00338;",
          # /nsubseteq N: not subset, equals
          "nsube" => "&#x02288;",
          # not superset
          "nsup" => "&#x02285;",
          # /nsupseteqq N: not superset, dbl eq
          "nsupE" => "&#x02AC6;&#x00338;",
          # /nsupseteq N: not superset, equals
          "nsupe" => "&#x02289;",
          # not greater, less
          "ntgl" => "&#x02279;",
          # not less, greater
          "ntlg" => "&#x02278;",
          # not, vert, approximate
          "nvap" => "&#x0224D;&#x020D2;",
          # /nVDash N: not dbl vert, dbl dash
          "nVDash" => "&#x022AF;",
          # /nVdash N: not dbl vertical, dash
          "nVdash" => "&#x022AE;",
          # /nvDash N: not vertical, dbl dash
          "nvDash" => "&#x022AD;",
          # /nvdash N: not vertical, dash
          "nvdash" => "&#x022AC;",
          # not, vert, greater-than-or-equal
          "nvge" => "&#x02265;&#x020D2;",
          # not, vert, greater-than
          "nvgt" => "&#x0003E;&#x020D2;",
          # not, vert, less-than-or-equal
          "nvle" => "&#x02264;&#x020D2;",
          # not, vert, less-than
          "nvlt" => "&#x0003C;&#x020D2;",
          # not, vert, left triangle, equals
          "nvltrie" => "&#x022B4;&#x020D2;",
          # not, vert, right triangle, equals
          "nvrtrie" => "&#x022B5;&#x020D2;",
          # not, vert, similar
          "nvsim" => "&#x0223C;&#x020D2;",
          # parallel, similar
          "parsim" => "&#x02AF3;",
          # /precnapprox N: precedes, not approx
          "prnap" => "&#x02AB9;",
          # /precneqq N: precedes, not dbl eq
          "prnE" => "&#x02AB5;",
          # /precnsim N: precedes, not similar
          "prnsim" => "&#x022E8;",
          # reverse /nmid
          "rnmid" => "&#x02AEE;",
          # /succnapprox N: succeeds, not approx
          "scnap" => "&#x02ABA;",
          # /succneqq N: succeeds, not dbl eq
          "scnE" => "&#x02AB6;",
          # /succnsim N: succeeds, not similar
          "scnsim" => "&#x022E9;",
          # similar, not equals
          "simne" => "&#x02246;",
          # solidus, bar through
          "solbar" => "&#x0233F;",
          # /subsetneqq N: subset, not dbl eq
          "subnE" => "&#x02ACB;",
          # /subsetneq N: subset, not equals
          "subne" => "&#x0228A;",
          # /supsetneqq N: superset, not dbl eq
          "supnE" => "&#x02ACC;",
          # /supsetneq N: superset, not equals
          "supne" => "&#x0228B;",
          # /nsubset N: not subset, var
          "vnsub" => "&#x02282;&#x020D2;",
          # /nsupset N: not superset, var
          "vnsup" => "&#x02283;&#x020D2;",
          # /varsubsetneqq N: subset not dbl eq, var
          "vsubnE" => "&#x02ACB;&#x0FE00;",
          # /varsubsetneq N: subset, not eq, var
          "vsubne" => "&#x0228A;&#x0FE00;",
          # /varsupsetneqq N: super not dbl eq, var
          "vsupnE" => "&#x02ACC;&#x0FE00;",
          # /varsupsetneq N: superset, not eq, var
          "vsupne" => "&#x0228B;&#x0FE00;",
          # /angle - angle
          "ang" => "&#x02220;",
          # angle, equal
          "ange" => "&#x029A4;",
          # /measuredangle - angle-measured
          "angmsd" => "&#x02221;",
          # angle-measured, arrow, up, right
          "angmsdaa" => "&#x029A8;",
          # angle-measured, arrow, up, left
          "angmsdab" => "&#x029A9;",
          # angle-measured, arrow, down, right
          "angmsdac" => "&#x029AA;",
          # angle-measured, arrow, down, left
          "angmsdad" => "&#x029AB;",
          # angle-measured, arrow, right, up
          "angmsdae" => "&#x029AC;",
          # angle-measured, arrow, left, up
          "angmsdaf" => "&#x029AD;",
          # angle-measured, arrow, right, down
          "angmsdag" => "&#x029AE;",
          # angle-measured, arrow, left, down
          "angmsdah" => "&#x029AF;",
          # right angle-measured
          "angrtvb" => "&#x022BE;",
          # right angle-measured, dot
          "angrtvbd" => "&#x0299D;",
          # bottom square bracket
          "bbrk" => "&#x023B5;",
          # bottom above top square bracket
          "bbrktbrk" => "&#x023B6;",
          # reversed circle, slash
          "bemptyv" => "&#x029B0;",
          # /beth - beth, Hebrew
          "beth" => "&#x02136;",
          # two joined squares
          "boxbox" => "&#x029C9;",
          # /backprime - reverse prime
          "bprime" => "&#x02035;",
          # reverse semi-colon
          "bsemi" => "&#x0204F;",
          # circle, slash, small circle above
          "cemptyv" => "&#x029B2;",
          # circle, two horizontal stroked to the right
          "cirE" => "&#x029C3;",
          # circle, small circle to the right
          "cirscir" => "&#x029C2;",
          # /complement - complement sign
          "comp" => "&#x02201;",
          # /daleth - daleth, Hebrew
          "daleth" => "&#x02138;",
          # circle, slash, bar above
          "demptyv" => "&#x029B1;",
          # /ell - cursive small l
          "ell" => "&#x02113;",
          # /emptyset - zero, slash
          "empty" => "&#x02205;",
          # /varnothing - circle, slash
          "emptyv" => "&#x02205;",
          # /gimel - gimel, Hebrew
          "gimel" => "&#x02137;",
          # inverted iota
          "iiota" => "&#x02129;",
          # /Im - imaginary
          "image" => "&#x02111;",
          # /imath - small i, no dot
          "imath" => "&#x00131;",
          # /jmath - small j, no dot
          "jmath" => "&#x0006A;",
          # circle, slash, left arrow above
          "laemptyv" => "&#x029B4;",
          # lower left triangle
          "lltri" => "&#x025FA;",
          # lower right triangle
          "lrtri" => "&#x022BF;",
          # /mho - conductance
          "mho" => "&#x02127;",
          # not, vert, angle
          "nang" => "&#x02220;&#x020D2;",
          # /nexists - negated exists
          "nexist" => "&#x02204;",
          # /circledS - capital S in circle
          "oS" => "&#x024C8;",
          # /hbar - Planck's over 2pi
          "planck" => "&#x0210F;",
          # /hslash - variant Planck's over 2pi
          "plankv" => "&#x0210F;",
          # circle, slash, right arrow above
          "raemptyv" => "&#x029B3;",
          # reverse angle, equal
          "range" => "&#x029A5;",
          # /Re - real
          "real" => "&#x0211C;",
          # top square bracket
          "tbrk" => "&#x023B4;",
          # trapezium
          "trpezium" => "&#x0FFFD;",
          # upper left triangle
          "ultri" => "&#x025F8;",
          # upper right triangle
          "urtri" => "&#x025F9;",
          # vertical zig-zag line
          "vzigzag" => "&#x0299A;",
          # /wp - Weierstrass p
          "weierp" => "&#x02118;",
          # approximately equal or equal to
          "apE" => "&#x02A70;",
          # /approxeq R: approximate, equals
          "ape" => "&#x0224A;",
          # approximately identical to
          "apid" => "&#x0224B;",
          # /asymp R: asymptotically equal to
          "asymp" => "&#x02248;",
          # vert, dbl bar (over)
          "Barv" => "&#x02AE7;",
          # /backcong R: reverse congruent
          "bcong" => "&#x0224C;",
          # /backepsilon R: such that
          "bepsi" => "&#x003F6;",
          # /bowtie R:
          "bowtie" => "&#x022C8;",
          # /backsim R: reverse similar
          "bsim" => "&#x0223D;",
          # /backsimeq R: reverse similar, eq
          "bsime" => "&#x022CD;",
          # reverse solidus, subset
          "bsolhsub" => "&#x0005C;&#x02282;",
          # /Bumpeq R: bumpy equals
          "bump" => "&#x0224E;",
          # bump, equals
          "bumpE" => "&#x02AAE;",
          # /bumpeq R: bumpy equals, equals
          "bumpe" => "&#x0224F;",
          # /circeq R: circle, equals
          "cire" => "&#x02257;",
          # /Colon, two colons
          "Colon" => "&#x02237;",
          # double colon, equals
          "Colone" => "&#x02A74;",
          # /coloneq R: colon, equals
          "colone" => "&#x02254;",
          # congruent, dot
          "congdot" => "&#x02A6D;",
          # subset, closed
          "csub" => "&#x02ACF;",
          # subset, closed, equals
          "csube" => "&#x02AD1;",
          # superset, closed
          "csup" => "&#x02AD0;",
          # superset, closed, equals
          "csupe" => "&#x02AD2;",
          # /curlyeqprec R: curly eq, precedes
          "cuepr" => "&#x022DE;",
          # /curlyeqsucc R: curly eq, succeeds
          "cuesc" => "&#x022DF;",
          # dbl dash, vertical
          "Dashv" => "&#x02AE4;",
          # /dashv R: dash, vertical
          "dashv" => "&#x022A3;",
          # equal, asterisk above
          "easter" => "&#x02A6E;",
          # /eqcirc R: circle on equals sign
          "ecir" => "&#x02256;",
          # /eqcolon R: equals, colon
          "ecolon" => "&#x02255;",
          # /ddotseq R: equal with four dots
          "eDDot" => "&#x02A77;",
          # /doteqdot /Doteq R: eq, even dots
          "eDot" => "&#x02251;",
          # /fallingdotseq R: eq, falling dots
          "efDot" => "&#x02252;",
          # equal-or-greater
          "eg" => "&#x02A9A;",
          # /eqslantgtr R: equal-or-gtr, slanted
          "egs" => "&#x02A96;",
          # equal-or-greater, slanted, dot inside
          "egsdot" => "&#x02A98;",
          # equal-or-less
          "el" => "&#x02A99;",
          # /eqslantless R: eq-or-less, slanted
          "els" => "&#x02A95;",
          # equal-or-less, slanted, dot inside
          "elsdot" => "&#x02A97;",
          # /questeq R: equal with questionmark
          "equest" => "&#x0225F;",
          # equivalent, four dots above
          "equivDD" => "&#x02A78;",
          # /risingdotseq R: eq, rising dots
          "erDot" => "&#x02253;",
          # /doteq R: equals, single dot above
          "esdot" => "&#x02250;",
          # equal, similar
          "Esim" => "&#x02A73;",
          # /esim R: equals, similar
          "esim" => "&#x02242;",
          # /pitchfork R: pitchfork
          "fork" => "&#x022D4;",
          # fork, variant
          "forkv" => "&#x02AD9;",
          # /frown R: down curve
          "frown" => "&#x02322;",
          # /gtrapprox R: greater, approximate
          "gap" => "&#x02A86;",
          # /geqq R: greater, double equals
          "gE" => "&#x02267;",
          # /gtreqqless R: gt, dbl equals, less
          "gEl" => "&#x02A8C;",
          # /gtreqless R: greater, equals, less
          "gel" => "&#x022DB;",
          # /geqslant R: gt-or-equal, slanted
          "ges" => "&#x02A7E;",
          # greater than, closed by curve, equal, slanted
          "gescc" => "&#x02AA9;",
          # greater-than-or-equal, slanted, dot inside
          "gesdot" => "&#x02A80;",
          # greater-than-or-equal, slanted, dot above
          "gesdoto" => "&#x02A82;",
          # greater-than-or-equal, slanted, dot above left
          "gesdotol" => "&#x02A84;",
          # greater, equal, slanted, less
          "gesl" => "&#x022DB;&#x0FE00;",
          # greater, equal, slanted, less, equal, slanted
          "gesles" => "&#x02A94;",
          # /ggg /Gg /gggtr R: triple gtr-than
          "Gg" => "&#x022D9;",
          # /gtrless R: greater, less
          "gl" => "&#x02277;",
          # greater, less, apart
          "gla" => "&#x02AA5;",
          # greater, less, equal
          "glE" => "&#x02A92;",
          # greater, less, overlapping
          "glj" => "&#x02AA4;",
          # /gtrsim R: greater, similar
          "gsim" => "&#x02273;",
          # greater, similar, equal
          "gsime" => "&#x02A8E;",
          # greater, similar, less
          "gsiml" => "&#x02A90;",
          # /gg R: dbl greater-than sign
          "Gt" => "&#x0226B;",
          # greater than, closed by curve
          "gtcc" => "&#x02AA7;",
          # greater than, circle inside
          "gtcir" => "&#x02A7A;",
          # /gtrdot R: greater than, with dot
          "gtdot" => "&#x022D7;",
          # greater than, questionmark above
          "gtquest" => "&#x02A7C;",
          # greater than, right arrow
          "gtrarr" => "&#x02978;",
          # homothetic
          "homtht" => "&#x0223B;",
          # /lessapprox R: less, approximate
          "lap" => "&#x02A85;",
          # larger than
          "lat" => "&#x02AAB;",
          # larger than or equal
          "late" => "&#x02AAD;",
          # larger than or equal, slanted
          "lates" => "&#x02AAD;&#x0FE00;",
          # /leqq R: less, double equals
          "lE" => "&#x02266;",
          # /lesseqqgtr R: less, dbl eq, greater
          "lEg" => "&#x02A8B;",
          # /lesseqgtr R: less, eq, greater
          "leg" => "&#x022DA;",
          # /leqslant R: less-than-or-eq, slant
          "les" => "&#x02A7D;",
          # less than, closed by curve, equal, slanted
          "lescc" => "&#x02AA8;",
          # less-than-or-equal, slanted, dot inside
          "lesdot" => "&#x02A7F;",
          # less-than-or-equal, slanted, dot above
          "lesdoto" => "&#x02A81;",
          # less-than-or-equal, slanted, dot above right
          "lesdotor" => "&#x02A83;",
          # less, equal, slanted, greater
          "lesg" => "&#x022DA;&#x0FE00;",
          # less, equal, slanted, greater, equal, slanted
          "lesges" => "&#x02A93;",
          # /lessgtr R: less, greater
          "lg" => "&#x02276;",
          # less, greater, equal
          "lgE" => "&#x02A91;",
          # /Ll /lll /llless R: triple less-than
          "Ll" => "&#x022D8;",
          # /lesssim R: less, similar
          "lsim" => "&#x02272;",
          # less, similar, equal
          "lsime" => "&#x02A8D;",
          # less, similar, greater
          "lsimg" => "&#x02A8F;",
          # /ll R: double less-than sign
          "Lt" => "&#x0226A;",
          # less than, closed by curve
          "ltcc" => "&#x02AA6;",
          # less than, circle inside
          "ltcir" => "&#x02A79;",
          # /lessdot R: less than, with dot
          "ltdot" => "&#x022D6;",
          # less than, left arrow
          "ltlarr" => "&#x02976;",
          # less than, questionmark above
          "ltquest" => "&#x02A7B;",
          # /trianglelefteq R: left triangle, eq
          "ltrie" => "&#x022B4;",
          # minus, comma above
          "mcomma" => "&#x02A29;",
          # minus with four dots, geometric properties
          "mDDot" => "&#x0223A;",
          # /mid R:
          "mid" => "&#x02223;",
          # /mlcp
          "mlcp" => "&#x02ADB;",
          # /models R:
          "models" => "&#x022A7;",
          # most positive
          "mstpos" => "&#x0223E;",
          # dbl precedes
          "Pr" => "&#x02ABB;",
          # /prec R: precedes
          "pr" => "&#x0227A;",
          # /precapprox R: precedes, approximate
          "prap" => "&#x02AB7;",
          # /preccurlyeq R: precedes, curly eq
          "prcue" => "&#x0227C;",
          # precedes, dbl equals
          "prE" => "&#x02AB3;",
          # /preceq R: precedes, equals
          "pre" => "&#x02AAF;",
          # /precsim R: precedes, similar
          "prsim" => "&#x0227E;",
          # element precedes under relation
          "prurel" => "&#x022B0;",
          # /ratio
          "ratio" => "&#x02236;",
          # /trianglerighteq R: right tri, eq
          "rtrie" => "&#x022B5;",
          # right triangle above left triangle
          "rtriltri" => "&#x029CE;",
          # dbl succeeds
          "Sc" => "&#x02ABC;",
          # /succ R: succeeds
          "sc" => "&#x0227B;",
          # /succapprox R: succeeds, approximate
          "scap" => "&#x02AB8;",
          # /succcurlyeq R: succeeds, curly eq
          "sccue" => "&#x0227D;",
          # succeeds, dbl equals
          "scE" => "&#x02AB4;",
          # /succeq R: succeeds, equals
          "sce" => "&#x02AB0;",
          # /succsim R: succeeds, similar
          "scsim" => "&#x0227F;",
          # equal, dot below
          "sdote" => "&#x02A66;",
          # /smallfrown R: small down curve
          "sfrown" => "&#x02322;",
          # similar, greater
          "simg" => "&#x02A9E;",
          # similar, greater, equal
          "simgE" => "&#x02AA0;",
          # similar, less
          "siml" => "&#x02A9D;",
          # similar, less, equal
          "simlE" => "&#x02A9F;",
          # /shortmid R:
          "smid" => "&#x02223;",
          # /smile R: up curve
          "smile" => "&#x02323;",
          # smaller than
          "smt" => "&#x02AAA;",
          # smaller than or equal
          "smte" => "&#x02AAC;",
          # smaller than or equal, slanted
          "smtes" => "&#x02AAC;&#x0FE00;",
          # /shortparallel R: short parallel
          "spar" => "&#x02225;",
          # /sqsubset R: square subset
          "sqsub" => "&#x0228F;",
          # /sqsubseteq R: square subset, equals
          "sqsube" => "&#x02291;",
          # /sqsupset R: square superset
          "sqsup" => "&#x02290;",
          # /sqsupseteq R: square superset, eq
          "sqsupe" => "&#x02292;",
          # /smallsmile R: small up curve
          "ssmile" => "&#x02323;",
          # /Subset R: double subset
          "Sub" => "&#x022D0;",
          # /subseteqq R: subset, dbl equals
          "subE" => "&#x02AC5;",
          # subset, equals, dot
          "subedot" => "&#x02AC3;",
          # subset, multiply
          "submult" => "&#x02AC1;",
          # subset, plus
          "subplus" => "&#x02ABF;",
          # subset, right arrow
          "subrarr" => "&#x02979;",
          # subset, similar
          "subsim" => "&#x02AC7;",
          # subset above subset
          "subsub" => "&#x02AD5;",
          # subset above superset
          "subsup" => "&#x02AD3;",
          # /Supset R: dbl superset
          "Sup" => "&#x022D1;",
          # superset, subset, dash joining them
          "supdsub" => "&#x02AD8;",
          # /supseteqq R: superset, dbl equals
          "supE" => "&#x02AC6;",
          # superset, equals, dot
          "supedot" => "&#x02AC4;",
          # superset, solidus
          "suphsol" => "&#x02283;&#x0002F;",
          # superset, subset
          "suphsub" => "&#x02AD7;",
          # superset, left arrow
          "suplarr" => "&#x0297B;",
          # superset, multiply
          "supmult" => "&#x02AC2;",
          # superset, plus
          "supplus" => "&#x02AC0;",
          # superset, similar
          "supsim" => "&#x02AC8;",
          # superset above subset
          "supsub" => "&#x02AD4;",
          # superset above superset
          "supsup" => "&#x02AD6;",
          # /thickapprox R: thick approximate
          "thkap" => "&#x02248;",
          # /thicksim R: thick similar
          "thksim" => "&#x0223C;",
          # fork with top
          "topfork" => "&#x02ADA;",
          # /triangleq R: triangle, equals
          "trie" => "&#x0225C;",
          # /between R: between
          "twixt" => "&#x0226C;",
          # dbl vert, bar (under)
          "Vbar" => "&#x02AEB;",
          # vert, dbl bar (under)
          "vBar" => "&#x02AE8;",
          # dbl bar, vert over and under
          "vBarv" => "&#x02AE9;",
          # dbl vert, dbl dash
          "VDash" => "&#x022AB;",
          # /Vdash R: dbl vertical, dash
          "Vdash" => "&#x022A9;",
          # /vDash R: vertical, dbl dash
          "vDash" => "&#x022A8;",
          # /vdash R: vertical, dash
          "vdash" => "&#x022A2;",
          # vertical, dash (long)
          "Vdashl" => "&#x02AE6;",
          # /vartriangleleft R: l tri, open, var
          "vltri" => "&#x022B2;",
          # /varpropto R: proportional, variant
          "vprop" => "&#x0221D;",
          # /vartriangleright R: r tri, open, var
          "vrtri" => "&#x022B3;",
          # /Vvdash R: triple vertical, dash
          "Vvdash" => "&#x022AA;",
          # lower left quadrant
          "boxDL" => "&#x02557;",
          # lower left quadrant
          "boxDl" => "&#x02556;",
          # lower left quadrant
          "boxdL" => "&#x02555;",
          # lower left quadrant
          "boxdl" => "&#x02510;",
          # lower right quadrant
          "boxDR" => "&#x02554;",
          # lower right quadrant
          "boxDr" => "&#x02553;",
          # lower right quadrant
          "boxdR" => "&#x02552;",
          # lower right quadrant
          "boxdr" => "&#x0250C;",
          # horizontal line
          "boxH" => "&#x02550;",
          # horizontal line
          "boxh" => "&#x02500;",
          # lower left and right quadrants
          "boxHD" => "&#x02566;",
          # lower left and right quadrants
          "boxHd" => "&#x02564;",
          # lower left and right quadrants
          "boxhD" => "&#x02565;",
          # lower left and right quadrants
          "boxhd" => "&#x0252C;",
          # upper left and right quadrants
          "boxHU" => "&#x02569;",
          # upper left and right quadrants
          "boxHu" => "&#x02567;",
          # upper left and right quadrants
          "boxhU" => "&#x02568;",
          # upper left and right quadrants
          "boxhu" => "&#x02534;",
          # upper left quadrant
          "boxUL" => "&#x0255D;",
          # upper left quadrant
          "boxUl" => "&#x0255C;",
          # upper left quadrant
          "boxuL" => "&#x0255B;",
          # upper left quadrant
          "boxul" => "&#x02518;",
          # upper right quadrant
          "boxUR" => "&#x0255A;",
          # upper right quadrant
          "boxUr" => "&#x02559;",
          # upper right quadrant
          "boxuR" => "&#x02558;",
          # upper right quadrant
          "boxur" => "&#x02514;",
          # vertical line
          "boxV" => "&#x02551;",
          # vertical line
          "boxv" => "&#x02502;",
          # all four quadrants
          "boxVH" => "&#x0256C;",
          # all four quadrants
          "boxVh" => "&#x0256B;",
          # all four quadrants
          "boxvH" => "&#x0256A;",
          # all four quadrants
          "boxvh" => "&#x0253C;",
          # upper and lower left quadrants
          "boxVL" => "&#x02563;",
          # upper and lower left quadrants
          "boxVl" => "&#x02562;",
          # upper and lower left quadrants
          "boxvL" => "&#x02561;",
          # upper and lower left quadrants
          "boxvl" => "&#x02524;",
          # upper and lower right quadrants
          "boxVR" => "&#x02560;",
          # upper and lower right quadrants
          "boxVr" => "&#x0255F;",
          # upper and lower right quadrants
          "boxvR" => "&#x0255E;",
          # upper and lower right quadrants
          "boxvr" => "&#x0251C;",
          # =capital A, Cyrillic
          "Acy" => "&#x00410;",
          # =small a, Cyrillic
          "acy" => "&#x00430;",
          # =capital BE, Cyrillic
          "Bcy" => "&#x00411;",
          # =small be, Cyrillic
          "bcy" => "&#x00431;",
          # =capital CHE, Cyrillic
          "CHcy" => "&#x00427;",
          # =small che, Cyrillic
          "chcy" => "&#x00447;",
          # =capital DE, Cyrillic
          "Dcy" => "&#x00414;",
          # =small de, Cyrillic
          "dcy" => "&#x00434;",
          # =capital E, Cyrillic
          "Ecy" => "&#x0042D;",
          # =small e, Cyrillic
          "ecy" => "&#x0044D;",
          # =capital EF, Cyrillic
          "Fcy" => "&#x00424;",
          # =small ef, Cyrillic
          "fcy" => "&#x00444;",
          # =capital GHE, Cyrillic
          "Gcy" => "&#x00413;",
          # =small ghe, Cyrillic
          "gcy" => "&#x00433;",
          # =capital HARD sign, Cyrillic
          "HARDcy" => "&#x0042A;",
          # =small hard sign, Cyrillic
          "hardcy" => "&#x0044A;",
          # =capital I, Cyrillic
          "Icy" => "&#x00418;",
          # =small i, Cyrillic
          "icy" => "&#x00438;",
          # =capital IE, Cyrillic
          "IEcy" => "&#x00415;",
          # =small ie, Cyrillic
          "iecy" => "&#x00435;",
          # =capital IO, Russian
          "IOcy" => "&#x00401;",
          # =small io, Russian
          "iocy" => "&#x00451;",
          # =capital short I, Cyrillic
          "Jcy" => "&#x00419;",
          # =small short i, Cyrillic
          "jcy" => "&#x00439;",
          # =capital KA, Cyrillic
          "Kcy" => "&#x0041A;",
          # =small ka, Cyrillic
          "kcy" => "&#x0043A;",
          # =capital HA, Cyrillic
          "KHcy" => "&#x00425;",
          # =small ha, Cyrillic
          "khcy" => "&#x00445;",
          # =capital EL, Cyrillic
          "Lcy" => "&#x0041B;",
          # =small el, Cyrillic
          "lcy" => "&#x0043B;",
          # =capital EM, Cyrillic
          "Mcy" => "&#x0041C;",
          # =small em, Cyrillic
          "mcy" => "&#x0043C;",
          # =capital EN, Cyrillic
          "Ncy" => "&#x0041D;",
          # =small en, Cyrillic
          "ncy" => "&#x0043D;",
          # =numero sign
          "numero" => "&#x02116;",
          # =capital O, Cyrillic
          "Ocy" => "&#x0041E;",
          # =small o, Cyrillic
          "ocy" => "&#x0043E;",
          # =capital PE, Cyrillic
          "Pcy" => "&#x0041F;",
          # =small pe, Cyrillic
          "pcy" => "&#x0043F;",
          # =capital ER, Cyrillic
          "Rcy" => "&#x00420;",
          # =small er, Cyrillic
          "rcy" => "&#x00440;",
          # =capital ES, Cyrillic
          "Scy" => "&#x00421;",
          # =small es, Cyrillic
          "scy" => "&#x00441;",
          # =capital SHCHA, Cyrillic
          "SHCHcy" => "&#x00429;",
          # =small shcha, Cyrillic
          "shchcy" => "&#x00449;",
          # =capital SHA, Cyrillic
          "SHcy" => "&#x00428;",
          # =small sha, Cyrillic
          "shcy" => "&#x00448;",
          # =capital SOFT sign, Cyrillic
          "SOFTcy" => "&#x0042C;",
          # =small soft sign, Cyrillic
          "softcy" => "&#x0044C;",
          # =capital TE, Cyrillic
          "Tcy" => "&#x00422;",
          # =small te, Cyrillic
          "tcy" => "&#x00442;",
          # =capital TSE, Cyrillic
          "TScy" => "&#x00426;",
          # =small tse, Cyrillic
          "tscy" => "&#x00446;",
          # =capital U, Cyrillic
          "Ucy" => "&#x00423;",
          # =small u, Cyrillic
          "ucy" => "&#x00443;",
          # =capital VE, Cyrillic
          "Vcy" => "&#x00412;",
          # =small ve, Cyrillic
          "vcy" => "&#x00432;",
          # =capital YA, Cyrillic
          "YAcy" => "&#x0042F;",
          # =small ya, Cyrillic
          "yacy" => "&#x0044F;",
          # =capital YERU, Cyrillic
          "Ycy" => "&#x0042B;",
          # =small yeru, Cyrillic
          "ycy" => "&#x0044B;",
          # =capital YU, Cyrillic
          "YUcy" => "&#x0042E;",
          # =small yu, Cyrillic
          "yucy" => "&#x0044E;",
          # =capital ZE, Cyrillic
          "Zcy" => "&#x00417;",
          # =small ze, Cyrillic
          "zcy" => "&#x00437;",
          # =capital ZHE, Cyrillic
          "ZHcy" => "&#x00416;",
          # =small zhe, Cyrillic
          "zhcy" => "&#x00436;",
          # =capital DJE, Serbian
          "DJcy" => "&#x00402;",
          # =small dje, Serbian
          "djcy" => "&#x00452;",
          # =capital DSE, Macedonian
          "DScy" => "&#x00405;",
          # =small dse, Macedonian
          "dscy" => "&#x00455;",
          # =capital dze, Serbian
          "DZcy" => "&#x0040F;",
          # =small dze, Serbian
          "dzcy" => "&#x0045F;",
          # =capital GJE Macedonian
          "GJcy" => "&#x00403;",
          # =small gje, Macedonian
          "gjcy" => "&#x00453;",
          # =capital I, Ukrainian
          "Iukcy" => "&#x00406;",
          # =small i, Ukrainian
          "iukcy" => "&#x00456;",
          # =capital JE, Serbian
          "Jsercy" => "&#x00408;",
          # =small je, Serbian
          "jsercy" => "&#x00458;",
          # =capital JE, Ukrainian
          "Jukcy" => "&#x00404;",
          # =small je, Ukrainian
          "jukcy" => "&#x00454;",
          # =capital KJE, Macedonian
          "KJcy" => "&#x0040C;",
          # =small kje Macedonian
          "kjcy" => "&#x0045C;",
          # =capital LJE, Serbian
          "LJcy" => "&#x00409;",
          # =small lje, Serbian
          "ljcy" => "&#x00459;",
          # =capital NJE, Serbian
          "NJcy" => "&#x0040A;",
          # =small nje, Serbian
          "njcy" => "&#x0045A;",
          # =capital TSHE, Serbian
          "TSHcy" => "&#x0040B;",
          # =small tshe, Serbian
          "tshcy" => "&#x0045B;",
          # =capital U, Byelorussian
          "Ubrcy" => "&#x0040E;",
          # =small u, Byelorussian
          "ubrcy" => "&#x0045E;",
          # =capital YI, Ukrainian
          "YIcy" => "&#x00407;",
          # =small yi, Ukrainian
          "yicy" => "&#x00457;",
          # =acute accent
          "acute" => "&#x000B4;",
          # =breve
          "breve" => "&#x002D8;",
          # =caron
          "caron" => "&#x002C7;",
          # =cedilla
          "cedil" => "&#x000B8;",
          # circumflex accent
          "circ" => "&#x002C6;",
          # =double acute accent
          "dblac" => "&#x002DD;",
          # =dieresis
          "die" => "&#x000A8;",
          # =dot above
          "dot" => "&#x002D9;",
          # =grave accent
          "grave" => "&#x00060;",
          # =macron
          "macr" => "&#x000AF;",
          # =ogonek
          "ogon" => "&#x002DB;",
          # =ring
          "ring" => "&#x002DA;",
          # =tilde
          "tilde" => "&#x002DC;",
          # =umlaut mark
          "uml" => "&#x000A8;",
          # /alpha small alpha, Greek
          "alpha" => "&#x003B1;",
          # /beta small beta, Greek
          "beta" => "&#x003B2;",
          # /chi small chi, Greek
          "chi" => "&#x003C7;",
          # /Delta capital Delta, Greek
          "Delta" => "&#x00394;",
          # /delta small delta, Greek
          "delta" => "&#x003B4;",
          # /straightepsilon, small epsilon, Greek
          "epsi" => "&#x003F5;",
          # /varepsilon
          "epsiv" => "&#x003B5;",
          # /eta small eta, Greek
          "eta" => "&#x003B7;",
          # /Gamma capital Gamma, Greek
          "Gamma" => "&#x00393;",
          # /gamma small gamma, Greek
          "gamma" => "&#x003B3;",
          # capital digamma
          "Gammad" => "&#x003DC;",
          # /digamma
          "gammad" => "&#x003DD;",
          # /iota small iota, Greek
          "iota" => "&#x003B9;",
          # /kappa small kappa, Greek
          "kappa" => "&#x003BA;",
          # /varkappa
          "kappav" => "&#x003F0;",
          # /Lambda capital Lambda, Greek
          "Lambda" => "&#x0039B;",
          # /lambda small lambda, Greek
          "lambda" => "&#x003BB;",
          # /mu small mu, Greek
          "mu" => "&#x003BC;",
          # /nu small nu, Greek
          "nu" => "&#x003BD;",
          # /Omega capital Omega, Greek
          "Omega" => "&#x003A9;",
          # /omega small omega, Greek
          "omega" => "&#x003C9;",
          # /Phi capital Phi, Greek
          "Phi" => "&#x003A6;",
          # /straightphi - small phi, Greek
          "phi" => "&#x003D5;",
          # /varphi - curly or open phi
          "phiv" => "&#x003C6;",
          # /Pi capital Pi, Greek
          "Pi" => "&#x003A0;",
          # /pi small pi, Greek
          "pi" => "&#x003C0;",
          # /varpi
          "piv" => "&#x003D6;",
          # /Psi capital Psi, Greek
          "Psi" => "&#x003A8;",
          # /psi small psi, Greek
          "psi" => "&#x003C8;",
          # /rho small rho, Greek
          "rho" => "&#x003C1;",
          # /varrho
          "rhov" => "&#x003F1;",
          # /Sigma capital Sigma, Greek
          "Sigma" => "&#x003A3;",
          # /sigma small sigma, Greek
          "sigma" => "&#x003C3;",
          # /varsigma
          "sigmav" => "&#x003C2;",
          # /tau small tau, Greek
          "tau" => "&#x003C4;",
          # /Theta capital Theta, Greek
          "Theta" => "&#x00398;",
          # /theta straight theta, small theta, Greek
          "theta" => "&#x003B8;",
          # /vartheta - curly or open theta
          "thetav" => "&#x003D1;",
          # /Upsilon capital Upsilon, Greek
          "Upsi" => "&#x003D2;",
          # /upsilon small upsilon, Greek
          "upsi" => "&#x003C5;",
          # /Xi capital Xi, Greek
          "Xi" => "&#x0039E;",
          # /xi small xi, Greek
          "xi" => "&#x003BE;",
          # /zeta small zeta, Greek
          "zeta" => "&#x003B6;",
          # =capital A, acute accent
          "Aacute" => "&#x000C1;",
          # =small a, acute accent
          "aacute" => "&#x000E1;",
          # =capital A, circumflex accent
          "Acirc" => "&#x000C2;",
          # =small a, circumflex accent
          "acirc" => "&#x000E2;",
          # =capital AE diphthong (ligature)
          "AElig" => "&#x000C6;",
          # =small ae diphthong (ligature)
          "aelig" => "&#x000E6;",
          # =capital A, grave accent
          "Agrave" => "&#x000C0;",
          # =small a, grave accent
          "agrave" => "&#x000E0;",
          # =capital A, ring
          "Aring" => "&#x000C5;",
          # =small a, ring
          "aring" => "&#x000E5;",
          # =capital A, tilde
          "Atilde" => "&#x000C3;",
          # =small a, tilde
          "atilde" => "&#x000E3;",
          # =capital A, dieresis or umlaut mark
          "Auml" => "&#x000C4;",
          # =small a, dieresis or umlaut mark
          "auml" => "&#x000E4;",
          # =capital C, cedilla
          "Ccedil" => "&#x000C7;",
          # =small c, cedilla
          "ccedil" => "&#x000E7;",
          # =capital E, acute accent
          "Eacute" => "&#x000C9;",
          # =small e, acute accent
          "eacute" => "&#x000E9;",
          # =capital E, circumflex accent
          "Ecirc" => "&#x000CA;",
          # =small e, circumflex accent
          "ecirc" => "&#x000EA;",
          # =capital E, grave accent
          "Egrave" => "&#x000C8;",
          # =small e, grave accent
          "egrave" => "&#x000E8;",
          # =capital Eth, Icelandic
          "ETH" => "&#x000D0;",
          # =small eth, Icelandic
          "eth" => "&#x000F0;",
          # =capital E, dieresis or umlaut mark
          "Euml" => "&#x000CB;",
          # =small e, dieresis or umlaut mark
          "euml" => "&#x000EB;",
          # =capital I, acute accent
          "Iacute" => "&#x000CD;",
          # =small i, acute accent
          "iacute" => "&#x000ED;",
          # =capital I, circumflex accent
          "Icirc" => "&#x000CE;",
          # =small i, circumflex accent
          "icirc" => "&#x000EE;",
          # =capital I, grave accent
          "Igrave" => "&#x000CC;",
          # =small i, grave accent
          "igrave" => "&#x000EC;",
          # =capital I, dieresis or umlaut mark
          "Iuml" => "&#x000CF;",
          # =small i, dieresis or umlaut mark
          "iuml" => "&#x000EF;",
          # =capital N, tilde
          "Ntilde" => "&#x000D1;",
          # =small n, tilde
          "ntilde" => "&#x000F1;",
          # =capital O, acute accent
          "Oacute" => "&#x000D3;",
          # =small o, acute accent
          "oacute" => "&#x000F3;",
          # =capital O, circumflex accent
          "Ocirc" => "&#x000D4;",
          # =small o, circumflex accent
          "ocirc" => "&#x000F4;",
          # =capital O, grave accent
          "Ograve" => "&#x000D2;",
          # =small o, grave accent
          "ograve" => "&#x000F2;",
          # =capital O, slash
          "Oslash" => "&#x000D8;",
          # latin small letter o with stroke
          "oslash" => "&#x000F8;",
          # =capital O, tilde
          "Otilde" => "&#x000D5;",
          # =small o, tilde
          "otilde" => "&#x000F5;",
          # =capital O, dieresis or umlaut mark
          "Ouml" => "&#x000D6;",
          # =small o, dieresis or umlaut mark
          "ouml" => "&#x000F6;",
          # =small sharp s, German (sz ligature)
          "szlig" => "&#x000DF;",
          # =capital THORN, Icelandic
          "THORN" => "&#x000DE;",
          # =small thorn, Icelandic
          "thorn" => "&#x000FE;",
          # =capital U, acute accent
          "Uacute" => "&#x000DA;",
          # =small u, acute accent
          "uacute" => "&#x000FA;",
          # =capital U, circumflex accent
          "Ucirc" => "&#x000DB;",
          # =small u, circumflex accent
          "ucirc" => "&#x000FB;",
          # =capital U, grave accent
          "Ugrave" => "&#x000D9;",
          # =small u, grave accent
          "ugrave" => "&#x000F9;",
          # =capital U, dieresis or umlaut mark
          "Uuml" => "&#x000DC;",
          # =small u, dieresis or umlaut mark
          "uuml" => "&#x000FC;",
          # =capital Y, acute accent
          "Yacute" => "&#x000DD;",
          # =small y, acute accent
          "yacute" => "&#x000FD;",
          # =small y, dieresis or umlaut mark
          "yuml" => "&#x000FF;",
          # =capital A, breve
          "Abreve" => "&#x00102;",
          # =small a, breve
          "abreve" => "&#x00103;",
          # =capital A, macron
          "Amacr" => "&#x00100;",
          # =small a, macron
          "amacr" => "&#x00101;",
          # =capital A, ogonek
          "Aogon" => "&#x00104;",
          # =small a, ogonek
          "aogon" => "&#x00105;",
          # =capital C, acute accent
          "Cacute" => "&#x00106;",
          # =small c, acute accent
          "cacute" => "&#x00107;",
          # =capital C, caron
          "Ccaron" => "&#x0010C;",
          # =small c, caron
          "ccaron" => "&#x0010D;",
          # =capital C, circumflex accent
          "Ccirc" => "&#x00108;",
          # =small c, circumflex accent
          "ccirc" => "&#x00109;",
          # =capital C, dot above
          "Cdot" => "&#x0010A;",
          # =small c, dot above
          "cdot" => "&#x0010B;",
          # =capital D, caron
          "Dcaron" => "&#x0010E;",
          # =small d, caron
          "dcaron" => "&#x0010F;",
          # =capital D, stroke
          "Dstrok" => "&#x00110;",
          # =small d, stroke
          "dstrok" => "&#x00111;",
          # =capital E, caron
          "Ecaron" => "&#x0011A;",
          # =small e, caron
          "ecaron" => "&#x0011B;",
          # =capital E, dot above
          "Edot" => "&#x00116;",
          # =small e, dot above
          "edot" => "&#x00117;",
          # =capital E, macron
          "Emacr" => "&#x00112;",
          # =small e, macron
          "emacr" => "&#x00113;",
          # =capital ENG, Lapp
          "ENG" => "&#x0014A;",
          # =small eng, Lapp
          "eng" => "&#x0014B;",
          # =capital E, ogonek
          "Eogon" => "&#x00118;",
          # =small e, ogonek
          "eogon" => "&#x00119;",
          # =small g, acute accent
          "gacute" => "&#x001F5;",
          # =capital G, breve
          "Gbreve" => "&#x0011E;",
          # =small g, breve
          "gbreve" => "&#x0011F;",
          # =capital G, cedilla
          "Gcedil" => "&#x00122;",
          # =capital G, circumflex accent
          "Gcirc" => "&#x0011C;",
          # =small g, circumflex accent
          "gcirc" => "&#x0011D;",
          # =capital G, dot above
          "Gdot" => "&#x00120;",
          # =small g, dot above
          "gdot" => "&#x00121;",
          # =capital H, circumflex accent
          "Hcirc" => "&#x00124;",
          # =small h, circumflex accent
          "hcirc" => "&#x00125;",
          # =capital H, stroke
          "Hstrok" => "&#x00126;",
          # =small h, stroke
          "hstrok" => "&#x00127;",
          # =capital I, dot above
          "Idot" => "&#x00130;",
          # =capital IJ ligature
          "IJlig" => "&#x00132;",
          # =small ij ligature
          "ijlig" => "&#x00133;",
          # =capital I, macron
          "Imacr" => "&#x0012A;",
          # =small i, macron
          "imacr" => "&#x0012B;",
          # =small i without dot
          "inodot" => "&#x00131;",
          # =capital I, ogonek
          "Iogon" => "&#x0012E;",
          # =small i, ogonek
          "iogon" => "&#x0012F;",
          # =capital I, tilde
          "Itilde" => "&#x00128;",
          # =small i, tilde
          "itilde" => "&#x00129;",
          # =capital J, circumflex accent
          "Jcirc" => "&#x00134;",
          # =small j, circumflex accent
          "jcirc" => "&#x00135;",
          # =capital K, cedilla
          "Kcedil" => "&#x00136;",
          # =small k, cedilla
          "kcedil" => "&#x00137;",
          # =small k, Greenlandic
          "kgreen" => "&#x00138;",
          # =capital L, acute accent
          "Lacute" => "&#x00139;",
          # =small l, acute accent
          "lacute" => "&#x0013A;",
          # =capital L, caron
          "Lcaron" => "&#x0013D;",
          # =small l, caron
          "lcaron" => "&#x0013E;",
          # =capital L, cedilla
          "Lcedil" => "&#x0013B;",
          # =small l, cedilla
          "lcedil" => "&#x0013C;",
          # =capital L, middle dot
          "Lmidot" => "&#x0013F;",
          # =small l, middle dot
          "lmidot" => "&#x00140;",
          # =capital L, stroke
          "Lstrok" => "&#x00141;",
          # =small l, stroke
          "lstrok" => "&#x00142;",
          # =capital N, acute accent
          "Nacute" => "&#x00143;",
          # =small n, acute accent
          "nacute" => "&#x00144;",
          # =small n, apostrophe
          "napos" => "&#x00149;",
          # =capital N, caron
          "Ncaron" => "&#x00147;",
          # =small n, caron
          "ncaron" => "&#x00148;",
          # =capital N, cedilla
          "Ncedil" => "&#x00145;",
          # =small n, cedilla
          "ncedil" => "&#x00146;",
          # =capital O, double acute accent
          "Odblac" => "&#x00150;",
          # =small o, double acute accent
          "odblac" => "&#x00151;",
          # =capital OE ligature
          "OElig" => "&#x00152;",
          # =small oe ligature
          "oelig" => "&#x00153;",
          # =capital O, macron
          "Omacr" => "&#x0014C;",
          # =small o, macron
          "omacr" => "&#x0014D;",
          # =capital R, acute accent
          "Racute" => "&#x00154;",
          # =small r, acute accent
          "racute" => "&#x00155;",
          # =capital R, caron
          "Rcaron" => "&#x00158;",
          # =small r, caron
          "rcaron" => "&#x00159;",
          # =capital R, cedilla
          "Rcedil" => "&#x00156;",
          # =small r, cedilla
          "rcedil" => "&#x00157;",
          # =capital S, acute accent
          "Sacute" => "&#x0015A;",
          # =small s, acute accent
          "sacute" => "&#x0015B;",
          # =capital S, caron
          "Scaron" => "&#x00160;",
          # =small s, caron
          "scaron" => "&#x00161;",
          # =capital S, cedilla
          "Scedil" => "&#x0015E;",
          # =small s, cedilla
          "scedil" => "&#x0015F;",
          # =capital S, circumflex accent
          "Scirc" => "&#x0015C;",
          # =small s, circumflex accent
          "scirc" => "&#x0015D;",
          # =capital T, caron
          "Tcaron" => "&#x00164;",
          # =small t, caron
          "tcaron" => "&#x00165;",
          # =capital T, cedilla
          "Tcedil" => "&#x00162;",
          # =small t, cedilla
          "tcedil" => "&#x00163;",
          # =capital T, stroke
          "Tstrok" => "&#x00166;",
          # =small t, stroke
          "tstrok" => "&#x00167;",
          # =capital U, breve
          "Ubreve" => "&#x0016C;",
          # =small u, breve
          "ubreve" => "&#x0016D;",
          # =capital U, double acute accent
          "Udblac" => "&#x00170;",
          # =small u, double acute accent
          "udblac" => "&#x00171;",
          # =capital U, macron
          "Umacr" => "&#x0016A;",
          # =small u, macron
          "umacr" => "&#x0016B;",
          # =capital U, ogonek
          "Uogon" => "&#x00172;",
          # =small u, ogonek
          "uogon" => "&#x00173;",
          # =capital U, ring
          "Uring" => "&#x0016E;",
          # =small u, ring
          "uring" => "&#x0016F;",
          # =capital U, tilde
          "Utilde" => "&#x00168;",
          # =small u, tilde
          "utilde" => "&#x00169;",
          # =capital W, circumflex accent
          "Wcirc" => "&#x00174;",
          # =small w, circumflex accent
          "wcirc" => "&#x00175;",
          # =capital Y, circumflex accent
          "Ycirc" => "&#x00176;",
          # =small y, circumflex accent
          "ycirc" => "&#x00177;",
          # =capital Y, dieresis or umlaut mark
          "Yuml" => "&#x00178;",
          # =capital Z, acute accent
          "Zacute" => "&#x00179;",
          # =small z, acute accent
          "zacute" => "&#x0017A;",
          # =capital Z, caron
          "Zcaron" => "&#x0017D;",
          # =small z, caron
          "zcaron" => "&#x0017E;",
          # =capital Z, dot above
          "Zdot" => "&#x0017B;",
          # =small z, dot above
          "zdot" => "&#x0017C;",
          # /frak A, upper case a
          "Afr" => "&#x1D504;",
          # /frak a, lower case a
          "afr" => "&#x1D51E;",
          # /frak B, upper case b
          "Bfr" => "&#x1D505;",
          # /frak b, lower case b
          "bfr" => "&#x1D51F;",
          # /frak C, upper case c
          "Cfr" => "&#x0212D;",
          # /frak c, lower case c
          "cfr" => "&#x1D520;",
          # /frak D, upper case d
          "Dfr" => "&#x1D507;",
          # /frak d, lower case d
          "dfr" => "&#x1D521;",
          # /frak E, upper case e
          "Efr" => "&#x1D508;",
          # /frak e, lower case e
          "efr" => "&#x1D522;",
          # /frak F, upper case f
          "Ffr" => "&#x1D509;",
          # /frak f, lower case f
          "ffr" => "&#x1D523;",
          # /frak G, upper case g
          "Gfr" => "&#x1D50A;",
          # /frak g, lower case g
          "gfr" => "&#x1D524;",
          # /frak H, upper case h
          "Hfr" => "&#x0210C;",
          # /frak h, lower case h
          "hfr" => "&#x1D525;",
          # /frak I, upper case i
          "Ifr" => "&#x02111;",
          # /frak i, lower case i
          "ifr" => "&#x1D526;",
          # /frak J, upper case j
          "Jfr" => "&#x1D50D;",
          # /frak j, lower case j
          "jfr" => "&#x1D527;",
          # /frak K, upper case k
          "Kfr" => "&#x1D50E;",
          # /frak k, lower case k
          "kfr" => "&#x1D528;",
          # /frak L, upper case l
          "Lfr" => "&#x1D50F;",
          # /frak l, lower case l
          "lfr" => "&#x1D529;",
          # /frak M, upper case m
          "Mfr" => "&#x1D510;",
          # /frak m, lower case m
          "mfr" => "&#x1D52A;",
          # /frak N, upper case n
          "Nfr" => "&#x1D511;",
          # /frak n, lower case n
          "nfr" => "&#x1D52B;",
          # /frak O, upper case o
          "Ofr" => "&#x1D512;",
          # /frak o, lower case o
          "ofr" => "&#x1D52C;",
          # /frak P, upper case p
          "Pfr" => "&#x1D513;",
          # /frak p, lower case p
          "pfr" => "&#x1D52D;",
          # /frak Q, upper case q
          "Qfr" => "&#x1D514;",
          # /frak q, lower case q
          "qfr" => "&#x1D52E;",
          # /frak R, upper case r
          "Rfr" => "&#x0211C;",
          # /frak r, lower case r
          "rfr" => "&#x1D52F;",
          # /frak S, upper case s
          "Sfr" => "&#x1D516;",
          # /frak s, lower case s
          "sfr" => "&#x1D530;",
          # /frak T, upper case t
          "Tfr" => "&#x1D517;",
          # /frak t, lower case t
          "tfr" => "&#x1D531;",
          # /frak U, upper case u
          "Ufr" => "&#x1D518;",
          # /frak u, lower case u
          "ufr" => "&#x1D532;",
          # /frak V, upper case v
          "Vfr" => "&#x1D519;",
          # /frak v, lower case v
          "vfr" => "&#x1D533;",
          # /frak W, upper case w
          "Wfr" => "&#x1D51A;",
          # /frak w, lower case w
          "wfr" => "&#x1D534;",
          # /frak X, upper case x
          "Xfr" => "&#x1D51B;",
          # /frak x, lower case x
          "xfr" => "&#x1D535;",
          # /frak Y, upper case y
          "Yfr" => "&#x1D51C;",
          # /frak y, lower case y
          "yfr" => "&#x1D536;",
          # /frak Z, upper case z
          "Zfr" => "&#x02128;",
          # /frak z, lower case z
          "zfr" => "&#x1D537;",
          # /Bbb A, open face A
          "Aopf" => "&#x1D538;",
          # /Bbb B, open face B
          "Bopf" => "&#x1D539;",
          # /Bbb C, open face C
          "Copf" => "&#x02102;",
          # /Bbb D, open face D
          "Dopf" => "&#x1D53B;",
          # /Bbb E, open face E
          "Eopf" => "&#x1D53C;",
          # /Bbb F, open face F
          "Fopf" => "&#x1D53D;",
          # /Bbb G, open face G
          "Gopf" => "&#x1D53E;",
          # /Bbb H, open face H
          "Hopf" => "&#x0210D;",
          # /Bbb I, open face I
          "Iopf" => "&#x1D540;",
          # /Bbb J, open face J
          "Jopf" => "&#x1D541;",
          # /Bbb K, open face K
          "Kopf" => "&#x1D542;",
          # /Bbb L, open face L
          "Lopf" => "&#x1D543;",
          # /Bbb M, open face M
          "Mopf" => "&#x1D544;",
          # /Bbb N, open face N
          "Nopf" => "&#x02115;",
          # /Bbb O, open face O
          "Oopf" => "&#x1D546;",
          # /Bbb P, open face P
          "Popf" => "&#x02119;",
          # /Bbb Q, open face Q
          "Qopf" => "&#x0211A;",
          # /Bbb R, open face R
          "Ropf" => "&#x0211D;",
          # /Bbb S, open face S
          "Sopf" => "&#x1D54A;",
          # /Bbb T, open face T
          "Topf" => "&#x1D54B;",
          # /Bbb U, open face U
          "Uopf" => "&#x1D54C;",
          # /Bbb V, open face V
          "Vopf" => "&#x1D54D;",
          # /Bbb W, open face W
          "Wopf" => "&#x1D54E;",
          # /Bbb X, open face X
          "Xopf" => "&#x1D54F;",
          # /Bbb Y, open face Y
          "Yopf" => "&#x1D550;",
          # /Bbb Z, open face Z
          "Zopf" => "&#x02124;",
          # /scr A, script letter A
          "Ascr" => "&#x1D49C;",
          # /scr a, script letter a
          "ascr" => "&#x1D4B6;",
          # /scr B, script letter B
          "Bscr" => "&#x0212C;",
          # /scr b, script letter b
          "bscr" => "&#x1D4B7;",
          # /scr C, script letter C
          "Cscr" => "&#x1D49E;",
          # /scr c, script letter c
          "cscr" => "&#x1D4B8;",
          # /scr D, script letter D
          "Dscr" => "&#x1D49F;",
          # /scr d, script letter d
          "dscr" => "&#x1D4B9;",
          # /scr E, script letter E
          "Escr" => "&#x02130;",
          # /scr e, script letter e
          "escr" => "&#x0212F;",
          # /scr F, script letter F
          "Fscr" => "&#x02131;",
          # /scr f, script letter f
          "fscr" => "&#x1D4BB;",
          # /scr G, script letter G
          "Gscr" => "&#x1D4A2;",
          # /scr g, script letter g
          "gscr" => "&#x0210A;",
          # /scr H, script letter H
          "Hscr" => "&#x0210B;",
          # /scr h, script letter h
          "hscr" => "&#x1D4BD;",
          # /scr I, script letter I
          "Iscr" => "&#x02110;",
          # /scr i, script letter i
          "iscr" => "&#x1D4BE;",
          # /scr J, script letter J
          "Jscr" => "&#x1D4A5;",
          # /scr j, script letter j
          "jscr" => "&#x1D4BF;",
          # /scr K, script letter K
          "Kscr" => "&#x1D4A6;",
          # /scr k, script letter k
          "kscr" => "&#x1D4C0;",
          # /scr L, script letter L
          "Lscr" => "&#x02112;",
          # /scr l, script letter l
          "lscr" => "&#x1D4C1;",
          # /scr M, script letter M
          "Mscr" => "&#x02133;",
          # /scr m, script letter m
          "mscr" => "&#x1D4C2;",
          # /scr N, script letter N
          "Nscr" => "&#x1D4A9;",
          # /scr n, script letter n
          "nscr" => "&#x1D4C3;",
          # /scr O, script letter O
          "Oscr" => "&#x1D4AA;",
          # /scr o, script letter o
          "oscr" => "&#x02134;",
          # /scr P, script letter P
          "Pscr" => "&#x1D4AB;",
          # /scr p, script letter p
          "pscr" => "&#x1D4C5;",
          # /scr Q, script letter Q
          "Qscr" => "&#x1D4AC;",
          # /scr q, script letter q
          "qscr" => "&#x1D4C6;",
          # /scr R, script letter R
          "Rscr" => "&#x0211B;",
          # /scr r, script letter r
          "rscr" => "&#x1D4C7;",
          # /scr S, script letter S
          "Sscr" => "&#x1D4AE;",
          # /scr s, script letter s
          "sscr" => "&#x1D4C8;",
          # /scr T, script letter T
          "Tscr" => "&#x1D4AF;",
          # /scr t, script letter t
          "tscr" => "&#x1D4C9;",
          # /scr U, script letter U
          "Uscr" => "&#x1D4B0;",
          # /scr u, script letter u
          "uscr" => "&#x1D4CA;",
          # /scr V, script letter V
          "Vscr" => "&#x1D4B1;",
          # /scr v, script letter v
          "vscr" => "&#x1D4CB;",
          # /scr W, script letter W
          "Wscr" => "&#x1D4B2;",
          # /scr w, script letter w
          "wscr" => "&#x1D4CC;",
          # /scr X, script letter X
          "Xscr" => "&#x1D4B3;",
          # /scr x, script letter x
          "xscr" => "&#x1D4CD;",
          # /scr Y, script letter Y
          "Yscr" => "&#x1D4B4;",
          # /scr y, script letter y
          "yscr" => "&#x1D4CE;",
          # /scr Z, script letter Z
          "Zscr" => "&#x1D4B5;",
          # /scr z, script letter z
          "zscr" => "&#x1D4CF;",
          # =ampersand
          "amp" => "&#38;",
          # =apostrophe
          "apos" => "&#x00027;",
          # /ast B: =asterisk
          "ast" => "&#x0002A;",
          # =broken (vertical) bar
          "brvbar" => "&#x000A6;",
          # /backslash =reverse solidus
          "bsol" => "&#x0005C;",
          # =cent sign
          "cent" => "&#x000A2;",
          # /colon P:
          "colon" => "&#x0003A;",
          # P: =comma
          "comma" => "&#x0002C;",
          # =commercial at
          "commat" => "&#x00040;",
          # =copyright sign
          "copy" => "&#x000A9;",
          # =general currency sign
          "curren" => "&#x000A4;",
          # /downarrow A: =downward arrow
          "darr" => "&#x02193;",
          # =degree sign
          "deg" => "&#x000B0;",
          # /div B: =divide sign
          "divide" => "&#x000F7;",
          # =dollar sign
          "dollar" => "&#x00024;",
          # =equals sign R:
          "equals" => "&#x0003D;",
          # =exclamation mark
          "excl" => "&#x00021;",
          # =fraction one-half
          "frac12" => "&#x000BD;",
          # =fraction one-quarter
          "frac14" => "&#x000BC;",
          # =fraction one-eighth
          "frac18" => "&#x0215B;",
          # =fraction three-quarters
          "frac34" => "&#x000BE;",
          # =fraction three-eighths
          "frac38" => "&#x0215C;",
          # =fraction five-eighths
          "frac58" => "&#x0215D;",
          # =fraction seven-eighths
          "frac78" => "&#x0215E;",
          # =greater-than sign R:
          "gt" => "&#x0003E;",
          # =fraction one-half
          "half" => "&#x000BD;",
          # =horizontal bar
          "horbar" => "&#x02015;",
          # =hyphen
          "hyphen" => "&#x02010;",
          # =inverted exclamation mark
          "iexcl" => "&#x000A1;",
          # =inverted question mark
          "iquest" => "&#x000BF;",
          # =angle quotation mark, left
          "laquo" => "&#x000AB;",
          # /leftarrow /gets A: =leftward arrow
          "larr" => "&#x02190;",
          # /lbrace O: =left curly bracket
          "lcub" => "&#x0007B;",
          # =double quotation mark, left
          "ldquo" => "&#x0201C;",
          # =low line
          "lowbar" => "&#x0005F;",
          # O: =left parenthesis
          "lpar" => "&#x00028;",
          # /lbrack O: =left square bracket
          "lsqb" => "&#x0005B;",
          # =single quotation mark, left
          "lsquo" => "&#x02018;",
          # =less-than sign R:
          "lt" => "&#60;",
          # =micro sign
          "micro" => "&#x000B5;",
          # /centerdot B: =middle dot
          "middot" => "&#x000B7;",
          # =no break (required) space
          "nbsp" => "&#x000A0;",
          # /neg /lnot =not sign
          "not" => "&#x000AC;",
          # =number sign
          "num" => "&#x00023;",
          # =ohm sign
          "ohm" => "&#x02126;",
          # =ordinal indicator, feminine
          "ordf" => "&#x000AA;",
          # =ordinal indicator, masculine
          "ordm" => "&#x000BA;",
          # =pilcrow (paragraph sign)
          "para" => "&#x000B6;",
          # =percent sign
          "percnt" => "&#x00025;",
          # =full stop, period
          "period" => "&#x0002E;",
          # =plus sign B:
          "plus" => "&#x0002B;",
          # /pm B: =plus-or-minus sign
          "plusmn" => "&#x000B1;",
          # =pound sign
          "pound" => "&#x000A3;",
          # =question mark
          "quest" => "&#x0003F;",
          # =quotation mark
          "quot" => "&#x00022;",
          # =angle quotation mark, right
          "raquo" => "&#x000BB;",
          # /rightarrow /to A: =rightward arrow
          "rarr" => "&#x02192;",
          # /rbrace C: =right curly bracket
          "rcub" => "&#x0007D;",
          # =double quotation mark, right
          "rdquo" => "&#x0201D;",
          # /circledR =registered sign
          "reg" => "&#x000AE;",
          # C: =right parenthesis
          "rpar" => "&#x00029;",
          # /rbrack C: =right square bracket
          "rsqb" => "&#x0005D;",
          # =single quotation mark, right
          "rsquo" => "&#x02019;",
          # =section sign
          "sect" => "&#x000A7;",
          # =semicolon P:
          "semi" => "&#x0003B;",
          # =soft hyphen
          "shy" => "&#x000AD;",
          # =solidus
          "sol" => "&#x0002F;",
          # =music note (sung text sign)
          "sung" => "&#x0266A;",
          # =superscript one
          "sup1" => "&#x000B9;",
          # =superscript two
          "sup2" => "&#x000B2;",
          # =superscript three
          "sup3" => "&#x000B3;",
          # /times B: =multiply sign
          "times" => "&#x000D7;",
          # =trade mark sign
          "trade" => "&#x02122;",
          # /uparrow A: =upward arrow
          "uarr" => "&#x02191;",
          # /vert =vertical bar
          "verbar" => "&#x0007C;",
          # /yen =yen sign
          "yen" => "&#x000A5;",
          # =significant blank symbol
          "blank" => "&#x02423;",
          # =50% shaded block
          "blk12" => "&#x02592;",
          # =25% shaded block
          "blk14" => "&#x02591;",
          # =75% shaded block
          "blk34" => "&#x02593;",
          # =full block
          "block" => "&#x02588;",
          # /bullet B: =round bullet, filled
          "bull" => "&#x02022;",
          # =caret (insertion mark)
          "caret" => "&#x02041;",
          # /checkmark =tick, check mark
          "check" => "&#x02713;",
          # /circ B: =circle, open
          "cir" => "&#x025CB;",
          # /clubsuit =club suit symbol
          "clubs" => "&#x02663;",
          # =sound recording copyright sign
          "copysr" => "&#x02117;",
          # =ballot cross
          "cross" => "&#x02717;",
          # /ddagger B: =double dagger
          "Dagger" => "&#x02021;",
          # /dagger B: =dagger
          "dagger" => "&#x02020;",
          # =hyphen (true graphic)
          "dash" => "&#x02010;",
          # /diamondsuit =diamond suit symbol
          "diams" => "&#x02666;",
          # downward left crop mark
          "dlcrop" => "&#x0230D;",
          # downward right crop mark
          "drcrop" => "&#x0230C;",
          # /triangledown =down triangle, open
          "dtri" => "&#x025BF;",
          # /blacktriangledown =dn tri, filled
          "dtrif" => "&#x025BE;",
          # =em space
          "emsp" => "&#x02003;",
          # =1/3-em space
          "emsp13" => "&#x02004;",
          # =1/4-em space
          "emsp14" => "&#x02005;",
          # =en space (1/2-em)
          "ensp" => "&#x02002;",
          # =female symbol
          "female" => "&#x02640;",
          # small ffi ligature
          "ffilig" => "&#x0FB03;",
          # small ff ligature
          "fflig" => "&#x0FB00;",
          # small ffl ligature
          "ffllig" => "&#x0FB04;",
          # small fi ligature
          "filig" => "&#x0FB01;",
          # /flat =musical flat
          "flat" => "&#x0266D;",
          # small fl ligature
          "fllig" => "&#x0FB02;",
          # =fraction one-third
          "frac13" => "&#x02153;",
          # =fraction one-fifth
          "frac15" => "&#x02155;",
          # =fraction one-sixth
          "frac16" => "&#x02159;",
          # =fraction two-thirds
          "frac23" => "&#x02154;",
          # =fraction two-fifths
          "frac25" => "&#x02156;",
          # =fraction three-fifths
          "frac35" => "&#x02157;",
          # =fraction four-fifths
          "frac45" => "&#x02158;",
          # =fraction five-sixths
          "frac56" => "&#x0215A;",
          # =hair space
          "hairsp" => "&#x0200A;",
          # /heartsuit =heart suit symbol
          "hearts" => "&#x02665;",
          # =ellipsis (horizontal)
          "hellip" => "&#x02026;",
          # rectangle, filled (hyphen bullet)
          "hybull" => "&#x02043;",
          # =in-care-of symbol
          "incare" => "&#x02105;",
          # =rising dbl quote, left (low)
          "ldquor" => "&#x0201E;",
          # =lower half block
          "lhblk" => "&#x02584;",
          # /lozenge - lozenge or total mark
          "loz" => "&#x025CA;",
          # /blacklozenge - lozenge, filled
          "lozf" => "&#x029EB;",
          # =rising single quote, left (low)
          "lsquor" => "&#x0201A;",
          # /triangleleft B: l triangle, open
          "ltri" => "&#x025C3;",
          # /blacktriangleleft R: =l tri, filled
          "ltrif" => "&#x025C2;",
          # =male symbol
          "male" => "&#x02642;",
          # /maltese =maltese cross
          "malt" => "&#x02720;",
          # =histogram marker
          "marker" => "&#x025AE;",
          # =em dash
          "mdash" => "&#x02014;",
          # em leader
          "mldr" => "&#x02026;",
          # /natural - music natural
          "natur" => "&#x0266E;",
          # =en dash
          "ndash" => "&#x02013;",
          # =double baseline dot (en leader)
          "nldr" => "&#x02025;",
          # =digit space (width of a number)
          "numsp" => "&#x02007;",
          # =telephone symbol
          "phone" => "&#x0260E;",
          # =punctuation space (width of comma)
          "puncsp" => "&#x02008;",
          # rising dbl quote, right (high)
          "rdquor" => "&#x0201D;",
          # =rectangle, open
          "rect" => "&#x025AD;",
          # rising single quote, right (high)
          "rsquor" => "&#x02019;",
          # /triangleright B: r triangle, open
          "rtri" => "&#x025B9;",
          # /blacktriangleright R: =r tri, filled
          "rtrif" => "&#x025B8;",
          # pharmaceutical prescription (Rx)
          "rx" => "&#x0211E;",
          # sextile (6-pointed star)
          "sext" => "&#x02736;",
          # /sharp =musical sharp
          "sharp" => "&#x0266F;",
          # /spadesuit =spades suit symbol
          "spades" => "&#x02660;",
          # =square, open
          "squ" => "&#x025A1;",
          # /blacksquare =sq bullet, filled
          "squf" => "&#x025AA;",
          # =star, open
          "star" => "&#x02606;",
          # /bigstar - star, filled
          "starf" => "&#x02605;",
          # register mark or target
          "target" => "&#x02316;",
          # =telephone recorder symbol
          "telrec" => "&#x02315;",
          # =thin space (1/6-em)
          "thinsp" => "&#x02009;",
          # =upper half block
          "uhblk" => "&#x02580;",
          # upward left crop mark
          "ulcrop" => "&#x0230F;",
          # upward right crop mark
          "urcrop" => "&#x0230E;",
          # /triangle =up triangle, open
          "utri" => "&#x025B5;",
          # /blacktriangle =up tri, filled
          "utrif" => "&#x025B4;",
          # vertical ellipsis
          "vellip" => "&#x022EE;",
          # ac current
          "acd" => "&#x0223F;",
          # /aleph aleph, Hebrew
          "aleph" => "&#x02135;",
          # dbl logical and
          "And" => "&#x02A53;",
          # /wedge /land B: logical and
          "and" => "&#x02227;",
          # two logical and
          "andand" => "&#x02A55;",
          # and, horizontal dash
          "andd" => "&#x02A5C;",
          # sloping large and
          "andslope" => "&#x02A58;",
          # and with middle stem
          "andv" => "&#x02A5A;",
          # right (90 degree) angle
          "angrt" => "&#x0221F;",
          # /sphericalangle angle-spherical
          "angsph" => "&#x02222;",
          # Angstrom capital A, ring
          "angst" => "&#x0212B;",
          # /approx R: approximate
          "ap" => "&#x02248;",
          # approximate, circumflex accent
          "apacir" => "&#x02A6F;",
          # contour integral, anti-clockwise
          "awconint" => "&#x02233;",
          # anti clock-wise integration
          "awint" => "&#x02A11;",
          # /because R: because
          "becaus" => "&#x02235;",
          # Bernoulli function (script capital B)
          "bernou" => "&#x0212C;",
          # reverse not equal
          "bne" => "&#x0003D;&#x020E5;",
          # reverse not equivalent
          "bnequiv" => "&#x02261;&#x020E5;",
          # reverse not with two horizontal strokes
          "bNot" => "&#x02AED;",
          # reverse not
          "bnot" => "&#x02310;",
          # /bot bottom
          "bottom" => "&#x022A5;",
          # /cap B: intersection
          "cap" => "&#x02229;",
          # triple contour integral operator
          "Cconint" => "&#x02230;",
          # circulation function
          "cirfnint" => "&#x02A10;",
          # /circ B: composite function (small circle)
          "compfn" => "&#x02218;",
          # /cong R: congruent with
          "cong" => "&#x02245;",
          # double contour integral operator
          "Conint" => "&#x0222F;",
          # /oint L: contour integral operator
          "conint" => "&#x0222E;",
          # /cdots, three dots, centered
          "ctdot" => "&#x022EF;",
          # /cup B: union or logical sum
          "cup" => "&#x0222A;",
          # contour integral, clockwise
          "cwconint" => "&#x02232;",
          # clockwise integral
          "cwint" => "&#x02231;",
          # cylindricity
          "cylcty" => "&#x0232D;",
          # set membership, long horizontal stroke
          "disin" => "&#x022F2;",
          # dieresis or umlaut mark
          "Dot" => "&#x000A8;",
          # solidus, bar above
          "dsol" => "&#x029F6;",
          # /ddots, three dots, descending
          "dtdot" => "&#x022F1;",
          # large downward pointing angle
          "dwangle" => "&#x029A6;",
          # electrical intersection
          "elinters" => "&#x0FFFD;",
          # parallel, equal; equal or parallel
          "epar" => "&#x022D5;",
          # parallel, slanted, equal; homothetically congruent to
          "eparsl" => "&#x029E3;",
          # /equiv R: identical with
          "equiv" => "&#x02261;",
          # equivalent, equal; congruent and parallel
          "eqvparsl" => "&#x029E5;",
          # /exists at least one exists
          "exist" => "&#x02203;",
          # flatness
          "fltns" => "&#x025B1;",
          # function of (italic small f)
          "fnof" => "&#x00192;",
          # /forall for all
          "forall" => "&#x02200;",
          # finite part integral
          "fpartint" => "&#x02A0D;",
          # /geq /ge R: greater-than-or-equal
          "ge" => "&#x02265;",
          # Hamiltonian (script capital H)
          "hamilt" => "&#x0210B;",
          # /iff if and only if
          "iff" => "&#x021D4;",
          # infinity sign, incomplete
          "iinfin" => "&#x029DC;",
          # impedance
          "imped" => "&#x001B5;",
          # /infty infinity
          "infin" => "&#x0221E;",
          # tie, infinity
          "infintie" => "&#x029DD;",
          # double integral operator
          "Int" => "&#x0222C;",
          # /int L: integral operator
          "int" => "&#x0222B;",
          # integral, left arrow with hook
          "intlarhk" => "&#x02A17;",
          # /in R: set membership
          "isin" => "&#x02208;",
          # set membership, dot above
          "isindot" => "&#x022F5;",
          # set membership, two horizontal strokes
          "isinE" => "&#x022F9;",
          # set membership, vertical bar on horizontal stroke
          "isins" => "&#x022F4;",
          # large set membership, vertical bar on horizontal stroke
          "isinsv" => "&#x022F3;",
          # set membership, variant
          "isinv" => "&#x02208;",
          # Lagrangian (script capital L)
          "lagran" => "&#x02112;",
          # left angle bracket, double
          "Lang" => "&#x0300A;",
          # /langle O: left angle bracket
          "lang" => "&#x02329;",
          # /Leftarrow A: is implied by
          "lArr" => "&#x021D0;",
          # left broken bracket
          "lbbrk" => "&#x03014;",
          # /leq /le R: less-than-or-equal
          "le" => "&#x02264;",
          # left open angular bracket
          "loang" => "&#x03018;",
          # left open bracket
          "lobrk" => "&#x0301A;",
          # left open parenthesis
          "lopar" => "&#x02985;",
          # low asterisk
          "lowast" => "&#x02217;",
          # B: minus sign
          "minus" => "&#x02212;",
          # /mp B: minus-or-plus sign
          "mnplus" => "&#x02213;",
          # /nabla del, Hamilton operator
          "nabla" => "&#x02207;",
          # /ne /neq R: not equal
          "ne" => "&#x02260;",
          # not equal, dot
          "nedot" => "&#x02250;&#x00338;",
          # not, horizontal, parallel
          "nhpar" => "&#x02AF2;",
          # /ni /owns R: contains
          "ni" => "&#x0220B;",
          # contains, vertical bar on horizontal stroke
          "nis" => "&#x022FC;",
          # contains, long horizontal stroke
          "nisd" => "&#x022FA;",
          # contains, variant
          "niv" => "&#x0220B;",
          # not with two horizontal strokes
          "Not" => "&#x02AEC;",
          # /notin N: negated set membership
          "notin" => "&#x02209;",
          # negated set membership, dot above
          "notindot" => "&#x022F5;&#x00338;",
          # negated set membership, two horizontal strokes
          "notinE" => "&#x022F9;&#x00338;",
          # negated set membership, variant
          "notinva" => "&#x02209;",
          # negated set membership, variant
          "notinvb" => "&#x022F7;",
          # negated set membership, variant
          "notinvc" => "&#x022F6;",
          # negated contains
          "notni" => "&#x0220C;",
          # negated contains, variant
          "notniva" => "&#x0220C;",
          # contains, variant
          "notnivb" => "&#x022FE;",
          # contains, variant
          "notnivc" => "&#x022FD;",
          # not parallel, slanted
          "nparsl" => "&#x02AFD;&#x020E5;",
          # not partial differential
          "npart" => "&#x02202;&#x00338;",
          # line integration, not including the pole
          "npolint" => "&#x02A14;",
          # not, vert, infinity
          "nvinfin" => "&#x029DE;",
          # circle, cross
          "olcross" => "&#x029BB;",
          # dbl logical or
          "Or" => "&#x02A54;",
          # /vee /lor B: logical or
          "or" => "&#x02228;",
          # or, horizontal dash
          "ord" => "&#x02A5D;",
          # order of (script small o)
          "order" => "&#x02134;",
          # two logical or
          "oror" => "&#x02A56;",
          # sloping large or
          "orslope" => "&#x02A57;",
          # or with middle stem
          "orv" => "&#x02A5B;",
          # /parallel R: parallel
          "par" => "&#x02225;",
          # parallel, slanted
          "parsl" => "&#x02AFD;",
          # /partial partial differential
          "part" => "&#x02202;",
          # per thousand
          "permil" => "&#x02030;",
          # /perp R: perpendicular
          "perp" => "&#x022A5;",
          # per 10 thousand
          "pertenk" => "&#x02031;",
          # physics M-matrix (script capital M)
          "phmmat" => "&#x02133;",
          # integral around a point operator
          "pointint" => "&#x02A15;",
          # double prime or second
          "Prime" => "&#x02033;",
          # /prime prime or minute
          "prime" => "&#x02032;",
          # all-around profile
          "profalar" => "&#x0232E;",
          # profile of a line
          "profline" => "&#x02312;",
          # profile of a surface
          "profsurf" => "&#x02313;",
          # /propto R: is proportional to
          "prop" => "&#x0221D;",
          # /iiiint quadruple integral operator
          "qint" => "&#x02A0C;",
          # quadruple prime
          "qprime" => "&#x02057;",
          # quaternion integral operator
          "quatint" => "&#x02A16;",
          # /surd radical
          "radic" => "&#x0221A;",
          # right angle bracket, double
          "Rang" => "&#x0300B;",
          # /rangle C: right angle bracket
          "rang" => "&#x0232A;",
          # /Rightarrow A: implies
          "rArr" => "&#x021D2;",
          # right broken bracket
          "rbbrk" => "&#x03015;",
          # right open angular bracket
          "roang" => "&#x03019;",
          # right open bracket
          "robrk" => "&#x0301B;",
          # right open parenthesis
          "ropar" => "&#x02986;",
          # line integration, rectangular path around pole
          "rppolint" => "&#x02A12;",
          # line integration, semi-circular path around pole
          "scpolint" => "&#x02A13;",
          # /sim R: similar
          "sim" => "&#x0223C;",
          # similar, dot
          "simdot" => "&#x02A6A;",
          # /simeq R: similar, equals
          "sime" => "&#x02243;",
          # similar, parallel, slanted, equal
          "smeparsl" => "&#x029E4;",
          # /square, square
          "square" => "&#x025A1;",
          # /blacksquare, square, filled
          "squarf" => "&#x025AA;",
          # straightness
          "strns" => "&#x000AF;",
          # /subset R: subset or is implied by
          "sub" => "&#x02282;",
          # /subseteq R: subset, equals
          "sube" => "&#x02286;",
          # /supset R: superset or implies
          "sup" => "&#x02283;",
          # /supseteq R: superset, equals
          "supe" => "&#x02287;",
          # /therefore R: therefore
          "there4" => "&#x02234;",
          # /iiint triple integral operator
          "tint" => "&#x0222D;",
          # /top top
          "top" => "&#x022A4;",
          # top and bottom
          "topbot" => "&#x02336;",
          # top, circle below
          "topcir" => "&#x02AF1;",
          # triple prime
          "tprime" => "&#x02034;",
          # three dots, ascending
          "utdot" => "&#x022F0;",
          # large upward pointing angle
          "uwangle" => "&#x029A7;",
          # right angle, variant
          "vangrt" => "&#x0299C;",
          # logical or, equals
          "veeeq" => "&#x0225A;",
          # /Vert dbl vertical bar
          "Verbar" => "&#x02016;",
          # /wedgeq R: corresponds to (wedge, equals)
          "wedgeq" => "&#x02259;",
          # large contains, vertical bar on horizontal stroke
          "xnis" => "&#x022FB;",
          # alias ISOAMSO ang
          "angle" => "&#x02220;",
          # character showing function application in presentation tagging
          "ApplyFunction" => "&#x02061;",
          # alias ISOTECH ap
          "approx" => "&#x02248;",
          # alias ISOAMSR ape
          "approxeq" => "&#x0224A;",
          # assignment operator, alias ISOAMSR colone
          "Assign" => "&#x02254;",
          # alias ISOAMSR bcong
          "backcong" => "&#x0224C;",
          # alias ISOAMSR bepsi
          "backepsilon" => "&#x003F6;",
          # alias ISOAMSO bprime
          "backprime" => "&#x02035;",
          # alias ISOAMSR bsim
          "backsim" => "&#x0223D;",
          # alias ISOAMSR bsime
          "backsimeq" => "&#x022CD;",
          # alias ISOAMSB setmn
          "Backslash" => "&#x02216;",
          # alias ISOAMSB barwed
          "barwedge" => "&#x02305;",
          # alias ISOTECH becaus
          "Because" => "&#x02235;",
          # alias ISOTECH becaus
          "because" => "&#x02235;",
          # alias ISOTECH bernou
          "Bernoullis" => "&#x0212C;",
          # alias ISOAMSR twixt
          "between" => "&#x0226C;",
          # alias ISOAMSB xcap
          "bigcap" => "&#x022C2;",
          # alias ISOAMSB xcirc
          "bigcirc" => "&#x025EF;",
          # alias ISOAMSB xcup
          "bigcup" => "&#x022C3;",
          # alias ISOAMSB xodot
          "bigodot" => "&#x02A00;",
          # alias ISOAMSB xoplus
          "bigoplus" => "&#x02A01;",
          # alias ISOAMSB xotime
          "bigotimes" => "&#x02A02;",
          # alias ISOAMSB xsqcup
          "bigsqcup" => "&#x02A06;",
          # ISOPUB    starf
          "bigstar" => "&#x02605;",
          # alias ISOAMSB xdtri
          "bigtriangledown" => "&#x025BD;",
          # alias ISOAMSB xutri
          "bigtriangleup" => "&#x025B3;",
          # alias ISOAMSB xuplus
          "biguplus" => "&#x02A04;",
          # alias ISOAMSB xvee
          "bigvee" => "&#x022C1;",
          # alias ISOAMSB xwedge
          "bigwedge" => "&#x022C0;",
          # alias ISOAMSA rbarr
          "bkarow" => "&#x0290D;",
          # alias ISOPUB lozf
          "blacklozenge" => "&#x029EB;",
          # ISOTECH  squarf
          "blacksquare" => "&#x025AA;",
          # alias ISOPUB utrif
          "blacktriangle" => "&#x025B4;",
          # alias ISOPUB dtrif
          "blacktriangledown" => "&#x025BE;",
          # alias ISOPUB ltrif
          "blacktriangleleft" => "&#x025C2;",
          # alias ISOPUB rtrif
          "blacktriangleright" => "&#x025B8;",
          # alias ISOTECH bottom
          "bot" => "&#x022A5;",
          # alias ISOAMSB minusb
          "boxminus" => "&#x0229F;",
          # alias ISOAMSB plusb
          "boxplus" => "&#x0229E;",
          # alias ISOAMSB timesb
          "boxtimes" => "&#x022A0;",
          # alias ISODIA breve
          "Breve" => "&#x002D8;",
          # alias ISOPUB bull
          "bullet" => "&#x02022;",
          # alias ISOAMSR bump
          "Bumpeq" => "&#x0224E;",
          # alias ISOAMSR bumpe
          "bumpeq" => "&#x0224F;",
          # D for use in differentials, e.g., within integrals
          "CapitalDifferentialD" => "&#x02145;",
          # the non-associative ring of octonions or Cayley numbers
          "Cayleys" => "&#x0212D;",
          # alias ISODIA cedil
          "Cedilla" => "&#x000B8;",
          # alias ISONUM middot
          "CenterDot" => "&#x000B7;",
          # alias ISONUM middot
          "centerdot" => "&#x000B7;",
          # alias ISOPUB check
          "checkmark" => "&#x02713;",
          # alias ISOAMSR cire
          "circeq" => "&#x02257;",
          # alias ISOAMSA olarr
          "circlearrowleft" => "&#x021BA;",
          # alias ISOAMSA orarr
          "circlearrowright" => "&#x021BB;",
          # alias ISOAMSB oast
          "circledast" => "&#x0229B;",
          # alias ISOAMSB ocir
          "circledcirc" => "&#x0229A;",
          # alias ISOAMSB odash
          "circleddash" => "&#x0229D;",
          # alias ISOAMSB odot
          "CircleDot" => "&#x02299;",
          # alias ISONUM reg
          "circledR" => "&#x000AE;",
          # alias ISOAMSO oS
          "circledS" => "&#x024C8;",
          # alias ISOAMSB ominus
          "CircleMinus" => "&#x02296;",
          # alias ISOAMSB oplus
          "CirclePlus" => "&#x02295;",
          # alias ISOAMSB otimes
          "CircleTimes" => "&#x02297;",
          # alias ISOTECH cwconint
          "ClockwiseContourIntegral" => "&#x02232;",
          # alias ISONUM rdquo
          "CloseCurlyDoubleQuote" => "&#x0201D;",
          # alias ISONUM rsquo
          "CloseCurlyQuote" => "&#x02019;",
          # ISOPUB    clubs
          "clubsuit" => "&#x02663;",
          # alias ISOAMSR colone
          "coloneq" => "&#x02254;",
          # alias ISOAMSO comp
          "complement" => "&#x02201;",
          # the field of complex numbers
          "complexes" => "&#x02102;",
          # alias ISOTECH equiv
          "Congruent" => "&#x02261;",
          # alias ISOTECH conint
          "ContourIntegral" => "&#x0222E;",
          # alias ISOAMSB coprod
          "Coproduct" => "&#x02210;",
          # alias ISOTECH awconint
          "CounterClockwiseContourIntegral" => "&#x02233;",
          # alias asympeq
          "CupCap" => "&#x0224D;",
          # alias ISOAMSR cuepr
          "curlyeqprec" => "&#x022DE;",
          # alias ISOAMSR cuesc
          "curlyeqsucc" => "&#x022DF;",
          # alias ISOAMSB cuvee
          "curlyvee" => "&#x022CE;",
          # alias ISOAMSB cuwed
          "curlywedge" => "&#x022CF;",
          # alias ISOAMSA cularr
          "curvearrowleft" => "&#x021B6;",
          # alias ISOAMSA curarr
          "curvearrowright" => "&#x021B7;",
          # alias ISOAMSA rBarr
          "dbkarow" => "&#x0290F;",
          # alias ISOPUB Dagger
          "ddagger" => "&#x02021;",
          # alias ISOAMSR eDDot
          "ddotseq" => "&#x02A77;",
          # alias ISOTECH nabla
          "Del" => "&#x02207;",
          # alias ISODIA acute
          "DiacriticalAcute" => "&#x000B4;",
          # alias ISODIA dot
          "DiacriticalDot" => "&#x002D9;",
          # alias ISODIA dblac
          "DiacriticalDoubleAcute" => "&#x002DD;",
          # alias ISODIA grave
          "DiacriticalGrave" => "&#x00060;",
          # alias ISODIA tilde
          "DiacriticalTilde" => "&#x002DC;",
          # alias ISOAMSB diam
          "Diamond" => "&#x022C4;",
          # alias ISOAMSB diam
          "diamond" => "&#x022C4;",
          # ISOPUB    diams
          "diamondsuit" => "&#x02666;",
          # d for use in differentials, e.g., within integrals
          "DifferentialD" => "&#x02146;",
          # alias ISOGRK3 gammad
          "digamma" => "&#x003DD;",
          # alias ISONUM divide
          "div" => "&#x000F7;",
          # alias ISOAMSB divonx
          "divideontimes" => "&#x022C7;",
          # alias ISOAMSR esdot
          "doteq" => "&#x02250;",
          # alias ISOAMSR eDot
          "doteqdot" => "&#x02251;",
          # alias ISOAMSR esdot
          "DotEqual" => "&#x02250;",
          # alias ISOAMSB minusd
          "dotminus" => "&#x02238;",
          # alias ISOAMSB plusdo
          "dotplus" => "&#x02214;",
          # alias ISOAMSB sdotb
          "dotsquare" => "&#x022A1;",
          # alias ISOAMSB Barwed
          "doublebarwedge" => "&#x02306;",
          # alias ISOTECH Conint
          "DoubleContourIntegral" => "&#x0222F;",
          # alias ISODIA die
          "DoubleDot" => "&#x000A8;",
          # alias ISOAMSA dArr
          "DoubleDownArrow" => "&#x021D3;",
          # alias ISOTECH lArr
          "DoubleLeftArrow" => "&#x021D0;",
          # alias ISOAMSA hArr
          "DoubleLeftRightArrow" => "&#x021D4;",
          # alias for  &Dashv;
          "DoubleLeftTee" => "&#x02AE4;",
          # alias ISOAMSA xlArr
          "DoubleLongLeftArrow" => "&#x027F8;",
          # alias ISOAMSA xhArr
          "DoubleLongLeftRightArrow" => "&#x027FA;",
          # alias ISOAMSA xrArr
          "DoubleLongRightArrow" => "&#x027F9;",
          # alias ISOTECH rArr
          "DoubleRightArrow" => "&#x021D2;",
          # alias ISOAMSR vDash
          "DoubleRightTee" => "&#x022A8;",
          # alias ISOAMSA uArr
          "DoubleUpArrow" => "&#x021D1;",
          # alias ISOAMSA vArr
          "DoubleUpDownArrow" => "&#x021D5;",
          # alias ISOTECH par
          "DoubleVerticalBar" => "&#x02225;",
          # alias ISONUM darr
          "DownArrow" => "&#x02193;",
          # alias ISOAMSA dArr
          "Downarrow" => "&#x021D3;",
          # alias ISONUM darr
          "downarrow" => "&#x02193;",
          # alias ISOAMSA duarr
          "DownArrowUpArrow" => "&#x021F5;",
          # alias ISOAMSA ddarr
          "downdownarrows" => "&#x021CA;",
          # alias ISOAMSA dharl
          "downharpoonleft" => "&#x021C3;",
          # alias ISOAMSA dharr
          "downharpoonright" => "&#x021C2;",
          # alias ISOAMSA lhard
          "DownLeftVector" => "&#x021BD;",
          # alias ISOAMSA rhard
          "DownRightVector" => "&#x021C1;",
          # alias ISOTECH top
          "DownTee" => "&#x022A4;",
          # alias for mapstodown
          "DownTeeArrow" => "&#x021A7;",
          # alias ISOAMSA RBarr
          "drbkarow" => "&#x02910;",
          # alias ISOTECH isinv
          "Element" => "&#x02208;",
          # alias ISOAMSO empty
          "emptyset" => "&#x02205;",
          # alias ISOAMSR ecir
          "eqcirc" => "&#x02256;",
          # alias ISOAMSR ecolon
          "eqcolon" => "&#x02255;",
          # alias ISOAMSR esim
          "eqsim" => "&#x02242;",
          # alias ISOAMSR egs
          "eqslantgtr" => "&#x02A96;",
          # alias ISOAMSR els
          "eqslantless" => "&#x02A95;",
          # alias ISOAMSR esim
          "EqualTilde" => "&#x02242;",
          # alias ISOAMSA rlhar
          "Equilibrium" => "&#x021CC;",
          # alias ISOTECH exist
          "Exists" => "&#x02203;",
          # expectation (operator)
          "expectation" => "&#x02130;",
          # e use for the exponential base of the natural logarithms
          "ExponentialE" => "&#x02147;",
          # base of the Napierian logarithms
          "exponentiale" => "&#x02147;",
          # alias ISOAMSR efDot
          "fallingdotseq" => "&#x02252;",
          # alias ISOTECH forall
          "ForAll" => "&#x02200;",
          # Fourier transform
          "Fouriertrf" => "&#x02131;",
          # alias ISOTECH ge
          "geq" => "&#x02265;",
          # alias ISOAMSR gE
          "geqq" => "&#x02267;",
          # alias ISOAMSR ges
          "geqslant" => "&#x02A7E;",
          # alias ISOAMSR Gt
          "gg" => "&#x0226B;",
          # alias ISOAMSR Gg
          "ggg" => "&#x022D9;",
          # alias ISOAMSN gnap
          "gnapprox" => "&#x02A8A;",
          # alias ISOAMSN gne
          "gneq" => "&#x02A88;",
          # alias ISOAMSN gnE
          "gneqq" => "&#x02269;",
          # alias ISOTECH ge
          "GreaterEqual" => "&#x02265;",
          # alias ISOAMSR gel
          "GreaterEqualLess" => "&#x022DB;",
          # alias ISOAMSR gE
          "GreaterFullEqual" => "&#x02267;",
          # alias ISOAMSR gl
          "GreaterLess" => "&#x02277;",
          # alias ISOAMSR ges
          "GreaterSlantEqual" => "&#x02A7E;",
          # alias ISOAMSR gsim
          "GreaterTilde" => "&#x02273;",
          # alias ISOAMSR gap
          "gtrapprox" => "&#x02A86;",
          # alias ISOAMSR gtdot
          "gtrdot" => "&#x022D7;",
          # alias ISOAMSR gel
          "gtreqless" => "&#x022DB;",
          # alias ISOAMSR gEl
          "gtreqqless" => "&#x02A8C;",
          # alias ISOAMSR gl
          "gtrless" => "&#x02277;",
          # alias ISOAMSR gsim
          "gtrsim" => "&#x02273;",
          # alias ISOAMSN gvnE
          "gvertneqq" => "&#x02269;&#x0FE00;",
          # alias ISODIA caron
          "Hacek" => "&#x002C7;",
          # alias ISOAMSO plank
          "hbar" => "&#x0210F;",
          # ISOPUB hearts
          "heartsuit" => "&#x02665;",
          # Hilbert space
          "HilbertSpace" => "&#x0210B;",
          # alias ISOAMSA searhk
          "hksearow" => "&#x02925;",
          # alias ISOAMSA swarhk
          "hkswarow" => "&#x02926;",
          # alias ISOAMSA larrhk
          "hookleftarrow" => "&#x021A9;",
          # alias ISOAMSA rarrhk
          "hookrightarrow" => "&#x021AA;",
          # alias ISOAMSO plankv
          "hslash" => "&#x0210F;",
          # alias ISOAMSR bump
          "HumpDownHump" => "&#x0224E;",
          # alias ISOAMSR bumpe
          "HumpEqual" => "&#x0224F;",
          # alias ISOTECH qint
          "iiiint" => "&#x02A0C;",
          # alias ISOTECH tint
          "iiint" => "&#x0222D;",
          # alias ISOAMSO image
          "Im" => "&#x02111;",
          # i for use as a square root of -1
          "ImaginaryI" => "&#x02148;",
          # the geometric imaginary line
          "imagline" => "&#x02110;",
          # alias ISOAMSO image
          "imagpart" => "&#x02111;",
          # alias ISOTECH rArr
          "Implies" => "&#x021D2;",
          # ISOTECH   isin
          "in" => "&#x02208;",
          # the ring of integers
          "integers" => "&#x02124;",
          # alias ISOTECH int
          "Integral" => "&#x0222B;",
          # alias ISOAMSB intcal
          "intercal" => "&#x022BA;",
          # alias ISOAMSB xcap
          "Intersection" => "&#x022C2;",
          # alias ISOAMSB iprod
          "intprod" => "&#x02A3C;",
          # used as a separator, e.g., in indices
          "InvisibleComma" => "&#x02063;",
          # marks multiplication when it is understood without a mark
          "InvisibleTimes" => "&#x02062;",
          # alias ISOTECH lang
          "langle" => "&#x02329;",
          # Laplace transform
          "Laplacetrf" => "&#x02112;",
          # alias ISONUM lcub
          "lbrace" => "&#x0007B;",
          # alias ISONUM lsqb
          "lbrack" => "&#x0005B;",
          # alias ISOTECH lang
          "LeftAngleBracket" => "&#x02329;",
          # alias ISONUM larr
          "LeftArrow" => "&#x02190;",
          # alias ISOTECH lArr
          "Leftarrow" => "&#x021D0;",
          # alias ISONUM larr
          "leftarrow" => "&#x02190;",
          # alias for larrb
          "LeftArrowBar" => "&#x021E4;",
          # alias ISOAMSA lrarr
          "LeftArrowRightArrow" => "&#x021C6;",
          # alias ISOAMSA larrtl
          "leftarrowtail" => "&#x021A2;",
          # alias ISOAMSC lceil
          "LeftCeiling" => "&#x02308;",
          # left double bracket delimiter
          "LeftDoubleBracket" => "&#x0301A;",
          # alias ISOAMSA dharl
          "LeftDownVector" => "&#x021C3;",
          # alias ISOAMSC lfloor
          "LeftFloor" => "&#x0230A;",
          # alias ISOAMSA lhard
          "leftharpoondown" => "&#x021BD;",
          # alias ISOAMSA lharu
          "leftharpoonup" => "&#x021BC;",
          # alias ISOAMSA llarr
          "leftleftarrows" => "&#x021C7;",
          # alias ISOAMSA harr
          "LeftRightArrow" => "&#x02194;",
          # alias ISOAMSA hArr
          "Leftrightarrow" => "&#x021D4;",
          # alias ISOAMSA harr
          "leftrightarrow" => "&#x02194;",
          # alias ISOAMSA lrarr
          "leftrightarrows" => "&#x021C6;",
          # alias ISOAMSA lrhar
          "leftrightharpoons" => "&#x021CB;",
          # alias ISOAMSA harrw
          "leftrightsquigarrow" => "&#x021AD;",
          # alias ISOAMSR dashv
          "LeftTee" => "&#x022A3;",
          # alias for mapstoleft
          "LeftTeeArrow" => "&#x021A4;",
          # alias ISOAMSB lthree
          "leftthreetimes" => "&#x022CB;",
          # alias ISOAMSR vltri
          "LeftTriangle" => "&#x022B2;",
          # alias ISOAMSR ltrie
          "LeftTriangleEqual" => "&#x022B4;",
          # alias ISOAMSA uharl
          "LeftUpVector" => "&#x021BF;",
          # alias ISOAMSA lharu
          "LeftVector" => "&#x021BC;",
          # alias ISOTECH le
          "leq" => "&#x02264;",
          # alias ISOAMSR lE
          "leqq" => "&#x02266;",
          # alias ISOAMSR les
          "leqslant" => "&#x02A7D;",
          # alias ISOAMSR lap
          "lessapprox" => "&#x02A85;",
          # alias ISOAMSR ltdot
          "lessdot" => "&#x022D6;",
          # alias ISOAMSR leg
          "lesseqgtr" => "&#x022DA;",
          # alias ISOAMSR lEg
          "lesseqqgtr" => "&#x02A8B;",
          # alias ISOAMSR leg
          "LessEqualGreater" => "&#x022DA;",
          # alias ISOAMSR lE
          "LessFullEqual" => "&#x02266;",
          # alias ISOAMSR lg
          "LessGreater" => "&#x02276;",
          # alias ISOAMSR lg
          "lessgtr" => "&#x02276;",
          # alias ISOAMSR lsim
          "lesssim" => "&#x02272;",
          # alias ISOAMSR les
          "LessSlantEqual" => "&#x02A7D;",
          # alias ISOAMSR lsim
          "LessTilde" => "&#x02272;",
          # alias ISOAMSR Lt
          "ll" => "&#x0226A;",
          # alias ISOAMSC dlcorn
          "llcorner" => "&#x0231E;",
          # alias ISOAMSA lAarr
          "Lleftarrow" => "&#x021DA;",
          # alias ISOAMSC lmoust
          "lmoustache" => "&#x023B0;",
          # alias ISOAMSN lnap
          "lnapprox" => "&#x02A89;",
          # alias ISOAMSN lne
          "lneq" => "&#x02A87;",
          # alias ISOAMSN lnE
          "lneqq" => "&#x02268;",
          # alias ISOAMSA xlarr
          "LongLeftArrow" => "&#x027F5;",
          # alias ISOAMSA xlArr
          "Longleftarrow" => "&#x027F8;",
          # alias ISOAMSA xlarr
          "longleftarrow" => "&#x027F5;",
          # alias ISOAMSA xharr
          "LongLeftRightArrow" => "&#x027F7;",
          # alias ISOAMSA xhArr
          "Longleftrightarrow" => "&#x027FA;",
          # alias ISOAMSA xharr
          "longleftrightarrow" => "&#x027F7;",
          # alias ISOAMSA xmap
          "longmapsto" => "&#x027FC;",
          # alias ISOAMSA xrarr
          "LongRightArrow" => "&#x027F6;",
          # alias ISOAMSA xrArr
          "Longrightarrow" => "&#x027F9;",
          # alias ISOAMSA xrarr
          "longrightarrow" => "&#x027F6;",
          # alias ISOAMSA larrlp
          "looparrowleft" => "&#x021AB;",
          # alias ISOAMSA rarrlp
          "looparrowright" => "&#x021AC;",
          # alias ISOAMSA swarr
          "LowerLeftArrow" => "&#x02199;",
          # alias ISOAMSA searr
          "LowerRightArrow" => "&#x02198;",
          # alias ISOPUB loz
          "lozenge" => "&#x025CA;",
          # alias ISOAMSC drcorn
          "lrcorner" => "&#x0231F;",
          # alias ISOAMSA lsh
          "Lsh" => "&#x021B0;",
          # alias ISOAMSN lvnE
          "lvertneqq" => "&#x02268;&#x0FE00;",
          # alias ISOPUB malt
          "maltese" => "&#x02720;",
          # alias ISOAMSA map
          "mapsto" => "&#x021A6;",
          # alias ISOAMSO angmsd
          "measuredangle" => "&#x02221;",
          # Mellin transform
          "Mellintrf" => "&#x02133;",
          # alias ISOTECH mnplus
          "MinusPlus" => "&#x02213;",
          # alias ISOTECH mnplus
          "mp" => "&#x02213;",
          # alias ISOAMSA mumap
          "multimap" => "&#x022B8;",
          # alias ISOAMSN nap
          "napprox" => "&#x02249;",
          # alias ISOPUB natur
          "natural" => "&#x0266E;",
          # the semi-ring of natural numbers
          "naturals" => "&#x02115;",
          # alias ISOAMSA nearr
          "nearrow" => "&#x02197;",
          # space of width -4/18 em
          "NegativeMediumSpace" => "&#x0200B;",
          # space of width -5/18 em
          "NegativeThickSpace" => "&#x0200B;",
          # space of width -3/18 em
          "NegativeThinSpace" => "&#x0200B;",
          # space of width -1/18 em
          "NegativeVeryThinSpace" => "&#x0200B;",
          # alias ISOAMSR Gt
          "NestedGreaterGreater" => "&#x0226B;",
          # alias ISOAMSR Lt
          "NestedLessLess" => "&#x0226A;",
          # alias ISOAMSO nexist
          "nexists" => "&#x02204;",
          # alias ISOAMSN nge
          "ngeq" => "&#x02271;",
          # alias ISOAMSN ngE
          "ngeqq" => "&#x02267;&#x00338;",
          # alias ISOAMSN nges
          "ngeqslant" => "&#x02A7E;&#x00338;",
          # alias ISOAMSN ngt
          "ngtr" => "&#x0226F;",
          # alias ISOAMSA nlArr
          "nLeftarrow" => "&#x021CD;",
          # alias ISOAMSA nlarr
          "nleftarrow" => "&#x0219A;",
          # alias ISOAMSA nhArr
          "nLeftrightarrow" => "&#x021CE;",
          # alias ISOAMSA nharr
          "nleftrightarrow" => "&#x021AE;",
          # alias ISOAMSN nle
          "nleq" => "&#x02270;",
          # alias ISOAMSN nlE
          "nleqq" => "&#x02266;&#x00338;",
          # alias ISOAMSN nles
          "nleqslant" => "&#x02A7D;&#x00338;",
          # alias ISOAMSN nlt
          "nless" => "&#x0226E;",
          # alias ISONUM nbsp
          "NonBreakingSpace" => "&#x000A0;",
          # alias ISOAMSN nequiv
          "NotCongruent" => "&#x02262;",
          # alias ISOAMSN npar
          "NotDoubleVerticalBar" => "&#x02226;",
          # alias ISOTECH notin
          "NotElement" => "&#x02209;",
          # alias ISOTECH ne
          "NotEqual" => "&#x02260;",
          # alias for  &nesim;
          "NotEqualTilde" => "&#x02242;&#x00338;",
          # alias ISOAMSO nexist
          "NotExists" => "&#x02204;",
          # alias ISOAMSN ngt
          "NotGreater" => "&#x0226F;",
          # alias ISOAMSN nge
          "NotGreaterEqual" => "&#x02271;",
          # alias ISOAMSN nlE
          "NotGreaterFullEqual" => "&#x02266;&#x00338;",
          # alias ISOAMSN nGtv
          "NotGreaterGreater" => "&#x0226B;&#x00338;",
          # alias ISOAMSN ntvgl
          "NotGreaterLess" => "&#x02279;",
          # alias ISOAMSN nges
          "NotGreaterSlantEqual" => "&#x02A7E;&#x00338;",
          # alias ISOAMSN ngsim
          "NotGreaterTilde" => "&#x02275;",
          # alias for &nbump;
          "NotHumpDownHump" => "&#x0224E;&#x00338;",
          # alias ISOAMSN nltri
          "NotLeftTriangle" => "&#x022EA;",
          # alias ISOAMSN nltrie
          "NotLeftTriangleEqual" => "&#x022EC;",
          # alias ISOAMSN nlt
          "NotLess" => "&#x0226E;",
          # alias ISOAMSN nle
          "NotLessEqual" => "&#x02270;",
          # alias ISOAMSN ntvlg
          "NotLessGreater" => "&#x02278;",
          # alias ISOAMSN nLtv
          "NotLessLess" => "&#x0226A;&#x00338;",
          # alias ISOAMSN nles
          "NotLessSlantEqual" => "&#x02A7D;&#x00338;",
          # alias ISOAMSN nlsim
          "NotLessTilde" => "&#x02274;",
          # alias ISOAMSN npr
          "NotPrecedes" => "&#x02280;",
          # alias ISOAMSN npre
          "NotPrecedesEqual" => "&#x02AAF;&#x00338;",
          # alias ISOAMSN nprcue
          "NotPrecedesSlantEqual" => "&#x022E0;",
          # alias ISOTECH notniva
          "NotReverseElement" => "&#x0220C;",
          # alias ISOAMSN nrtri
          "NotRightTriangle" => "&#x022EB;",
          # alias ISOAMSN nrtrie
          "NotRightTriangleEqual" => "&#x022ED;",
          # alias ISOAMSN nsqsube
          "NotSquareSubsetEqual" => "&#x022E2;",
          # alias ISOAMSN nsqsupe
          "NotSquareSupersetEqual" => "&#x022E3;",
          # alias ISOAMSN vnsub
          "NotSubset" => "&#x02282;&#x020D2;",
          # alias ISOAMSN nsube
          "NotSubsetEqual" => "&#x02288;",
          # alias ISOAMSN nsc
          "NotSucceeds" => "&#x02281;",
          # alias ISOAMSN nsce
          "NotSucceedsEqual" => "&#x02AB0;&#x00338;",
          # alias ISOAMSN nsccue
          "NotSucceedsSlantEqual" => "&#x022E1;",
          # alias ISOAMSN vnsup
          "NotSuperset" => "&#x02283;&#x020D2;",
          # alias ISOAMSN nsupe
          "NotSupersetEqual" => "&#x02289;",
          # alias ISOAMSN nsim
          "NotTilde" => "&#x02241;",
          # alias ISOAMSN nsime
          "NotTildeEqual" => "&#x02244;",
          # alias ISOAMSN ncong
          "NotTildeFullEqual" => "&#x02247;",
          # alias ISOAMSN nap
          "NotTildeTilde" => "&#x02249;",
          # alias ISOAMSN nmid
          "NotVerticalBar" => "&#x02224;",
          # alias ISOAMSN npar
          "nparallel" => "&#x02226;",
          # alias ISOAMSN npr
          "nprec" => "&#x02280;",
          # alias ISOAMSN npre
          "npreceq" => "&#x02AAF;&#x00338;",
          # alias ISOAMSA nrArr
          "nRightarrow" => "&#x021CF;",
          # alias ISOAMSA nrarr
          "nrightarrow" => "&#x0219B;",
          # alias ISOAMSN nsmid
          "nshortmid" => "&#x02224;",
          # alias ISOAMSN nspar
          "nshortparallel" => "&#x02226;",
          # alias ISOAMSN nsime
          "nsimeq" => "&#x02244;",
          # alias ISOAMSN vnsub
          "nsubset" => "&#x02282;&#x020D2;",
          # alias ISOAMSN nsube
          "nsubseteq" => "&#x02288;",
          # alias ISOAMSN nsubE
          "nsubseteqq" => "&#x02AC5;&#x00338;",
          # alias ISOAMSN nsc
          "nsucc" => "&#x02281;",
          # alias ISOAMSN nsce
          "nsucceq" => "&#x02AB0;&#x00338;",
          # alias ISOAMSN vnsup
          "nsupset" => "&#x02283;&#x020D2;",
          # alias ISOAMSN nsupe
          "nsupseteq" => "&#x02289;",
          # alias ISOAMSN nsupE
          "nsupseteqq" => "&#x02AC6;&#x00338;",
          # alias ISOAMSN nltri
          "ntriangleleft" => "&#x022EA;",
          # alias ISOAMSN nltrie
          "ntrianglelefteq" => "&#x022EC;",
          # alias ISOAMSN nrtri
          "ntriangleright" => "&#x022EB;",
          # alias ISOAMSN nrtrie
          "ntrianglerighteq" => "&#x022ED;",
          # alias ISOAMSA nwarr
          "nwarrow" => "&#x02196;",
          # alias ISOTECH conint
          "oint" => "&#x0222E;",
          # alias ISONUM ldquo
          "OpenCurlyDoubleQuote" => "&#x0201C;",
          # alias ISONUM lsquo
          "OpenCurlyQuote" => "&#x02018;",
          # alias ISOTECH order
          "orderof" => "&#x02134;",
          # alias ISOTECH par
          "parallel" => "&#x02225;",
          # alias ISOTECH part
          "PartialD" => "&#x02202;",
          # alias ISOAMSR fork
          "pitchfork" => "&#x022D4;",
          # alias ISONUM plusmn
          "PlusMinus" => "&#x000B1;",
          # alias ISONUM plusmn
          "pm" => "&#x000B1;",
          # the Poincare upper half-plane
          "Poincareplane" => "&#x0210C;",
          # alias ISOAMSR pr
          "prec" => "&#x0227A;",
          # alias ISOAMSR prap
          "precapprox" => "&#x02AB7;",
          # alias ISOAMSR prcue
          "preccurlyeq" => "&#x0227C;",
          # alias ISOAMSR pr
          "Precedes" => "&#x0227A;",
          # alias ISOAMSR pre
          "PrecedesEqual" => "&#x02AAF;",
          # alias ISOAMSR prcue
          "PrecedesSlantEqual" => "&#x0227C;",
          # alias ISOAMSR prsim
          "PrecedesTilde" => "&#x0227E;",
          # alias ISOAMSR pre
          "preceq" => "&#x02AAF;",
          # alias ISOAMSN prnap
          "precnapprox" => "&#x02AB9;",
          # alias ISOAMSN prnE
          "precneqq" => "&#x02AB5;",
          # alias ISOAMSN prnsim
          "precnsim" => "&#x022E8;",
          # alias ISOAMSR prsim
          "precsim" => "&#x0227E;",
          # the prime natural numbers
          "primes" => "&#x02119;",
          # alias ISOAMSR Colon
          "Proportion" => "&#x02237;",
          # alias ISOTECH prop
          "Proportional" => "&#x0221D;",
          # alias ISOTECH prop
          "propto" => "&#x0221D;",
          # the ring (skew field) of quaternions
          "quaternions" => "&#x0210D;",
          # alias ISOAMSR equest
          "questeq" => "&#x0225F;",
          # alias ISOTECH rang
          "rangle" => "&#x0232A;",
          # the field of rational numbers
          "rationals" => "&#x0211A;",
          # alias ISONUM rcub
          "rbrace" => "&#x0007D;",
          # alias ISONUM rsqb
          "rbrack" => "&#x0005D;",
          # alias ISOAMSO real
          "Re" => "&#x0211C;",
          # the geometric real line
          "realine" => "&#x0211B;",
          # alias ISOAMSO real
          "realpart" => "&#x0211C;",
          # the field of real numbers
          "reals" => "&#x0211D;",
          # alias ISOTECH niv
          "ReverseElement" => "&#x0220B;",
          # alias ISOAMSA lrhar
          "ReverseEquilibrium" => "&#x021CB;",
          # alias ISOAMSA duhar
          "ReverseUpEquilibrium" => "&#x0296F;",
          # alias ISOTECH rang
          "RightAngleBracket" => "&#x0232A;",
          # alias ISONUM rarr
          "RightArrow" => "&#x02192;",
          # alias ISOTECH rArr
          "Rightarrow" => "&#x021D2;",
          # alias ISONUM rarr
          "rightarrow" => "&#x02192;",
          # alias for rarrb
          "RightArrowBar" => "&#x021E5;",
          # alias ISOAMSA rlarr
          "RightArrowLeftArrow" => "&#x021C4;",
          # alias ISOAMSA rarrtl
          "rightarrowtail" => "&#x021A3;",
          # alias ISOAMSC rceil
          "RightCeiling" => "&#x02309;",
          # right double bracket delimiter
          "RightDoubleBracket" => "&#x0301B;",
          # alias ISOAMSA dharr
          "RightDownVector" => "&#x021C2;",
          # alias ISOAMSC rfloor
          "RightFloor" => "&#x0230B;",
          # alias ISOAMSA rhard
          "rightharpoondown" => "&#x021C1;",
          # alias ISOAMSA rharu
          "rightharpoonup" => "&#x021C0;",
          # alias ISOAMSA rlarr
          "rightleftarrows" => "&#x021C4;",
          # alias ISOAMSA rlhar
          "rightleftharpoons" => "&#x021CC;",
          # alias ISOAMSA rrarr
          "rightrightarrows" => "&#x021C9;",
          # alias ISOAMSA rarrw
          "rightsquigarrow" => "&#x0219D;",
          # alias ISOAMSR vdash
          "RightTee" => "&#x022A2;",
          # alias ISOAMSA map
          "RightTeeArrow" => "&#x021A6;",
          # alias ISOAMSB rthree
          "rightthreetimes" => "&#x022CC;",
          # alias ISOAMSR vrtri
          "RightTriangle" => "&#x022B3;",
          # alias ISOAMSR rtrie
          "RightTriangleEqual" => "&#x022B5;",
          # alias ISOAMSA uharr
          "RightUpVector" => "&#x021BE;",
          # alias ISOAMSA rharu
          "RightVector" => "&#x021C0;",
          # alias ISOAMSR erDot
          "risingdotseq" => "&#x02253;",
          # alias ISOAMSC rmoust
          "rmoustache" => "&#x023B1;",
          # alias ISOAMSA rAarr
          "Rrightarrow" => "&#x021DB;",
          # alias ISOAMSA rsh
          "Rsh" => "&#x021B1;",
          # alias ISOAMSA searr
          "searrow" => "&#x02198;",
          # alias ISOAMSB setmn
          "setminus" => "&#x02216;",
          # short down arrow
          "ShortDownArrow" => "&#x02193;",
          # alias ISOAMSA slarr
          "ShortLeftArrow" => "&#x02190;",
          # alias ISOAMSR smid
          "shortmid" => "&#x02223;",
          # alias ISOAMSR spar
          "shortparallel" => "&#x02225;",
          # alias ISOAMSA srarr
          "ShortRightArrow" => "&#x02192;",
          # short up arrow
          "ShortUpArrow" => "&#x02191;",
          # alias ISOTECH sime
          "simeq" => "&#x02243;",
          # alias ISOTECH compfn
          "SmallCircle" => "&#x02218;",
          # alias ISOAMSB ssetmn
          "smallsetminus" => "&#x02216;",
          # ISOPUB    spades
          "spadesuit" => "&#x02660;",
          # alias ISOTECH radic
          "Sqrt" => "&#x0221A;",
          # alias ISOAMSR sqsub
          "sqsubset" => "&#x0228F;",
          # alias ISOAMSR sqsube
          "sqsubseteq" => "&#x02291;",
          # alias ISOAMSR sqsup
          "sqsupset" => "&#x02290;",
          # alias ISOAMSR sqsupe
          "sqsupseteq" => "&#x02292;",
          # alias for square
          "Square" => "&#x025A1;",
          # alias ISOAMSB sqcap
          "SquareIntersection" => "&#x02293;",
          # alias ISOAMSR sqsub
          "SquareSubset" => "&#x0228F;",
          # alias ISOAMSR sqsube
          "SquareSubsetEqual" => "&#x02291;",
          # alias ISOAMSR sqsup
          "SquareSuperset" => "&#x02290;",
          # alias ISOAMSR sqsupe
          "SquareSupersetEqual" => "&#x02292;",
          # alias ISOAMSB sqcup
          "SquareUnion" => "&#x02294;",
          # alias ISOAMSB sstarf
          "Star" => "&#x022C6;",
          # alias ISOGRK3 epsi
          "straightepsilon" => "&#x003F5;",
          # alias ISOGRK3 phi
          "straightphi" => "&#x003D5;",
          # alias ISOAMSR Sub
          "Subset" => "&#x022D0;",
          # alias ISOTECH sub
          "subset" => "&#x02282;",
          # alias ISOTECH sube
          "subseteq" => "&#x02286;",
          # alias ISOAMSR subE
          "subseteqq" => "&#x02AC5;",
          # alias ISOTECH sube
          "SubsetEqual" => "&#x02286;",
          # alias ISOAMSN subne
          "subsetneq" => "&#x0228A;",
          # alias ISOAMSN subnE
          "subsetneqq" => "&#x02ACB;",
          # alias ISOAMSR sc
          "succ" => "&#x0227B;",
          # alias ISOAMSR scap
          "succapprox" => "&#x02AB8;",
          # alias ISOAMSR sccue
          "succcurlyeq" => "&#x0227D;",
          # alias ISOAMSR sc
          "Succeeds" => "&#x0227B;",
          # alias ISOAMSR sce
          "SucceedsEqual" => "&#x02AB0;",
          # alias ISOAMSR sccue
          "SucceedsSlantEqual" => "&#x0227D;",
          # alias ISOAMSR scsim
          "SucceedsTilde" => "&#x0227F;",
          # alias ISOAMSR sce
          "succeq" => "&#x02AB0;",
          # alias ISOAMSN scnap
          "succnapprox" => "&#x02ABA;",
          # alias ISOAMSN scnE
          "succneqq" => "&#x02AB6;",
          # alias ISOAMSN scnsim
          "succnsim" => "&#x022E9;",
          # alias ISOAMSR scsim
          "succsim" => "&#x0227F;",
          # ISOTECH  ni
          "SuchThat" => "&#x0220B;",
          # alias ISOAMSB sum
          "Sum" => "&#x02211;",
          # alias ISOTECH sup
          "Superset" => "&#x02283;",
          # alias ISOTECH supe
          "SupersetEqual" => "&#x02287;",
          # alias ISOAMSR Sup
          "Supset" => "&#x022D1;",
          # alias ISOTECH sup
          "supset" => "&#x02283;",
          # alias ISOTECH supe
          "supseteq" => "&#x02287;",
          # alias ISOAMSR supE
          "supseteqq" => "&#x02AC6;",
          # alias ISOAMSN supne
          "supsetneq" => "&#x0228B;",
          # alias ISOAMSN supnE
          "supsetneqq" => "&#x02ACC;",
          # alias ISOAMSA swarr
          "swarrow" => "&#x02199;",
          # alias ISOTECH there4
          "Therefore" => "&#x02234;",
          # alias ISOTECH there4
          "therefore" => "&#x02234;",
          # ISOAMSR   thkap
          "thickapprox" => "&#x02248;",
          # ISOAMSR   thksim
          "thicksim" => "&#x0223C;",
          # space of width 3/18 em alias ISOPUB thinsp
          "ThinSpace" => "&#x02009;",
          # alias ISOTECH sim
          "Tilde" => "&#x0223C;",
          # alias ISOTECH sime
          "TildeEqual" => "&#x02243;",
          # alias ISOTECH cong
          "TildeFullEqual" => "&#x02245;",
          # alias ISOTECH ap
          "TildeTilde" => "&#x02248;",
          # alias ISOAMSA nesear
          "toea" => "&#x02928;",
          # alias ISOAMSA seswar
          "tosa" => "&#x02929;",
          # alias ISOPUB utri
          "triangle" => "&#x025B5;",
          # alias ISOPUB dtri
          "triangledown" => "&#x025BF;",
          # alias ISOPUB ltri
          "triangleleft" => "&#x025C3;",
          # alias ISOAMSR ltrie
          "trianglelefteq" => "&#x022B4;",
          # alias ISOAMSR trie
          "triangleq" => "&#x0225C;",
          # alias ISOPUB rtri
          "triangleright" => "&#x025B9;",
          # alias ISOAMSR rtrie
          "trianglerighteq" => "&#x022B5;",
          # alias ISOAMSA Larr
          "twoheadleftarrow" => "&#x0219E;",
          # alias ISOAMSA Rarr
          "twoheadrightarrow" => "&#x021A0;",
          # alias ISOAMSC ulcorn
          "ulcorner" => "&#x0231C;",
          # alias ISOAMSB xcup
          "Union" => "&#x022C3;",
          # alias ISOAMSB uplus
          "UnionPlus" => "&#x0228E;",
          # alias ISONUM uarr
          "UpArrow" => "&#x02191;",
          # alias ISOAMSA uArr
          "Uparrow" => "&#x021D1;",
          # alias ISONUM uarr
          "uparrow" => "&#x02191;",
          # alias ISOAMSA udarr
          "UpArrowDownArrow" => "&#x021C5;",
          # alias ISOAMSA varr
          "UpDownArrow" => "&#x02195;",
          # alias ISOAMSA vArr
          "Updownarrow" => "&#x021D5;",
          # alias ISOAMSA varr
          "updownarrow" => "&#x02195;",
          # alias ISOAMSA udhar
          "UpEquilibrium" => "&#x0296E;",
          # alias ISOAMSA uharl
          "upharpoonleft" => "&#x021BF;",
          # alias ISOAMSA uharr
          "upharpoonright" => "&#x021BE;",
          # alias ISOAMSA nwarr
          "UpperLeftArrow" => "&#x02196;",
          # alias ISOAMSA nearr
          "UpperRightArrow" => "&#x02197;",
          # alias ISOGRK3 upsi
          "upsilon" => "&#x003C5;",
          # alias ISOTECH perp
          "UpTee" => "&#x022A5;",
          # Alias mapstoup
          "UpTeeArrow" => "&#x021A5;",
          # alias ISOAMSA uuarr
          "upuparrows" => "&#x021C8;",
          # alias ISOAMSC urcorn
          "urcorner" => "&#x0231D;",
          # alias ISOGRK3 epsiv
          "varepsilon" => "&#x003B5;",
          # alias ISOGRK3 kappav
          "varkappa" => "&#x003F0;",
          # alias ISOAMSO emptyv
          "varnothing" => "&#x02205;",
          # alias ISOGRK3 phiv
          "varphi" => "&#x003C6;",
          # alias ISOGRK3 piv
          "varpi" => "&#x003D6;",
          # alias ISOAMSR vprop
          "varpropto" => "&#x0221D;",
          # alias ISOGRK3 rhov
          "varrho" => "&#x003F1;",
          # alias ISOGRK3 sigmav
          "varsigma" => "&#x003C2;",
          # alias ISOAMSN vsubne
          "varsubsetneq" => "&#x0228A;&#x0FE00;",
          # alias ISOAMSN vsubnE
          "varsubsetneqq" => "&#x02ACB;&#x0FE00;",
          # alias ISOAMSN vsupne
          "varsupsetneq" => "&#x0228B;&#x0FE00;",
          # alias ISOAMSN vsupnE
          "varsupsetneqq" => "&#x02ACC;&#x0FE00;",
          # alias ISOGRK3 thetav
          "vartheta" => "&#x003D1;",
          # alias ISOAMSR vltri
          "vartriangleleft" => "&#x022B2;",
          # alias ISOAMSR vrtri
          "vartriangleright" => "&#x022B3;",
          # alias ISOAMSB xvee
          "Vee" => "&#x022C1;",
          # alias ISOTECH or
          "vee" => "&#x02228;",
          # alias ISOTECH Verbar
          "Vert" => "&#x02016;",
          # alias ISONUM verbar
          "vert" => "&#x0007C;",
          # alias ISOAMSR mid
          "VerticalBar" => "&#x02223;",
          # alias ISOAMSB wreath
          "VerticalTilde" => "&#x02240;",
          # space of width 1/18 em alias ISOPUB hairsp
          "VeryThinSpace" => "&#x0200A;",
          # alias ISOAMSB xwedge
          "Wedge" => "&#x022C0;",
          # alias ISOTECH and
          "wedge" => "&#x02227;",
          # alias ISOAMSO weierp
          "wp" => "&#x02118;",
          # alias ISOAMSB wreath
          "wr" => "&#x02240;",
          # zee transform
          "zeetrf" => "&#x02128;",
          # character showing function application in presentation tagging
          "af" => "&#x02061;",
          # 
          "aopf" => "&#x1D552;",
          # Old ISOAMSR asymp (for HTML compatibility)
          "asympeq" => "&#x0224D;",
          # 
          "bopf" => "&#x1D553;",
          # 
          "copf" => "&#x1D554;",
          # cross or vector product
          "Cross" => "&#x02A2F;",
          # D for use in differentials, e.g., within integrals
          "DD" => "&#x02145;",
          # d for use in differentials, e.g., within integrals
          "dd" => "&#x02146;",
          # 
          "dopf" => "&#x1D555;",
          # down arrow to bar
          "DownArrowBar" => "&#x02913;",
          # left-down-right-down harpoon
          "DownLeftRightVector" => "&#x02950;",
          # left-down harpoon from bar
          "DownLeftTeeVector" => "&#x0295E;",
          # left-down harpoon to bar
          "DownLeftVectorBar" => "&#x02956;",
          # right-down harpoon from bar
          "DownRightTeeVector" => "&#x0295F;",
          # right-down harpoon to bar
          "DownRightVectorBar" => "&#x02957;",
          # e use for the exponential base of the natural logarithms
          "ee" => "&#x02147;",
          # empty small square
          "EmptySmallSquare" => "&#x025FB;",
          # empty small square
          "EmptyVerySmallSquare" => "&#x025AB;",
          # 
          "eopf" => "&#x1D556;",
          # two consecutive equal signs
          "Equal" => "&#x02A75;",
          # filled small square
          "FilledSmallSquare" => "&#x025FC;",
          # filled very small square
          "FilledVerySmallSquare" => "&#x025AA;",
          # 
          "fopf" => "&#x1D557;",
          # 
          "gopf" => "&#x1D558;",
          # alias for GT
          "GreaterGreater" => "&#x02AA2;",
          # circumflex accent
          "Hat" => "&#x0005E;",
          # 
          "hopf" => "&#x1D559;",
          # short horizontal line
          "HorizontalLine" => "&#x02500;",
          # short form of  &InvisibleComma;
          "ic" => "&#x02063;",
          # i for use as a square root of -1
          "ii" => "&#x02148;",
          # 
          "iopf" => "&#x1D55A;",
          # marks multiplication when it is understood without a mark
          "it" => "&#x02062;",
          # 
          "jopf" => "&#x1D55B;",
          # 
          "kopf" => "&#x1D55C;",
          # leftwards arrow to bar
          "larrb" => "&#x021E4;",
          # down-left harpoon from bar
          "LeftDownTeeVector" => "&#x02961;",
          # down-left harpoon to bar
          "LeftDownVectorBar" => "&#x02959;",
          # left-up-right-up harpoon
          "LeftRightVector" => "&#x0294E;",
          # left-up harpoon from bar
          "LeftTeeVector" => "&#x0295A;",
          # left triangle, vertical bar
          "LeftTriangleBar" => "&#x029CF;",
          # up-left-down-left harpoon
          "LeftUpDownVector" => "&#x02951;",
          # up-left harpoon from bar
          "LeftUpTeeVector" => "&#x02960;",
          # up-left harpoon to bar
          "LeftUpVectorBar" => "&#x02958;",
          # left-up harpoon to bar
          "LeftVectorBar" => "&#x02952;",
          # alias for Lt
          "LessLess" => "&#x02AA1;",
          # 
          "lopf" => "&#x1D55D;",
          # downwards arrow from bar
          "mapstodown" => "&#x021A7;",
          # leftwards arrow from bar
          "mapstoleft" => "&#x021A4;",
          # upwards arrow from bar
          "mapstoup" => "&#x021A5;",
          # space of width 4/18 em
          "MediumSpace" => "&#x0205F;",
          # 
          "mopf" => "&#x1D55E;",
          # not bumpy equals
          "nbump" => "&#x0224E;&#x00338;",
          # not bumpy single equals
          "nbumpe" => "&#x0224F;&#x00338;",
          # not equal or similar
          "nesim" => "&#x02242;&#x00338;",
          # force a line break; line feed
          "NewLine" => "&#x0000A;",
          # never break line here
          "NoBreak" => "&#x02060;",
          # 
          "nopf" => "&#x1D55F;",
          # alias for &nasymp;
          "NotCupCap" => "&#x0226D;",
          # alias for &nbumpe;
          "NotHumpEqual" => "&#x0224F;&#x00338;",
          # not left triangle, vertical bar
          "NotLeftTriangleBar" => "&#x029CF;&#x00338;",
          # not double greater-than sign
          "NotNestedGreaterGreater" => "&#x02AA2;&#x00338;",
          # not double less-than sign
          "NotNestedLessLess" => "&#x02AA1;&#x00338;",
          # not vertical bar, right triangle
          "NotRightTriangleBar" => "&#x029D0;&#x00338;",
          # square not subset
          "NotSquareSubset" => "&#x0228F;&#x00338;",
          # negated set-like partial order operator
          "NotSquareSuperset" => "&#x02290;&#x00338;",
          # not succeeds or similar
          "NotSucceedsTilde" => "&#x0227F;&#x00338;",
          # 
          "oopf" => "&#x1D560;",
          # over bar
          "OverBar" => "&#x000AF;",
          # over brace
          "OverBrace" => "&#x0FE37;",
          # over bracket
          "OverBracket" => "&#x023B4;",
          # over parenthesis
          "OverParenthesis" => "&#x0FE35;",
          # the ring (skew field) of quaternions
          "planckh" => "&#x0210E;",
          # 
          "popf" => "&#x1D561;",
          # alias for &prod;
          "Product" => "&#x0220F;",
          # 
          "qopf" => "&#x1D562;",
          # leftwards arrow to bar
          "rarrb" => "&#x021E5;",
          # down-right harpoon from bar
          "RightDownTeeVector" => "&#x0295D;",
          # down-right harpoon to bar
          "RightDownVectorBar" => "&#x02955;",
          # right-up harpoon from bar
          "RightTeeVector" => "&#x0295B;",
          # vertical bar, right triangle
          "RightTriangleBar" => "&#x029D0;",
          # up-right-down-right harpoon
          "RightUpDownVector" => "&#x0294F;",
          # up-right harpoon from bar
          "RightUpTeeVector" => "&#x0295C;",
          # up-right harpoon to bar
          "RightUpVectorBar" => "&#x02954;",
          # up-right harpoon to bar
          "RightVectorBar" => "&#x02953;",
          # 
          "ropf" => "&#x1D563;",
          # round implies
          "RoundImplies" => "&#x02970;",
          # rule-delayed (colon right arrow)
          "RuleDelayed" => "&#x029F4;",
          # 
          "sopf" => "&#x1D564;",
          # tabulator stop; horizontal tabulation
          "Tab" => "&#x00009;",
          # space of width 5/18 em
          "ThickSpace" => "&#x02009;&#x0200A;&#x0200A;",
          # 
          "topf" => "&#x1D565;",
          # under brace
          "UnderBrace" => "&#x0FE38;",
          # under bracket
          "UnderBracket" => "&#x023B5;",
          # under parenthesis
          "UnderParenthesis" => "&#x0FE36;",
          # 
          "uopf" => "&#x1D566;",
          # up arrow to bar
          "UpArrowBar" => "&#x02912;",
          # ISOGRK1 Ugr, HTML4 Upsilon
          "Upsilon" => "&#x003A5;",
          # alias ISONUM verbar
          "VerticalLine" => "&#x0007C;",
          # vertical separating operator
          "VerticalSeparator" => "&#x02758;",
          # 
          "vopf" => "&#x1D567;",
          # 
          "wopf" => "&#x1D568;",
          # 
          "xopf" => "&#x1D569;",
          # 
          "yopf" => "&#x1D56A;",
          # zero width space
          "ZeroWidthSpace" => "&#x0200B;",
          # 
          "zopf" => "&#x1D56B;",
        }
      end
    end
  end
end

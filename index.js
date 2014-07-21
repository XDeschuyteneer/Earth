// Generated by js_of_ocaml 2.3
(function(m){"use strict";var
aE=125,aH=123,bI=254,w=255,bw="x",J=".",bH=108,ah=65535,ai="+",by='"',k=16777215,af="g",aD="f",bv=1073741823,ak=250,B=105,bx="jsError",bF=-88,M=110,bu=246,ae="'",N=115,ad="int_of_string",bB=-32,aL=102,aJ=111,bt="Unix.Unix_error",aG=120,r=" ",L="e",aF=117,A="-",ac=-48,bA="nan",c="",aC=116,aj=100,aM=" : file already exists",f="0",aI="/",ag=114,aK=103,bG="fd ",bE=101,bD="index out of bounds",bz="number",bC=1e3;function
bS(a,b){throw[0,a,b]}function
ap(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
b=m.console;b&&b.error&&b.error(a)}var
d=[0];function
Q(a,b){if(!a)return c;if(a&1)return Q(a-1,b)+b;var
d=Q(a>>1,b);return d+d}function
e(a){if(a!=null){this.bytes=this.fullBytes=a;this.last=this.len=a.length}}function
bT(){bS(d[4],new
e(bD))}e.prototype={string:null,bytes:null,fullBytes:null,array:null,len:null,last:0,toJsString:function(){var
a=this.getFullBytes();try{return this.string=decodeURIComponent(escape(a))}catch(f){ap('MlString.toJsString: wrong encoding for "%s" ',a);return a}},toBytes:function(){if(this.string!=null)try{var
a=unescape(encodeURIComponent(this.string))}catch(f){ap('MlString.toBytes: wrong encoding for "%s" ',this.string);var
a=this.string}else{var
a=c,d=this.array,e=d.length;for(var
b=0;b<e;b++)a+=String.fromCharCode(d[b])}this.bytes=this.fullBytes=a;this.last=this.len=a.length;return a},getBytes:function(){var
a=this.bytes;if(a==null)a=this.toBytes();return a},getFullBytes:function(){var
a=this.fullBytes;if(a!==null)return a;a=this.bytes;if(a==null)a=this.toBytes();if(this.last<this.len){this.bytes=a+=Q(this.len-this.last,"\0");this.last=this.len}this.fullBytes=a;return a},toArray:function(){var
c=this.bytes;if(c==null)c=this.toBytes();var
b=[],d=this.last;for(var
a=0;a<d;a++)b[a]=c.charCodeAt(a);for(d=this.len;a<d;a++)b[a]=0;this.string=this.bytes=this.fullBytes=null;this.last=this.len;this.array=b;return b},getArray:function(){var
a=this.array;if(!a)a=this.toArray();return a},getLen:function(){var
a=this.len;if(a!==null)return a;this.toBytes();return this.len},toString:function(){var
a=this.string;return a?a:this.toJsString()},valueOf:function(){var
a=this.string;return a?a:this.toJsString()},blitToArray:function(a,b,c,d){var
g=this.array;if(g)if(c<=a)for(var
e=0;e<d;e++)b[c+e]=g[a+e];else
for(var
e=d-1;e>=0;e--)b[c+e]=g[a+e];else{var
f=this.bytes;if(f==null)f=this.toBytes();var
h=this.last-a;if(d<=h)for(var
e=0;e<d;e++)b[c+e]=f.charCodeAt(a+e);else{for(var
e=0;e<h;e++)b[c+e]=f.charCodeAt(a+e);for(;e<d;e++)b[c+e]=0}}},get:function(a){var
c=this.array;if(c)return c[a];var
b=this.bytes;if(b==null)b=this.toBytes();return a<this.last?b.charCodeAt(a):0},safeGet:function(a){if(this.len==null)this.toBytes();if(a<0||a>=this.len)bT();return this.get(a)},set:function(a,b){var
c=this.array;if(!c){if(this.last==a){this.bytes+=String.fromCharCode(b&w);this.last++;return 0}c=this.toArray()}else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;c[a]=b&w;return 0},safeSet:function(a,b){if(this.len==null)this.toBytes();if(a<0||a>=this.len)bT();this.set(a,b)},fill:function(a,b,c){if(a>=this.last&&this.last&&c==0)return;var
d=this.array;if(!d)d=this.toArray();else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;var
f=a+b;for(var
e=a;e<f;e++)d[e]=c},compare:function(a){if(this.string!=null&&a.string!=null){if(this.string<a.string)return-1;if(this.string>a.string)return 1;return 0}var
b=this.getFullBytes(),c=a.getFullBytes();if(b<c)return-1;if(b>c)return 1;return 0},equal:function(a){if(this.string!=null&&a.string!=null)return this.string==a.string;return this.getFullBytes()==a.getFullBytes()},lessThan:function(a){if(this.string!=null&&a.string!=null)return this.string<a.string;return this.getFullBytes()<a.getFullBytes()},lessEqual:function(a){if(this.string!=null&&a.string!=null)return this.string<=a.string;return this.getFullBytes()<=a.getFullBytes()}};function
x(a){this.string=a}x.prototype=new
e();function
aQ(a,b){bS(a,new
x(b))}function
P(a){aQ(d[4],a)}function
bL(){P(bD)}function
ea(a,b){if(b<0||b>=a.length-1)bL();return a[b+1]}function
eb(a,b,c){if(b<0||b>=a.length-1)bL();a[b+1]=c;return 0}function
ec(a,b,c,d,e){if(e===0)return;if(a.array!=null&&c.last==0&&d==0&&e==c.len){c.array=a.array.slice(b,b+e);c.bytes=c.string=null;return}if(d===c.last&&c.bytes!=null){var
f=a.bytes;if(f==null)f=a.toBytes();if(b>0||a.last>e)f=f.slice(b,b+e);c.bytes+=f;c.last+=f.length;return}var
g=c.array;if(!g)g=c.toArray();else
c.bytes=c.string=null;a.blitToArray(b,g,d,e)}function
D(c,b){if(c.fun)return D(c.fun,b);var
a=c.length,d=a-b.length;if(d==0)return c.apply(null,b);else
if(d<0)return D(c.apply(null,b.slice(0,a)),b.slice(a));else
return function(a){return D(c,b.concat([a]))}}function
ed(a){if(isFinite(a)){if(Math.abs(a)>=2.22507385850720138e-308)return 0;if(a!=0)return 1;return 2}return isNaN(a)?4:3}function
bJ(a){this.bytes=c;this.len=a}bJ.prototype=new
e();function
ef(a){if(a<0)P("String.create");return new
bJ(a)}function
eh(a,b,c,d){a.fill(b,c,d)}function
aP(a){a=a.toString();var
e=a.length;if(e>31)P("format_int: format too long");var
b={justify:ai,signstyle:A,filler:r,alternate:false,base:0,signedconv:false,width:0,uppercase:false,sign:1,prec:-1,conv:aD};for(var
d=0;d<e;d++){var
c=a.charAt(d);switch(c){case
A:b.justify=A;break;case
ai:case
r:b.signstyle=c;break;case
f:b.filler=f;break;case"#":b.alternate=true;break;case"1":case"2":case"3":case"4":case"5":case"6":case"7":case"8":case"9":b.width=0;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.width=b.width*10+c;d++}d--;break;case
J:b.prec=0;d++;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.prec=b.prec*10+c;d++}d--;case"d":case"i":b.signedconv=true;case"u":b.base=10;break;case
bw:b.base=16;break;case"X":b.base=16;b.uppercase=true;break;case"o":b.base=8;break;case
L:case
aD:case
af:b.signedconv=true;b.conv=c;break;case"E":case"F":case"G":b.signedconv=true;b.uppercase=true;b.conv=c.toLowerCase();break}}return b}function
aN(a,b){if(a.uppercase)b=b.toUpperCase();var
g=b.length;if(a.signedconv&&(a.sign<0||a.signstyle!=A))g++;if(a.alternate){if(a.base==8)g+=1;if(a.base==16)g+=2}var
d=c;if(a.justify==ai&&a.filler==r)for(var
e=g;e<a.width;e++)d+=r;if(a.signedconv)if(a.sign<0)d+=A;else
if(a.signstyle!=A)d+=a.signstyle;if(a.alternate&&a.base==8)d+=f;if(a.alternate&&a.base==16)d+="0x";if(a.justify==ai&&a.filler==f)for(var
e=g;e<a.width;e++)d+=f;d+=b;if(a.justify==A)for(var
e=g;e<a.width;e++)d+=r;return new
x(d)}function
ei(a,b){var
c,g=aP(a),e=g.prec<0?6:g.prec;if(b<0){g.sign=-1;b=-b}if(isNaN(b)){c=bA;g.filler=r}else
if(!isFinite(b)){c="inf";g.filler=r}else
switch(g.conv){case
L:var
c=b.toExponential(e),d=c.length;if(c.charAt(d-3)==L)c=c.slice(0,d-1)+f+c.slice(d-1);break;case
aD:c=b.toFixed(e);break;case
af:e=e?e:1;c=b.toExponential(e-1);var
j=c.indexOf(L),i=+c.slice(j+1);if(i<-4||b.toFixed(0).length>e){var
d=j-1;while(c.charAt(d)==f)d--;if(c.charAt(d)==J)d--;c=c.slice(0,d+1)+c.slice(j);d=c.length;if(c.charAt(d-3)==L)c=c.slice(0,d-1)+f+c.slice(d-1);break}else{var
h=e;if(i<0){h-=i+1;c=b.toFixed(h)}else
while(c=b.toFixed(h),c.length>e+1)h--;if(h){var
d=c.length-1;while(c.charAt(d)==f)d--;if(c.charAt(d)==J)d--;c=c.slice(0,d+1)}}break}return aN(g,c)}function
ej(a,b){if(a.toString()=="%d")return new
x(c+b);var
d=aP(a);if(b<0)if(d.signedconv){d.sign=-1;b=-b}else
b>>>=0;var
e=b.toString(d.base);if(d.prec>=0){d.filler=r;var
g=d.prec-e.length;if(g>0)e=Q(g,f)+e}return aN(d,e)}function
em(a,b){var
c=a[3]<<16,d=b[3]<<16;if(c>d)return 1;if(c<d)return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
ew(a,b){if(a<b)return-1;if(a==b)return 0;return 1}function
ee(a,b,c){var
f=[];for(;;){if(!(c&&a===b))if(a
instanceof
e)if(b
instanceof
e){if(a!==b){var
d=a.compare(b);if(d!=0)return d}}else
return 1;else
if(a
instanceof
Array&&a[0]===(a[0]|0)){var
g=a[0];if(g===bI)g=0;if(g===ak){a=a[1];continue}else
if(b
instanceof
Array&&b[0]===(b[0]|0)){var
h=b[0];if(h===bI)h=0;if(h===ak){b=b[1];continue}else
if(g!=h)return g<h?-1:1;else
switch(g){case
248:var
d=ew(a[2],b[2]);if(d!=0)return d;break;case
251:P("equal: abstract value");case
w:var
d=em(a,b);if(d!=0)return d;break;default:if(a.length!=b.length)return a.length<b.length?-1:1;if(a.length>1)f.push(a,b,1)}}else
return 1}else
if(b
instanceof
e||b
instanceof
Array&&b[0]===(b[0]|0))return-1;else
if(typeof
a!=bz&&a&&a.compare)return a.compare(b,c);else{if(a<b)return-1;if(a>b)return 1;if(a!=b){if(!c)return NaN;if(a==a)return 1;if(b==b)return-1}}if(f.length==0)return 0;var
i=f.pop();b=f.pop();a=f.pop();if(i+1<a.length)f.push(a,b,i+1);a=a[i];b=b[i]}}function
el(a,b){return+(ee(a,b,false)>=0)}function
ep(a){return(a[3]|a[2]|a[1])==0}function
es(a){return[w,a&k,a>>24&k,a>>31&ah]}function
et(a,b){var
c=a[1]-b[1],d=a[2]-b[2]+(c>>24),e=a[3]-b[3]+(d>>24);return[w,c&k,d&k,e&ah]}function
bN(a,b){if(a[3]>b[3])return 1;if(a[3]<b[3])return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
bM(a){a[3]=a[3]<<1|a[2]>>23;a[2]=(a[2]<<1|a[1]>>23)&k;a[1]=a[1]<<1&k}function
eq(a){a[1]=(a[1]>>>1|a[2]<<23)&k;a[2]=(a[2]>>>1|a[3]<<23)&k;a[3]=a[3]>>>1}function
ev(a,b){var
e=0,d=a.slice(),c=b.slice(),f=[w,0,0,0];while(bN(d,c)>0){e++;bM(c)}while(e>=0){e--;bM(f);if(bN(d,c)>=0){f[1]++;d=et(d,c)}eq(c)}return[0,f,d]}function
eu(a){return a[1]|a[2]<<24}function
eo(a){return a[3]<<16<0}function
er(a){var
b=-a[1],c=-a[2]+(b>>24),d=-a[3]+(c>>24);return[w,b&k,c&k,d&ah]}function
en(a,b){var
d=aP(a);if(d.signedconv&&eo(b)){d.sign=-1;b=er(b)}var
e=c,j=es(d.base),i="0123456789abcdef";do{var
h=ev(b,j);b=h[1];e=i.charAt(eu(h[2]))+e}while(!ep(b));if(d.prec>=0){d.filler=r;var
g=d.prec-e.length;if(g>0)e=Q(g,f)+e}return aN(d,e)}function
eR(a){var
b=0,c=10,d=a.get(0)==45?(b++,-1):1;if(a.get(b)==48)switch(a.get(b+1)){case
aG:case
88:c=16;b+=2;break;case
aJ:case
79:c=8;b+=2;break;case
98:case
66:c=2;b+=2;break}return[b,d,c]}function
bP(a){if(a>=48&&a<=57)return a-48;if(a>=65&&a<=90)return a-55;if(a>=97&&a<=122)return a-87;return-1}function
al(a){aQ(d[3],a)}function
ex(a){var
g=eR(a),f=g[0],h=g[1],d=g[2],i=-1>>>0,e=a.get(f),c=bP(e);if(c<0||c>=d)al(ad);var
b=c;for(;;){f++;e=a.get(f);if(e==95)continue;c=bP(e);if(c<0||c>=d)break;b=d*b+c;if(b>i)al(ad)}if(f!=a.getLen())al(ad);b=h*b;if(d==10&&(b|0)!=b)al(ad);return b|0}function
ey(a){return+(a>31&&a<127)}function
ez(a){return a.getFullBytes()}function
eA(){var
b=m.console?m.console:{},c=["log","debug","info","warn","error","assert","dir","dirxml","trace","group","groupCollapsed","groupEnd","time","timeEnd"];function
d(){}for(var
a=0;a<c.length;a++)if(!b[c[a]])b[c[a]]=d;return b}function
eB(a,b){switch(b.length){case
1:return new
a();case
2:return new
a(b[1]);case
3:return new
a(b[1],b[2]);case
4:return new
a(b[1],b[2],b[3]);case
5:return new
a(b[1],b[2],b[3],b[4]);case
6:return new
a(b[1],b[2],b[3],b[4],b[5]);case
7:return new
a(b[1],b[2],b[3],b[4],b[5],b[6]);case
8:return new
a(b[1],b[2],b[3],b[4],b[5],b[6],b[7])}function
c(){return a.apply(this,b.slice(1))}c.prototype=a.prototype;return new
c()}function
eC(a){return a()}function
eD(a){var
a=a.toString();if(!a.match(/^[a-zA-Z_$][a-zA-Z_$0-9]*(\.[a-zA-Z_$][a-zA-Z_$0-9]*)*$/))ap('caml_js_var: "'+a+'" is not a valid JavaScript variable. continuing ..');return eval(a)}function
eE(a){var
c=Array.prototype.slice;return function(){var
b=arguments.length>0?c.call(arguments):[undefined];return D(a,b)}}function
eF(a,b){var
d=[0];for(var
c=1;c<=a;c++)d[c]=b;return d}function
bK(a){var
b=a.length;this.array=a;this.len=this.last=b}bK.prototype=new
e();var
eG=function(){function
m(a,b){return a+b|0}function
l(a,b,c,d,e,f){b=m(m(b,a),m(d,f));return m(b<<e|b>>>32-e,c)}function
h(a,b,c,d,e,f,g){return l(b&c|~b&d,a,b,e,f,g)}function
i(a,b,c,d,e,f,g){return l(b&d|c&~d,a,b,e,f,g)}function
j(a,b,c,d,e,f,g){return l(b^c^d,a,b,e,f,g)}function
k(a,b,c,d,e,f,g){return l(c^(b|~d),a,b,e,f,g)}function
n(a,b){var
g=b;a[g>>2]|=128<<8*(g&3);for(g=(g&~3)+8;(g&63)<60;g+=4)a[(g>>2)-1]=0;a[(g>>2)-1]=b<<3;a[g>>2]=b>>29&536870911;var
l=[1732584193,4023233417,2562383102,271733878];for(g=0;g<a.length;g+=16){var
c=l[0],d=l[1],e=l[2],f=l[3];c=h(c,d,e,f,a[g+0],7,3614090360);f=h(f,c,d,e,a[g+1],12,3905402710);e=h(e,f,c,d,a[g+2],17,606105819);d=h(d,e,f,c,a[g+3],22,3250441966);c=h(c,d,e,f,a[g+4],7,4118548399);f=h(f,c,d,e,a[g+5],12,1200080426);e=h(e,f,c,d,a[g+6],17,2821735955);d=h(d,e,f,c,a[g+7],22,4249261313);c=h(c,d,e,f,a[g+8],7,1770035416);f=h(f,c,d,e,a[g+9],12,2336552879);e=h(e,f,c,d,a[g+10],17,4294925233);d=h(d,e,f,c,a[g+11],22,2304563134);c=h(c,d,e,f,a[g+12],7,1804603682);f=h(f,c,d,e,a[g+13],12,4254626195);e=h(e,f,c,d,a[g+14],17,2792965006);d=h(d,e,f,c,a[g+15],22,1236535329);c=i(c,d,e,f,a[g+1],5,4129170786);f=i(f,c,d,e,a[g+6],9,3225465664);e=i(e,f,c,d,a[g+11],14,643717713);d=i(d,e,f,c,a[g+0],20,3921069994);c=i(c,d,e,f,a[g+5],5,3593408605);f=i(f,c,d,e,a[g+10],9,38016083);e=i(e,f,c,d,a[g+15],14,3634488961);d=i(d,e,f,c,a[g+4],20,3889429448);c=i(c,d,e,f,a[g+9],5,568446438);f=i(f,c,d,e,a[g+14],9,3275163606);e=i(e,f,c,d,a[g+3],14,4107603335);d=i(d,e,f,c,a[g+8],20,1163531501);c=i(c,d,e,f,a[g+13],5,2850285829);f=i(f,c,d,e,a[g+2],9,4243563512);e=i(e,f,c,d,a[g+7],14,1735328473);d=i(d,e,f,c,a[g+12],20,2368359562);c=j(c,d,e,f,a[g+5],4,4294588738);f=j(f,c,d,e,a[g+8],11,2272392833);e=j(e,f,c,d,a[g+11],16,1839030562);d=j(d,e,f,c,a[g+14],23,4259657740);c=j(c,d,e,f,a[g+1],4,2763975236);f=j(f,c,d,e,a[g+4],11,1272893353);e=j(e,f,c,d,a[g+7],16,4139469664);d=j(d,e,f,c,a[g+10],23,3200236656);c=j(c,d,e,f,a[g+13],4,681279174);f=j(f,c,d,e,a[g+0],11,3936430074);e=j(e,f,c,d,a[g+3],16,3572445317);d=j(d,e,f,c,a[g+6],23,76029189);c=j(c,d,e,f,a[g+9],4,3654602809);f=j(f,c,d,e,a[g+12],11,3873151461);e=j(e,f,c,d,a[g+15],16,530742520);d=j(d,e,f,c,a[g+2],23,3299628645);c=k(c,d,e,f,a[g+0],6,4096336452);f=k(f,c,d,e,a[g+7],10,1126891415);e=k(e,f,c,d,a[g+14],15,2878612391);d=k(d,e,f,c,a[g+5],21,4237533241);c=k(c,d,e,f,a[g+12],6,1700485571);f=k(f,c,d,e,a[g+3],10,2399980690);e=k(e,f,c,d,a[g+10],15,4293915773);d=k(d,e,f,c,a[g+1],21,2240044497);c=k(c,d,e,f,a[g+8],6,1873313359);f=k(f,c,d,e,a[g+15],10,4264355552);e=k(e,f,c,d,a[g+6],15,2734768916);d=k(d,e,f,c,a[g+13],21,1309151649);c=k(c,d,e,f,a[g+4],6,4149444226);f=k(f,c,d,e,a[g+11],10,3174756917);e=k(e,f,c,d,a[g+2],15,718787259);d=k(d,e,f,c,a[g+9],21,3951481745);l[0]=m(c,l[0]);l[1]=m(d,l[1]);l[2]=m(e,l[2]);l[3]=m(f,l[3])}var
o=[];for(var
g=0;g<4;g++)for(var
n=0;n<4;n++)o[g*4+n]=l[g]>>8*n&w;return o}return function(a,b,c){var
h=[];if(a.array){var
f=a.array;for(var
d=0;d<c;d+=4){var
e=d+b;h[d>>2]=f[e]|f[e+1]<<8|f[e+2]<<16|f[e+3]<<24}for(;d<c;d++)h[d>>2]|=f[d+b]<<8*(d&3)}else{var
g=a.getFullBytes();for(var
d=0;d<c;d+=4){var
e=d+b;h[d>>2]=g.charCodeAt(e)|g.charCodeAt(e+1)<<8|g.charCodeAt(e+2)<<16|g.charCodeAt(e+3)<<24}for(;d<c;d++)h[d>>2]|=g.charCodeAt(d+b)<<8*(d&3)}return new
bK(n(h,c))}}();function
o(a){aQ(d[2],a)}function
eH(a){if(!a.opened)o("Cannot flush a closed channel");if(a.buffer==c)return 0;if(a.output)switch(a.output.length){case
2:a.output(a,a.buffer);break;default:a.output(a.buffer)}a.buffer=c}function
bR(a){a=a
instanceof
e?a.toString():a;o(a+": No such file or directory")}var
eg=aI;function
am(a){a=a
instanceof
e?a.toString():a;if(a.charCodeAt(0)!=47)a=eg+a;var
f=a.split(aI),b=[];for(var
d=0;d<f.length;d++)switch(f[d]){case"..":if(b.length>1)b.pop();break;case
J:case
c:if(b.length==0)b.push(c);break;default:b.push(f[d]);break}b.orig=a;return b}function
C(){this.content={}}C.prototype={exists:function(a){return this.content[a]?1:0},mk:function(a,b){this.content[a]=b},get:function(a){return this.content[a]},list:function(){var
a=[];for(var
b
in
this.content)a.push(b);return a},remove:function(a){delete
this.content[a]}};var
ao=new
C();ao.mk(c,new
C());function
aO(a){var
b=ao;for(var
c=0;c<a.length;c++){if(!(b.exists&&b.exists(a[c])))bR(a.orig);b=b.get(a[c])}return b}function
e0(a){var
c=am(a),b=aO(c);return b
instanceof
C?1:0}function
O(a){this.data=a}O.prototype={content:function(){return this.data},truncate:function(){this.data.length=0}};function
ek(a,b){var
f=am(a),c=ao;for(var
g=0;g<f.length-1;g++){var
d=f[g];if(!c.exists(d))c.mk(d,new
C());c=c.get(d);if(!(c
instanceof
C))o(f.orig+aM)}var
d=f[f.length-1];if(c.exists(d))o(f.orig+aM);if(b
instanceof
C)c.mk(d,b);else
if(b
instanceof
O)c.mk(d,b);else
if(b
instanceof
e)c.mk(d,new
O(b.getArray()));else
if(b
instanceof
Array)c.mk(d,new
O(b));else
if(b.toString)c.mk(d,new
O(new
e(b.toString()).getArray()));else
P("caml_fs_register")}function
eY(a){var
b=ao,d=am(a),e;for(var
c=0;c<d.length;c++){if(b.auto)e=b.auto;if(!(b.exists&&b.exists(d[c])))return e?e(d.join(aI)):0;b=b.get(d[c])}return 1}function
R(a,b,c){if(d.fds===undefined)d.fds=new
Array();c=c?c:{};var
e={};e.array=b;e.offset=c.append?e.array.length:0;e.flags=c;d.fds[a]=e;d.fd_last_idx=a;return a}function
e5(a,b,c){var
e={};while(b){switch(b[1]){case
0:e.rdonly=1;break;case
1:e.wronly=1;break;case
2:e.append=1;break;case
3:e.create=1;break;case
4:e.truncate=1;break;case
5:e.excl=1;break;case
6:e.binary=1;break;case
7:e.text=1;break;case
8:e.nonblock=1;break}b=b[2]}var
g=a.toString(),i=am(a);if(e.rdonly&&e.wronly)o(g+" : flags Open_rdonly and Open_wronly are not compatible");if(e.text&&e.binary)o(g+" : flags Open_text and Open_binary are not compatible");if(eY(a)){if(e0(a))o(g+" : is a directory");if(e.create&&e.excl)o(g+aM);var
h=d.fd_last_idx?d.fd_last_idx:0,f=aO(i);if(e.truncate)f.truncate();return R(h+1,f.content(),e)}else
if(e.create){var
h=d.fd_last_idx?d.fd_last_idx:0;ek(a,[]);var
f=aO(i);return R(h+1,f.content(),e)}else
bR(a)}R(0,[]);R(1,[]);R(2,[]);function
eI(a){var
b=d.fds[a];if(b.flags.wronly)o(bG+a+" is writeonly");return{data:b,fd:a,opened:true}}function
e3(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
b=m.console;b&&b.log&&b.log(a)}var
an=new
Array();function
eW(a,b){var
f=new
e(b),d=f.getLen();for(var
c=0;c<d;c++)a.data.array[a.data.offset+c]=f.get(c);a.data.offset+=d;return 0}function
eJ(a){var
b;switch(a){case
1:b=e3;break;case
2:b=ap;break;default:b=eW}var
f=d.fds[a];if(f.flags.rdonly)o(bG+a+" is readonly");var
e={data:f,fd:a,opened:true,buffer:c,output:b};an[e.fd]=e;return e}function
eK(){var
a=0;for(var
b
in
an)if(an[b].opened)a=[0,an[b],a];return a}function
bQ(a){throw[0,a]}function
eT(){bQ(d[6])}function
eL(a,b){if(b==0)eT();return a%b}if(!Math.imul)Math.imul=function(a,b){return((a>>16)*b<<16)+(a&ah)*b|0};var
eM=Math.imul;function
eO(a){return new
e(a)}function
eP(a,b){a[0]=b;return 0}function
eQ(a){return a
instanceof
Array?a[0]:bC}function
eU(a,b){d[a+1]=b}var
bO={};function
eV(a,b){bO[a.toString()]=b;return 0}function
eX(){return 32}function
eS(){bQ(d[7])}function
eZ(a){var
b=m,c=a.toString();if(b.process&&b.process.env&&b.process.env[c]!=undefined)return new
x(b.process.env[c]);eS()}function
e1(){var
a=new
Date()^4294967295*Math.random();return{valueOf:function(){return a},0:0,1:a,length:2}}function
eN(a){return bO[a]}function
e2(a){if(a
instanceof
Array)return a;if(m.RangeError&&a
instanceof
m.RangeError&&a.message&&a.message.match(/maximum call stack/i))return[0,d[9]];if(m.InternalError&&a
instanceof
m.InternalError&&a.message&&a.message.match(/too much recursion/i))return[0,d[9]];if(a
instanceof
m.Error)return[0,eN(bx),a];return[0,d[3],new
x(String(a))]}function
e4(){return 0}var
K=ea,g=eb,_=ec,t=ef,br=ei,$=ej,aA=ey,d$=eD,aB=eE,v=eF,bp=eJ,b=eO,a=eU,bq=eV,bs=eZ,u=e2,ab=e4;function
p(a,b){return a.length==1?a(b):D(a,[b])}function
q(a,b,c){return a.length==2?a(b,c):D(a,[b,c])}function
h(a,b,c,d){return a.length==3?a(b,c,d):D(a,[b,c,d])}var
E=[0,b("Failure")],aR=[0,b("Invalid_argument")],y=[0,b("Not_found")],aT=[0,b("Assert_failure")];a(11,[0,b("Undefined_recursive_module")]);a(10,aT);a(9,[0,b("Sys_blocked_io")]);a(8,[0,b("Stack_overflow")]);a(7,[0,b("Match_failure")]);a(6,y);a(5,[0,b("Division_by_zero")]);a(4,[0,b("End_of_file")]);a(3,aR);a(2,E);a(1,[0,b("Sys_error")]);a(0,[0,b("Out_of_memory")]);var
bU=b("true"),bV=b("false"),bW=b("Pervasives.do_at_exit"),bZ=b("\\b"),b0=b("\\t"),b1=b("\\n"),b2=b("\\r"),bY=b("\\\\"),bX=b("\\'"),b5=b("String.contains_from"),b4=b("String.blit"),b3=b("String.sub"),b7=b("CamlinternalLazy.Undefined"),b$=b("Buffer.add: cannot grow buffer"),cp=b(c),cq=b(c),ct=b("%.12g"),cu=b(by),cv=b(by),cr=b(ae),cs=b(ae),co=b(bA),cm=b("neg_infinity"),cn=b("infinity"),cl=b(J),ck=b("printf: bad positional specification (0)."),cj=b("%_"),ci=[0,b("printf.ml"),143,8],cg=b(ae),ch=b("Printf: premature end of format string '"),cc=b(ae),cd=b(" in format string '"),ce=b(", at char number "),cf=b("Printf: bad conversion %"),ca=b("Sformat.index_of_int: negative argument "),cw=b(bw),d9=b("OCAMLRUNPARAM"),d7=b("CAMLRUNPARAM"),cx=b(c),cE=b("E2BIG"),cG=b("EACCES"),cH=b("EAGAIN"),cI=b("EBADF"),cJ=b("EBUSY"),cK=b("ECHILD"),cL=b("EDEADLK"),cM=b("EDOM"),cN=b("EEXIST"),cO=b("EFAULT"),cP=b("EFBIG"),cQ=b("EINTR"),cR=b("EINVAL"),cS=b("EIO"),cT=b("EISDIR"),cU=b("EMFILE"),cV=b("EMLINK"),cW=b("ENAMETOOLONG"),cX=b("ENFILE"),cY=b("ENODEV"),cZ=b("ENOENT"),c0=b("ENOEXEC"),c1=b("ENOLCK"),c2=b("ENOMEM"),c3=b("ENOSPC"),c4=b("ENOSYS"),c5=b("ENOTDIR"),c6=b("ENOTEMPTY"),c7=b("ENOTTY"),c8=b("ENXIO"),c9=b("EPERM"),c_=b("EPIPE"),c$=b("ERANGE"),da=b("EROFS"),db=b("ESPIPE"),dc=b("ESRCH"),dd=b("EXDEV"),de=b("EWOULDBLOCK"),df=b("EINPROGRESS"),dg=b("EALREADY"),dh=b("ENOTSOCK"),di=b("EDESTADDRREQ"),dj=b("EMSGSIZE"),dk=b("EPROTOTYPE"),dl=b("ENOPROTOOPT"),dm=b("EPROTONOSUPPORT"),dn=b("ESOCKTNOSUPPORT"),dp=b("EOPNOTSUPP"),dq=b("EPFNOSUPPORT"),dr=b("EAFNOSUPPORT"),ds=b("EADDRINUSE"),dt=b("EADDRNOTAVAIL"),du=b("ENETDOWN"),dv=b("ENETUNREACH"),dw=b("ENETRESET"),dx=b("ECONNABORTED"),dy=b("ECONNRESET"),dz=b("ENOBUFS"),dA=b("EISCONN"),dB=b("ENOTCONN"),dC=b("ESHUTDOWN"),dD=b("ETOOMANYREFS"),dE=b("ETIMEDOUT"),dF=b("ECONNREFUSED"),dG=b("EHOSTDOWN"),dH=b("EHOSTUNREACH"),dI=b("ELOOP"),dJ=b("EOVERFLOW"),dK=b("EUNKNOWNERR %d"),cF=b("Unix.Unix_error(Unix.%s, %S, %S)"),cA=b(bt),cB=b(c),cC=b(c),cD=b(bt),dL=b("0.0.0.0"),dM=b("127.0.0.1"),d6=b("::"),d5=b("::1"),dS=b("Js.Error"),dT=b(bx),dV=b("[\\][()\\\\|+*.?{}^$]"),dZ=b("ocaml START"),d0=b("textures/earthmap1k.jpg"),d1=b("textures/earthbump1k.jpg"),d2=b("textures/earthspec1k.jpg"),d3=b("gray"),d4=b("ocaml END"),dY=b("THREE.Color"),dW=b("THREE.MeshPhongMaterial"),dX=b("textures/earthcloudmap.jpg");function
aq(a){throw[0,E,a]}function
F(a){throw[0,aR,a]}function
i(a,b){var
c=a.getLen(),e=b.getLen(),d=t(c+e|0);_(a,0,d,0,c);_(b,0,d,c,e);return d}function
S(a){return b(c+a)}eI(0);bp(1);bp(2);function
aS(a){var
b=eK(0);for(;;){if(b){var
c=b[2],d=b[1];try{eH(d)}catch(f){}var
b=c;continue}return 0}}bq(bW,aS);function
T(a,b){var
c=t(a);eh(c,0,a,b);return c}function
U(a,b,c){if(0<=b)if(0<=c)if(!((a.getLen()-c|0)<b)){var
d=t(c);_(a,b,d,0,c);return d}return F(b3)}function
V(a,b,c,d,e){if(0<=e)if(0<=b)if(!((a.getLen()-e|0)<b))if(0<=d)if(!((c.getLen()-e|0)<d))return _(a,b,c,d,e);return F(b4)}var
ar=eX(0),aW=(1<<(ar-10|0))-1|0,G=eM(ar/8|0,aW)-1|0,b8=[0,b7];function
b9(a){throw[0,b8]}function
as(a){var
b=1<=a?a:1,c=G<b?G:b,d=t(c);return[0,d,0,c,d]}function
at(a){return U(a[1],0,a[2])}function
aY(a,b){var
c=[0,a[3]];for(;;){if(c[1]<(a[2]+b|0)){c[1]=2*c[1]|0;continue}if(G<c[1])if((a[2]+b|0)<=G)c[1]=G;else
aq(b$);var
d=t(c[1]);V(a[1],0,d,0,a[2]);a[1]=d;a[3]=c[1];return 0}}function
H(a,b){var
c=a[2];if(a[3]<=c)aY(a,1);a[1].safeSet(c,b);a[2]=c+1|0;return 0}function
au(a,b){var
c=b.getLen(),d=a[2]+c|0;if(a[3]<d)aY(a,c);V(b,0,a[1],a[2],c);a[2]=d;return 0}function
av(a){return 0<=a?a:aq(i(ca,S(a)))}function
aZ(a,b){return av(a+b|0)}var
cb=1;function
a0(a){return aZ(cb,a)}function
a1(a){return U(a,0,a.getLen())}function
a2(a,b,c){var
d=i(cd,i(a,cc)),e=i(ce,i(S(b),d));return F(i(cf,i(T(1,c),e)))}function
I(a,b,c){return a2(a1(a),b,c)}function
X(a){return F(i(ch,i(a1(a),cg)))}function
z(i,b,c,d){function
j(a){if(9<(i.safeGet(a)+ac|0)>>>0)return a;var
b=a+1|0;for(;;){var
c=i.safeGet(b);if(48<=c){if(!(58<=c)){var
b=b+1|0;continue}}else
if(36===c)return b+1|0;return a}}var
k=j(b+1|0),f=as((c-k|0)+10|0);H(f,37);var
e=d,h=0;for(;;){if(e){var
m=[0,e[1],h],e=e[2],h=m;continue}var
a=k,g=h;for(;;){if(a<=c){var
l=i.safeGet(a);if(42===l){if(g){var
n=g[2];au(f,S(g[1]));var
a=j(a+1|0),g=n;continue}throw[0,aT,ci]}H(f,l);var
a=a+1|0;continue}return at(f)}}}function
a3(a,b,c,d,e){var
f=z(b,c,d,e);if(78!==a)if(M!==a)return f;f.safeSet(f.getLen()-1|0,aF);return f}function
a4(a){return function(d,b){var
j=d.getLen();function
k(a,b){var
m=40===a?41:aE,c=b;for(;;){if(j<=c)return X(d);if(37===d.safeGet(c)){var
e=c+1|0;if(j<=e)return X(d);var
f=d.safeGet(e),h=f-40|0;if(1<h>>>0){var
l=h-83|0;if(2<l>>>0)var
g=1;else
switch(l){case
1:var
g=1;break;case
2:var
i=1,g=0;break;default:var
i=0,g=0}if(g){var
c=e+1|0;continue}}else
var
i=0===h?0:1;if(i)return f===m?e+1|0:I(d,b,f);var
c=k(f,e+1|0)+1|0;continue}var
c=c+1|0;continue}}return k(a,b)}}function
a5(j,b,c){var
m=j.getLen()-1|0;function
r(a){var
l=a;a:for(;;){if(l<m){if(37===j.safeGet(l)){var
f=0,i=l+1|0;for(;;){if(m<i)var
e=X(j);else{var
n=j.safeGet(i);if(58<=n){if(95===n){var
f=1,i=i+1|0;continue}}else
if(32<=n)switch(n+bB|0){case
1:case
2:case
4:case
5:case
6:case
7:case
8:case
9:case
12:case
15:break;case
0:case
3:case
11:case
13:var
i=i+1|0;continue;case
10:var
i=h(b,f,i,B);continue;default:var
i=i+1|0;continue}var
d=i;b:for(;;){if(m<d)var
e=X(j);else{var
k=j.safeGet(d);if(126<=k)var
g=0;else
switch(k){case
78:case
88:case
aj:case
B:case
aJ:case
aF:case
aG:var
e=h(b,f,d,B),g=1;break;case
69:case
70:case
71:case
bE:case
aL:case
aK:var
e=h(b,f,d,aL),g=1;break;case
33:case
37:case
44:case
64:var
e=d+1|0,g=1;break;case
83:case
91:case
N:var
e=h(b,f,d,N),g=1;break;case
97:case
ag:case
aC:var
e=h(b,f,d,k),g=1;break;case
76:case
bH:case
M:var
s=d+1|0;if(m<s)var
e=h(b,f,d,B),g=1;else{var
t=j.safeGet(s)+bF|0;if(32<t>>>0)var
p=1;else
switch(t){case
0:case
12:case
17:case
23:case
29:case
32:var
e=q(c,h(b,f,d,k),B),g=1,p=0;break;default:var
p=1}if(p)var
e=h(b,f,d,B),g=1}break;case
67:case
99:var
e=h(b,f,d,99),g=1;break;case
66:case
98:var
e=h(b,f,d,66),g=1;break;case
41:case
aE:var
e=h(b,f,d,k),g=1;break;case
40:var
e=r(h(b,f,d,k)),g=1;break;case
aH:var
u=h(b,f,d,k),v=q(a4(k),j,u),o=u;for(;;){if(o<(v-2|0)){var
o=q(c,o,j.safeGet(o));continue}var
d=v-1|0;continue b}default:var
g=0}if(!g)var
e=I(j,d,k)}break}}var
l=e;continue a}}var
l=l+1|0;continue}return l}}r(0);return 0}function
a6(a){var
d=[0,0,0,0];function
b(a,b,c){var
f=41!==c?1:0,g=f?aE!==c?1:0:f;if(g){var
e=97===c?2:1;if(ag===c)d[3]=d[3]+1|0;if(a)d[2]=d[2]+e|0;else
d[1]=d[1]+e|0}return b+1|0}a5(a,b,function(a,b){return a+1|0});return d[1]}function
a7(a,b,c){var
g=a.safeGet(c);if(9<(g+ac|0)>>>0)return q(b,0,c);var
e=g+ac|0,d=c+1|0;for(;;){var
f=a.safeGet(d);if(48<=f){if(!(58<=f)){var
e=(10*e|0)+(f+ac|0)|0,d=d+1|0;continue}}else
if(36===f)return 0===e?aq(ck):q(b,[0,av(e-1|0)],d+1|0);return q(b,0,c)}}function
l(a,b){return a?b:a0(b)}function
a8(a,b){return a?a[1]:b}function
a9(c){function
e(a){var
b=at(a);a[2]=0;return p(c,b)}var
a9=1;return function(h){var
J=as(2*h.getLen()|0);function
al(a){return au(J,a)}function
aY(a,b,g,a1){var
f=g.getLen();function
C(h,b){var
k=b;for(;;){if(f<=k)return p(a,J);var
c=g.safeGet(k);if(37===c){var
j=function(a,b){return K(a1,a8(a,b))},aB=function(f,e,c,d){var
a=d;for(;;){var
aD=g.safeGet(a)+bB|0;if(!(25<aD>>>0))switch(aD){case
1:case
2:case
4:case
5:case
6:case
7:case
8:case
9:case
12:case
15:break;case
10:return a7(g,function(a,b){var
d=[0,j(a,e),c];return aB(f,l(a,e),d,b)},a+1|0);default:var
a=a+1|0;continue}var
m=g.safeGet(a);if(!(124<=m))switch(m){case
78:case
88:case
aj:case
B:case
aJ:case
aF:case
aG:var
bq=j(f,e),bs=$(a3(m,g,k,a,c),bq);return n(l(f,e),bs,a+1|0);case
69:case
71:case
bE:case
aL:case
aK:var
bj=j(f,e),bk=br(z(g,k,a,c),bj);return n(l(f,e),bk,a+1|0);case
76:case
bH:case
M:var
aO=g.safeGet(a+1|0)+bF|0;if(!(32<aO>>>0))switch(aO){case
0:case
12:case
17:case
23:case
29:case
32:var
Z=a+1|0,aP=m-108|0;if(2<aP>>>0)var
am=0;else{switch(aP){case
1:var
am=0,an=0;break;case
2:var
bp=j(f,e),aR=$(z(g,k,Z,c),bp),an=1;break;default:var
bo=j(f,e),aR=$(z(g,k,Z,c),bo),an=1}if(an)var
aQ=aR,am=1}if(!am)var
bn=j(f,e),aQ=en(z(g,k,Z,c),bn);return n(l(f,e),aQ,Z+1|0)}var
bl=j(f,e),bm=$(a3(M,g,k,a,c),bl);return n(l(f,e),bm,a+1|0);case
37:case
64:return n(e,T(1,m),a+1|0);case
83:case
N:var
x=j(f,e);if(N===m)var
y=x;else{var
b=[0,0],ar=x.getLen()-1|0,a_=0;if(!(ar<0)){var
P=a_;for(;;){var
w=x.safeGet(P),by=14<=w?34===w?1:92===w?1:0:11<=w?13<=w?1:0:8<=w?1:0,bb=by?2:aA(w)?1:4;b[1]=b[1]+bb|0;var
bc=P+1|0;if(ar!==P){var
P=bc;continue}break}}if(b[1]===x.getLen())var
aT=x;else{var
h=t(b[1]);b[1]=0;var
av=x.getLen()-1|0,a$=0;if(!(av<0)){var
L=a$;for(;;){var
v=x.safeGet(L),O=v-34|0;if(58<O>>>0)if(-20<=O)var
_=1;else{switch(O+34|0){case
8:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],98);var
K=1;break;case
9:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],aC);var
K=1;break;case
10:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],M);var
K=1;break;case
13:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],ag);var
K=1;break;default:var
_=1,K=0}if(K)var
_=0}else
var
_=56<(O-1|0)>>>0?(h.safeSet(b[1],92),b[1]++,h.safeSet(b[1],v),0):1;if(_)if(aA(v))h.safeSet(b[1],v);else{h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],48+(v/aj|0)|0);b[1]++;h.safeSet(b[1],48+((v/10|0)%10|0)|0);b[1]++;h.safeSet(b[1],48+(v%10|0)|0)}b[1]++;var
ba=L+1|0;if(av!==L){var
L=ba;continue}break}}var
aT=h}var
y=i(cv,i(aT,cu))}if(a===(k+1|0))var
aS=y;else{var
G=z(g,k,a,c);try{var
aa=0,r=1;for(;;){if(G.getLen()<=r)var
aw=[0,0,aa];else{var
ab=G.safeGet(r);if(49<=ab)if(58<=ab)var
ao=0;else
var
aw=[0,ex(U(G,r,(G.getLen()-r|0)-1|0)),aa],ao=1;else{if(45===ab){var
aa=1,r=r+1|0;continue}var
ao=0}if(!ao){var
r=r+1|0;continue}}var
ad=aw;break}}catch(f){f=u(f);if(f[1]!==E)throw f;var
ad=a2(G,0,N)}var
Q=ad[1],A=y.getLen(),bd=ad[2],R=0,be=32;if(Q===A)if(0===R)var
ae=y,ap=1;else
var
ap=0;else
var
ap=0;if(!ap)if(Q<=A)var
ae=U(y,R,A);else{var
ac=T(Q,be);if(bd)V(y,R,ac,0,A);else
V(y,R,ac,Q-A|0,A);var
ae=ac}var
aS=ae}return n(l(f,e),aS,a+1|0);case
67:case
99:var
o=j(f,e);if(99===m)var
aM=T(1,o);else{if(39===o)var
s=bX;else
if(92===o)var
s=bY;else{if(14<=o)var
D=0;else
switch(o){case
8:var
s=bZ,D=1;break;case
9:var
s=b0,D=1;break;case
10:var
s=b1,D=1;break;case
13:var
s=b2,D=1;break;default:var
D=0}if(!D)if(aA(o)){var
aq=t(1);aq.safeSet(0,o);var
s=aq}else{var
F=t(4);F.safeSet(0,92);F.safeSet(1,48+(o/aj|0)|0);F.safeSet(2,48+((o/10|0)%10|0)|0);F.safeSet(3,48+(o%10|0)|0);var
s=F}}var
aM=i(cs,i(s,cr))}return n(l(f,e),aM,a+1|0);case
66:case
98:var
bh=a+1|0,bi=j(f,e)?bU:bV;return n(l(f,e),bi,bh);case
40:case
aH:var
Y=j(f,e),aE=q(a4(m),g,a+1|0);if(aH===m){var
S=as(Y.getLen()),ax=function(a,b){H(S,b);return a+1|0};a5(Y,function(a,b,c){if(a)au(S,cj);else
H(S,37);return ax(b,c)},ax);var
bf=at(S);return n(l(f,e),bf,aE)}var
aI=l(f,e),bx=aZ(a6(Y),aI);return aY(function(a){return C(bx,aE)},aI,Y,a1);case
33:return C(e,a+1|0);case
41:return n(e,cp,a+1|0);case
44:return n(e,cq,a+1|0);case
70:var
ai=j(f,e);if(0===c)var
aN=ct;else{var
af=z(g,k,a,c);if(70===m)af.safeSet(af.getLen()-1|0,aK);var
aN=af}var
az=ed(ai);if(3===az)var
ak=ai<0?cm:cn;else
if(4<=az)var
ak=co;else{var
X=br(aN,ai),W=0,bg=X.getLen();for(;;){if(bg<=W)var
ay=i(X,cl);else{var
ah=X.safeGet(W)-46|0,bz=23<ah>>>0?55===ah?1:0:21<(ah-1|0)>>>0?1:0;if(!bz){var
W=W+1|0;continue}var
ay=X}var
ak=ay;break}}return n(l(f,e),ak,a+1|0);case
91:return I(g,a,m);case
97:var
aU=j(f,e),aV=a0(a8(f,e)),aW=j(0,aV),bt=a+1|0,bu=l(f,aV);if(a9)al(q(aU,0,aW));else
q(aU,J,aW);return C(bu,bt);case
ag:return I(g,a,m);case
aC:var
aX=j(f,e),bv=a+1|0,bw=l(f,e);if(a9)al(p(aX,0));else
p(aX,J);return C(bw,bv)}return I(g,a,m)}},d=k+1|0,e=0;return a7(g,function(a,b){return aB(a,h,e,b)},d)}H(J,c);var
k=k+1|0;continue}}function
n(a,b,c){al(b);return C(a,c)}return C(b,0)}var
d=av(0);function
j(a,b){return aY(e,d,a,b)}var
c=a6(h);if(6<c>>>0){var
k=function(i,b){if(c<=i){var
l=v(c,0),m=function(a,b){return g(l,(c-a|0)-1|0,b)},d=0,a=b;for(;;){if(a){var
e=a[2],f=a[1];if(e){m(d,f);var
d=d+1|0,a=e;continue}m(d,f)}return j(h,l)}}return function(a){return k(i+1|0,[0,a,b])}};return k(0,0)}switch(c){case
1:return function(a){var
b=v(1,0);g(b,0,a);return j(h,b)};case
2:return function(a,b){var
c=v(2,0);g(c,0,a);g(c,1,b);return j(h,c)};case
3:return function(a,b,c){var
d=v(3,0);g(d,0,a);g(d,1,b);g(d,2,c);return j(h,d)};case
4:return function(a,b,c,d){var
e=v(4,0);g(e,0,a);g(e,1,b);g(e,2,c);g(e,3,d);return j(h,e)};case
5:return function(a,b,c,d,e){var
f=v(5,0);g(f,0,a);g(f,1,b);g(f,2,c);g(f,3,d);g(f,4,e);return j(h,f)};case
6:return function(a,b,c,d,e,f){var
i=v(6,0);g(i,0,a);g(i,1,b);g(i,2,c);g(i,3,d);g(i,4,e);g(i,5,f);return j(h,i)};default:return j(h,[0])}}}function
a_(a){return p(a9(function(a){return a}),a)}var
a$=[0,0];function
aw(a){a$[1]=[0,a,a$[1]];return 0}32===ar;try{var
d_=bs(d9),ax=d_}catch(f){f=u(f);if(f[1]!==y)throw f;try{var
d8=bs(d7),bb=d8}catch(f){f=u(f);if(f[1]!==y)throw f;var
bb=cx}var
ax=bb}var
aU=ax.getLen(),cy=82,aV=0;if(0<=0)if(aU<aV)var
aa=0;else
try{var
W=aV;for(;;){if(aU<=W)throw[0,y];if(ax.safeGet(W)!==cy){var
W=W+1|0;continue}var
b6=1,ay=b6,aa=1;break}}catch(f){f=u(f);if(f[1]!==y)throw f;var
ay=0,aa=1}else
var
aa=0;if(!aa)var
ay=F(b5);var
s=[bu,function(a){var
n=e1(0),b=[0,v(55,0),0],k=0===n.length-1?[0,0]:n,f=k.length-1,o=0;if(!0){var
d=o;for(;;){g(b[1],d,d);var
u=d+1|0;if(54!==d){var
d=u;continue}break}}var
h=[0,cw],p=0,q=55,r=el(55,f)?q:f,l=54+r|0;if(!(l<0)){var
c=p;for(;;){var
m=c%55|0,s=h[1],j=i(s,S(K(k,eL(c,f))));h[1]=eG(j,0,j.getLen());var
e=h[1];g(b[1],m,(K(b[1],m)^(((e.safeGet(0)+(e.safeGet(1)<<8)|0)+(e.safeGet(2)<<16)|0)+(e.safeGet(3)<<24)|0))&bv);var
t=c+1|0;if(l!==c){var
c=t;continue}break}}b[2]=0;return b}];function
bd(a,b){return bq(a,b[0+1])}var
be=[0,cA];bd(cD,[0,be,0,cC,cB]);aw(function(a){if(a[1]===be){var
c=a[2],d=a[4],e=a[3];if(typeof
c===bz)switch(c){case
1:var
b=cG;break;case
2:var
b=cH;break;case
3:var
b=cI;break;case
4:var
b=cJ;break;case
5:var
b=cK;break;case
6:var
b=cL;break;case
7:var
b=cM;break;case
8:var
b=cN;break;case
9:var
b=cO;break;case
10:var
b=cP;break;case
11:var
b=cQ;break;case
12:var
b=cR;break;case
13:var
b=cS;break;case
14:var
b=cT;break;case
15:var
b=cU;break;case
16:var
b=cV;break;case
17:var
b=cW;break;case
18:var
b=cX;break;case
19:var
b=cY;break;case
20:var
b=cZ;break;case
21:var
b=c0;break;case
22:var
b=c1;break;case
23:var
b=c2;break;case
24:var
b=c3;break;case
25:var
b=c4;break;case
26:var
b=c5;break;case
27:var
b=c6;break;case
28:var
b=c7;break;case
29:var
b=c8;break;case
30:var
b=c9;break;case
31:var
b=c_;break;case
32:var
b=c$;break;case
33:var
b=da;break;case
34:var
b=db;break;case
35:var
b=dc;break;case
36:var
b=dd;break;case
37:var
b=de;break;case
38:var
b=df;break;case
39:var
b=dg;break;case
40:var
b=dh;break;case
41:var
b=di;break;case
42:var
b=dj;break;case
43:var
b=dk;break;case
44:var
b=dl;break;case
45:var
b=dm;break;case
46:var
b=dn;break;case
47:var
b=dp;break;case
48:var
b=dq;break;case
49:var
b=dr;break;case
50:var
b=ds;break;case
51:var
b=dt;break;case
52:var
b=du;break;case
53:var
b=dv;break;case
54:var
b=dw;break;case
55:var
b=dx;break;case
56:var
b=dy;break;case
57:var
b=dz;break;case
58:var
b=dA;break;case
59:var
b=dB;break;case
60:var
b=dC;break;case
61:var
b=dD;break;case
62:var
b=dE;break;case
63:var
b=dF;break;case
64:var
b=dG;break;case
65:var
b=dH;break;case
66:var
b=dI;break;case
67:var
b=dJ;break;default:var
b=cE}else
var
f=c[1],b=p(a_(dK),f);return[0,h(a_(cF),b,e,d)]}return 0});ab(dL);ab(dM);try{ab(d6)}catch(f){f=u(f);if(f[1]!==E)throw f}try{ab(d5)}catch(f){f=u(f);if(f[1]!==E)throw f}var
dN=7,dO=0,cz=0?dO[1]:ay,Y=16;for(;;){if(!(dN<=Y))if(!(aW<(Y*2|0))){var
Y=Y*2|0;continue}if(cz){var
bc=eQ(s);if(ak===bc)var
n=s[1];else
if(bu===bc){var
b_=s[0+1];s[0+1]=b9;try{var
aX=p(b_,0);s[0+1]=aX;eP(s,ak)}catch(f){f=u(f);s[0+1]=function(a){throw f};throw f}var
n=aX}else
var
n=s;n[2]=(n[2]+1|0)%55|0;var
ba=K(n[1],n[2]);g(n[1],n[2],(K(n[1],(n[2]+24|0)%55|0)+(ba^(ba>>>25|0)&31)|0)&bv)}var
j=m,bf=undefined,az=true,bg=j.RegExp,bh=[0,dS],dQ=j.Array,dR=j.Date;bd(dT,[0,bh,{}]);var
dP=false;aw(function(a){return a[1]===bh?[0,new
x(a[2].toString())]:0});aw(function(a){return a
instanceof
dQ?0:[0,new
x(a.toString())]});j.HTMLElement===bf;var
bi=eC(function(a){var
g=[0,j.requestAnimationFrame,[0,j.mozRequestAnimationFrame,[0,j.webkitRequestAnimationFrame,[0,j.oRequestAnimationFrame,[0,j.msRequestAnimationFrame,0]]]]];try{var
b=g;for(;;){if(!b)throw[0,y];var
c=b[1],f=b[2];if(c===bf){var
b=f;continue}var
h=function(a){return c(a)};break}}catch(f){f=u(f);if(f[1]===y){var
d=function(a){return new
dR().getTime()},e=[0,d(0)];return function(a){var
b=d(0),c=e[1]+16.6666666666666679-b,f=c<0?0:c;e[1]=b;j.setTimeout(a,f);return 0}}throw f}return h});new
bg("[$]",af);new
bg(ez(dV),af);var
dU=eA(0),bj=function(a){return p(a9(function(a){return dU.log(a.toString())}),a)},bk=function(g,b,c,d,e,f){g.render(b,c);d.rotation.y=d.rotation.y+0.015625;e.rotation.y=e.rotation.y+0.0158730158730158721;return p(bi,aB(function(a){return bk(g,b,c,d,e,a)}))},bl=function(a,b){return eB(d$(a),b)},bm=function(a,b,c){return new(THREE.SphereGeometry)(a,b,c)},Z=function(a){return THREE.ImageUtils.loadTexture(a)},bn=function(a,b){return new(THREE.Mesh)(a,b)},bo=function(a){bj(dZ);var
e=new(THREE.WebGLRenderer)();e.setSize(window.innerWidth,window.innerHeight);document.body.appendChild(e.domElement);var
i=new(THREE.PerspectiveCamera)(0.2,window.innerWidth/window.innerHeight,1,bC);i.position[b("z")]=500;var
f=new(THREE.Scene)(),l=bm(0.5,32,32),c=new(THREE.MeshPhongMaterial)(),h=bn(l,c),g=new(THREE.SpotLight)(4210752),m=bm(0.51,32,32),d=bl(dW,[0]);d.map=Z(dX);d.side=THREE.DoubleSide;d[b("opacity")]=0.4;d.transparent=az;d.depthWrite=dP;var
j=bn(m,d);h.add(j);g.position.set(10,10,15);g[b("intensity")]=4;c.map=Z(d0);c.bumpMap=Z(d1);c[b("bumpScale")]=0.65;c.specularMap=Z(d2);c.specular=bl(dY,[0,d3]);e.alpha=az;e.setClearColor(k,1);f.add(h);f.add(g);bj(d4);p(bi,aB(function(a){return bk(e,f,i,h,j,a)}));return az};j.onload=aB(function(a){if(a){var
d=bo(a);if(!(d|0))a.preventDefault();return d}var
c=event,b=bo(c);if(!(b|0))c.returnValue=b;return b});aS(0);return}}(function(){return this}()));
import{d as H,r as m,k as u,s as I,o as N,c as D,l as J,t as f,b as i,g as E,u as L,i as K,C as Q,m as M}from"./index.021dac2a.js";import{G as U}from"./Goban.e40c8785.js";import{r as Z}from"./ArrowUturnLeftIcon.6e4819f4.js";import{r as ee}from"./FlagIcon.1f52365a.js";import"./_plugin-vue_export-helper.a81e96fd.js";const oe={class:"mx-auto",style:{"max-width":"calc(100vh - 220px)"}},te={key:1},se={class:"flex items-center"},ne={class:"text-2xl mx-2"},le={class:"text-base mx-2"},ie={class:"text-2xl mx-2"},ae={class:"text-base mx-2"},ce=K(" Save Result "),pe=H({__name:"ScoreRecordView",props:{id:{type:Number,required:!0}},setup(O){const T=O;function X(e){return e==="B"?"W":"B"}const V=new Q,s=m(0),z=m(0),x=m(0),y=m(0),_=m(0),b=m(0);function S(){return x.value+_.value}function G(){return z.value+y.value+b.value}const p=u([]),g=u([]),v=u([]),R=u([]);V.getRecord(T.id).then(e=>{z.value=e.komi,s.value=e.board_size;for(let o=0;o<s.value;o+=1){const t=u([]);for(let n=0;n<s.value;n+=1)t.push(" ");p.push(t);const l=u([]);for(let n=0;n<s.value;n+=1)l.push(" ");g.push(l);const a=u([]);for(let n=0;n<s.value;n+=1)a.push(null);v.push(a)}for(const{x:o,y:t,color:l}of e.stones)p[o][t]=l;for(const o of e.moves)o.color=="B"&&(x.value+=o.captures.length),o.color=="W"&&(y.value+=o.captures.length);Y(),q()});function*P(e,o){e>0&&(yield{x:e-1,y:o}),o>0&&(yield{x:e,y:o-1}),e<s.value-1&&(yield{x:e+1,y:o}),o<s.value-1&&(yield{x:e,y:o+1})}function Y(){for(let e=0;e<s.value;e+=1)for(let o=0;o<s.value;o+=1){const t=p[e][o];if(t===" "||v[e][o]!==null)continue;const l=u({color:t,dead:!1,stones:[]}),a=[{x:e,y:o}];for(;a.length>0;){const{x:n,y:c}=a.pop();l.stones.push({x:n,y:c}),v[n][c]=l;for(const{x:r,y:d}of P(n,c))p[r][d]===t&&!a.includes({x:r,y:d})&&v[r][d]===null&&a.push({x:r,y:d})}}}function k(e,o){return e+19*o}function j(e){return{x:e%19,y:Math.floor(e/19)}}function q(){_.value=0,b.value=0;const e=[];for(let o=0;o<s.value;o+=1){const t=u([]);for(let l=0;l<s.value;l+=1)t.push(null);e.push(t)}for(let o=0;o<s.value;o+=1)for(let t=0;t<s.value;t+=1){if(p[o][t]!==" "||e[o][t]!==null)continue;let a=0,n=0;const c=[],r=[k(o,t)];for(;r.length>0;){const{x:w,y:B}=j(r.pop());c.push(k(w,B));for(const{x:h,y:C}of P(w,B)){const W=v[h][C];W&&!W.dead?W.color==="B"?a+=1:W.color==="W"&&(n+=1):!r.includes(k(h,C))&&!c.includes(k(h,C))&&r.push(k(h,C))}}let d=" ";a==0&&n>=1&&(d="W",b.value+=c.length),n==0&&a>=1&&(d="B",_.value+=c.length);for(const w of c){const{x:B,y:h}=j(w);e[B][h]=d}}Object.assign(R,e)}I(R,()=>{for(let e=0;e<s.value;e+=1)for(let o=0;o<s.value;o+=1){const t=v[e][o],l=R[e][o];t&&t.dead?g[e][o]=X(t.color):l!==" "&&l!==null?g[e][o]=l:g[e][o]=" "}});function $(e,o){const t=v[e][o];t&&(t.dead=!t.dead,t.dead&&t.color=="B"&&(y.value+=t.stones.length),t.dead&&t.color=="W"&&(x.value+=t.stones.length),!t.dead&&t.color=="B"&&(y.value-=t.stones.length),!t.dead&&t.color=="W"&&(x.value-=t.stones.length)),q()}function A(){M.back()}async function F(){console.log("Saving");const e=await V.getRecord(T.id);G()>S()?e.winner="W":e.winner="B",await V.updateRecord(T.id,e),M.back()}return(e,o)=>(N(),D("div",oe,[s.value?(N(),J(U,{key:0,size:s.value,matrix:p,decorations:g,onClick:$,style:{"max-width":"calc(100vh - 220px)","max-height":"calc(100vh - 220px)"}},null,8,["size","matrix","decorations"])):(N(),D("div",te,"Loading game..."+f(s.value),1)),i("div",se,[i("button",{onClick:A,class:"rounded-md ring m-2 bg-green-400"},[E(L(Z),{class:"block h-7 w-7 m-2"})]),i("div",ne,"Black: "+f(S()),1),i("div",le,[i("div",null,f(x.value)+" captures",1),i("div",null,f(_.value)+" territory",1)]),i("div",ie,"White: "+f(G()),1),i("div",ae,[i("div",null,f(y.value)+" captures",1),i("div",null,f(b.value)+" territory",1),i("div",null,f(z.value)+" komi",1)]),i("button",{onClick:F,class:"grow flex items-center rounded-md ring m-2"},[E(L(ee),{class:"block h-7 w-7 m-2"}),ce])])]))}});export{pe as default};
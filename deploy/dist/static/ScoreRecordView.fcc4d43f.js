import{d as H,r as m,k as u,s as I,o as N,c as E,l as J,t as f,b as n,g as L,u as S,i as K,C as Q,m as M}from"./index.e92037f6.js";import{G as U}from"./Goban.63cb18dc.js";import{r as Z}from"./ArrowUturnLeftIcon.50481e55.js";import{r as ee}from"./FlagIcon.a0cb4c28.js";import"./_plugin-vue_export-helper.a81e96fd.js";const te={class:"mx-auto",style:{"max-width":"calc(100vh - 220px)"}},oe={key:1},se={class:"flex items-center"},le={class:"text-2xl mx-2"},ne={class:"text-base mx-2"},ie={class:"text-2xl mx-2"},ae={class:"text-base mx-2"},ce=K(" Save Result "),pe=H({__name:"ScoreRecordView",props:{id:{type:Number,required:!0}},setup(O){const V=O;function X(e){return e==="B"?"W":"B"}const z=new Q,s=m(0);let R=m(0);const x=m(0),y=m(0),b=m(0),w=m(0);function G(){return x.value+b.value}function P(){return R.value+y.value+w.value}const v=u([]),g=u([]),p=u([]),k=u([]);z.getRecord(V.id).then(e=>{R.value=e.komi,s.value=e.board_size;for(let t=0;t<s.value;t+=1){const o=u([]);for(let l=0;l<s.value;l+=1)o.push(" ");v.push(o);const i=u([]);for(let l=0;l<s.value;l+=1)i.push(" ");g.push(i);const a=u([]);for(let l=0;l<s.value;l+=1)a.push(null);p.push(a)}for(const{x:t,y:o,color:i}of e.stones)v[t][o]=i;for(const t of e.moves)t.color=="B"&&(x.value+=t.captures.length),t.color=="W"&&(y.value+=t.captures.length);Y(),D()});function*j(e,t){e>0&&(yield{x:e-1,y:t}),t>0&&(yield{x:e,y:t-1}),e<s.value-1&&(yield{x:e+1,y:t}),t<s.value-1&&(yield{x:e,y:t+1})}function Y(){for(let e=0;e<s.value;e+=1)for(let t=0;t<s.value;t+=1){const o=v[e][t];if(o===" "||p[e][t]!==null)continue;const i=u({color:o,dead:!1,stones:[]}),a=[{x:e,y:t}];for(;a.length>0;){const{x:l,y:c}=a.pop();i.stones.push({x:l,y:c}),p[l][c]=i;for(const{x:r,y:d}of j(l,c))v[r][d]===o&&!a.includes({x:r,y:d})&&p[r][d]===null&&a.push({x:r,y:d})}}}function _(e,t){return e+19*t}function q(e){return{x:e%19,y:Math.floor(e/19)}}function D(){b.value=0,w.value=0;const e=[];for(let t=0;t<s.value;t+=1){const o=u([]);for(let i=0;i<s.value;i+=1)o.push(null);e.push(o)}for(let t=0;t<s.value;t+=1)for(let o=0;o<s.value;o+=1){if(v[t][o]!==" "||e[t][o]!==null)continue;let a=0,l=0;const c=[],r=[_(t,o)];for(;r.length>0;){const{x:B,y:C}=q(r.pop());c.push(_(B,C));for(const{x:h,y:W}of j(B,C)){const T=p[h][W];T&&!T.dead?T.color==="B"?a+=1:T.color==="W"&&(l+=1):!r.includes(_(h,W))&&!c.includes(_(h,W))&&r.push(_(h,W))}}let d=" ";a==0&&l>=1&&(d="W",w.value+=c.length),l==0&&a>=1&&(d="B",b.value+=c.length);for(const B of c){const{x:C,y:h}=q(B);e[C][h]=d}}Object.assign(k,e)}I(k,()=>{for(let e=0;e<s.value;e+=1)for(let t=0;t<s.value;t+=1){let o=p[e][t];o&&o.dead?g[e][t]=X(v[e][t]):k[e][t]!==" "&&k[e][t]!==null?g[e][t]=k[e][t]:g[e][t]=" "}});function $(e,t){let o=p[e][t];o&&(o.dead=!o.dead,o.dead&&o.color=="B"&&(y.value+=o.stones.length),o.dead&&o.color=="W"&&(x.value+=o.stones.length),!o.dead&&o.color=="B"&&(y.value-=o.stones.length),!o.dead&&o.color=="W"&&(x.value-=o.stones.length)),D()}function A(){M.back()}async function F(){console.log("Saving");const e=await z.getRecord(V.id);P()>G()?e.winner="W":e.winner="B",await z.updateRecord(V.id,e),M.back()}return(e,t)=>(N(),E("div",te,[s.value?(N(),J(U,{key:0,size:s.value,matrix:v,decorations:g,onClick:$,style:{"max-width":"calc(100vh - 220px)","max-height":"calc(100vh - 220px)"}},null,8,["size","matrix","decorations"])):(N(),E("div",oe,"Loading game..."+f(s.value),1)),n("div",se,[n("button",{onClick:A,class:"rounded-md ring m-2 bg-green-400"},[L(S(Z),{class:"block h-7 w-7 m-2"})]),n("div",le," Black: "+f(G()),1),n("div",ne,[n("div",null,f(x.value)+" captures ",1),n("div",null,f(b.value)+" territory ",1)]),n("div",ie," White: "+f(P()),1),n("div",ae,[n("div",null,f(y.value)+" captures ",1),n("div",null,f(w.value)+" territory ",1),n("div",null,f(S(R))+" komi ",1)]),n("button",{onClick:F,class:"grow flex items-center rounded-md ring m-2"},[L(S(ee),{class:"block h-7 w-7 m-2"}),ce])])]))}});export{pe as default};

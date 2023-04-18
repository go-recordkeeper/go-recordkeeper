import{o as m,c as h,b as a,d as N,r as R,k,u as c,l as V,t as j,g as f,F,C as G,m as w}from"./index.2c195443.js";import{G as M}from"./Goban.62e39a82.js";import{r as S,a as $}from"./PencilIcon.915f1eba.js";import"./_plugin-vue_export-helper.a81e96fd.js";function q(y,x){return m(),h("svg",{xmlns:"http://www.w3.org/2000/svg",fill:"none",viewBox:"0 0 24 24","stroke-width":"1.5",stroke:"currentColor","aria-hidden":"true"},[a("path",{"stroke-linecap":"round","stroke-linejoin":"round",d:"M2.036 12.322a1.012 1.012 0 010-.639C3.423 7.51 7.36 4.5 12 4.5c4.638 0 8.573 3.007 9.963 7.178.07.207.07.431 0 .639C20.577 16.49 16.64 19.5 12 19.5c-4.638 0-8.573-3.007-9.963-7.178z"}),a("path",{"stroke-linecap":"round","stroke-linejoin":"round",d:"M15 12a3 3 0 11-6 0 3 3 0 016 0z"})])}const D={key:1},E={class:"flex items-center mx-auto",style:{"max-width":"calc(100vh - 128px)"}},H=N({__name:"RecordView",props:{id:{type:Number,required:!0}},setup(y){const x=y;let{id:n}=x,d=new G,i=R(0),r=k([]);d.getRecord(n).then(s=>{i.value=s.board_size;for(let t=0;t<i.value;t+=1){let e=k([]);for(let o=0;o<i.value;o+=1)e.push(" ");r.push(e)}for(let{x:t,y:e,color:o}of s.stones)r[t][e]=o});async function g(s,t){d.playStone(n,s,t).then(({add:e,remove:o})=>{for(let l of e){let{x:u,y:p,color:B}=l;r[u][p]=B}for(let l of o){let{x:u,y:p}=l;r[u][p]=" "}})}async function v(){let{add:s,remove:t}=await d.undo(n);for(let e of s){let{x:o,y:l,color:u}=e;r[o][l]=u}for(let e of t){let{x:o,y:l}=e;r[o][l]=" "}}async function b(){await d.downloadRecord(n)}async function _(){w.push({name:"update",params:{id:n}})}async function C(){w.push({name:"replay",params:{id:n}})}async function z(){await d.pass(n)}return(s,t)=>(m(),h(F,null,[c(i)?(m(),V(M,{key:0,size:c(i),matrix:c(r),onClick:g,style:{"max-width":"calc(100vh - 128px)","max-height":"calc(100vh - 128px)"}},null,8,["size","matrix"])):(m(),h("div",D,"Loading game..."+j(c(i)),1)),a("div",E,[a("button",{onClick:b,class:"rounded-md ring m-2"},[f(c(S),{class:"block h-8 w-8 m-2"})]),a("button",{onClick:_,class:"rounded-md ring m-2"},[f(c($),{class:"block h-8 w-8 m-2"})]),a("button",{onClick:C,class:"rounded-md ring m-2"},[f(c(q),{class:"block h-8 w-8 m-2"})]),a("button",{onClick:z,class:"grow m-2 h-12 rounded-md bg-red-600 text-gray-800"},"Pass"),a("button",{onClick:v,class:"grow m-2 h-12 rounded-md bg-yellow-600 text-gray-800"},"Undo")])],64))}});export{H as default};

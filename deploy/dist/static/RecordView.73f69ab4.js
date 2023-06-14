import{o as p,c as y,b as s,d as N,r as R,k,l as V,t as $,g as u,u as m,F as j,C as F,m as h}from"./index.0d93720c.js";import{G}from"./Goban.c97d41e8.js";import{r as M,a as S}from"./PencilIcon.da7d3fda.js";import{r as q}from"./FlagIcon.b240fbee.js";import"./_plugin-vue_export-helper.a81e96fd.js";function D(x,n){return p(),y("svg",{xmlns:"http://www.w3.org/2000/svg",fill:"none",viewBox:"0 0 24 24","stroke-width":"1.5",stroke:"currentColor","aria-hidden":"true"},[s("path",{"stroke-linecap":"round","stroke-linejoin":"round",d:"M2.036 12.322a1.012 1.012 0 010-.639C3.423 7.51 7.36 4.5 12 4.5c4.638 0 8.573 3.007 9.963 7.178.07.207.07.431 0 .639C20.577 16.49 16.64 19.5 12 19.5c-4.638 0-8.573-3.007-9.963-7.178z"}),s("path",{"stroke-linecap":"round","stroke-linejoin":"round",d:"M15 12a3 3 0 11-6 0 3 3 0 016 0z"})])}const E={key:1},L={class:"flex items-center mx-auto",style:{"max-width":"calc(100vh - 128px)"}},J=N({__name:"RecordView",props:{id:{type:Number,required:!0}},setup(x){const n=x,d=new F,i=R(0),a=k([]);d.getRecord(n.id).then(r=>{i.value=r.board_size;for(let t=0;t<i.value;t+=1){const o=k([]);for(let e=0;e<i.value;e+=1)o.push(" ");a.push(o)}for(const{x:t,y:o,color:e}of r.stones)a[t][o]=e});async function w(r,t){d.playStone(n.id,r,t).then(({add:o,remove:e})=>{for(const c of o){const{x:l,y:f,color:B}=c;a[l][f]=B}for(const c of e){const{x:l,y:f}=c;a[l][f]=" "}})}async function g(){const{add:r,remove:t}=await d.undo(n.id);for(const o of r){const{x:e,y:c,color:l}=o;a[e][c]=l}for(const o of t){const{x:e,y:c}=o;a[e][c]=" "}}async function v(){await d.downloadRecord(n.id)}async function b(){h.push({name:"update",params:{id:n.id}})}async function C(){h.push({name:"replay",params:{id:n.id}})}async function _(){await d.pass(n.id)}async function z(){h.push({name:"score",params:{id:n.id}})}return(r,t)=>(p(),y(j,null,[i.value?(p(),V(G,{key:0,size:i.value,matrix:a,onClick:w,style:{"max-width":"calc(100vh - 128px)","max-height":"calc(100vh - 128px)"}},null,8,["size","matrix"])):(p(),y("div",E,"Loading game..."+$(i.value),1)),s("div",L,[s("button",{onClick:v,class:"rounded-md ring m-2"},[u(m(M),{class:"block h-8 w-8 m-2"})]),s("button",{onClick:b,class:"rounded-md ring m-2"},[u(m(S),{class:"block h-8 w-8 m-2"})]),s("button",{onClick:C,class:"rounded-md ring m-2"},[u(m(D),{class:"block h-8 w-8 m-2"})]),s("button",{onClick:z,class:"rounded-md ring m-2"},[u(m(q),{class:"block h-8 w-8 m-2"})]),s("button",{onClick:_,class:"grow m-2 h-12 rounded-md bg-red-600 text-gray-800"}," Pass "),s("button",{onClick:g,class:"grow m-2 h-12 rounded-md bg-yellow-600 text-gray-800"}," Undo ")])],64))}});export{J as default};

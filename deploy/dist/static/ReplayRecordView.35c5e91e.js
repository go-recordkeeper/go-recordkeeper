import{d as T,r as d,k as h,s as z,o as m,c as _,l as V,t as v,b as e,g as N,u as R,n as D,v as E,C as F,m as G}from"./index.0d93720c.js";import{G as q}from"./Goban.c97d41e8.js";import{r as L}from"./ArrowUturnLeftIcon.1452112b.js";import"./_plugin-vue_export-helper.a81e96fd.js";const S={class:"mx-auto",style:{"max-width":"calc(100vh - 220px)"}},U={key:1},j={class:"flex items-center"},A=e("div",{class:"block h-7 w-7 m-2"},"|<",-1),H=[A],I=e("div",{class:"block h-7 w-7 m-2"},"<<",-1),J=[I],K=e("div",{class:"grow block h-7 w-7 m-2 text-center"},"<",-1),O=[K],P=e("div",{class:"grow block h-7 w-7 m-2 text-center"},">",-1),Q=[P],W=e("div",{class:"block h-7 w-7 m-2"},">>",-1),X=[W],Y=e("div",{class:"block h-7 w-7 m-2"},">|",-1),Z=[Y],$={class:"mx-auto my-4 text-center"},ee={class:"flex mx-4 my-4"},oe=["max"],le=T({__name:"ReplayRecordView",props:{id:{type:Number,required:!0}},setup(g){const k=g,p=new F,n=d(0),o=d(0),a=d([]),l=h([]);z(o,c=>{for(let t=0;t<n.value;t+=1)for(let s=0;s<n.value;s+=1)l[t][s]=" ";for(let t=0;t<c;t+=1){const{position:s,color:i,captures:M}=a.value[t];if(s){const{x:r,y:u}=s;l[r][u]=i}for(const{x:r,y:u}of M)l[r][u]=" "}}),p.getRecord(k.id).then(c=>{n.value=c.board_size,a.value=c.moves,o.value=a.value.length;for(let t=0;t<n.value;t+=1){const s=h([]);for(let i=0;i<n.value;i+=1)s.push(" ");l.push(s)}});function x(){G.back()}function f(){o.value=0}function b(){o.value=Math.max(0,o.value-10)}function w(){o.value=Math.max(0,o.value-1)}function y(){o.value=Math.min(a.value.length,o.value+1)}function C(){o.value=Math.min(a.value.length,o.value+10)}function B(){o.value=a.value.length}return(c,t)=>(m(),_("div",S,[n.value?(m(),V(q,{key:0,size:n.value,matrix:l,onClick:()=>{},style:{"max-width":"calc(100vh - 220px)","max-height":"calc(100vh - 220px)"}},null,8,["size","matrix","onClick"])):(m(),_("div",U,"Loading game..."+v(n.value),1)),e("div",j,[e("button",{onClick:x,class:"rounded-md ring m-2 bg-green-400"},[N(R(L),{class:"block h-7 w-7 m-2"})]),e("button",{onClick:f,class:"rounded-md ring m-2"},H),e("button",{onClick:b,class:"rounded-md ring m-2"},J),e("button",{onClick:w,class:"flex grow rounded-md ring m-2"},O),e("button",{onClick:y,class:"flex grow rounded-md ring m-2"},Q),e("button",{onClick:C,class:"rounded-md ring m-2"},X),e("button",{onClick:B,class:"rounded-md ring m-2"},Z)]),e("div",$," Move "+v(o.value)+" / "+v(a.value.length),1),e("div",ee,[D(e("input",{type:"range",min:0,max:a.value.length,"onUpdate:modelValue":t[0]||(t[0]=s=>o.value=s),class:"grow"},null,8,oe),[[E,o.value]])])]))}});export{le as default};

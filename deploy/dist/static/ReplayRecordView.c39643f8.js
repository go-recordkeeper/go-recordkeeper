import{o as d,c as _,b as e,d as R,r as h,i as x,s as V,u as a,j as N,t as v,f as j,m as D,v as E,n as F,C as G,k as L}from"./index.1b1e0f5e.js";import{G as q}from"./Goban.e0a5552e.js";import"./_plugin-vue_export-helper.a81e96fd.js";function S(k,g){return d(),_("svg",{xmlns:"http://www.w3.org/2000/svg",fill:"none",viewBox:"0 0 24 24","stroke-width":"1.5",stroke:"currentColor","aria-hidden":"true"},[e("path",{"stroke-linecap":"round","stroke-linejoin":"round",d:"M9 15L3 9m0 0l6-6M3 9h12a6 6 0 010 12h-3"})])}const U={class:"mx-auto",style:{"max-width":"calc(100vh - 220px)"}},A={key:1},H={class:"flex items-center"},I=e("div",{class:"block h-7 w-7 m-2"},"|<",-1),J=[I],K=e("div",{class:"block h-7 w-7 m-2"},"<<",-1),O=[K],P=e("div",{class:"grow block h-7 w-7 m-2 text-center"},"<",-1),Q=[P],W=e("div",{class:"grow block h-7 w-7 m-2 text-center"},">",-1),X=[W],Y=e("div",{class:"block h-7 w-7 m-2"},">>",-1),Z=[Y],$=e("div",{class:"block h-7 w-7 m-2"},">|",-1),ee=[$],te={class:"mx-auto my-4 text-center"},oe={class:"flex mx-4 my-4"},se=["max"],ie=R({__name:"ReplayRecordView",props:{id:{type:Number,required:!0}},setup(k){const g=k;let{id:f}=g,p=new G,n=h(0),t=h(0),l=h([]),i=x([]);V(t,c=>{for(let o=0;o<n.value;o+=1)for(let s=0;s<n.value;s+=1)i[o][s]=" ";for(let o=0;o<c;o+=1){const{position:s,color:r,captures:z}=l.value[o];if(s){const u=s%n.value,m=Math.floor(s/n.value);i[u][m]=r}for(const{x:u,y:m}of z)i[u][m]=" "}}),p.getRecord(f).then(c=>{n.value=c.board_size,l.value=c.moves,t.value=l.value.length;for(let o=0;o<n.value;o+=1){let s=x([]);for(let r=0;r<n.value;r+=1)s.push(" ");i.push(s)}});function w(){L.back()}function b(){t.value=0}function y(){t.value=Math.max(0,t.value-10)}function C(){t.value=Math.max(0,t.value-1)}function M(){t.value=Math.min(l.value.length,t.value+1)}function B(){t.value=Math.min(l.value.length,t.value+10)}function T(){t.value=l.value.length}return(c,o)=>(d(),_("div",U,[a(n)?(d(),N(q,{key:0,size:a(n),matrix:a(i),onClick:()=>{},style:{"max-width":"calc(100vh - 220px)","max-height":"calc(100vh - 220px)"}},null,8,["size","matrix","onClick"])):(d(),_("div",A,"Loading game..."+v(a(n)),1)),e("div",H,[e("button",{onClick:w,class:"rounded-md ring m-2 bg-green-400"},[j(a(S),{class:"block h-7 w-7 m-2"})]),e("button",{onClick:b,class:"rounded-md ring m-2"},J),e("button",{onClick:y,class:"rounded-md ring m-2"},O),e("button",{onClick:C,class:"flex grow rounded-md ring m-2"},Q),e("button",{onClick:M,class:"flex grow rounded-md ring m-2"},X),e("button",{onClick:B,class:"rounded-md ring m-2"},Z),e("button",{onClick:T,class:"rounded-md ring m-2"},ee)]),e("div",te,"Move "+v(a(t))+" / "+v(a(l).length),1),e("div",oe,[D(e("input",{type:"range",min:0,max:a(l).length,"onUpdate:modelValue":o[0]||(o[0]=s=>F(t)?t.value=s:t=s),class:"grow"},null,8,se),[[E,a(t)]])])]))}});export{ie as default};
import{o as d,c as _,b as e,d as R,r as h,k as x,s as V,u as n,l as N,t as v,g as D,n as E,v as F,p as G,C as L,m as j}from"./index.2c195443.js";import{G as q}from"./Goban.62e39a82.js";import"./_plugin-vue_export-helper.a81e96fd.js";function S(g,k){return d(),_("svg",{xmlns:"http://www.w3.org/2000/svg",fill:"none",viewBox:"0 0 24 24","stroke-width":"1.5",stroke:"currentColor","aria-hidden":"true"},[e("path",{"stroke-linecap":"round","stroke-linejoin":"round",d:"M9 15L3 9m0 0l6-6M3 9h12a6 6 0 010 12h-3"})])}const U={class:"mx-auto",style:{"max-width":"calc(100vh - 220px)"}},A={key:1},H={class:"flex items-center"},I=e("div",{class:"block h-7 w-7 m-2"},"|<",-1),J=[I],K=e("div",{class:"block h-7 w-7 m-2"},"<<",-1),O=[K],P=e("div",{class:"grow block h-7 w-7 m-2 text-center"},"<",-1),Q=[P],W=e("div",{class:"grow block h-7 w-7 m-2 text-center"},">",-1),X=[W],Y=e("div",{class:"block h-7 w-7 m-2"},">>",-1),Z=[Y],$=e("div",{class:"block h-7 w-7 m-2"},">|",-1),ee=[$],te={class:"mx-auto my-4 text-center"},oe={class:"flex mx-4 my-4"},se=["max"],ie=R({__name:"ReplayRecordView",props:{id:{type:Number,required:!0}},setup(g){const k=g;let{id:p}=k,f=new L,l=h(0),t=h(0),a=h([]),i=x([]);V(t,c=>{for(let o=0;o<l.value;o+=1)for(let s=0;s<l.value;s+=1)i[o][s]=" ";for(let o=0;o<c;o+=1){const{position:s,color:r,captures:z}=a.value[o];if(s){const{x:u,y:m}=s;i[u][m]=r}for(const{x:u,y:m}of z)i[u][m]=" "}}),f.getRecord(p).then(c=>{l.value=c.board_size,a.value=c.moves,t.value=a.value.length;for(let o=0;o<l.value;o+=1){let s=x([]);for(let r=0;r<l.value;r+=1)s.push(" ");i.push(s)}});function w(){j.back()}function b(){t.value=0}function y(){t.value=Math.max(0,t.value-10)}function C(){t.value=Math.max(0,t.value-1)}function B(){t.value=Math.min(a.value.length,t.value+1)}function M(){t.value=Math.min(a.value.length,t.value+10)}function T(){t.value=a.value.length}return(c,o)=>(d(),_("div",U,[n(l)?(d(),N(q,{key:0,size:n(l),matrix:n(i),onClick:()=>{},style:{"max-width":"calc(100vh - 220px)","max-height":"calc(100vh - 220px)"}},null,8,["size","matrix","onClick"])):(d(),_("div",A,"Loading game..."+v(n(l)),1)),e("div",H,[e("button",{onClick:w,class:"rounded-md ring m-2 bg-green-400"},[D(n(S),{class:"block h-7 w-7 m-2"})]),e("button",{onClick:b,class:"rounded-md ring m-2"},J),e("button",{onClick:y,class:"rounded-md ring m-2"},O),e("button",{onClick:C,class:"flex grow rounded-md ring m-2"},Q),e("button",{onClick:B,class:"flex grow rounded-md ring m-2"},X),e("button",{onClick:M,class:"rounded-md ring m-2"},Z),e("button",{onClick:T,class:"rounded-md ring m-2"},ee)]),e("div",te,"Move "+v(n(t))+" / "+v(n(a).length),1),e("div",oe,[E(e("input",{type:"range",min:0,max:n(a).length,"onUpdate:modelValue":o[0]||(o[0]=s=>G(t)?t.value=s:t=s),class:"grow"},null,8,se),[[F,n(t)]])])]))}});export{ie as default};

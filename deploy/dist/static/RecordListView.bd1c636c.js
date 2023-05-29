import{o as s,c as n,b as e,d as F,r as b,w as D,F as m,e as k,f as g,C as P,g as o,h as u,t as a,i as S,u as d,j as x}from"./index.e92037f6.js";import{r as C,a as R}from"./PencilIcon.85b18bcc.js";function L(j,r){return s(),n("svg",{xmlns:"http://www.w3.org/2000/svg",fill:"none",viewBox:"0 0 24 24","stroke-width":"1.5",stroke:"currentColor","aria-hidden":"true"},[e("path",{"stroke-linecap":"round","stroke-linejoin":"round",d:"M14.74 9l-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 01-2.244 2.077H8.084a2.25 2.25 0 01-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 00-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 013.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 00-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 00-7.5 0"})])}const E=e("div",{class:"text-xl m-4"},"Records",-1),T={key:0,class:"table-auto w-full"},A={class:"p-4"},H={class:"text-lg"},M={class:"text-md text-gray-700"},q={class:"p-4 text-sm"},z={class:"p-4"},G=["onClick"],I={class:"p-4"},J={class:"p-4"},K=["onClick"],N={class:"p-4 block"},O={class:"text-sm text-gray-700"},Q={class:"text-sm text-gray-700"},U={class:"p-1"},W=["onClick"],X={class:"p-1"},Y={class:"p-1"},Z=["onClick"],tt={key:1,class:"my-4 flex flex-row space-x-2"},et=e("div",{class:"grow"},null,-1),st={key:1,class:"w-8 h-8 rounded-md flex items-center justify-center bg-gray-200 text-gray-400"},nt=["onClick"],ot={key:0,class:"w-8 h-8 rounded-md flex items-center justify-center bg-gray-300"},at={key:1,class:"w-8 h-8 rounded-md flex items-center justify-center bg-gray-200"},ct={key:3,class:"w-8 h-8 rounded-md flex items-center justify-center bg-gray-200 text-gray-400"},lt=e("div",{class:"grow"},null,-1),ut=F({__name:"RecordListView",setup(j){const r=new P,l=b(1),c=b(null);D(()=>{r.getRecords(l.value).then(i=>{c.value=i})});async function f(i){confirm("This action cannot be undone. Are you sure?")&&(await r.deleteRecord(i),c.value=await r.getRecords(l.value))}async function v(i){await r.downloadRecord(i)}async function $(i){l.value=i}function w(){return l.value>1}function B(){w()&&(l.value-=1)}function y(){return c.value&&l.value<c.value.pages}function V(){y()&&(l.value+=1)}return(i,it)=>{const h=x("router-link"),p=x("RouterLink");return s(),n("div",null,[E,c.value?(s(),n("table",T,[e("tbody",null,[(s(!0),n(m,null,k(c.value.results,t=>(s(),n("tr",{key:t.id,class:"border-b hidden md:table-row"},[e("td",A,[o(h,{to:{name:"record",params:{id:t.id}}},{default:u(()=>[e("div",H,a(t.black_player)+" vs. "+a(t.white_player),1),e("div",M,a(t.name),1)]),_:2},1032,["to"])]),e("td",q,[o(h,{to:{name:"record",params:{id:t.id}}},{default:u(()=>[S(a(new Date(t.created).toLocaleString()),1)]),_:2},1032,["to"])]),e("td",z,[e("button",{onClick:_=>v(t.id)},[o(d(C),{class:"block h-6 w-6"})],8,G)]),e("td",I,[o(p,{to:{name:"update",params:{id:t.id}}},{default:u(()=>[o(d(R),{class:"block h-6 w-6"})]),_:2},1032,["to"])]),e("td",J,[e("button",{onClick:_=>f(t.id)},[o(d(L),{class:"block h-6 w-6"})],8,K)])]))),128)),(s(!0),n(m,null,k(c.value.results,t=>(s(),n("tr",{key:t.id,class:"border-b table-row md:hidden"},[e("td",N,[o(h,{to:{name:"record",params:{id:t.id}}},{default:u(()=>[e("div",null,a(t.black_player)+" vs. "+a(t.white_player),1),e("div",O,a(t.name),1),e("div",Q,a(new Date(t.created).toLocaleString()),1)]),_:2},1032,["to"])]),e("td",U,[e("button",{onClick:_=>v(t.id)},[o(d(C),{class:"block h-6 w-6"})],8,W)]),e("td",X,[o(p,{to:{name:"update",params:{id:t.id}}},{default:u(()=>[o(d(R),{class:"block h-6 w-6"})]),_:2},1032,["to"])]),e("td",Y,[e("button",{onClick:_=>f(t.id)},[o(d(L),{class:"block h-6 w-6"})],8,Z)])]))),128))])])):g("",!0),c.value?(s(),n("div",tt,[et,w()?(s(),n("button",{key:0,onClick:B,class:"w-8 h-8 rounded-md flex items-center justify-center bg-gray-200"}," < ")):(s(),n("button",st," < ")),(s(!0),n(m,null,k(c.value.pages,t=>(s(),n("div",{key:t,onClick:_=>$(t)},[t===l.value?(s(),n("button",ot,a(t),1)):(s(),n("button",at,a(t),1))],8,nt))),128)),y()?(s(),n("button",{key:2,onClick:V,class:"w-8 h-8 rounded-md flex items-center justify-center bg-gray-200"}," > ")):(s(),n("button",ct," > ")),lt])):g("",!0)])}}});export{ut as default};

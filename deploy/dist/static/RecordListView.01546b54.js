import{o as d,c as i,b as t,d as v,r as b,F as C,e as g,u as n,C as x,f as a,w as c,g as u,t as l,h as m}from"./index.19440734.js";import{r as R,a as L}from"./PencilIcon.130259fc.js";function y(p,s){return d(),i("svg",{xmlns:"http://www.w3.org/2000/svg",fill:"none",viewBox:"0 0 24 24","stroke-width":"1.5",stroke:"currentColor","aria-hidden":"true"},[t("path",{"stroke-linecap":"round","stroke-linejoin":"round",d:"M14.74 9l-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 01-2.244 2.077H8.084a2.25 2.25 0 01-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 00-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 013.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 00-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 00-7.5 0"})])}const B=t("div",{class:"text-xl m-4"},"Games",-1),V={class:"table-auto w-full"},$={class:"p-4"},N={class:"p-4"},D={class:"p-4"},F={class:"p-4"},S=["onClick"],j={class:"p-4"},E={class:"p-4"},G=["onClick"],q=v({__name:"RecordListView",setup(p){let s=new x,r=b([]);s.getRecords().then(o=>{r.value=o});async function h(o){await s.deleteRecord(o),r.value=await s.getRecords()}async function w(o){await s.downloadRecord(o)}return(o,H)=>{const _=m("router-link"),k=m("RouterLink");return d(),i("div",null,[B,t("table",V,[t("tbody",null,[(d(!0),i(C,null,g(n(r),e=>(d(),i("tr",{key:e.id,class:"border-b"},[t("td",$,[a(_,{to:{name:"record",params:{id:e.id}}},{default:c(()=>[u(l(e.name),1)]),_:2},1032,["to"])]),t("td",N,[a(_,{to:{name:"record",params:{id:e.id}}},{default:c(()=>[u(l(e.black_player)+" vs. "+l(e.white_player),1)]),_:2},1032,["to"])]),t("td",D,[a(_,{to:{name:"record",params:{id:e.id}}},{default:c(()=>[u(l(new Date(e.created).toLocaleString()),1)]),_:2},1032,["to"])]),t("td",F,[t("button",{onClick:f=>w(e.id)},[a(n(R),{class:"block h-6 w-6"})],8,S)]),t("td",j,[a(k,{to:{name:"update",params:{id:e.id}}},{default:c(()=>[a(n(L),{class:"block h-6 w-6"})]),_:2},1032,["to"])]),t("td",E,[t("button",{onClick:f=>h(e.id)},[a(n(y),{class:"block h-6 w-6"})],8,G)])]))),128))])])])}}});export{q as default};

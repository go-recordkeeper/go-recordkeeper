import{d as o,j as n,C as c,o as s,k as i}from"./index.e67c51dd.js";import{_}from"./GameSettings.vue_vue_type_script_setup_true_lang.f41d1af8.js";const f=o({__name:"CreateRecordView",setup(p){let a=new c;async function t(r){let e=await a.createNewRecord(r);return e.is_ok()&&await i.push({name:"record",params:{id:e.json().id}}),e}return(r,e)=>(s(),n(_,{create:t}))}});export{f as default};

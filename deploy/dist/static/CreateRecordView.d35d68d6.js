import{d as t,o as n,l as c,C as s,m as i}from"./index.5fa4ddc8.js";import{_}from"./GameSettings.vue_vue_type_script_setup_true_lang.5c1555a9.js";const f=t({__name:"CreateRecordView",setup(m){const a=new s;async function o(r){const e=await a.createNewRecord(r);return e.is_ok()&&await i.push({name:"record",params:{id:e.json().id}}),e}return(r,e)=>(n(),c(_,{create:o}))}});export{f as default};

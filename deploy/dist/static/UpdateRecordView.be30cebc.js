import{d,r as n,o as i,l as p,u as l,f,C as m,m as _}from"./index.e92037f6.js";import{_ as k}from"./GameSettings.vue_vue_type_script_setup_true_lang.d33b00c8.js";const R=d({__name:"UpdateRecordView",props:{id:{type:Number,required:!0}},setup(u){const t=u,r=new m,o=n(!1),s=n(null);r.getRecord(t.id).then(e=>{s.value=e,o.value=!0});async function c(e){const a=await r.updateRecord(t.id,e);return a.is_ok()&&await _.push({name:"record",params:{id:t.id}}),a}return(e,a)=>o.value?(i(),p(k,{key:0,defaults:l(s),update:c},null,8,["defaults"])):f("",!0)}});export{R as default};

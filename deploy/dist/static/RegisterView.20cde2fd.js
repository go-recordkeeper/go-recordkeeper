import{d as w,r as n,o as s,c as l,b as e,n as r,v as c,F as m,e as _,f as v,g as k,h as b,i as g,C as V,m as C,j as L,t as x}from"./index.cef8a470.js";const N={class:"mx-auto max-w-lg"},S=e("div",{class:"my-10 text-4xl text-center"},"Sign up",-1),U=e("div",{class:"mt-4 mb-2 mx-0 text-gray-800"},"Username",-1),B={class:"flex"},D={class:"grow"},E={key:0},R=e("div",{class:"mt-4 mb-2 mx-0 text-gray-800"},"Email",-1),F={class:"flex"},T={class:"grow"},j={key:0},A=e("div",{class:"mt-4 mb-2 mx-0 text-gray-800"},"Password",-1),M={class:"flex"},P={class:"grow"},q={key:0},z=e("button",{type:"submit",class:"my-6 py-1 w-full bg-gray-200 rounded-md text-xl"}," Sign up ",-1),G={class:"my-6 text-center"},H=g(" Already have an account? "),I=g("Log in!"),O=w({__name:"RegisterView",setup(J){const y=new V,i=n(""),u=n(""),d=n(""),a=n({});async function f(p){p.preventDefault();const o=await y.register(i.value,u.value,d.value);o.is_ok()?C.push({name:"records"}):a.value=o.error()}return(p,o)=>{const h=L("RouterLink");return s(),l("div",N,[S,e("form",{onSubmit:f},[U,e("div",B,[e("div",D,[e("div",null,[r(e("input",{"onUpdate:modelValue":o[0]||(o[0]=t=>i.value=t),class:"w-full rounded-md text-lg px-2"},null,512),[[c,i.value]])]),a.value.username?(s(),l("ul",E,[(s(!0),l(m,null,_(a.value.username,t=>(s(),l("li",{key:t,class:"text-sm text-red-600"},x(t),1))),128))])):v("",!0)])]),R,e("div",F,[e("div",T,[e("div",null,[r(e("input",{"onUpdate:modelValue":o[1]||(o[1]=t=>u.value=t),type:"email",class:"w-full rounded-md text-lg px-2"},null,512),[[c,u.value]])]),a.value.email?(s(),l("ul",j,[(s(!0),l(m,null,_(a.value.email,t=>(s(),l("li",{key:t,class:"text-sm text-red-600"},x(t),1))),128))])):v("",!0)])]),A,e("div",M,[e("div",P,[e("div",null,[r(e("input",{"onUpdate:modelValue":o[2]||(o[2]=t=>d.value=t),type:"password",class:"w-full rounded-m text-lg px-2"},null,512),[[c,d.value]])]),a.value.password?(s(),l("ul",q,[(s(!0),l(m,null,_(a.value.password,t=>(s(),l("li",{key:t,class:"text-sm text-red-600"},x(t),1))),128))])):v("",!0)])]),z],32),e("div",G,[H,k(h,{to:{name:"login"},class:"text-blue-800 underline"},{default:b(()=>[I]),_:1})])])}}});export{O as default};

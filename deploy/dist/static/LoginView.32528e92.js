import{d as y,r as d,o as s,c as o,b as e,n as c,v as m,F as _,e as p,f as u,g as h,h as b,i as x,C as k,m as V,j as C,t as v}from"./index.0d93720c.js";const L={class:"mx-auto max-w-lg"},N=e("div",{class:"mt-6 mb-2 mx-0 text-gray-800"},"Username",-1),B={class:"mb-6 flex"},D={class:"grow"},F={key:0},S=e("div",{class:"mt-6 mb-2 mx-0 text-gray-800"},"Password",-1),U={class:"mb-6 flex"},E={class:"grow"},R={key:0},T={key:0,class:"my-2 text-md text-red-600 text-center"},j=e("button",{type:"submit",class:"my-2 py-1 w-full bg-gray-200 rounded-md text-xl"}," Log in ",-1),I={class:"my-6 text-center"},M=x(" No account? "),P=x(" Sign up! "),A=y({__name:"LoginView",setup(q){const g=new k,l=d(""),r=d(""),n=d({});async function f(i){i.preventDefault();const a=await g.login(l.value,r.value);a.is_ok()?V.push({name:"records"}):n.value=a.error()}return(i,a)=>{const w=C("RouterLink");return s(),o("div",L,[e("form",{onSubmit:f,class:"my-20"},[N,e("div",B,[e("div",D,[e("div",null,[c(e("input",{"onUpdate:modelValue":a[0]||(a[0]=t=>l.value=t),class:"w-full rounded-md text-lg px-2"},null,512),[[m,l.value]])]),n.value.username?(s(),o("ul",F,[(s(!0),o(_,null,p(n.value.username,t=>(s(),o("li",{key:t,class:"text-sm text-red-600"},v(t),1))),128))])):u("",!0)])]),S,e("div",U,[e("div",E,[e("div",null,[c(e("input",{"onUpdate:modelValue":a[1]||(a[1]=t=>r.value=t),type:"password",class:"w-full rounded-md text-lg px-2"},null,512),[[m,r.value]])]),n.value.password?(s(),o("ul",R,[(s(!0),o(_,null,p(n.value.password,t=>(s(),o("li",{key:t,class:"text-sm text-red-600"},v(t),1))),128))])):u("",!0)])]),n.value.authFailed?(s(),o("div",T," Incorrect username or password ")):u("",!0),j],32),e("div",I,[M,h(w,{to:{name:"register"},class:"text-blue-800 underline"},{default:b(()=>[P]),_:1})])])}}});export{A as default};

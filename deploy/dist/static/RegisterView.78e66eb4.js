import{d as V,r as u,c as t,b as e,m as c,v as m,u as a,n as _,F as p,e as v,p as f,f as b,w as C,g as y,C as L,k as N,h as R,o as l,t as x}from"./index.e67c51dd.js";const S={class:"mx-auto max-w-lg"},U=e("div",{class:"my-10 text-4xl text-center"},"Sign up",-1),B={class:"my-6 flex"},D=e("div",{class:"mr-4"},"Username",-1),E={class:"grow"},F={key:0},T={class:"my-6 flex"},A=e("div",{class:"mr-4"},"Email",-1),M={class:"grow"},P={key:0},j={class:"my-6 flex"},q=e("div",{class:"mr-4"},"Password",-1),z={class:"grow"},G={key:0},H=e("button",{type:"submit",class:"my-2 w-full bg-gray-200 rounded-md"},"Sign up",-1),I={class:"my-6 text-center"},J=y(" Already have an account? "),K=y("Log in!"),W=V({__name:"RegisterView",setup(O){let h=new L,i=u(""),d=u(""),r=u(""),n=u({});async function g(w){w.preventDefault();let o=await h.register(i.value,d.value,r.value);o.is_ok()?N.push({name:"records"}):n.value=o.error()}return(w,o)=>{const k=R("RouterLink");return l(),t("div",S,[U,e("form",{onSubmit:g},[e("div",B,[D,e("div",E,[e("div",null,[c(e("input",{"onUpdate:modelValue":o[0]||(o[0]=s=>_(i)?i.value=s:i=s),class:"w-full rounded-md"},null,512),[[m,a(i)]])]),a(n).username?(l(),t("ul",F,[(l(!0),t(p,null,v(a(n).username,s=>(l(),t("li",{key:s,class:"text-sm text-red-600"},x(s),1))),128))])):f("",!0)])]),e("div",T,[A,e("div",M,[e("div",null,[c(e("input",{"onUpdate:modelValue":o[1]||(o[1]=s=>_(d)?d.value=s:d=s),type:"email",class:"w-full rounded-md"},null,512),[[m,a(d)]])]),a(n).email?(l(),t("ul",P,[(l(!0),t(p,null,v(a(n).email,s=>(l(),t("li",{key:s,class:"text-sm text-red-600"},x(s),1))),128))])):f("",!0)])]),e("div",j,[q,e("div",z,[e("div",null,[c(e("input",{"onUpdate:modelValue":o[2]||(o[2]=s=>_(r)?r.value=s:r=s),type:"password",class:"w-full rounded-md"},null,512),[[m,a(r)]])]),a(n).password?(l(),t("ul",G,[(l(!0),t(p,null,v(a(n).password,s=>(l(),t("li",{key:s,class:"text-sm text-red-600"},x(s),1))),128))])):f("",!0)])]),H],32),e("div",I,[J,b(k,{to:{name:"login"},class:"text-blue-800 underline"},{default:C(()=>[K]),_:1})])])}}});export{W as default};

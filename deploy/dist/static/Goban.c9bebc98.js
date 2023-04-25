import{d as b,w as g,o as p,c as v}from"./index.5fa4ddc8.js";import{_ as C}from"./_plugin-vue_export-helper.a81e96fd.js";var n=globalThis&&globalThis.__classPrivateFieldGet||function(o,i,t,s){if(t==="a"&&!s)throw new TypeError("Private accessor was defined without a getter");if(typeof i=="function"?o!==i||!s:!i.has(o))throw new TypeError("Cannot read private member from an object whose class did not declare it");return t==="m"?s:t==="a"?s.call(o):s?s.value:i.get(o)},e,d,c,f,u,m,_,w,h,l;(function(o){o[o.None=0]="None",o[o.Black=1]="Black",o[o.White=2]="White"})(l||(l={}));function y(o){switch(o){case"B":return l.Black;case"W":return l.White;case" ":return l.None}}class G{constructor(i,t,s=()=>{}){e.add(this),this.isPointerDown=!1,this.pointerCoordinates=null,this.lastMatrix=null,this.canvasSelector=i,this.size=t,this.onClick=s}initialize(){let i=n(this,e,"m",d).call(this);if(i===null)throw`Cannot locate ${this.canvasSelector}`;i.width=100*this.size,i.height=100*this.size,i.addEventListener("pointerdown",t=>{this.isPointerDown=!0,this.pointerCoordinates=n(this,e,"m",c).call(this,i,t),this.draw()}),i.addEventListener("pointerup",t=>{let{x:s,y:a}=n(this,e,"m",c).call(this,i,t);this.onClick(s,a),this.isPointerDown=!1,this.draw()}),i.addEventListener("touchend",t=>{let{x:s,y:a}=n(this,e,"m",c).call(this,i,t.changedTouches[0]);this.onClick(s,a),this.isPointerDown=!1,this.draw()}),i.addEventListener("pointermove",t=>{this.isPointerDown&&(this.pointerCoordinates=n(this,e,"m",c).call(this,i,t),this.draw())}),i.addEventListener("touchmove",t=>{this.isPointerDown&&(this.pointerCoordinates=n(this,e,"m",c).call(this,i,t.changedTouches[0]),this.draw())})}draw(i){i?this.lastMatrix=i:this.lastMatrix&&(i=this.lastMatrix);let s=n(this,e,"m",d).call(this).getContext("2d");if(n(this,e,"m",f).call(this,s),n(this,e,"m",u).call(this,s),n(this,e,"m",m).call(this,s),n(this,e,"m",_).call(this,s),i)for(let a=0;a<this.size;a+=1)for(let r=0;r<this.size;r+=1)n(this,e,"m",w).call(this,s,i[a][r],a,r)}}e=new WeakSet,d=function(){return document.querySelector(this.canvasSelector)},c=function(i,t){let{x:s,y:a}=i.getBoundingClientRect(),r=t.clientX-s,z=t.clientY-a;return{x:Math.floor(this.size*r/i.clientWidth),y:Math.floor(this.size*z/i.clientHeight)}},f=function(i){let t=n(this,e,"m",d).call(this);i.fillStyle="#f4e5b8",i.fillRect(0,0,t.width,t.height)},u=function(i){i.lineWidth=4,i.strokeStyle="#000000";for(let t=0;t<this.size;t+=1)i.beginPath(),i.moveTo(100*t+50,48),i.lineTo(100*t+50,this.size*100-48),i.stroke();for(let t=0;t<this.size;t+=1)i.beginPath(),i.moveTo(50,100*t+50),i.lineTo(this.size*100-50,100*t+50),i.stroke()},m=function(i){if(this.size%2==0)return;i.fillStyle="#000000";const t=10;n(this,e,"m",h).call(this,i,this.size*50,this.size*50,t),this.size>=9&&this.size<13&&(n(this,e,"m",h).call(this,i,250,250,t),n(this,e,"m",h).call(this,i,this.size*100-250,250,t),n(this,e,"m",h).call(this,i,250,this.size*100-250,t),n(this,e,"m",h).call(this,i,this.size*100-250,this.size*100-250,t)),this.size>=13&&(n(this,e,"m",h).call(this,i,350,350,t),n(this,e,"m",h).call(this,i,this.size*100-350,350,t),n(this,e,"m",h).call(this,i,350,this.size*100-350,t),n(this,e,"m",h).call(this,i,this.size*100-350,this.size*100-350,t)),this.size>=15&&(n(this,e,"m",h).call(this,i,350,this.size*50,t),n(this,e,"m",h).call(this,i,this.size*100-350,this.size*50,t),n(this,e,"m",h).call(this,i,this.size*50,350,t),n(this,e,"m",h).call(this,i,this.size*50,this.size*100-350,t))},_=function(i){if(this.isPointerDown&&this.pointerCoordinates){i.strokeStyle="#ff0000";let{x:t,y:s}=this.pointerCoordinates;i.beginPath(),i.moveTo(0,s*100+50),i.lineTo(this.size*100,s*100+50),i.moveTo(t*100+50,0),i.lineTo(t*100+50,this.size*100),i.stroke()}},w=function(i,t,s,a){t!=l.None&&(t==l.Black&&(i.fillStyle="#000000"),t==l.White&&(i.fillStyle="#ffffff"),n(this,e,"m",h).call(this,i,100*s+50,100*a+50,48),t==l.White&&(i.strokeStyle="#000000",i.lineWidth=2,i.beginPath(),i.arc(100*s+50,100*a+50,48,0,Math.PI*2),i.stroke()))},h=function(i,t,s,a){i.beginPath(),i.arc(t,s,a,0,Math.PI*2),i.fill()};const k=b({props:{size:{type:Number,required:!0},matrix:{type:Array,required:!0},onClick:{type:Function,required:!0}},setup(o){return{goban:new G("#goban",o.size,o.onClick)}},mounted(){this.goban.initialize(),g(()=>{const o=this.matrix.map(i=>i.map(t=>y(t)));this.goban.draw(o)})}}),P={id:"goban",class:"mx-auto w-full h-full",style:{"aspect-ratio":"1 / 1"}};function T(o,i,t,s,a,r){return p(),v("canvas",P)}const S=C(k,[["render",T]]);export{S as G};

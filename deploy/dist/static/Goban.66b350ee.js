import{d as y,w as G,o as v,c as C}from"./index.cef8a470.js";import{_ as k}from"./_plugin-vue_export-helper.a81e96fd.js";var o=globalThis&&globalThis.__classPrivateFieldGet||function(s,i,t,a){if(t==="a"&&!a)throw new TypeError("Private accessor was defined without a getter");if(typeof i=="function"?s!==i||!a:!i.has(s))throw new TypeError("Cannot read private member from an object whose class did not declare it");return t==="m"?a:t==="a"?a.call(s):a?a.value:i.get(s)},e,d,c,m,w,b,z,p,g,l,u,r;(function(s){s[s.None=0]="None",s[s.Black=1]="Black",s[s.White=2]="White"})(r||(r={}));function _(s){switch(s){case"B":return r.Black;case"W":return r.White;case" ":return r.None}}class P{constructor(i,t,a=()=>{}){e.add(this),this.isPointerDown=!1,this.pointerCoordinates=null,this.lastMatrix=null,this.lastDecorations=null,this.canvasSelector=i,this.size=t,this.onClick=a}initialize(){let i=o(this,e,"m",d).call(this);if(i===null)throw`Cannot locate ${this.canvasSelector}`;i.width=100*this.size,i.height=100*this.size,i.addEventListener("pointerdown",t=>{this.isPointerDown=!0,this.pointerCoordinates=o(this,e,"m",c).call(this,i,t),this.draw()}),i.addEventListener("pointerup",t=>{let{x:a,y:n}=o(this,e,"m",c).call(this,i,t);this.onClick(a,n),this.isPointerDown=!1,this.draw()}),i.addEventListener("pointermove",t=>{this.isPointerDown&&(this.pointerCoordinates=o(this,e,"m",c).call(this,i,t),this.draw())}),i.addEventListener("touchmove",t=>{this.isPointerDown&&(this.pointerCoordinates=o(this,e,"m",c).call(this,i,t.changedTouches[0]),this.draw())})}draw(i,t){i?this.lastMatrix=i:this.lastMatrix&&(i=this.lastMatrix),t?this.lastDecorations=t:this.lastDecorations&&(t=this.lastDecorations);let n=o(this,e,"m",d).call(this).getContext("2d");if(o(this,e,"m",m).call(this,n),o(this,e,"m",w).call(this,n),o(this,e,"m",b).call(this,n),o(this,e,"m",z).call(this,n),i)for(let h=0;h<this.size;h+=1)for(let f=0;f<this.size;f+=1)o(this,e,"m",p).call(this,n,i[h][f],h,f);if(t)for(let h=0;h<this.size;h+=1)for(let f=0;f<this.size;f+=1)o(this,e,"m",g).call(this,n,t[h][f],h,f)}}e=new WeakSet,d=function(){return document.querySelector(this.canvasSelector)},c=function(i,t){let{x:a,y:n}=i.getBoundingClientRect(),h=t.clientX-a,f=t.clientY-n;return{x:Math.floor(this.size*h/i.clientWidth),y:Math.floor(this.size*f/i.clientHeight)}},m=function(i){let t=o(this,e,"m",d).call(this);i.fillStyle="#f4e5b8",i.fillRect(0,0,t.width,t.height)},w=function(i){i.lineWidth=4,i.strokeStyle="#000000";for(let t=0;t<this.size;t+=1)i.beginPath(),i.moveTo(100*t+50,48),i.lineTo(100*t+50,this.size*100-48),i.stroke();for(let t=0;t<this.size;t+=1)i.beginPath(),i.moveTo(50,100*t+50),i.lineTo(this.size*100-50,100*t+50),i.stroke()},b=function(i){if(this.size%2==0)return;i.fillStyle="#000000";const t=10;o(this,e,"m",l).call(this,i,this.size*50,this.size*50,t),this.size>=9&&this.size<13&&(o(this,e,"m",l).call(this,i,250,250,t),o(this,e,"m",l).call(this,i,this.size*100-250,250,t),o(this,e,"m",l).call(this,i,250,this.size*100-250,t),o(this,e,"m",l).call(this,i,this.size*100-250,this.size*100-250,t)),this.size>=13&&(o(this,e,"m",l).call(this,i,350,350,t),o(this,e,"m",l).call(this,i,this.size*100-350,350,t),o(this,e,"m",l).call(this,i,350,this.size*100-350,t),o(this,e,"m",l).call(this,i,this.size*100-350,this.size*100-350,t)),this.size>=15&&(o(this,e,"m",l).call(this,i,350,this.size*50,t),o(this,e,"m",l).call(this,i,this.size*100-350,this.size*50,t),o(this,e,"m",l).call(this,i,this.size*50,350,t),o(this,e,"m",l).call(this,i,this.size*50,this.size*100-350,t))},z=function(i){if(this.isPointerDown&&this.pointerCoordinates){i.strokeStyle="#ff0000";let{x:t,y:a}=this.pointerCoordinates;i.beginPath(),i.moveTo(0,a*100+50),i.lineTo(this.size*100,a*100+50),i.moveTo(t*100+50,0),i.lineTo(t*100+50,this.size*100),i.stroke()}},p=function(i,t,a,n){t!=r.None&&(t==r.Black&&(i.fillStyle="#000000"),t==r.White&&(i.fillStyle="#ffffff"),o(this,e,"m",l).call(this,i,100*a+50,100*n+50,48),t==r.White&&(i.strokeStyle="#000000",i.lineWidth=2,i.beginPath(),i.arc(100*a+50,100*n+50,48,0,Math.PI*2),i.stroke()))},g=function(i,t,a,n){t!=r.None&&(t==r.Black&&(i.fillStyle="#000000"),t==r.White&&(i.fillStyle="#ffffff"),o(this,e,"m",u).call(this,i,100*a+25,100*n+25,50))},l=function(i,t,a,n){i.beginPath(),i.arc(t,a,n,0,Math.PI*2),i.fill()},u=function(i,t,a,n){i.fillRect(t,a,n,n)};const S=y({props:{size:{type:Number,required:!0},matrix:{type:Array,required:!0},decorations:{type:Array,required:!1},onClick:{type:Function,required:!0}},setup(s){return{goban:new P("#goban",s.size,s.onClick)}},mounted(){this.goban.initialize(),G(()=>{const s=this.matrix.map(i=>i.map(t=>_(t)));if(this.decorations){const i=this.decorations.map(t=>t.map(a=>_(a)));this.goban.draw(s,i)}else this.goban.draw(s)})}}),D={id:"goban",class:"mx-auto w-full h-full",style:{"aspect-ratio":"1 / 1"}};function T(s,i,t,a,n,h){return v(),C("canvas",D)}const W=k(S,[["render",T]]);export{W as G};

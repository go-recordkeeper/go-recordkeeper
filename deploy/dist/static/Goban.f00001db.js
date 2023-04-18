import{d as z,w as b,o as p,c as g}from"./index.e163935a.js";import{_ as v}from"./_plugin-vue_export-helper.a81e96fd.js";var o=globalThis&&globalThis.__classPrivateFieldGet||function(s,i,t,a){if(t==="a"&&!a)throw new TypeError("Private accessor was defined without a getter");if(typeof i=="function"?s!==i||!a:!i.has(s))throw new TypeError("Cannot read private member from an object whose class did not declare it");return t==="m"?a:t==="a"?a.call(s):a?a.value:i.get(s)},e,f,c,d,u,_,m,w,n,l;(function(s){s[s.None=0]="None",s[s.Black=1]="Black",s[s.White=2]="White"})(l||(l={}));function G(s){switch(s){case"B":return l.Black;case"W":return l.White;case" ":return l.None}}class y{constructor(i,t,a=()=>{}){e.add(this),this.isPointerDown=!1,this.pointerCoordinates=null,this.lastMatrix=null,this.canvasSelector=i,this.size=t,this.onClick=a}initialize(){let i=o(this,e,"m",f).call(this);if(i===null)throw`Cannot locate ${this.canvasSelector}`;i.width=100*this.size,i.height=100*this.size,i.addEventListener("pointerdown",t=>{this.isPointerDown=!0,this.pointerCoordinates=o(this,e,"m",c).call(this,i,t),this.draw()}),i.addEventListener("pointerup",t=>{let{x:a,y:h}=o(this,e,"m",c).call(this,i,t);this.onClick(a,h),this.isPointerDown=!1,this.draw()}),i.addEventListener("pointercancel",t=>{this.isPointerDown=!1,this.draw()}),i.addEventListener("pointermove",t=>{this.isPointerDown&&(this.pointerCoordinates=o(this,e,"m",c).call(this,i,t),this.draw())})}draw(i){i?this.lastMatrix=i:this.lastMatrix&&(i=this.lastMatrix);let a=o(this,e,"m",f).call(this).getContext("2d");if(o(this,e,"m",d).call(this,a),o(this,e,"m",u).call(this,a),o(this,e,"m",_).call(this,a),o(this,e,"m",m).call(this,a),i)for(let h=0;h<this.size;h+=1)for(let r=0;r<this.size;r+=1)o(this,e,"m",w).call(this,a,i[h][r],h,r)}}e=new WeakSet,f=function(){return document.querySelector(this.canvasSelector)},c=function(i,t){return{x:Math.floor(this.size*t.offsetX/i.clientWidth),y:Math.floor(this.size*t.offsetY/i.clientHeight)}},d=function(i){let t=o(this,e,"m",f).call(this);i.fillStyle="#f4e5b8",i.fillRect(0,0,t.width,t.height)},u=function(i){i.lineWidth=4,i.strokeStyle="#000000";for(let t=0;t<this.size;t+=1)i.beginPath(),i.moveTo(100*t+50,48),i.lineTo(100*t+50,this.size*100-48),i.stroke();for(let t=0;t<this.size;t+=1)i.beginPath(),i.moveTo(50,100*t+50),i.lineTo(this.size*100-50,100*t+50),i.stroke()},_=function(i){if(this.size%2==0)return;i.fillStyle="#000000";const t=10;o(this,e,"m",n).call(this,i,this.size*50,this.size*50,t),this.size>=9&&this.size<13&&(o(this,e,"m",n).call(this,i,250,250,t),o(this,e,"m",n).call(this,i,this.size*100-250,250,t),o(this,e,"m",n).call(this,i,250,this.size*100-250,t),o(this,e,"m",n).call(this,i,this.size*100-250,this.size*100-250,t)),this.size>=13&&(o(this,e,"m",n).call(this,i,350,350,t),o(this,e,"m",n).call(this,i,this.size*100-350,350,t),o(this,e,"m",n).call(this,i,350,this.size*100-350,t),o(this,e,"m",n).call(this,i,this.size*100-350,this.size*100-350,t)),this.size>=15&&(o(this,e,"m",n).call(this,i,350,this.size*50,t),o(this,e,"m",n).call(this,i,this.size*100-350,this.size*50,t),o(this,e,"m",n).call(this,i,this.size*50,350,t),o(this,e,"m",n).call(this,i,this.size*50,this.size*100-350,t))},m=function(i){if(this.isPointerDown&&this.pointerCoordinates){i.strokeStyle="#ff0000";let{x:t,y:a}=this.pointerCoordinates;i.beginPath(),i.moveTo(0,a*100+50),i.lineTo(this.size*100,a*100+50),i.moveTo(t*100+50,0),i.lineTo(t*100+50,this.size*100),i.stroke()}},w=function(i,t,a,h){t!=l.None&&(t==l.Black&&(i.fillStyle="#000000"),t==l.White&&(i.fillStyle="#ffffff"),o(this,e,"m",n).call(this,i,100*a+50,100*h+50,48),t==l.White&&(i.strokeStyle="#000000",i.lineWidth=2,i.beginPath(),i.arc(100*a+50,100*h+50,48,0,Math.PI*2),i.stroke()))},n=function(i,t,a,h){i.beginPath(),i.arc(t,a,h,0,Math.PI*2),i.fill()};const C=z({props:{size:{type:Number,required:!0},matrix:{type:Array,required:!0},onClick:{type:Function,required:!0}},setup(s){return{goban:new y("#goban",s.size,s.onClick)}},mounted(){this.goban.initialize(),b(()=>{const s=this.matrix.map(i=>i.map(t=>G(t)));this.goban.draw(s)})}}),k={id:"goban",class:"mx-auto w-full h-full",style:{"aspect-ratio":"1 / 1"}};function P(s,i,t,a,h,r){return p(),g("canvas",k)}const S=v(C,[["render",P]]);export{S as G};

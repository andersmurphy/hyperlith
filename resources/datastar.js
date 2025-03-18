// Datastar v1.0.0-beta.9
var qe=/🖕JS_DS🚀/.source,ue=qe.slice(0,5),we=qe.slice(4),H="datastar",We="Datastar-Request",$e=1e3,Ge="type module",pe=!1,Ue=!1,_e=!0,G={Morph:"morph",Inner:"inner",Outer:"outer",Prepend:"prepend",Append:"append",Before:"before",After:"after",UpsertAttributes:"upsertAttributes"},je=G.Morph,O={MergeFragments:"datastar-merge-fragments",MergeSignals:"datastar-merge-signals",RemoveFragments:"datastar-remove-fragments",RemoveSignals:"datastar-remove-signals",ExecuteScript:"datastar-execute-script"};var M=(r=>(r[r.Attribute=1]="Attribute",r[r.Watcher=2]="Watcher",r[r.Action=3]="Action",r))(M||{});var B=`${H}-signals`;var J=n=>n.trim()==="true",j=n=>n.replace(/[A-Z]+(?![a-z])|[A-Z]/g,(e,t)=>(t?"-":"")+e.toLowerCase()),K=n=>j(n).replace(/-./g,e=>e[1].toUpperCase()),Re=n=>j(n).replace(/-/g,"_"),an=n=>K(n).replace(/^./,e=>e[0].toUpperCase()),fe=n=>new Function(`return Object.assign({}, ${n})`)(),z=n=>n.startsWith("$")?n.slice(1):n,ln={kebab:j,snake:Re,pascal:an};function L(n,e){for(let t of e.get("case")||[]){let r=ln[t];r&&(n=r(n))}return n}var cn="computed",Ke={type:1,name:cn,keyReq:1,valReq:1,onLoad:({key:n,mods:e,signals:t,genRX:r})=>{n=L(n,e);let{deps:s,rxFn:i}=r();t.setComputed(n,s,i)}};var Be={type:1,name:"signals",onLoad:n=>{let{key:e,mods:t,signals:r,value:s,genRX:i}=n,o=t.has("ifmissing"),{rxFn:u}=i();if(e!==""){let p=L(e,t),v=s===""?s:u();o?r.upsertIfMissing(p,v):r.setValue(p,v)}else{let p=fe(n.value);n.value=JSON.stringify(p);let v=u();r.merge(v,o)}}};var Je={type:1,name:"star",keyReq:2,valReq:2,onLoad:()=>{alert("YOU ARE PROBABLY OVERCOMPLICATING IT")}};var Q=class{#e=0;#t;constructor(e=H){this.#t=e}with(e){if(typeof e=="string")for(let t of e.split(""))this.with(t.charCodeAt(0));else typeof e=="boolean"?this.with(1<<(e?7:3)):this.#e=this.#e*33^e;return this}get value(){return this.#e}get string(){return this.#t+Math.abs(this.#e).toString(36)}};function de(n){if(n.id)return n.id;let e=new Q,t=n;for(;t;){if(e.with(t.tagName||""),t.id){e.with(t.id);break}let r=t?.parentNode;r&&e.with([...r.children].indexOf(t)),t=r}return e.string}function me(n,e){return new Q().with(n).with(e).value}function oe(n,e){if(!n||!(n instanceof HTMLElement||n instanceof SVGElement))return null;let t=n.dataset;if("starIgnore"in t)return null;"starIgnore__self"in t||e(n);let r=n.firstElementChild;for(;r;)oe(r,e),r=r.nextElementSibling}var un="https://data-star.dev/errors";function xe(n,e,t={}){let r=new Error;r.name=`${H} ${n} error`;let s=Re(e),i=new URLSearchParams({metadata:JSON.stringify(t)}).toString(),o=JSON.stringify(t,null,2);return r.message=`${e}
More info: ${un}/${n}/${s}?${i}
Context: ${o}`,r}function Me(n,e,t={}){return xe("internal",e,Object.assign({from:n},t))}function q(n,e,t={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]}};return xe("init",n,Object.assign(r,t))}function N(n,e,t={}){let r={plugin:{name:e.plugin.name,type:M[e.plugin.type]},element:{id:e.el.id,tag:e.el.tagName},expression:{rawKey:e.rawKey,key:e.key,value:e.value,validSignals:e.signals.paths(),fnContent:e.fnContent}};return xe("runtime",n,Object.assign(r,t))}var ze="namespacedSignals",ee=n=>{document.dispatchEvent(new CustomEvent(B,{detail:Object.assign({added:[],removed:[],updated:[]},n)}))};function Xe(n,e=!1){let t={};for(let r in n)if(Object.hasOwn(n,r)){if(e&&r.startsWith("_"))continue;let s=n[r];s instanceof te?t[r]=s.value:t[r]=Xe(s)}return t}function Ye(n,e,t,r=!1){let s={added:[],removed:[],updated:[]};for(let i in e)if(Object.hasOwn(e,i)){if(i.match(/\_\_+/))throw Me(ze,"InvalidSignalKey",{key:i});let o=t?`${t}.${i}`:i,u=e[i];if(u instanceof Object&&!Array.isArray(u)){n[i]||(n[i]={});let p=Ye(n[i],u,o,r);s.added.push(...p.added.map(v=>`${o}.${v}`)),s.removed.push(...p.removed.map(v=>`${o}.${v}`)),s.updated.push(...p.updated.map(v=>`${o}.${v}`))}else{if(Object.hasOwn(n,i)){if(r)continue;let w=n[i];if(w instanceof ne){let x=w.value;w.value=u,x!==u&&s.updated.push(o);continue}}let v=new ne(u,()=>ee({updated:[o]}));n[i]=v,s.added.push(o)}}return s}function Ze(n,e){for(let t in n)if(Object.hasOwn(n,t)){let r=n[t];r instanceof te?e(t,r):Ze(r,(s,i)=>{e(`${t}.${s}`,i)})}}function pn(n,...e){let t={};for(let r of e){let s=r.split("."),i=n,o=t;for(let p=0;p<s.length-1;p++){let v=s[p];if(!i[v])return{};o[v]||(o[v]={}),i=i[v],o=o[v]}let u=s[s.length-1];o[u]=i[u]}return t}var ge=class{#e={};exists(e){return!!this.signal(e)}signal(e){let t=e.split("."),r=this.#e;for(let o=0;o<t.length-1;o++){let u=t[o];if(!r[u])return null;r=r[u]}let s=t[t.length-1],i=r[s];if(!i)throw Me(ze,"SignalNotFound",{path:e});return i}setSignal(e,t){let r=e.split("."),s=this.#e;for(let o=0;o<r.length-1;o++){let u=r[o];s[u]||(s[u]={}),s=s[u]}let i=r[r.length-1];s[i]=t}setComputed(e,t,r){let s=fn(t,r);this.setSignal(e,s)}value(e){return this.signal(e)?.value}setValue(e,t){let{signal:r}=this.upsertIfMissing(e,t),s=r.value;r.value=t,s!==t&&ee({updated:[e]})}upsertIfMissing(e,t){let r=e.split("."),s=this.#e;for(let p=0;p<r.length-1;p++){let v=r[p];s[v]||(s[v]={}),s=s[v]}let i=r[r.length-1],o=s[i];if(o instanceof ne)return{signal:o,inserted:!1};let u=new ne(t);return u.onChange=()=>{ee({updated:[e]})},s[i]=u,ee({added:[e]}),{signal:u,inserted:!0}}remove(...e){if(!e.length){this.#e={};return}let t=Array();for(let r of e){let s=r.split("."),i=this.#e;for(let u=0;u<s.length-1;u++){let p=s[u];if(!i[p])return;i=i[p]}let o=s[s.length-1];delete i[o],t.push(r)}ee({removed:t})}merge(e,t=!1){let r=Ye(this.#e,e,"",t);(r.added.length||r.removed.length||r.updated.length)&&ee(r)}subset(...e){return pn(this.values(),...e)}walk(e){Ze(this.#e,e)}paths(){let e=new Array;return this.walk(t=>e.push(t)),e}values(e=!1){return Xe(this.#e,e)}JSON(e=!0,t=!1){let r=this.values(t);return e?JSON.stringify(r,null,2):JSON.stringify(r)}toString(){return this.JSON()}};var te=class{},ne=class extends te{constructor(t,r){super();this.val=t;this.onChange=r;this.subs=new Set;this.ver=1}set value(t){this.val!==t&&(this.val=t,this.ver++,this.markDirty(),this.onChange?.(t))}markDirty(){for(let t of this.subs)t.markDirty()}get value(){return this.val}version(){return this.ver}addSubscribers(...t){for(let r of t)this.subs.add(r)}removeSubscribers(...t){for(let r of t)this.subs.delete(r)}};var Pe=class extends te{constructor(t,r){super();this.deps=t;this.fn=r;this.subs=new Set;this.isDirty=!0;this.ver=1;this.versionSum=0;for(let s of t)s.addSubscribers(this)}get value(){if(!this.isDirty)return this.val;this.isDirty=!1;let t=0;for(let i of this.deps)t+=i.version();if(t===this.versionSum)return this.val;this.versionSum=t;let r=this.deps.map(i=>i.value),s=this.fn(...r);return this.val===s?this.val:(this.val=s,this.ver++,this.val)}version(){return this.ver}markDirty(){this.isDirty=!0;for(let t of this.subs)t.markDirty()}addSubscribers(...t){for(let r of t)this.subs.add(r)}removeSubscribers(...t){for(let r of t)this.subs.delete(r)}};function fn(n,e){return new Pe(n,e)}var De=class{constructor(e,t){this.deps=e;this.fn=t;this.depsVersionSum=-1;for(let r of e)r.addSubscribers(this)}markDirty(){let e=0;for(let r of this.deps)e+=r.version();if(e===this.depsVersionSum)return;this.depsVersionSum=e;let t=this.deps.map(r=>r.value);this.fn(...t)}};function Ne(n,e){let t=new De(n,e);return t.markDirty(),()=>{for(let r of n)r.removeSubscribers(t)}}var Qe=new ge,he={},Ce=[],Z=new Map,Ie=null,Ve="";function et(n){Ve=n}function ye(...n){for(let e of n){let t={plugin:e,signals:Qe,effect:(s,i)=>Ne(s,i),actions:he,removals:Z,applyToElement:ve},r;switch(e.type){case 3:{he[e.name]=e;break}case 1:{let s=e;Ce.push(s),r=s.onGlobalInit;break}case 2:{r=e.onGlobalInit;break}default:throw q("InvalidPluginType",t)}r&&r(t)}Ce.sort((e,t)=>{let r=t.name.length-e.name.length;return r!==0?r:e.name.localeCompare(t.name)})}function Le(){queueMicrotask(()=>{ve(document.documentElement),dn()})}function ve(n){oe(n,e=>{let t=new Array,r=Z.get(e.id)||new Map,s=new Map([...r]),i=new Map;for(let o of Object.keys(e.dataset)){if(!o.startsWith(Ve))break;let u=e.dataset[o]||"",p=me(o,u);i.set(o,p),r.has(p)?s.delete(p):t.push(o)}for(let[o,u]of s)u();for(let o of t){let u=i.get(o);mn(e,o,u)}})}function dn(){Ie||(Ie=new MutationObserver(n=>{let e=new Set,t=new Set;for(let{target:r,type:s,addedNodes:i,removedNodes:o}of n)switch(s){case"childList":{for(let u of o)e.add(u);for(let u of i)t.add(u)}break;case"attributes":{t.add(r);break}}for(let r of e){let s=Z.get(r.id);if(s){for(let[i,o]of s)o(),s.delete(i);s.size===0&&Z.delete(r.id)}}for(let r of t)ve(r)}),Ie.observe(document.body,{attributes:!0,attributeOldValue:!0,childList:!0,subtree:!0}))}function mn(n,e,t){let r=K(e.slice(Ve.length)),s=Ce.find(g=>new RegExp(`^${g.name}([A-Z]|_|$)`).test(r));if(!s)return;n.id.length||(n.id=de(n));let[i,...o]=r.slice(s.name.length).split(/\_\_+/),u=i.length>0;u&&(i=K(i));let p=n.dataset[e]||"",v=p.length>0,w={signals:Qe,applyToElement:ve,effect:(g,b)=>Ne(g,b),actions:he,removals:Z,genRX:()=>gn(w,...s.argNames||[]),plugin:s,el:n,rawKey:r,key:i,value:p,mods:new Map},x=s.keyReq||0;if(u){if(x===2)throw N(`${s.name}KeyNotAllowed`,w)}else if(x===1)throw N(`${s.name}KeyRequired`,w);let A=s.valReq||0;if(v){if(A===2)throw N(`${s.name}ValueNotAllowed`,w)}else if(A===1)throw N(`${s.name}ValueRequired`,w);if(x===3||A===3){if(u&&v)throw N(`${s.name}KeyAndValueProvided`,w);if(!u&&!v)throw N(`${s.name}KeyOrValueRequired`,w)}for(let g of o){let[b,...S]=g.split(".");w.mods.set(K(b),new Set(S.map(c=>c.toLowerCase())))}let E=s.onLoad(w)??(()=>{}),y=Z.get(n.id);y||(y=new Map,Z.set(n.id,y)),y.set(t,E)}function gn(n,...e){let t="",r=new Set,s=/(\/(\\\/|[^\/])*\/|"(\\"|[^\"])*"|'(\\'|[^'])*'|`(\\`|[^`])*`|[^;])+/gm,i=n.value.trim().match(s);if(i){let g=i.length-1,b=i[g].trim();b.startsWith("return")||(i[g]=`return (${b});`),t=i.join(`;
`)}let o=new Map,u=new RegExp(`(?:${ue})(.*?)(?:${we})`,"gm");for(let g of t.matchAll(u)){let b=g[1],S=new Q("dsEscaped").with(b).string;o.set(S,b),t=t.replace(ue+b+we,S)}let p=/@(\w*)\(/gm,v=t.matchAll(p),w=new Set;for(let g of v)w.add(g[1]);let x=new RegExp(`@(${Object.keys(he).join("|")})\\(`,"gm");t=t.replaceAll(x,"ctx.actions.$1.fn(ctx,");let A=n.signals.paths();if(A.length){let g=new RegExp(`\\$(${A.join("|")})(\\W|$)`,"gm");t=t.replaceAll(g,"ctx.signals.signal('$1').value$2");let b=/ctx.signals.signal\('(.+?)'\).value/gm;for(let S of t.matchAll(b))r.add(S[1]);if(t.includes("ctx.signals.JSON()"))for(let S of n.signals.paths())r.add(S)}let E=new Array;for(let g of r){let b=n.signals.signal(g);b&&E.push(b)}for(let[g,b]of o)t=t.replace(g,b);let y=`return (() => {
${t}
})()`;n.fnContent=y;try{let g=new Function("ctx",...e,y);return{deps:E,rxFn:(...b)=>{try{return g(n,...b)}catch(S){throw N("ExecuteExpression",n,{error:S.message})}}}}catch(g){throw N("GenerateExpression",n,{error:g.message})}}ye(Je,Be,Ke);async function hn(n,e){let t=n.getReader(),r;for(;!(r=await t.read()).done;)e(r.value)}function yn(n){let e,t,r,s=!1;return function(o){e===void 0?(e=o,t=0,r=-1):e=bn(e,o);let u=e.length,p=0;for(;t<u;){s&&(e[t]===10&&(p=++t),s=!1);let v=-1;for(;t<u&&v===-1;++t)switch(e[t]){case 58:r===-1&&(r=t-p);break;case 13:s=!0;case 10:v=t;break}if(v===-1)break;n(e.subarray(p,v),r),p=t,r=-1}p===u?e=void 0:p!==0&&(e=e.subarray(p),t-=p)}}function vn(n,e,t){let r=tt(),s=new TextDecoder;return function(o,u){if(o.length===0)t?.(r),r=tt();else if(u>0){let p=s.decode(o.subarray(0,u)),v=u+(o[u+1]===32?2:1),w=s.decode(o.subarray(v));switch(p){case"data":r.data=r.data?`${r.data}
${w}`:w;break;case"event":r.event=w;break;case"id":n(r.id=w);break;case"retry":{let x=Number.parseInt(w,10);Number.isNaN(x)||e(r.retry=x);break}}}}}function bn(n,e){let t=new Uint8Array(n.length+e.length);return t.set(n),t.set(e,n.length),t}function tt(){return{data:"",event:"",id:"",retry:void 0}}var Sn="text/event-stream",nt="last-event-id";function rt(n,{signal:e,headers:t,onopen:r,onmessage:s,onclose:i,onerror:o,openWhenHidden:u,fetch:p,retryInterval:v=1e3,retryScaler:w=2,retryMaxWaitMs:x=3e4,retryMaxCount:A=10,...E}){return new Promise((y,g)=>{let b=0,S={...t};S.accept||(S.accept=Sn);let c;function d(){c.abort(),document.hidden||f()}u||document.addEventListener("visibilitychange",d);let l=0;function a(){document.removeEventListener("visibilitychange",d),window.clearTimeout(l),c.abort()}e?.addEventListener("abort",()=>{a(),y()});let h=p??window.fetch,m=r??function(){};async function f(){c=new AbortController;try{let T=await h(n,{...E,headers:S,signal:c.signal});await m(T),await hn(T.body,yn(vn(R=>{R?S[nt]=R:delete S[nt]},R=>{v=R},s))),i?.(),a(),y()}catch(T){if(!c.signal.aborted)try{let R=o?.(T)??v;window.clearTimeout(l),l=window.setTimeout(f,R),v*=w,v=Math.min(v,x),b++,b>A?(a(),g("Max retries reached.")):console.error(`Datastar failed to reach ${n.toString()} retrying in ${R}ms.`)}catch(R){a(),g(R)}}}f()})}var re=`${H}-sse`,be="started",Se="finished",st="error",it="retrying";function U(n,e){document.addEventListener(re,t=>{if(t.detail.type!==n)return;let{argsRaw:r}=t.detail;e(r)})}function ae(n,e,t){n.dispatchEvent(new CustomEvent(re,{detail:{type:e,argsRaw:t},bubbles:!0}))}var ot=n=>`${n}`.includes("text/event-stream"),_=async(n,e,t,r)=>{let{el:{id:s},el:i,signals:o}=n,{headers:u,contentType:p,includeLocal:v,selector:w,openWhenHidden:x,retryInterval:A,retryScaler:E,retryMaxWaitMs:y,retryMaxCount:g,abort:b}=Object.assign({headers:{},contentType:"json",includeLocal:!1,selector:null,openWhenHidden:!1,retryInterval:$e,retryScaler:2,retryMaxWaitMs:3e4,retryMaxCount:10,abort:void 0},r),S=e.toLowerCase(),c=()=>{};try{if(ae(i,be,{elId:s}),!t?.length)throw N("SseNoUrlProvided",n,{action:S});let d={};d[We]=!0,p==="json"&&(d["Content-Type"]="application/json");let l=Object.assign({},d,u),a={method:e,headers:l,openWhenHidden:x,retryInterval:A,retryScaler:E,retryMaxWaitMs:y,retryMaxCount:g,signal:b,onopen:async f=>{if(f.status>=400){let T=f.status.toString();ae(i,st,{status:T})}},onmessage:f=>{if(!f.event.startsWith(H))return;let T=f.event,R={},D=f.data.split(`
`);for(let C of D){let I=C.indexOf(" "),k=C.slice(0,I),V=R[k];V||(V=[],R[k]=V);let W=C.slice(I+1);V.push(W)}let P={};for(let[C,I]of Object.entries(R))P[C]=I.join(`
`);ae(i,T,P)},onerror:f=>{if(ot(f))throw N("InvalidContentType",n,{url:t});f&&(console.error(f.message),ae(i,it,{message:f.message}))}},h=new URL(t,window.location.origin),m=new URLSearchParams(h.search);if(p==="json"){let f=o.JSON(!1,!v);e==="GET"?m.set(H,f):a.body=f}else if(p==="form"){let f=w?document.querySelector(w):i.closest("form");if(f===null)throw w?N("SseFormNotFound",n,{action:S,selector:w}):N("SseClosestFormNotFound",n,{action:S});if(i!==f){let R=D=>D.preventDefault();f.addEventListener("submit",R),c=()=>f.removeEventListener("submit",R)}if(!f.checkValidity()){f.reportValidity(),c();return}let T=new FormData(f);if(e==="GET"){let R=new URLSearchParams(T);for(let[D,P]of R)m.set(D,P)}else a.body=T}else throw N("SseInvalidContentType",n,{action:S,contentType:p});h.search=m.toString();try{await rt(h.toString(),a)}catch(f){if(!ot(f))throw N("SseFetchFailed",n,{method:e,url:t,error:f})}}finally{ae(i,Se,{elId:s}),c()}};var at={type:3,name:"delete",fn:async(n,e,t)=>_(n,"DELETE",e,{...t})};var lt={type:3,name:"get",fn:async(n,e,t)=>_(n,"GET",e,{...t})};var ct={type:3,name:"patch",fn:async(n,e,t)=>_(n,"PATCH",e,{...t})};var ut={type:3,name:"post",fn:async(n,e,t)=>_(n,"POST",e,{...t})};var pt={type:3,name:"put",fn:async(n,e,t)=>_(n,"PUT",e,{...t})};var ft={type:1,name:"indicator",keyReq:3,valReq:3,onLoad:({el:n,key:e,mods:t,signals:r,value:s})=>{let i=e?L(e,t):z(s),{signal:o}=r.upsertIfMissing(i,!1),u=p=>{let{type:v,argsRaw:{elId:w}}=p.detail;if(w===n.id)switch(v){case be:o.value=!0;break;case Se:o.value=!1;break}};return document.addEventListener(re,u),()=>{o.value=!1,document.removeEventListener(re,u)}}};var dt={type:2,name:O.ExecuteScript,onGlobalInit:async n=>{U(O.ExecuteScript,({autoRemove:e=`${_e}`,attributes:t=Ge,script:r})=>{let s=J(e);if(!r?.length)throw q("NoScriptProvided",n);let i=document.createElement("script");for(let o of t.split(`
`)){let u=o.indexOf(" "),p=u?o.slice(0,u):o,v=u?o.slice(u):"";i.setAttribute(p.trim(),v.trim())}i.text=r,document.head.appendChild(i),s&&i.remove()})}};var le=document,X=!!le.startViewTransition;var mt=function(){"use strict";let n=()=>{},e={morphStyle:"outerHTML",callbacks:{beforeNodeAdded:n,afterNodeAdded:n,beforeNodeMorphed:n,afterNodeMorphed:n,beforeNodeRemoved:n,afterNodeRemoved:n,beforeAttributeUpdated:n},head:{style:"merge",shouldPreserve:A=>A.getAttribute("im-preserve")==="true",shouldReAppend:A=>A.getAttribute("im-re-append")==="true",shouldRemove:n,afterHeadMorphed:n},restoreFocus:!0};function t(A,E,y={}){A=w(A);let g=x(E),b=v(A,g,y),S=s(b,()=>u(b,A,g,c=>c.morphStyle==="innerHTML"?(i(c,A,g),Array.from(A.childNodes)):r(c,A,g)));return b.pantry.remove(),S}function r(A,E,y){let g=x(E);return i(A,g,y,E,E.nextSibling),Array.from(g.childNodes)}function s(A,E){if(!A.config.restoreFocus)return E();let y=document.activeElement;if(!(y instanceof HTMLInputElement||y instanceof HTMLTextAreaElement))return E();let{id:g,selectionStart:b,selectionEnd:S}=y,c=E();return g&&g!==document.activeElement?.id&&(y=A.target.querySelector(`[id="${g}"]`),y?.focus()),y&&!y.selectionEnd&&S&&y.setSelectionRange(b,S),c}let i=function(){function A(l,a,h,m=null,f=null){a instanceof HTMLTemplateElement&&h instanceof HTMLTemplateElement&&(a=a.content,h=h.content),m||=a.firstChild;for(let T of h.childNodes){if(m&&m!=f){let D=y(l,T,m,f);if(D){D!==m&&b(l,m,D),o(D,T,l),m=D.nextSibling;continue}}if(T instanceof Element&&l.persistentIds.has(T.id)){let D=S(a,T.id,m,l);o(D,T,l),m=D.nextSibling;continue}let R=E(a,T,m,l);R&&(m=R.nextSibling)}for(;m&&m!=f;){let T=m;m=m.nextSibling,g(l,T)}}function E(l,a,h,m){if(m.callbacks.beforeNodeAdded(a)===!1)return null;if(m.idMap.has(a)){let f=document.createElement(a.tagName);return l.insertBefore(f,h),o(f,a,m),m.callbacks.afterNodeAdded(f),f}else{let f=document.importNode(a,!0);return l.insertBefore(f,h),m.callbacks.afterNodeAdded(f),f}}let y=function(){function l(m,f,T,R){let D=null,P=f.nextSibling,C=0,I=T;for(;I&&I!=R;){if(h(I,f)){if(a(m,I,f))return I;D===null&&(m.idMap.has(I)||(D=I))}if(D===null&&P&&h(I,P)&&(C++,P=P.nextSibling,C>=2&&(D=void 0)),I.contains(document.activeElement))break;I=I.nextSibling}return D||null}function a(m,f,T){let R=m.idMap.get(f),D=m.idMap.get(T);if(!D||!R)return!1;for(let P of R)if(D.has(P))return!0;return!1}function h(m,f){let T=m,R=f;return T.nodeType===R.nodeType&&T.tagName===R.tagName&&(!T.id||T.id===R.id)}return l}();function g(l,a){if(l.idMap.has(a))d(l.pantry,a,null);else{if(l.callbacks.beforeNodeRemoved(a)===!1)return;a.parentNode?.removeChild(a),l.callbacks.afterNodeRemoved(a)}}function b(l,a,h){let m=a;for(;m&&m!==h;){let f=m;m=m.nextSibling,g(l,f)}return m}function S(l,a,h,m){let f=m.target.id===a&&m.target||m.target.querySelector(`[id="${a}"]`)||m.pantry.querySelector(`[id="${a}"]`);return c(f,m),d(l,f,h),f}function c(l,a){let h=l.id;for(;l=l.parentNode;){let m=a.idMap.get(l);m&&(m.delete(h),m.size||a.idMap.delete(l))}}function d(l,a,h){if(l.moveBefore)try{l.moveBefore(a,h)}catch{l.insertBefore(a,h)}else l.insertBefore(a,h)}return A}(),o=function(){function A(c,d,l){return l.ignoreActive&&c===document.activeElement?null:(l.callbacks.beforeNodeMorphed(c,d)===!1||(c instanceof HTMLHeadElement&&l.head.ignore||(c instanceof HTMLHeadElement&&l.head.style!=="morph"?p(c,d,l):(E(c,d,l),S(c,l)||i(l,c,d))),l.callbacks.afterNodeMorphed(c,d)),c)}function E(c,d,l){let a=d.nodeType;if(a===1){let h=c,m=d,f=h.attributes,T=m.attributes;for(let R of T)b(R.name,h,"update",l)||h.getAttribute(R.name)!==R.value&&h.setAttribute(R.name,R.value);for(let R=f.length-1;0<=R;R--){let D=f[R];if(D&&!m.hasAttribute(D.name)){if(b(D.name,h,"remove",l))continue;h.removeAttribute(D.name)}}S(h,l)||y(h,m,l)}(a===8||a===3)&&c.nodeValue!==d.nodeValue&&(c.nodeValue=d.nodeValue)}function y(c,d,l){if(c instanceof HTMLInputElement&&d instanceof HTMLInputElement&&d.type!=="file"){let a=d.value,h=c.value;g(c,d,"checked",l),g(c,d,"disabled",l),d.hasAttribute("value")?h!==a&&(b("value",c,"update",l)||(c.setAttribute("value",a),c.value=a)):b("value",c,"remove",l)||(c.value="",c.removeAttribute("value"))}else if(c instanceof HTMLOptionElement&&d instanceof HTMLOptionElement)g(c,d,"selected",l);else if(c instanceof HTMLTextAreaElement&&d instanceof HTMLTextAreaElement){let a=d.value,h=c.value;if(b("value",c,"update",l))return;a!==h&&(c.value=a),c.firstChild&&c.firstChild.nodeValue!==a&&(c.firstChild.nodeValue=a)}}function g(c,d,l,a){let h=d[l],m=c[l];if(h!==m){let f=b(l,c,"update",a);f||(c[l]=d[l]),h?f||c.setAttribute(l,""):b(l,c,"remove",a)||c.removeAttribute(l)}}function b(c,d,l,a){return c==="value"&&a.ignoreActiveValue&&d===document.activeElement?!0:a.callbacks.beforeAttributeUpdated(c,d,l)===!1}function S(c,d){return!!d.ignoreActiveValue&&c===document.activeElement&&c!==document.body}return A}();function u(A,E,y,g){if(A.head.block){let b=E.querySelector("head"),S=y.querySelector("head");if(b&&S){let c=p(b,S,A);return Promise.all(c).then(()=>{let d=Object.assign(A,{head:{block:!1,ignore:!0}});return g(d)})}}return g(A)}function p(A,E,y){let g=[],b=[],S=[],c=[],d=new Map;for(let a of E.children)d.set(a.outerHTML,a);for(let a of A.children){let h=d.has(a.outerHTML),m=y.head.shouldReAppend(a),f=y.head.shouldPreserve(a);h||f?m?b.push(a):(d.delete(a.outerHTML),S.push(a)):y.head.style==="append"?m&&(b.push(a),c.push(a)):y.head.shouldRemove(a)!==!1&&b.push(a)}c.push(...d.values());let l=[];for(let a of c){let h=document.createRange().createContextualFragment(a.outerHTML).firstChild;if(y.callbacks.beforeNodeAdded(h)!==!1){if("href"in h&&h.href||"src"in h&&h.src){let m,f=new Promise(function(T){m=T});h.addEventListener("load",function(){m()}),l.push(f)}A.appendChild(h),y.callbacks.afterNodeAdded(h),g.push(h)}}for(let a of b)y.callbacks.beforeNodeRemoved(a)!==!1&&(A.removeChild(a),y.callbacks.afterNodeRemoved(a));return y.head.afterHeadMorphed(A,{added:g,kept:S,removed:b}),l}let v=function(){function A(d,l,a){let{persistentIds:h,idMap:m}=S(d,l),f=E(a),T=f.morphStyle||"outerHTML";if(!["innerHTML","outerHTML"].includes(T))throw`Do not understand how to morph style ${T}`;return{target:d,newContent:l,config:f,morphStyle:T,ignoreActive:f.ignoreActive,ignoreActiveValue:f.ignoreActiveValue,restoreFocus:f.restoreFocus,idMap:m,persistentIds:h,pantry:y(),callbacks:f.callbacks,head:f.head}}function E(d){let l=Object.assign({},e);return Object.assign(l,d),l.callbacks=Object.assign({},e.callbacks,d.callbacks),l.head=Object.assign({},e.head,d.head),l}function y(){let d=document.createElement("div");return d.hidden=!0,document.body.insertAdjacentElement("afterend",d),d}function g(d){let l=Array.from(d.querySelectorAll("[id]"));return d.id&&l.push(d),l}function b(d,l,a,h){for(let m of h)if(l.has(m.id)){let f=m;for(;f;){let T=d.get(f);if(T==null&&(T=new Set,d.set(f,T)),T.add(m.id),f===a)break;f=f.parentElement}}}function S(d,l){let a=g(d),h=g(l),m=c(a,h),f=new Map;b(f,m,d,a);let T=l.__idiomorphRoot||l;return b(f,m,T,h),{persistentIds:m,idMap:f}}function c(d,l){let a=new Set,h=new Map;for(let{id:f,tagName:T}of d)h.has(f)?a.add(f):h.set(f,T);let m=new Set;for(let{id:f,tagName:T}of l)m.has(f)?a.add(f):h.get(f)===T&&m.add(f);for(let f of a)m.delete(f);return m}return A}(),{normalizeElement:w,normalizeParent:x}=function(){let A=new WeakSet;function E(S){return S instanceof Document?S.documentElement:S}function y(S){if(S==null)return document.createElement("div");if(typeof S=="string")return y(b(S));if(A.has(S))return S;if(S instanceof Node){if(S.parentNode)return new g(S);{let c=document.createElement("div");return c.append(S),c}}else{let c=document.createElement("div");for(let d of[...S])c.append(d);return c}}class g{constructor(c){this.originalNode=c,this.realParentNode=c.parentNode,this.previousSibling=c.previousSibling,this.nextSibling=c.nextSibling}get childNodes(){let c=[],d=this.previousSibling?this.previousSibling.nextSibling:this.realParentNode.firstChild;for(;d&&d!=this.nextSibling;)c.push(d),d=d.nextSibling;return c}querySelectorAll(c){return this.childNodes.reduce((d,l)=>{if(l instanceof Element){l.matches(c)&&d.push(l);let a=l.querySelectorAll(c);for(let h=0;h<a.length;h++)d.push(a[h])}return d},[])}insertBefore(c,d){return this.realParentNode.insertBefore(c,d)}moveBefore(c,d){return this.realParentNode.moveBefore(c,d)}get __idiomorphRoot(){return this.originalNode}}function b(S){let c=new DOMParser,d=S.replace(/<svg(\s[^>]*>|>)([\s\S]*?)<\/svg>/gim,"");if(d.match(/<\/html>/)||d.match(/<\/head>/)||d.match(/<\/body>/)){let l=c.parseFromString(S,"text/html");if(d.match(/<\/html>/))return A.add(l),l;{let a=l.firstChild;return a&&A.add(a),a}}else{let a=c.parseFromString("<body><template>"+S+"</template></body>","text/html").body.querySelector("template").content;return A.add(a),a}}return{normalizeElement:E,normalizeParent:y}}();return{morph:t,defaults:e}}();var ht={type:2,name:O.MergeFragments,onGlobalInit:async n=>{let e=document.createElement("template");U(O.MergeFragments,({fragments:t="<div></div>",selector:r="",mergeMode:s=je,useViewTransition:i=`${pe}`})=>{let o=J(i);e.innerHTML=t.trim();let u=[...e.content.children];for(let p of u){if(!(p instanceof Element))throw q("NoFragmentsFound",n);let v=r||`#${p.getAttribute("id")}`,w=[...document.querySelectorAll(v)||[]];if(!w.length)throw q("NoTargetsFound",n,{selectorOrID:v});o&&X?le.startViewTransition(()=>gt(n,s,p,w)):gt(n,s,p,w)}})}};function gt(n,e,t,r){for(let s of r)switch(e){case G.Morph:{let i=t.cloneNode(!0);oe(i,o=>{!o.id?.length&&Object.keys(o.dataset).length&&(o.id=de(o));let u=n.removals.get(o.id);if(u){let p=new Map;for(let[v,w]of u){let x=me(v,v);p.set(x,w),u.delete(v)}n.removals.set(o.id,p)}}),mt.morph(s,i);break}case G.Inner:s.innerHTML=t.outerHTML;break;case G.Outer:s.replaceWith(t);break;case G.Prepend:s.prepend(t);break;case G.Append:s.append(t);break;case G.Before:s.before(t);break;case G.After:s.after(t);break;case G.UpsertAttributes:for(let i of t.getAttributeNames()){let o=t.getAttribute(i);s.setAttribute(i,o)}break;default:throw q("InvalidMergeMode",n,{mergeMode:e})}}var yt={type:2,name:O.MergeSignals,onGlobalInit:async n=>{U(O.MergeSignals,({signals:e="{}",onlyIfMissing:t=`${Ue}`})=>{let{signals:r}=n,s=J(t);r.merge(fe(e),s)})}};var vt={type:2,name:O.RemoveFragments,onGlobalInit:async n=>{U(O.RemoveFragments,({selector:e,useViewTransition:t=`${pe}`})=>{if(!e.length)throw q("NoSelectorProvided",n);let r=J(t),s=document.querySelectorAll(e),i=()=>{for(let o of s)o.remove()};r&&X?le.startViewTransition(()=>i()):i()})}};var bt={type:2,name:O.RemoveSignals,onGlobalInit:async n=>{U(O.RemoveSignals,({paths:e=""})=>{let t=e.split(`
`).map(r=>r.trim());if(!t?.length)throw q("NoPathsProvided",n);n.signals.remove(...t)})}};var St={type:3,name:"clipboard",fn:(n,e)=>{if(!navigator.clipboard)throw N("ClipboardNotAvailable",n);navigator.clipboard.writeText(e)}};var At={type:1,name:"customValidity",keyReq:2,valReq:1,onLoad:n=>{let{el:e,genRX:t,effect:r}=n;if(!(e instanceof HTMLInputElement||e instanceof HTMLSelectElement||e instanceof HTMLTextAreaElement))throw N("CustomValidityInvalidElement",n);let{deps:s,rxFn:i}=t();return r(s,()=>{let o=i();if(typeof o!="string")throw N("CustomValidityInvalidExpression",n,{result:o});e.setCustomValidity(o)})}};var Et="once",Tt="half",wt="full",Rt={type:1,name:"intersects",keyReq:2,mods:new Set([Et,Tt,wt]),onLoad:({el:n,rawKey:e,mods:t,genRX:r})=>{let s={threshold:0};t.has(wt)?s.threshold=1:t.has(Tt)&&(s.threshold=.5);let{rxFn:i}=r(),o=new IntersectionObserver(u=>{for(let p of u)p.isIntersecting&&(i(),t.has(Et)&&(o.disconnect(),delete n.dataset[e]))},s);return o.observe(n),()=>o.disconnect()}};function ce(n){if(!n||n.size<=0)return 0;for(let e of n){if(e.endsWith("ms"))return Number(e.replace("ms",""));if(e.endsWith("s"))return Number(e.replace("s",""))*1e3;try{return Number.parseFloat(e)}catch{}}return 0}function se(n,e,t=!1){return n?n.has(e.toLowerCase()):t}function xt(n,e){return(...t)=>{setTimeout(()=>{n(...t)},e)}}function Mt(n,e,t=!1,r=!0){let s=-1,i=()=>s&&clearTimeout(s);return(...o)=>{i(),t&&!s&&n(...o),s=setTimeout(()=>{r&&n(...o),i()},e)}}function Pt(n,e,t=!0,r=!1){let s=!1;return(...i)=>{s||(t&&n(...i),s=!0,setTimeout(()=>{s=!1,r&&n(...i)},e))}}var An="evt",ie="signalsChange",En=ie.length,Dt={type:1,name:"on",keyReq:1,valReq:1,argNames:[An],onLoad:({el:n,key:e,mods:t,genRX:r})=>{let{rxFn:s}=r(),i=n;t.has("window")&&(i=window);let o=E=>{E&&((t.has("prevent")||e==="submit")&&E.preventDefault(),t.has("stop")&&E.stopPropagation()),s(E)},u=t.get("delay");if(u){let E=ce(u);o=xt(o,E)}let p=t.get("debounce");if(p){let E=ce(p),y=se(p,"leading",!1),g=!se(p,"notrail",!1);o=Mt(o,E,y,g)}let v=t.get("throttle");if(v){let E=ce(v),y=!se(v,"noleading",!1),g=se(v,"trail",!1);o=Pt(o,E,y,g)}if(t.has("viewtransition")&&X){let E=o;o=(...y)=>document.startViewTransition(()=>E(...y))}let w={capture:!0,passive:!1,once:!1};if(t.has("capture")||(w.capture=!1),t.has("passive")&&(w.passive=!0),t.has("once")&&(w.once=!0),e==="load")return setTimeout(o,0),()=>{};if(e==="interval"){let E=1e3,y=t.get("duration");y&&(E=ce(y),se(y,"leading",!1)&&o());let g=setInterval(o,E);return()=>{clearInterval(g)}}if(e==="raf"){let E,y=()=>{o(),E=requestAnimationFrame(y)};return E=requestAnimationFrame(y),()=>{E&&cancelAnimationFrame(E)}}if(e.startsWith(ie)){let E=e!==ie,y=L(K(e.slice(En)),t),g=b=>{if(E){let{added:S,removed:c,updated:d}=b.detail;if(![...S,...c,...d].some(l=>l.startsWith(y)))return}o(b)};return document.addEventListener(B,g),()=>{document.removeEventListener(B,g)}}if(t.has("outside")){i=document;let E=o;o=g=>{let b=g?.target;n.contains(b)||E(g)}}let A=j(e);return A=L(A,t),i.addEventListener(A,o,w),()=>{i.removeEventListener(A,o)}}};var Nt="session",It={type:1,name:"persist",mods:new Set([Nt]),onLoad:({key:n,mods:e,signals:t,value:r})=>{n=L(n,e),n===""&&(n=H);let s=e.has(Nt)?sessionStorage:localStorage,i=r.split(/\s+/).filter(x=>x!=="");i=i.map(x=>z(x));let o=()=>{let x=s.getItem(n)||"{}",A=JSON.parse(x);t.merge(A)},u=()=>{let x;i.length?x=t.subset(...i):x=t.values(),s.setItem(n,JSON.stringify(x))},p=n!==ie,v=L(K(n.slice(ie.length)),e),w=x=>{if(p){let{added:A,removed:E,updated:y}=x.detail;if(![...A,...E,...y].some(g=>g.startsWith(v)))return}u()};return document.addEventListener(B,w),o(),()=>{document.removeEventListener(B,w)}}};var Ct={type:1,name:"replaceUrl",keyReq:2,valReq:1,onLoad:({effect:n,genRX:e})=>{let{deps:t,rxFn:r}=e();return n(t,()=>{let s=r(),i=window.location.href,o=new URL(s,i).toString();window.history.replaceState({},"",o)})}};var Ae="smooth",ke="instant",Oe="auto",Vt="hstart",Lt="hcenter",kt="hend",Ot="hnearest",Ft="vstart",Ht="vcenter",qt="vend",Wt="vnearest",Tn="focus",Ee="center",$t="start",Gt="end",Ut="nearest",_t={type:1,name:"scrollIntoView",keyReq:2,valReq:2,mods:new Set([Ae,ke,Oe,Vt,Lt,kt,Ot,Ft,Ht,qt,Wt,Tn]),onLoad:n=>{let{el:e,mods:t,rawKey:r}=n;e.tabIndex||e.setAttribute("tabindex","0");let s={behavior:Ae,block:Ee,inline:Ee};if(t.has(Ae)&&(s.behavior=Ae),t.has(ke)&&(s.behavior=ke),t.has(Oe)&&(s.behavior=Oe),t.has(Vt)&&(s.inline=$t),t.has(Lt)&&(s.inline=Ee),t.has(kt)&&(s.inline=Gt),t.has(Ot)&&(s.inline=Ut),t.has(Ft)&&(s.block=$t),t.has(Ht)&&(s.block=Ee),t.has(qt)&&(s.block=Gt),t.has(Wt)&&(s.block=Ut),!(e instanceof HTMLElement||e instanceof SVGElement))throw N("ScrollIntoViewInvalidElement",n);e.tabIndex||e.setAttribute("tabindex","0"),e.scrollIntoView(s),t.has("focus")&&e.focus(),delete e.dataset[r]}};var jt="none",Kt="display",Bt={type:1,name:"show",keyReq:2,valReq:1,onLoad:({el:{style:n},genRX:e,effect:t})=>{let{deps:r,rxFn:s}=e();return t(r,async()=>{s()?n.display===jt&&n.removeProperty(Kt):n.setProperty(Kt,jt)})}};var Jt="view-transition",zt={type:1,name:"viewTransition",keyReq:2,valReq:1,onGlobalInit(){let n=!1;for(let e of document.head.childNodes)e instanceof HTMLMetaElement&&e.name===Jt&&(n=!0);if(!n){let e=document.createElement("meta");e.name=Jt,e.content="same-origin",document.head.appendChild(e)}},onLoad:({effect:n,el:e,genRX:t})=>{if(!X){console.error("Browser does not support view transitions");return}let{deps:r,rxFn:s}=t();return n(r,()=>{let i=s();if(!i?.length)return;let o=e.style;o.viewTransitionName=i})}};var Xt={type:1,name:"attr",valReq:1,onLoad:({el:n,key:e,effect:t,genRX:r})=>{let{deps:s,rxFn:i}=r();return e===""?t(s,async()=>{let o=i();for(let[u,p]of Object.entries(o))p===!1?n.removeAttribute(u):n.setAttribute(u,p)}):(e=j(e),t(s,async()=>{let o=!1;try{o=i()}catch{}let u;typeof o=="string"?u=o:u=JSON.stringify(o),!u||u==="false"||u==="null"||u==="undefined"?n.removeAttribute(e):n.setAttribute(e,u)}))}};var wn=/^data:(?<mime>[^;]+);base64,(?<contents>.*)$/,Yt=["change","input","keydown"],Zt={type:1,name:"bind",keyReq:3,valReq:3,onLoad:n=>{let{el:e,key:t,mods:r,signals:s,value:i,effect:o}=n,u=e,p=t?L(t,r):z(i),v=e.tagName.toLowerCase(),w=v.includes("input"),x=v.includes("select"),A=e.getAttribute("type"),E=e.hasAttribute("value"),y="",g=w&&A==="checkbox";g&&(y=E?"":!1);let b=w&&A==="number";b&&(y=0);let S=w&&A==="radio";S&&(e.getAttribute("name")?.length||e.setAttribute("name",p));let c=w&&A==="file",{signal:d,inserted:l}=s.upsertIfMissing(p,y),a=-1;Array.isArray(d.value)&&(e.getAttribute("name")===null&&e.setAttribute("name",p),a=[...document.querySelectorAll(`[name="${p}"]`)].findIndex(P=>P===n.el));let h=a>=0,m=()=>[...s.value(p)],f=()=>{let P=s.value(p);h&&!x&&(P=P[a]||y);let C=`${P}`;if(g||S)typeof P=="boolean"?u.checked=P:u.checked=C===u.value;else if(x){let I=e;if(I.multiple){if(!h)throw N("BindSelectMultiple",n);for(let k of I.options){if(k?.disabled)return;let V=b?Number(k.value):k.value;k.selected=P.includes(V)}}else I.value=C}else c||("value"in e?e.value=C:e.setAttribute("value",C))},T=async()=>{let P=s.value(p);if(h){let V=P;for(;a>=V.length;)V.push(y);P=V[a]||y}let C=(V,W)=>{let $=W;h&&!x&&($=m(),$[a]=W),s.setValue(V,$)};if(c){let V=[...u?.files||[]],W=[],$=[],Fe=[];await Promise.all(V.map(He=>new Promise(on=>{let Y=new FileReader;Y.onload=()=>{if(typeof Y.result!="string")throw N("InvalidFileResultType",n,{resultType:typeof Y.result});let Te=Y.result.match(wn);if(!Te?.groups)throw N("InvalidDataUri",n,{result:Y.result});W.push(Te.groups.contents),$.push(Te.groups.mime),Fe.push(He.name)},Y.onloadend=()=>on(void 0),Y.readAsDataURL(He)}))),C(p,W),C(`${p}Mimes`,$),C(`${p}Names`,Fe);return}let I=u.value||"",k;if(g){let V=u.checked||u.getAttribute("checked")==="true";E?k=V?I:"":k=V}else if(x){let W=[...e.selectedOptions];h?k=W.filter($=>$.selected).map($=>$.value):k=W[0]?.value||y}else typeof P=="boolean"?k=!!I:typeof P=="number"?k=Number(I):k=I||"";C(p,k)};l&&T();for(let P of Yt)e.addEventListener(P,T);let R=P=>{P.persisted&&T()};window.addEventListener("pageshow",R);let D=o([d],()=>{f()});return()=>{D();for(let P of Yt)e.removeEventListener(P,T);window.removeEventListener("pageshow",R)}}};var Qt={type:1,name:"class",valReq:1,onLoad:({el:n,key:e,mods:t,effect:r,genRX:s})=>{let i=n.classList,{deps:o,rxFn:u}=s();return r(o,()=>{if(e===""){let p=u();for(let[v,w]of Object.entries(p)){let x=v.split(/\s+/);w?i.add(...x):i.remove(...x)}}else{let p=j(e);p=L(p,t),u()?i.add(p):i.remove(p)}})}};var en={type:1,name:"ref",keyReq:3,valReq:3,onLoad:({el:n,key:e,mods:t,signals:r,value:s})=>{let i=e?L(e,t):z(s);r.setValue(i,n)}};var tn={type:1,name:"text",keyReq:2,valReq:1,onLoad:n=>{let{el:e,effect:t,genRX:r}=n,{deps:s,rxFn:i}=r();return e instanceof HTMLElement||N("TextInvalidElement",n),t(s,()=>{let o=i(n);e.textContent=`${o}`})}};var{round:Rn,max:xn,min:Mn}=Math,nn={type:3,name:"fit",fn:(n,e,t,r,s,i,o=!1,u=!1)=>{let p=(e-t)/(r-t)*(i-s)+s;return u&&(p=Rn(p)),o&&(p=xn(s,Mn(i,p))),p}};var rn={type:3,name:"setAll",fn:({signals:n},e,t)=>{n.walk((r,s)=>{r.startsWith(e)&&(s.value=t)})}};var sn={type:3,name:"toggleAll",fn:({signals:n},e)=>{n.walk((t,r)=>{t.startsWith(e)&&(r.value=!r.value)})}};ye(Xt,Zt,Qt,Dt,en,Bt,tn,ft,lt,ut,pt,ct,at,ht,yt,vt,bt,dt,St,At,Rt,It,Ct,_t,zt,nn,rn,sn);Le();export{Le as apply,ye as load,et as setAlias};
//# sourceMappingURL=datastar.js.map
 
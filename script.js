/* Restricted expression interpreter. All numeric components use arbitrary precision. */
function calculate(req) {
 const digits=Number(req.digits); if(!Number.isInteger(digits)||digits<1||digits>100) throw Error('precision');
 const m=math.create({number:'BigNumber',precision:digits+40});
 const B=x=>m.bignumber(x), C=(r,i=0)=>({r:B(r),i:B(i)}), zero=x=>x.eq(0);
 const add=(a,b)=>C(a.r.plus(b.r),a.i.plus(b.i)), neg=a=>C(a.r.neg(),a.i.neg()), sub=(a,b)=>add(a,neg(b));
 const mul=(a,b)=>C(a.r.mul(b.r).minus(a.i.mul(b.i)),a.r.mul(b.i).plus(a.i.mul(b.r)));
 const div=(a,b)=>{let d=b.r.mul(b.r).plus(b.i.mul(b.i));if(zero(d))throw Error('division');return C(a.r.mul(b.r).plus(a.i.mul(b.i)).div(d),a.i.mul(b.r).minus(a.r.mul(b.i)).div(d));};
 const abs=a=>a.r.mul(a.r).plus(a.i.mul(a.i)).sqrt();
 const sqrt=a=>{if(zero(a.i)) return a.r.gte(0)?C(a.r.sqrt()):C(0,a.r.neg().sqrt());const h=abs(a);if(a.r.gte(0)){const r=h.plus(a.r).div(2).sqrt();return C(r,a.i.div(r.mul(2)));}const i=h.minus(a.r).div(2).sqrt().mul(a.i.lt(0)?-1:1);return C(a.i.div(i.mul(2)),i);};
 const exp=a=>C(m.exp(a.r).mul(m.cos(a.i)),m.exp(a.r).mul(m.sin(a.i)));
 const log=a=>{if(zero(abs(a)))throw Error('domain');return C(m.log(abs(a)),m.atan2(a.i,a.r));};
 const pow=(a,b)=>{if(zero(b.i)&&b.r.isInteger()&&b.r.abs().lte(10000)){let n=b.r.toNumber(),out=C(1),v=a;for(let k=Math.abs(n);k;k=Math.floor(k/2)){if(k%2)out=mul(out,v);v=mul(v,v);}return n<0?div(C(1),out):out;}if(zero(abs(a))){if(zero(b.i)&&b.r.gt(0))return C(0);throw Error('domain');}return exp(mul(b,log(a)));};
 const sin=a=>C(m.sin(a.r).mul(m.cosh(a.i)),m.cos(a.r).mul(m.sinh(a.i)));
 const cos=a=>C(m.cos(a.r).mul(m.cosh(a.i)),m.sin(a.r).mul(m.sinh(a.i)).neg());
 let input=req.expression.trim().replace(/π/g,'pi').replace(/√\s*\(/g,'sqrt(').replace(/−/g,'-').replace(/×/g,'*').replace(/÷/g,'/').replace(/\btg\b/g,'tan').replace(/\bctg\b/g,'cot').replace(/\bln\b/g,'log');
 if(!input||input.length>1000)throw Error('input');
 if(!/^[\w\s.+*/^(),-]+$/.test(input))throw Error('syntax');
 const tree=m.parse(input);let count=0;const names=new Set();
 tree.traverse(n=>{if(++count>250)throw Error('complexity');if(!['OperatorNode','FunctionNode','ParenthesisNode','ConstantNode','SymbolNode'].includes(n.type))throw Error('syntax');if(n.isSymbolNode)names.add(n.name);if(n.isOperatorNode&&!['+','-','*','/','^'].includes(n.op))throw Error('syntax');if(n.isFunctionNode&&n.args.length!==1)throw Error('arguments');if(n.isFunctionNode&&!['sqrt','sin','cos','tan','cot','asin','acos','atan','sinh','cosh','tanh','exp','log','log10','abs'].includes(n.fn.name))throw Error('function');});
 if(req.mode==='symbolic') {if(req.angle==='deg'&&/\b(sin|cos|tan|cot)\s*\(/.test(input))throw Error('symbolicDegrees');let unknown=[...names].filter(x=>!['pi','e','i','sqrt','sin','cos','tan','cot','asin','acos','atan','sinh','cosh','tanh','exp','log','log10','abs'].includes(x));if(unknown.some(x=>! /^[A-Za-z][A-Za-z0-9_]*$/.test(x)))throw Error('variable');if(unknown.length) return {symbolic:true,principal:'√('+input+')',other:'−√('+input+')',formal:true};run(tree);const exact=nerdamer('sqrt('+input+')').toString();if(/NaN|Infinity|undefined/.test(exact))throw Error('domain');return {symbolic:true,principal:exact,other:exact==='0'?'0':'−('+exact+')',isZero:exact==='0'};}
 function run(n){if(n.isParenthesisNode)return run(n.content);if(n.isConstantNode)return C(n.value.toString());if(n.isSymbolNode){if(n.name==='pi')return C(m.acos(B(-1)));if(n.name==='e')return C(m.exp(B(1)));if(n.name==='i')return C(0,1);throw Error('variable');}if(n.isOperatorNode){const a=run(n.args[0]);if(n.args.length===1)return n.op==='-'?neg(a):a;const b=run(n.args[1]);return ({'+':add,'-':sub,'*':mul,'/':div,'^':pow})[n.op](a,b);}if(n.isFunctionNode){if(n.args.length!==1)throw Error('arguments');let a=run(n.args[0]),f=n.fn.name;const angle=C(m.acos(B(-1)).div(180));if(req.angle==='deg'&&['sin','cos','tan','cot'].includes(f))a=mul(a,angle);switch(f){case 'sqrt':return sqrt(a);case 'sin':return sin(a);case 'cos':return cos(a);case 'tan':if(cos(a).r.abs().lt(B(10).pow(-digits-20))&&zero(a.i))throw Error('domain');return div(sin(a),cos(a));case 'cot':if(sin(a).r.abs().lt(B(10).pow(-digits-20))&&zero(a.i))throw Error('domain');return div(cos(a),sin(a));case 'exp':return exp(a);case 'log':return log(a);case 'log10':return div(log(a),C(m.log(B(10))));case 'abs':return C(abs(a));case 'sinh':return div(sub(exp(a),exp(neg(a))),C(2));case 'cosh':return div(add(exp(a),exp(neg(a))),C(2));case 'tanh':return div(sub(exp(a),exp(neg(a))),add(exp(a),exp(neg(a))));default:if(!zero(a.i))throw Error('inverse');let v=m[f](a.r);return req.angle==='deg'?div(C(v),angle):C(v);}}throw Error('syntax');}
 const value=run(tree); const root=sqrt(value);
 const fmt=x=>{if(!x.isFinite()||Math.abs(x.e)>10000)throw Error('range');return x.toSignificantDigits(digits).toString();};
 const show=a=>zero(a.i)?fmt(a.r):zero(a.r)?fmt(a.i)+'i':fmt(a.r)+(a.i.lt(0)?' − ':' + ')+fmt(a.i.abs())+'i';
 return {principal:show(root),other:show(neg(root)),value:show(value),isZero:zero(root.r)&&zero(root.i),complex:!zero(root.i)};
}
if(typeof module!=='undefined')module.exports=calculate;


'use strict';
const $ = id => document.getElementById(id);
const languages = window.PEAR_LANGUAGES;
let lang = 'ru', scale = 100, contrast = false;
try {
 const saved = JSON.parse(localStorage.getItem('radical-settings') || '{}');
 if (Object.hasOwn(languages, saved.lang)) lang = saved.lang;
 if ([100,125,150,175,200].includes(saved.scale)) scale = saved.scale;
 contrast = !!saved.contrast;
} catch {}
const t = key => languages[lang].strings[key] || languages.en.strings[key] || key;
function save() { try { localStorage.setItem('radical-settings', JSON.stringify({lang, scale, contrast})); } catch {} }
let result = null, lastReq = null, worker = null, timer = null, currentError = null, dirty = false;
let generation = 0, powerSelection = {start:0,end:0};
function fitExpression() {
 const input = $('expression'), measure = $('inputMeasure');
 let width = 0;
 for (const line of input.value.split('\n')) { measure.textContent = line || ' '; width = Math.max(width, measure.getBoundingClientRect().width); }
 const font = parseFloat(getComputedStyle(input).fontSize);
 input.style.width = Math.max(font * 3.5, width + font) + 'px';
 input.style.height = Math.max(2, input.value.split('\n').length) * font * 1.6 + 'px';
 measure.textContent = '';
}
function applyLanguage() {
 document.documentElement.lang = lang;
 document.documentElement.dir = lang === 'ar' ? 'rtl' : 'ltr';
 document.title = 'pear — ' + t('title');
 document.querySelectorAll('[data-t]').forEach(el => {el.textContent = t(el.dataset.t);});
 for (const id of ['fontDown','fontUp','power']) {$(id).setAttribute('aria-label',t(id)); $(id).title=t(id);}
 $('powerBase').placeholder = t('basePlaceholder');
 $('powerExponent').placeholder = t('exponentPlaceholder');
 render();
 if (currentError) showError(currentError);
 save(); fitExpression();
}
function accessibility() {
 document.documentElement.style.fontSize = scale + '%';
 $('fontValue').textContent = scale + '%';
 $('fontDown').disabled = scale === 100; $('fontUp').disabled = scale === 200;
 document.body.classList.toggle('contrast',contrast);
 $('contrast').setAttribute('aria-pressed',String(contrast));
 save(); fitExpression();
}
function displayResult(el, text, formal) {
 el.replaceChildren();
 if (!formal) {el.textContent=text;return;}
 const negative = text.startsWith('−');
 if (negative) el.append(document.createTextNode('−'));
 const root = document.createElement('span'); root.className = 'formula-root';
 root.setAttribute('aria-label', text);
 const sign = document.querySelector('.root-sign').cloneNode(true);
 const inside = document.createElement('span'); inside.className = 'formula-radicand';
 inside.textContent = text.slice(negative ? 3 : 2, -1);
 root.append(sign,inside); el.append(root);
}
function render() {
 $('copy').textContent=t('copy');
 $('submit').textContent=t(worker?'busy':'calculate');
 $('copy').disabled=!result || dirty;
 if (!result) {
  $('result').textContent=worker?'…':'—';
  $('status').textContent=t(worker?'busy':currentError?'failed':'ready');
  $('detail').textContent=worker||currentError?'':t('resultHint');
  $('secondWrap').hidden=true; return;
 }
 displayResult($('result'),result.principal,result.formal);
 displayResult($('second'),result.other,result.formal);
 $('secondWrap').hidden=!$('both').checked || result.isZero;
 $('status').textContent=t(dirty?'stale':result.formal?'formal':result.symbolic?'exact':'done');
 $('detail').textContent=dirty?t('staleNote'):result.isZero?t('zeroNote'):result.formal?t('formalNote'):result.symbolic?t('exactNote'):t('valueLabel')+result.value+' · '+t('precisionLabel')+lastReq.digits;
}
function showError(code) { currentError=code; $('error').hidden=false; $('error').textContent=t(Object.hasOwn(languages.en.strings,code)?code:'generic'); }
function finish() { clearTimeout(timer); if(worker)worker.terminate();worker=null;generation++;$('submit').disabled=false;$('submit').textContent=t('calculate'); }
function fail(code) { result=null; showError(code); render(); }
function compute() {
 finish(); currentError=null; $('error').hidden=true; dirty=false;
 lastReq={expression:$('expression').value,digits:Number($('digits').value),mode:$('mode').value,angle:$('angle').value};
 result=null;
 if (!lastReq.expression.trim()) {fail('input');return;}
 try {worker=new Worker('worker.js');}catch{fail('generic');return;}
 const requestGeneration=generation;
 $('submit').disabled=true; render();
 worker.onmessage=e=>{if(requestGeneration!==generation)return;finish();if(e.data.ok){result=e.data;render();}else fail(e.data.error);};
 worker.onerror=()=>{if(requestGeneration!==generation)return;finish();fail('generic');};
 timer=setTimeout(()=>{finish();fail('timeout');},8000);
 const res = calculate(lastReq); 
 handleResult(res);
}
function markDirty() {
 if(worker){finish();result=null;}
 if(result)dirty=true;
 currentError=null;$('error').hidden=true;render();fitExpression();
}
function clearInput() {
 finish();result=null;lastReq=null;currentError=null;dirty=false;
 $('expression').value='';$('error').hidden=true;$('second').textContent='';
 $('powerBase').value='';$('powerExponent').value='';
 render();fitExpression();$('expression').focus();
}
$('calc').onsubmit=e=>{e.preventDefault();compute();};
$('expression').onkeydown=e=>{if(e.key==='Enter'&&!e.shiftKey&&!e.isComposing){e.preventDefault();$('calc').requestSubmit();}};
$('clear').onclick=clearInput;
document.querySelector('.expression-box').addEventListener('click',()=>{$('expression').focus();});
$('language').replaceChildren(...Object.entries(languages).map(([code,pack])=>{const option=document.createElement('option');option.value=code;option.textContent=pack.name;return option;}));
$('language').value=lang;
$('language').onchange=()=>{lang=$('language').value;applyLanguage();};
$('fontUp').onclick=()=>{scale=Math.min(200,scale+25);accessibility();};
$('fontDown').onclick=()=>{scale=Math.max(100,scale-25);accessibility();};
$('contrast').onclick=()=>{contrast=!contrast;accessibility();};
$('both').onchange=render;
for(const id of ['expression','mode','angle','digits'])$(id).addEventListener('input',markDirty);
$('keys').onclick=e=>{
 const button=e.target.closest('[data-insert]'); if(!button)return;
 const input=$('expression'),start=input.selectionStart,end=input.selectionEnd,text=button.dataset.insert;
 if(input.value.length-(end-start)+text.length>1000){showError('input');return;}
 input.setRangeText(text,start,end,'end');
 if(text.endsWith('()'))input.setSelectionRange(start+text.length-1,start+text.length-1);
 input.focus();markDirty();
};
$('power').onclick=()=>{
 const input=$('expression');powerSelection={start:input.selectionStart,end:input.selectionEnd};
 $('powerBase').value=input.value.slice(powerSelection.start,powerSelection.end);
 $('powerExponent').value='';$('powerError').hidden=true;updatePowerPreview();
 $('powerDialog').showModal();($('powerBase').value?$('powerExponent'):$('powerBase')).focus();
};
function updatePowerPreview(){
 $('powerPreview').replaceChildren();
 $('powerPreview').append(document.createTextNode($('powerBase').value.trim()||'□'));
 const sup=document.createElement('sup');sup.textContent=$('powerExponent').value.trim()||'□';$('powerPreview').append(sup);
}
$('powerBase').oninput=updatePowerPreview;$('powerExponent').oninput=updatePowerPreview;
$('powerCancel').onclick=()=>{$('powerDialog').close();$('power').focus();};
$('powerForm').onsubmit=e=>{
 e.preventDefault();const base=$('powerBase').value.trim(),exponent=$('powerExponent').value.trim();
 if(!base||!exponent){$('powerError').textContent=t('powerRequired');$('powerError').hidden=false;return;}
 const input=$('expression'),text='('+base+')^('+exponent+')';
 if(input.value.length-(powerSelection.end-powerSelection.start)+text.length>1000){$('powerError').textContent=t('input');$('powerError').hidden=false;return;}
 input.setRangeText(text,powerSelection.start,powerSelection.end,'end');$('powerDialog').close();input.focus();markDirty();
};
document.querySelectorAll('[data-example]').forEach(button=>button.onclick=()=>{
 $('expression').value=button.dataset.example;$('mode').value=button.dataset.symbolic?'symbolic':'numeric';$('angle').value='rad';fitExpression();compute();
});
$('copy').onclick=async()=>{
 if(!result||dirty)return;
 const text=result.principal+(!$('secondWrap').hidden?' ; '+result.other:'');
 try {
  if(navigator.clipboard&&window.isSecureContext)await navigator.clipboard.writeText(text);
  else {const field=document.createElement('textarea');field.value=text;field.style.position='fixed';field.style.opacity='0';document.body.append(field);field.select();const ok=document.execCommand('copy');field.remove();if(!ok)throw Error();}
  $('copy').textContent=t('copied');
 }catch{showError('copyFailed');}
};
window.addEventListener('resize',fitExpression);
applyLanguage();accessibility();




function handleResult(result) {
  result = result || {};
  render();
  
  if (!result.ok && result.error) {
      showError(result.error);
      return;
  }

  $('result').textContent = '';
  displayResult($('result'), result.principal, result.formal);
  displayResult($('second'), result.other, result.formal);

  currentError = null; // Сброс ошибки после успешного расчёта
}
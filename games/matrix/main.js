const canvas = document.getElementById('canv');
const ctx = canvas.getContext('2d');

const w = canvas.width = document.body.offsetWidth;
const h = canvas.height = document.body.offsetHeight;

ctx.fillStyle = '#000';
ctx.fillRect(0, 0, w, h);

// parameter defaults
let colw = 30;
let msecs = 50;
let getfontsize = () => randint(9, 12);
let getrandchar = () => {
  // String.fromCharCode(Math.random() * 128)
  let characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  return characters[Math.floor(Math.random() * characters.length)];
}

// global
let pause = false;
let cols, matrices;
updateColw(colw);

function randint(min, max) {
  return Math.floor((Math.random() * max) + min);
}

function makeMatrixObj() {
  return { y: 0, fontsize: getfontsize(), alpha: Math.random() };
}

function updateColw(cw) {
  colw = cw;
  cols = Math.floor(w / colw) + 1;
  matrices = Array(cols)
    .fill(null)
    .map(makeMatrixObj);
}

function togglePause() {
  pause = !pause;
  if (!pause) matrix();
}

function matrix() {
  if (pause) return console.log('pasued!');

  ctx.fillStyle = `rgba(0, 0, 0, ${0.08})`;
  ctx.fillRect(0, 0, w, h);

  matrices.forEach((m, i) => {
    const text = getrandchar();
    // added correction to x offset in relation to fontsize
    const x = i * colw;
    ctx.font = m.fontsize + 'pt monospace';
    ctx.fillStyle = `rgba(0, 255, 0, ${m.alpha})`;
    ctx.fillText(text, x, m.y);
    if (m.y > h * 0.1 + Math.random() * h * 50) {
      matrices[i] = makeMatrixObj();
    } else
      m.y += m.fontsize * 1.5;
  });
  if (!pause)
    setTimeout(matrix, msecs);
}

matrix();

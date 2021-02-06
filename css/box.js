// this is the most primitive way of writing js
const box = document.getElementById("box");
let start = 0;
let request;
console.log("box");

function step(timestamp) {
  if (start == undefined) {
    start = timestamp;
  }

  const elapsed = timestamp - start;
  if (elapsed < 2000) {
    box.style.transform =
      `translateX(${Math.min(0.1 * elapsed, 200)}px)`;
  }
  request = window.requestAnimationFrame(step);
}
window.requestAnimationFrame(step);

window.cancelAnimationFrame(request);

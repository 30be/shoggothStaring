// SPDX-License-Identifier: MIT
// See LICENSE for details.

(() => {
  const canvas = document.getElementById('ebbinghaus');
  canvas.width = 700;
  canvas.height = 300;
  const ctx = canvas.getContext('2d');
  const slider = document.getElementById('ebbinghaus-slider');

  const padding = 70;
  const AXIS_COLOR = window.getComputedStyle(document.body).color;

  const decayFunction = (x, k) => Math.exp(-k * x);

  const drawPlot = (decayRate) => {
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    ctx.strokeStyle = AXIS_COLOR;
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    ctx.moveTo(padding, canvas.height - padding);
    ctx.lineTo(padding, padding);
    ctx.stroke();

    ctx.beginPath();
    ctx.moveTo(padding, canvas.height - padding);
    ctx.lineTo(canvas.width, canvas.height - padding);
    ctx.stroke();

    ctx.fillStyle = AXIS_COLOR;
    ctx.font = '20px sans-serif';
    ctx.textAlign = 'center';
    ctx.fillText('time →', canvas.width - padding / 2, canvas.height - padding / 2);

    ctx.save();
    ctx.translate(padding - 20, canvas.height / 2);
    ctx.rotate(-Math.PI / 2);
    ctx.fillText('strength of memory', 0, 0);
    ctx.restore();

    ctx.beginPath();
    ctx.strokeStyle = "red";
    ctx.lineWidth = 4;

    const plotW = canvas.width - padding;
    const plotH = canvas.height - 2 * padding;

    ctx.moveTo(padding, canvas.height - padding - plotH * decayFunction(0, decayRate));

    for (let i = 0; i <= plotW; i++) {
      const x_normalized = i / plotW;
      const y_normalized = decayFunction(x_normalized * 10, decayRate * 10);
      const x_canvas = padding + i;
      const y_canvas = canvas.height - padding - plotH * y_normalized;
      ctx.lineTo(x_canvas, y_canvas);
    }

    ctx.stroke();
  };

  slider.addEventListener('input', () => drawPlot(parseFloat(slider.value)));

  drawPlot(parseFloat(slider.value));

})();

const canvas: HTMLCanvasElement = document.createElement('canvas');
const ctx: CanvasRenderingContext2D = canvas.getContext('2d');
canvas.width = 512;
canvas.height = 480;
document.body.append(canvas);

interface GameAsserts {
  bgImage: ImageData;
  heroImage: ImageData;
  monsterImage: ImageData;
  heroImage: ImageData;

};

interface GameConfig {
  bgReady: boolean;
  heroReady: boolean;
  monsterReady: boolean;
  redReady: boolean;
  fireReady: boolean;
  anger: number;
};


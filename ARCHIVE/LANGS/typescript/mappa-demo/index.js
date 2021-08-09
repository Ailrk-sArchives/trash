let myMap;

let canvas;

const mappa = new Mappa('Leaflet');

const options = {
    lat: 30,
    lng: 113,
    zoom: 5,
    style: "http://{s}.tile.osm.org/{z}/{x}/{y}.png"
}

function setup() {
    /* create map */
    canvas = createCanvas(480, 480);

    // background(100);

    myMap = mappa.tileMap(options);
    myMap.overlay(canvas);

    /* modify map */
    fill(200, 100, 100);
    myMap.onChange(drawPont);
}

function draw() {
}

function drawPont() {
    clear();
    const nantong = myMap.latLngToPixel(32.1, 120.8);
    const chongqing = myMap.latLngToPixel(29.6, 106.5);
    const chengdu = myMap.latLngToPixel(30.7, 104.1);
    const shanghai = myMap.latLngToPixel(31.2, 121.4);
    const wuhan = myMap.latLngToPixel(30.6, 114.3);
    const yibin = myMap.latLngToPixel(28.8, 104.6);
    const hefei = myMap.latLngToPixel(31.8, 117.2);
    const changzhou = myMap.latLngToPixel(31.8, 120);
    ellipse(nantong.x, nantong.y, 10, 10);
    ellipse(chongqing.x, chongqing.y, 10, 10);
    ellipse(chengdu.x, chengdu.y, 10, 10);
    ellipse(shanghai.x, shanghai.y, 10, 10);
    ellipse(wuhan.x, wuhan.y, 10, 10);
    ellipse(yibin.x, yibin.y, 10, 10);
    ellipse(hefei.x, hefei.y, 10, 10);
    ellipse(changzhou.x, changzhou.y, 10, 10);
}



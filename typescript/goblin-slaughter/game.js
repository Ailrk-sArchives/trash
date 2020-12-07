let canvas = document.createElement('canvas');
let ctx = canvas.getContext('2d');
canvas.width = 512;
canvas.height = 480;
document.body.append(canvas);

let bgReady = false;
let heroReady = false;
let monsterReady = false;
let redReady = false;
let fireReady = false;

let anger = 600;

// bg image
let bgImage = new Image();
bgImage.onload = function () {
    bgReady = true;
};
bgImage.src = "./background.png";

// hero image
let heroImage = new Image();
heroImage.onload = function () {
    heroReady = true;
};
heroImage.src = "./hero.png";

// monster image
let monsterImage = new Image();
monsterImage.onload = function () {
    monsterReady= true;
};
monsterImage.src = "./monster.png";

let redImage = new Image();
redImage.onload = function () {
    redReady= true;
};
redImage.src = "./goblinred.png";

let fireImage = new Image();
fireImage.onload = function () {
    fireReady  = true;
};
fireImage.src = "./fire.png"

// global randomier
let getRand = function() {
    return Math.random();
}


// Game objects
let hero = {
    speed: 146,
    x: 0,
    y: 0,
    dy: 0,
    dx: 0
};

let fireArrow = null;
let fire = function() {
    return {
        x: 0,
        y: 0,
        dx: 0,
        dy: 0,
        speed: 10,
        init: function(hero) {
            this.x = hero.x;
            this.y = hero.y;
            this.dx = hero.dy;
            this.dy = hero.dy;
        },

        move: function() {
            this.x +=  (this.dx > 0 ? 1 : -1) * this.speed;
            this.y +=  (this.dy > 0 ? 1 : -1) * this.speed;
        }
    };
}

let red = {
    speed: 2.5,
    x: 0,
    y: 0,
    xdirection: 1,
    ydirection: 1,
    hp: 1200,
    time_stayed: 0,
    move: function(time_eaplsed) {
        this.time_stayed += time_eaplsed;
        if (this.time_stayed >= 2) {
            this.xdirection = (Math.floor(getRand() * 2) == 1 ? 1 : -1);
            this.ydirection = (Math.floor(getRand() * 2) == 1 ? 1 : -1);
            this.time_stayed = 0;
        }

        this.x +=  this.xdirection * this.speed;
        this.y +=  this.ydirection * this.speed;
        if (this.y > canvas.height - 32) this.y = canvas.height - 32;
        if (this.y < 20) this.y = 20;
        if (this.x > canvas.width - 32) this.x = canvas.width - 32;
        if (this.x < 20) this.x = 20;

    },
};

let monster1 = {
    speed: 1,
    x: 0,
    y: 0,
    xdirection: 1,
    ydirection: 1,
    hp: 100,
    time_stayed: 0,
    move: function(time_eaplsed) {
        this.time_stayed += time_eaplsed;
        if (this.time_stayed >= 0.3) {
            this.xdirection = (Math.floor(getRand() * 2) == 1 ? 1 : -1);
            this.ydirection = (Math.floor(getRand() * 2) == 1 ? 1 : -1);
            this.time_stayed = 0;
        }

        this.x +=  this.xdirection * this.speed;
        this.y +=  this.ydirection * this.speed;
        if (this.y > canvas.height) this.y = canvas.height;
        if (this.y < 0) this.y = 0;
        if (this.x > canvas.width) this.x = canvas.width;
        if (this.x < 0) this.x = 0;

    },

};

let monster2 = {
    speed: 0.5,
    x: 0,
    y: 0,
    hp: 50,
    time_stayed: 0,
    move: function(time_eaplsed) {
        this.time_stayed += time_eaplsed;
        if (this.time_stayed >= 0.3) {
            this.x +=  (Math.floor(getRand() * 2) == 1 ? 1 : -1) * 20;
            this.y +=  (Math.floor(getRand() * 2) == 1 ? 1 : -1) * 20;
            if (this.y > canvas.height) this.y = canvas.height;
            if (this.y < 0) this.y = 0;
            if (this.x > canvas.width) this.x = canvas.width;
            if (this.x < 0) this.x = 0;

            this.time_stayed = 0;

        }
    },

};
let monster3 = {
    speed: 1,
    x: 0,
    y: 0,
    xdirection: 1,
    ydirection: 1,
    hp: 100,
    time_stayed: 0,
    move: function(time_eaplsed) {
        this.time_stayed += time_eaplsed;
        if (this.time_stayed >= 0.3) {
            this.xdirection = (Math.floor(getRand() * 2) == 1 ? 1 : -1);
            this.ydirection = (Math.floor(getRand() * 2) == 1 ? 1 : -1);
            this.time_stayed = 0;
        }

        this.x += this.xdirection * this.speed;
        this.y += this.ydirection * this.speed;
        if (this.y > canvas.height + 32) this.y = canvas.height + 32;
        if (this.y < 0) this.y = 0;
        if (this.x > canvas.width + 32) this.x = canvas.width + 32;
        if (this.x < 0) this.x = 0;

    },

};

let monster4 = {
    speed: 1,
    x: 0,
    y: 0,
    xdirection: 1,
    ydirection: 1,
    hp: 100,
    time_stayed: 0,
    move: function(time_eaplsed) {
        this.time_stayed += time_eaplsed;
        if (this.time_stayed >= 0.3) {
            this.xdirection = (Math.floor(getRand() * 2) == 1 ? 1 : -1);
            this.ydirection = (Math.floor(getRand() * 2) == 1 ? 1 : -1);
            this.time_stayed = 0;
        }

        this.x +=  this.xdirection * this.speed;
        this.y +=  this.ydirection * this.speed;
        if (this.y > canvas.height) this.y = canvas.height;
        if (this.y < 0) this.y = 0;
        if (this.x > canvas.width) this.x = canvas.width;
        if (this.x < 0) this.x = 0;

    },

};


let monsterCaught = 0;

// handle keyboard controls
let keysDown = {};

addEventListener("keydown", function(e) {
    keysDown[e.keyCode] = true;
}, false);

addEventListener("keyup", function(e) {
    delete keysDown[e.keyCode];
}, false);

// reset the game when a goblin is caught.
let resetRed = function () {
    red.x = 32 + (Math.random() * (canvas.width - 64));
    red.y = 32 + (Math.random() * (canvas.width - 64));
    red.hp = 1500;
};
let resetMonster1 = function() {
    monster1.x = 32 + (Math.random() * (canvas.width - 64));
    monster1.y = 32 + (Math.random() * (canvas.width - 64));
    monster1.hp = 100;
};
let resetMonster2 = function() {
    monster2.x = 32 + (Math.random() * (canvas.width - 64));
    monster2.y = 32 + (Math.random() * (canvas.width - 64));
    monster2.hp = 50;
};
let resetMonster3 = function() {
    monster3.x = 32 + (Math.random() * (canvas.width - 64));
    monster3.y = 32 + (Math.random() * (canvas.width - 64));
    monster3.hp = 50;
};
let resetMonster4 = function() {
    monster3.x = 32 + (Math.random() * (canvas.width - 64));
    monster3.y = 32 + (Math.random() * (canvas.width - 64));
    monster3.hp = 50;
};


let reset = function() {
    resetRed();
    resetMonster1();
    resetMonster2();
    resetMonster3();
    resetMonster4();
}


//update
let update = function (modifier) {
    monster1.move(modifier);
    monster2.move(modifier);
    monster3.move(modifier);
    monster4.move(modifier);

    if (fireArrow) {
        fireArrow.move();
    }

    if (81 in keysDown && anger > 300) {  // press A fire.
        fireArrow = fire();
        fireArrow.init(hero);
        anger -= 300;
    }

    if (38 in keysDown) {
        hero.dy = -hero.speed * modifier;
        hero.y += hero.dy;
    }

    if (40 in keysDown) {
        hero.dy = hero.speed * modifier;
        hero.y += hero.dy;
    }

    if (37 in keysDown) {
        hero.dx = -hero.speed * modifier;
        hero.x += hero.dx;
    }

    if (39 in keysDown) {
        hero.dx = hero.speed * modifier;
        hero.x += hero.dx;
    }

    // collision
    if (
        hero.x <= (red.x + 32) &&
        red.x <= (hero.x + 32) &&
        hero.y <= (red.y + 32) &&
        red.y <= (hero.y + 32)
    ) {
        red.hp -= 1;
        anger += 3;
    }
    if (
        monster1.x <= (red.x + 32) &&
        red.x <= (monster1.x + 32) &&
        monster1.y <= (red.y + 32) &&
        red.y <= (monster1.y + 32)
    ) {
        monster1.hp -= 20;
        red.hp += 100;
        if (monster1.hp <= 0) {
            resetMonster1();
        }
    }
    if (
        monster2.x <= (red.x + 32) &&
        red.x <= (monster2.x + 32) &&
        monster2.y <= (red.y + 32) &&
        red.y <= (monster2.y + 32)
    ) {
        monster2.hp -= 20;
        red.hp += 50;
        if (monster2.hp <= 0) {
            resetMonster2();
        }
    }
    if (
        monster3.x <= (red.x + 32) &&
        red.x <= (monster3.x + 32) &&
        monster3.y <= (red.y + 32) &&
        red.y <= (monster3.y + 32)
    ) {
        monster3.hp -= 20;
        red.hp += 50;
        if (monster3.hp <= 0) {
            resetMonster3();
        }
    }
    if (
        monster4.x <= (red.x + 32) &&
        red.x <= (monster4.x + 32) &&
        monster4.y <= (red.y + 32) &&
        red.y <= (monster4.y + 32)
    ) {
        monster4.hp -= 20;
        red.hp += 30;
        if (monster4.hp <= 0) {
            resetMonster4();
        }
    }
    if (
        fireArrow &&
        fireArrow.x <= (red.x + 32) &&
        red.x <= (fireArrow.x + 32) &&
        fireArrow.y <= (red.y + 32) &&
        red.y <= (fireArrow.y + 32)
    ) {
        red.hp -= 500;
        fireArrow = null;
    }
    if (red.hp >= 0) {
        red.move(modifier);
    }

};

let render = function () {
    if (bgReady) {
        ctx.drawImage(bgImage, 0, 0);
    }

    if (heroReady) {
        ctx.drawImage(heroImage, hero.x, hero.y);
    }

    if (monsterReady) {
        ctx.drawImage(monsterImage, monster1.x, monster1.y);
        ctx.drawImage(monsterImage, monster2.x, monster2.y);
        ctx.drawImage(monsterImage, monster3.x, monster3.y);
        ctx.drawImage(monsterImage, monster4.x, monster4.y);
    }

    if (redReady) {
        ctx.drawImage(redImage, red.x, red.y);
    }

    if (fireArrow && fireReady) {
        ctx.drawImage(fireImage, fireArrow.x, fireArrow.y);
    }

    ctx.fillStyle = "rgb(250, 250, 250)";
    ctx.font = "14px Helvetica";
    ctx.textAlign = "left";
    ctx.textBaseline = "top";
    ctx.fillText("Anger: " + anger + "/300", 32, 32);
    ctx.fillText("HP: " + red.hp, red.x, red.y - 10);
//    ctx.fillText("dx, dy: " + hero.dx + ", " + hero.dy, 32, 64);
    ctx.fillText("Anger > 500 press q use fire", 32, 440);
    if (red.hp <= 0) {
        ctx.fillText("YOU WIN!", canvas.width / 2 - 30, canvas.height / 2);
    }
}

// main loop
let main = function () {
    let now = Date.now();
    let delta = now - then;

    update(delta / 1000);
    render();

    then = now;
    requestAnimationFrame(main);
}

let then = Date.now();
reset();
main();

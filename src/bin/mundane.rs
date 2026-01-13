use macroquad::{
    color::{BLACK, BLUE, RED},
    input::{KeyCode, is_key_pressed},
    math::Vec2,
    miniquad::window::{screen_size, set_fullscreen},
    shapes::{draw_circle, draw_rectangle},
    window::{clear_background, next_frame},
};

#[macroquad::main("mundane")]
async fn main() {
    let game = Game::init_new();
    while game.is_running() {
        game.update();
        game.draw();
        next_frame().await
    }
}

struct Game {
    player: Player,
    red_square: RedSquare,
}

impl Game {
    fn init_new() -> Self {
        let res = Self::new();
        res.init();
        res
    }

    fn new() -> Self {
        let player = Player::new();
        let red_square = RedSquare::new();
        Self { player, red_square }
    }

    fn init(&self) {
        set_fullscreen(true);
    }

    fn is_running(&self) -> bool {
        !is_key_pressed(KeyCode::Escape)
    }

    fn update(&self) {}
}

trait Draw {
    fn draw(&self);
}

impl Draw for Game {
    fn draw(&self) {
        clear_background(BLACK);
        self.red_square.draw();
        self.player.draw();
    }
}

struct Player {}

impl Player {
    fn new() -> Self {
        Self {}
    }
}

impl Draw for Player {
    fn draw(&self) {
        let pos = get_screen_size() * 0.5;
        draw_circle(pos.x, pos.y, 25.0, BLUE);
    }
}

fn get_screen_size() -> Vec2 {
    screen_size().into()
}

struct RedSquare {}

impl RedSquare {
    fn new() -> Self {
        Self {}
    }
}

impl Draw for RedSquare {
    fn draw(&self) {
        let size = 50.0;
        let size = Vec2::new(size, size);
        let screen_size = get_screen_size();
        let pos = (screen_size - size) * 0.5;
        draw_rectangle(pos.x, pos.y, size.x, size.y, RED);
    }
}

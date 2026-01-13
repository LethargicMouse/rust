use macroquad::{
    color::{BLACK, BLUE},
    input::{KeyCode, is_key_pressed},
    math::Vec2,
    miniquad::window::{screen_size, set_fullscreen},
    shapes::draw_circle,
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
}

impl Game {
    fn init_new() -> Self {
        let res = Self::new();
        res.init();
        res
    }

    fn new() -> Self {
        let player = Player::new();
        Self { player }
    }

    fn init(&self) {
        set_fullscreen(true);
    }

    fn is_running(&self) -> bool {
        !is_key_pressed(KeyCode::Escape)
    }

    fn update(&self) {}
}

impl Draw for Game {
    fn draw(&self) {
        clear_background(BLACK);
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
        draw_circle(pos.x, pos.y, 50.0, BLUE);
    }
}

trait Draw {
    fn draw(&self);
}

fn get_screen_size() -> Vec2 {
    screen_size().into()
}

use macroquad::{
    color::{BLACK, BLUE, Color, GREEN, RED},
    input::{KeyCode, is_key_down, is_key_pressed},
    math::Vec2,
    miniquad::window::{screen_size, set_fullscreen},
    shapes::{draw_circle, draw_rectangle},
    time::get_frame_time,
    window::{clear_background, next_frame},
};

#[macroquad::main("mundane")]
async fn main() {
    let mut game = Game::new();
    while game.is_running() {
        game.update();
        game.draw();
        next_frame().await
    }
}

struct Game {
    player_pos: Vec2,
}

impl Game {
    fn new() -> Self {
        set_fullscreen(true);
        let player_pos = Vec2::ZERO;
        Self { player_pos }
    }

    fn is_running(&self) -> bool {
        !is_key_pressed(KeyCode::Escape)
    }

    fn draw(&self) {
        clear_background(BLACK);
        self.draw_background();
        self.draw_player();
    }

    fn draw_background(&self) {
        self.draw_red_square();
        self.draw_green_square();
    }

    fn draw_red_square(&self) {
        self.draw_square(Vec2::ZERO, RED);
    }

    fn draw_green_square(&self) {
        self.draw_square(Vec2::new(300.0, 0.0), GREEN);
    }

    fn draw_square(&self, pos: Vec2, color: Color) {
        let size = 50.0;
        let size = Vec2::new(size, size);
        let screen_size = get_screen_size();
        let pos = (screen_size - size) * 0.5 + pos - self.player_pos;
        draw_rectangle(pos.x, pos.y, size.x, size.y, color);
    }

    fn draw_player(&self) {
        let pos = get_screen_size() * 0.5;
        draw_circle(pos.x, pos.y, 25.0, BLUE);
    }

    fn update(&mut self) {
        self.update_player();
    }

    fn update_player(&mut self) {
        let mut v = Vec2::ZERO;
        if is_key_down(KeyCode::D) {
            v.x += 1.0;
        }
        if is_key_down(KeyCode::A) {
            v.x -= 1.0;
        }
        if is_key_down(KeyCode::S) {
            v.y += 1.0;
        }
        if is_key_down(KeyCode::W) {
            v.y -= 1.0;
        }
        self.player_pos += v.normalize_or_zero() * 200.0 * get_frame_time();
    }
}

fn get_screen_size() -> Vec2 {
    screen_size().into()
}

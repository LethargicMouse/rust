use std::io;

use ratatui::{
    DefaultTerminal, Frame,
    crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind},
    layout::Constraint,
    style::Stylize,
    text::{Line, Text},
    widgets::{Paragraph, Widget},
};

fn main() -> io::Result<()> {
    ratatui::run(|terminal| App::default().run(terminal))
}

#[derive(Default)]
enum State {
    #[default]
    Menu,
    Exit,
}

impl State {
    /// Returns `true` if the state is [`Exit`].
    ///
    /// [`Exit`]: State::Exit
    #[must_use]
    fn is_exit(&self) -> bool {
        matches!(self, Self::Exit)
    }
}

#[derive(Default)]
struct App {
    state: State,
}

impl App {
    fn run(&mut self, terminal: &mut DefaultTerminal) -> io::Result<()> {
        while !self.state.is_exit() {
            terminal.draw(|frame| self.draw(frame))?;
            self.handle_events()?;
        }
        Ok(())
    }

    fn handle_events(&mut self) -> io::Result<()> {
        match event::read()? {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.handle_key_event(key_event)
            }
            _ => {}
        }
        Ok(())
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) {
        if let KeyCode::Char('q') = key_event.code {
            self.exit()
        }
    }

    fn exit(&mut self) {
        self.state = State::Exit;
    }

    fn draw(&self, frame: &mut Frame) {
        frame.render_widget(self, frame.area())
    }
}

impl Widget for &App {
    fn render(self, area: ratatui::prelude::Rect, buf: &mut ratatui::prelude::Buffer) {
        let text = Text::from(vec![Line::from("Best Theorem Prover".bold())]);
        Paragraph::new(text)
            .centered()
            .render(area.centered_vertically(Constraint::Length(1)), buf)
    }
}

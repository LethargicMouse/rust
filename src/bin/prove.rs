use std::{collections::BTreeSet, io, mem::take};

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
    Redact(Redact),
    Prove(Sea),
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
        let key_code = key_event.code;
        self.state = match take(&mut self.state) {
            State::Menu => match key_code {
                KeyCode::Char('q') => State::Exit,
                KeyCode::Char('p') => State::Redact(Redact::new()),
                _ => State::Menu,
            },
            State::Exit => State::Exit,
            State::Redact(redact) => handle_redact_key_event(key_code, redact),
            State::Prove(mut sea) => match key_code {
                KeyCode::Char('q') => State::Redact(sea.into()),
                _ => {
                    if let KeyCode::Char('=') = key_code {
                        sea.rfl()
                    }
                    State::Prove(sea)
                }
            },
        }
    }

    fn draw(&self, frame: &mut Frame) {
        frame.render_widget(self, frame.area())
    }
}

fn handle_redact_key_event(key_code: KeyCode, mut redact: Redact) -> State {
    match key_code {
        KeyCode::Char('q') => State::Menu,
        KeyCode::Enter => State::Prove(redact.sea),
        _ => {
            match key_code {
                KeyCode::Char('1') => redact.set(Literal::Top),
                KeyCode::Char('=') => redact.set_eq(),
                _ => {}
            }
            State::Redact(redact)
        }
    }
}

impl Widget for &App {
    fn render(self, area: ratatui::prelude::Rect, buf: &mut ratatui::prelude::Buffer) {
        match &self.state {
            State::Menu => {
                let text = Text::from(vec![Line::from("Best Theorem Prover".bold())]);
                Paragraph::new(text)
                    .centered()
                    .render(area.centered_vertically(Constraint::Length(1)), buf)
            }
            State::Exit => {}
            State::Redact(redact) => {
                let goal = redact.draw();
                Paragraph::new(goal)
                    .centered()
                    .render(area.centered_vertically(Constraint::Length(1)), buf)
            }
            State::Prove(sea) => {
                let goal = sea.draw();
                Paragraph::new(goal)
                    .centered()
                    .render(area.centered_vertically(Constraint::Length(1)), buf)
            }
        }
    }
}

struct Sea {
    nodes: Vec<Node>,
    main: usize,
    rest_holes: BTreeSet<usize>,
}

impl Sea {
    fn new() -> Self {
        Self {
            nodes: [Node::Hole].into(),
            main: 0,
            rest_holes: [0].into(),
        }
    }

    fn add(&mut self, node: Node) -> usize {
        let id = self.nodes.len();
        if node.is_hole() {
            self.rest_holes.insert(id);
        }
        self.nodes.push(node);
        id
    }

    fn draw(&self) -> String {
        self.draw_only(self.main)
    }

    fn rfl(&mut self) {
        for i in 0..self.nodes.len() {
            if let Node::Equality(left, right) = self.nodes[i]
                && self.equal(left, right)
            {
                self.nodes[i] = Literal::Top.into();
                break;
            }
        }
    }

    fn equal(&self, left: usize, right: usize) -> bool {
        match (self.nodes[left], self.nodes[right]) {
            (a, b) if a == b => true,
            (Node::Equality(a, b), Node::Equality(c, d)) => self.equal(a, c) && self.equal(b, d),
            _ => false,
        }
    }
}

impl DrawSea for Sea {
    fn get_node(&self, id: usize) -> &Node {
        &self.nodes[id]
    }
}

#[derive(Default, PartialEq, Clone, Copy)]
enum Node {
    #[default]
    Hole,
    Literal(Literal),
    Equality(usize, usize),
}

impl Node {
    /// Returns `true` if the node is [`Hole`].
    ///
    /// [`Hole`]: Node::Hole
    #[must_use]
    fn is_hole(&self) -> bool {
        matches!(self, Self::Hole)
    }

    fn is_simple(&self) -> bool {
        matches!(self, Node::Hole | Node::Literal(_))
    }
}

impl From<Literal> for Node {
    fn from(v: Literal) -> Self {
        Self::Literal(v)
    }
}

struct Redact {
    sea: Sea,
    current: usize,
}

impl From<Sea> for Redact {
    fn from(sea: Sea) -> Self {
        let current = sea.rest_holes.first().copied().unwrap_or(sea.main);
        Self { sea, current }
    }
}

impl Redact {
    fn new() -> Self {
        let sea = Sea::new();
        let current = 0;
        Self { sea, current }
    }

    fn set(&mut self, literal: Literal) {
        self.sea.nodes[self.current] = literal.into();
        self.sea.rest_holes.remove(&self.current);
        self.new_current();
    }

    fn new_current(&mut self) {
        self.sea.rest_holes.remove(&self.current);
        self.current = self
            .sea
            .rest_holes
            .first()
            .copied()
            .unwrap_or(self.sea.main);
    }

    fn set_eq(&mut self) {
        let node = take(&mut self.sea.nodes[self.current]);
        let left = self.sea.add(node);
        let right = self.sea.add(Node::Hole);
        self.sea.nodes[self.current] = Node::Equality(left, right);
        self.new_current();
    }

    fn draw(&self) -> String {
        self.draw_only(self.sea.main)
    }
}

impl DrawSea for Redact {
    fn draw_at(&self, id: usize, res: &mut String) {
        let is_current = self.current == id;
        if is_current {
            res.push('[')
        }
        self.draw_node(&self.sea.nodes[id], res);
        if is_current {
            res.push(']');
        }
    }

    fn get_node(&self, id: usize) -> &Node {
        &self.sea.nodes[id]
    }
}

#[derive(PartialEq, Clone, Copy)]
enum Literal {
    Top,
}

impl Literal {
    fn draw(&self) -> &str {
        match self {
            Literal::Top => "⊤",
        }
    }
}

trait DrawSea {
    fn get_node(&self, id: usize) -> &Node;

    fn draw_at(&self, id: usize, res: &mut String) {
        self.draw_node(self.get_node(id), res)
    }

    fn draw_only(&self, id: usize) -> String {
        let mut res = String::new();
        self.draw_at(id, &mut res);
        res
    }

    fn draw_node(&self, node: &Node, res: &mut String) {
        match node {
            Node::Hole => *res += "_",
            Node::Literal(lit) => *res += lit.draw(),
            Node::Equality(left, right) => {
                let is_complex = !self.get_node(*left).is_simple();
                if is_complex {
                    res.push('(');
                }
                self.draw_at(*left, res);
                if is_complex {
                    res.push(')');
                }
                *res += " = ";
                let is_complex = !self.get_node(*right).is_simple();
                if is_complex {
                    res.push('(');
                }
                self.draw_at(*right, res);
                if is_complex {
                    res.push(')');
                }
            }
        }
    }
}

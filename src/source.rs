use std::mem::replace;

use crate::{
    file::read,
    location::{BEGIN, Pos},
};

pub struct Source {
    pub name: String,
    pub code: Vec<u8>,
    pub poses: Vec<Pos>,
    lines: Vec<(usize, usize)>,
}

pub fn read_source(path: String) -> Source {
    let code = read(&path);
    Source {
        lines: get_lines(&code),
        poses: get_poses(&code),
        name: path,
        code,
    }
}

fn get_poses(code: &[u8]) -> Vec<Pos> {
    let mut res: Vec<Pos> = code
        .iter()
        .scan(BEGIN, |p, c| Some(replace(p, p.after(*c))))
        .collect();
    res.push(res.last().unwrap_or(&BEGIN).after(b' '));
    res.push(res.last().unwrap().after(b' '));
    res
}

impl Source {
    pub fn get_line(&self, index: usize) -> &str {
        let (start, len) = self.lines[index];
        str::from_utf8(&self.code[start..start + len]).unwrap()
    }
}

fn get_lines(code: &[u8]) -> Vec<(usize, usize)> {
    code.split(|c| *c == b'\n')
        .map(|l| l.len())
        .scan(0, |s, l| {
            let res = Some((*s, l));
            *s += l + 1;
            res
        })
        .collect()
}

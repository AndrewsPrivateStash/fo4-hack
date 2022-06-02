/*
    FO4 Terminal Hacker
    display current state as you add guesses, as well as filter choices if entered

*/

use std::io::{self, Write};

#[derive(Debug, Clone, Copy, PartialEq)]
enum State {
    // the three states of a given char
    Known,
    Maybe,
    Not,
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            State::Known => write!(f, "K"),
            State::Maybe => write!(f, "M"),
            State::Not => write!(f, "R"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Cps {
    // character position and state
    c: char,
    pos: u8,
    state: State,
}

impl Cps {
    fn new(c: char, p: u8, s: State) -> Self {
        Self {
            c,
            pos: p,
            state: s,
        }
    }
}

impl std::fmt::Display for Cps {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{},{})", self.c, self.pos, self.state)
    }
}

#[derive(Debug)]
struct StateVector {
    inner: Vec<Vec<Cps>>,
}

impl StateVector {
    fn new() -> Self {
        Self { inner: Vec::new() }
    }
    fn pop_with_guess(&mut self, g: &Guess) {
        // make sure vector is empty
        if !self.inner.is_empty() {
            eprintln!("tried to intialize a populated state vector");
            return;
        }
        for c in g.to_chars() {
            self.inner.push(vec![c]);
        }
    }
    fn update(&mut self, c: Cps, upd: bool) {
        // use the passed Cps to update the appropriate location in the vector
        if self.inner.is_empty() {
            eprintln!("tried to update empty state vector");
            return;
        }

        if self.is_col_known(c.pos) {
            return; //early return if column is known, nothing to store
        }

        // find pos in vec, then if exist, update or if not push
        let mut found = false;
        if let Some(v) = self.inner.get_mut(c.pos as usize) {
            for ch in v {
                if ch.c == c.c {
                    if upd {
                        ch.state = c.state;
                    }
                    found = true;
                    break;
                }
            }
        }
        if !found {
            self.inner.get_mut(c.pos as usize).unwrap().push(c);
        }
    }
    fn update_column_not(&mut self, c: &(char, u8)) {
        // use the passed Cps to update the column of values not equal to the passed Cps
        // assumes d is already populated, and intention is to set non matches to Not
        if let Some(v) = self.inner.get_mut(c.1 as usize) {
            for ch in v {
                if ch.c != c.0 {
                    ch.state = State::Not;
                }
            }
        }
    }
    fn is_col_known(&self, col: u8) -> bool {
        if let Some(column) = self.inner.get(col as usize) {
            for c in column {
                if c.state == State::Known {
                    return true;
                }
            }
        }
        false
    }
    fn get_known_char(&self, col: u8) -> Option<char> {
        if let Some(column) = self.inner.get(col as usize) {
            for c in column {
                if c.state == State::Known {
                    return Some(c.c);
                }
            }
        }
        None
    }
    fn retain_known(&mut self, col: u8) {
        // remove all elements in column but the known character
        if let Some(column) = self.inner.get_mut(col as usize) {
            column.retain(|&c| c.state == State::Known);
        }
    }
    fn keep_choice(&self, c: &str) -> bool {
        // check each char in choice to ensure it doesn't carry a Not state, if so return false
        for (i, ch_chr) in c.chars().enumerate() {
            if let Some(sv_pos) = self.inner.get(i) {
                for sv_cps in sv_pos {
                    if sv_cps.c == ch_chr {
                        // when the char matched then..
                        match sv_cps.state {
                            State::Known => continue,
                            State::Maybe => continue,
                            State::Not => return false,
                        }
                    } else {
                        // when the char doesn't match then..
                        match sv_cps.state {
                            State::Known => return false,
                            State::Maybe => continue,
                            State::Not => continue,
                        }
                    }
                }
            }
        }
        true
    }
}

impl std::fmt::Display for StateVector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fmt_str = String::new();
        for pos in &self.inner {
            let mut maybes: Vec<String> = Vec::new();
            let mut nots: Vec<String> = Vec::new();
            for c in pos {
                match c.state {
                    State::Known => {
                        fmt_str.push_str(&c.c.to_uppercase().to_string());
                        break;
                    }
                    State::Maybe => maybes.push(c.c.to_string()),
                    State::Not => nots.push(format!("({})", c.c)),
                }
            }
            fmt_str.push_str(&maybes.join(""));
            fmt_str.push_str(&nots.join(""));
            fmt_str.push_str(", ");
        }
        fmt_str = fmt_str.trim().to_string();
        fmt_str.pop();
        write!(f, "{}", fmt_str)
    }
}

#[derive(Debug, Clone)]
struct Guess {
    word: String,
    likeness: u8,
}

impl Guess {
    fn new(w: &str, c: u8) -> Self {
        Self {
            word: w.to_string(),
            likeness: c,
        }
    }

    fn to_chars(&self) -> Vec<Cps> {
        let mut out_vec: Vec<Cps> = Vec::new();
        for (p, c) in self.word.chars().enumerate() {
            let cp = Cps::new(
                c,
                p as u8,
                if self.likeness > 0 {
                    State::Maybe
                } else {
                    State::Not
                },
            );
            out_vec.push(cp);
        }
        out_vec
    }
}

impl std::fmt::Display for Guess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.word, self.likeness)
    }
}

fn main() {
    let mut guesses: Vec<Guess> = Vec::with_capacity(5);
    let mut choices: Vec<String> = Vec::with_capacity(20);
    let mut state_vec = StateVector::new();

    loop {
        match menu(&guesses) {
            Some(1) => add_guess(&mut guesses, &choices, &mut state_vec),
            Some(2) => add_choices(&mut choices, &guesses, &mut state_vec),
            Some(3) => reset("guesses", &mut guesses, &mut choices, &mut state_vec),
            Some(4) => reset("choices", &mut guesses, &mut choices, &mut state_vec),
            Some(5) => reset("all", &mut guesses, &mut choices, &mut state_vec),
            Some(6) => show_data(&guesses, &choices, &state_vec),
            Some(_) => continue,
            None => {
                println!("goodbye!");
                break;
            }
        }
    }
}

fn menu(g: &[Guess]) -> Option<u8> {
    println!("\nFO4 Terminal Hacker:\n###############");
    println!("1) Add Guess ({} so far)", g.len());
    println!("2) Add Choices");
    println!("3) Reset guesses");
    println!("4) Reset choices");
    println!("5) Reset all");
    println!("6) Show Data");
    println!("Enter to Quit");
    print!("Command (1-6): ");
    io::stdout().flush().expect("flush borked somehow");

    // get option and parse
    loop {
        let selection = get_input("").trim().to_owned();
        if selection.is_empty() {
            return None;
        }

        match selection.parse::<u8>() {
            Ok(i) => return Some(i),
            Err(_) => {
                eprintln!("{} is not in 1-6", selection);
                print!("try again: ");
                continue;
            }
        };
    }
}

fn get_input(s: &str) -> String {
    print!("{} ", s);
    io::stdout().flush().expect("flush borked somehow");

    let mut buffer = String::new();
    io::stdin()
        .read_line(&mut buffer)
        .expect("read_line borked somehow");
    buffer.trim().to_owned()
}

fn add_guess(gs: &mut Vec<Guess>, cs: &[String], sv: &mut StateVector) {
    // add single or multiple guesses to process
    // multiple guesses seperated by `;` eg. guess1, 2; guess2, 0; etc;..
    // handle case when empty input or terminated string gracefully (ignore termination, repeat on empty)
    let raw_instr = get_input("New Guess (g1,n1; g2,n2;..):");
    let guesses: Vec<String> = raw_instr
        .split(';')
        .map(|x| x.trim().to_lowercase())
        .filter(|x| !x.is_empty())
        .collect();

    // check for empty choices vector, and return to main menu
    if guesses.is_empty() {
        eprintln!("no guesses entered!..");
        return;
    }

    // check against the word length in storage
    if !gs.is_empty() && bad_len(&words_from_guesses(&guesses), gs[0].word.len()) {
        return;
    }

    // check entered word lengths are consistent
    let new_words = words_from_guesses(&guesses);
    if bad_len(&new_words, new_words[0].len()) {
        return;
    }

    // store the guesses
    for g in &guesses {
        let vals: Vec<&str> = g.split(',').map(|x| x.trim()).collect();
        match vals[1].parse::<u8>() {
            Ok(i) => {
                if usize::from(i) > vals[0].len() {
                    eprintln!("likeness {} greater than word length {}", i, vals[0].len());
                    return;
                }
                if vals[0].is_empty() {
                    eprintln!("oops, empty word received");
                    return;
                }
                gs.push(Guess::new(vals[0], i));
            }
            Err(e) => {
                eprintln!("bad u8 parse: {}", e);
                return;
            }
        }
    }

    // make sure state vector recieves first value
    if sv.inner.is_empty() && !gs.is_empty() {
        sv.pop_with_guess(gs.get(0).unwrap());
    }

    // output current state
    find_chars(gs, sv);

    // if choices exist, filter them
    if !cs.is_empty() {
        find_choices(cs, gs, sv);
    }
}

fn add_choices(cs: &mut Vec<String>, gs: &[Guess], sv: &mut StateVector) {
    // add list of choices
    // seperate with commas: choice1, choice2, etc,..

    let raw_instr = get_input("Choices (c1, c2, c3,..):");
    let choices: Vec<String> = raw_instr
        .split(',')
        .map(|x| x.trim().to_lowercase())
        .filter(|x| !x.is_empty())
        .collect();

    // check for empty choices vector, and return to main menu
    if choices.is_empty() {
        eprintln!("no choices entered!..");
        return;
    }

    // check against the word length in storage
    if !cs.is_empty() && bad_len(&choices, cs[0].len()) {
        return;
    }

    // check entered word lengths are consistent
    if bad_len(&choices, choices[0].len()) {
        return;
    }

    for c in &choices {
        cs.push(c.to_owned())
    }

    if !sv.inner.is_empty() {
        find_choices(cs, gs, sv);
    }
}

fn reset(opt: &str, gs: &mut Vec<Guess>, cs: &mut Vec<String>, sv: &mut StateVector) {
    match opt {
        "guesses" => {
            gs.clear();
            sv.inner.clear();
        }
        "choices" => cs.clear(),
        "all" => {
            gs.clear();
            cs.clear();
            sv.inner.clear();
        }
        _ => (),
    }
}

fn show_data(gs: &Vec<Guess>, cs: &[String], sv: &StateVector) {
    // print the gueses an choices currently stored
    if gs.is_empty() {
        println!("no guesses yet..");
    } else {
        println!("Guesses:");
        let mut out_str = String::new();
        for g in gs {
            out_str += &format!("{} ", g);
        }
        println!("{}", out_str.trim());
        println!("[A-Z] -> Known, [a-z] -> Maybe, ([a-z]) -> Not");
        println!("State Vector: {}", sv);
    }

    if cs.is_empty() {
        println!("no choices entered..");
    } else if !sv.inner.is_empty() {
        println!("Filtered Choices:");
        find_choices(cs, gs, sv);
    } else {
        println!("Choices:");
        println!("{}", cs.join(", ").trim());
    }
}

// #### UTILS #### //
fn find_chars(gs: &[Guess], sv: &mut StateVector) {
    if gs.len() >= 2 {
        general_pass(gs, sv);
    }
    println!("{}", sv);
}

fn find_choices(cs: &[String], gs: &[Guess], sv: &StateVector) {
    if cs.is_empty() {
        return;
    }

    let gs_words: Vec<String> = gs.iter().map(|g| g.word.clone()).collect();
    let mut res_vec: Vec<String> = Vec::new();

    for choice in cs {
        // make sure choice is not a current guess
        if gs_words.iter().any(|s| s == choice) {
            continue;
        }

        if sv.keep_choice(choice) {
            res_vec.push(choice.clone());
        }
    }
    match res_vec.len() {
        0 => println!("oops, it apears all the choices were excluded, perhaps an entry error?"),
        1 => println!("FOUND! ==>  {}  <==", res_vec[0].to_uppercase()),
        _ => println!("{}", res_vec.join(", ").trim()),
    }
}

fn bad_len(strs: &Vec<String>, cor_len: usize) -> bool {
    for s in strs {
        if s.len() != cor_len {
            eprintln!("{} is {} chars, expected {}", s, s.len(), cor_len);
            return true;
        }
    }
    false
}

fn words_from_guesses(gs: &Vec<String>) -> Vec<String> {
    let mut out_vec: Vec<String> = Vec::new();
    for g in gs {
        let fst_elem = g.split(',').next().unwrap();
        out_vec.push(fst_elem.to_string());
    }
    out_vec
}

fn pos_matches(g1: &Guess, g2: &Guess) -> Vec<(char, u8)> {
    // take two guesses and a return a list of matching character positions
    let mut out_vec: Vec<(char, u8)> = Vec::new();
    let g1_chars = g1.to_chars();
    let g2_chars = g2.to_chars();
    for i in 0..g1.word.len() {
        if g1_chars[i].c == g2_chars[i].c {
            out_vec.push((g1_chars[i].c, i as u8));
        }
    }
    out_vec
}

fn general_pass(gs: &[Guess], sv: &mut StateVector) {
    dir_pass(gs, sv, false);
    dir_pass(gs, sv, true);
    post_pass(gs, sv);
}

fn dir_pass(gs: &[Guess], sv: &mut StateVector, rev: bool) {
    // assume: given g1 has exactly n matches with g2, and g1 likeness is > 0
    // then the matches are promoted to Known, and the unmatched chars are demoted to Not

    // reverse guesses if specified
    let mut gs = gs.to_owned();
    if rev {
        gs.reverse();
    }

    // loop over guesses in a pyramidal fashion
    // to compare all pairwise combinations
    for i in 0..gs.len() - 1 {
        for j in i + 1..gs.len() {
            let g1 = gs.get(i).unwrap();
            let g2 = gs.get(j).unwrap();
            let matches = pos_matches(g1, g2);

            // add left guess to state vector, if not Known
            for c in g1.to_chars() {
                sv.update(c, false);
            }

            // if one of the guesses has 0 likeness then set matched chars to Not
            if g1.likeness == 0 || g2.likeness == 0 {
                for m in &matches {
                    sv.update(Cps::new(m.0, m.1, State::Not), true);
                }
            }

            // set known values if match count equals likeness count in left guess
            if g1.likeness == matches.len() as u8 {
                // set matched to Known
                for m in &matches {
                    sv.update(Cps::new(m.0, m.1, State::Known), true);
                }
                // set unmatched to Not in left Guess (row)
                for c in g1.to_chars() {
                    if !in_matches(c, &matches) {
                        sv.update(Cps::new(c.c, c.pos, State::Not), true);
                    }
                }
                // set other values to Not in Known columns
                for m in &matches {
                    sv.update_column_not(m);
                }
                // clean up columns of any other character now that they are known
                for m in &matches {
                    sv.retain_known(m.1);
                }
            }
        }
    }
}

fn post_pass(gs: &[Guess], sv: &mut StateVector) {
    // after the general passes we may be left with several known value columns
    // this will expose new reductions having the following conditons:
    // if a known column exists and for a given guess that column's char is not the Known value
    // and count of the remaining column chars is equal to its likeness then the remaining columns are Known

    for g in gs {
        let mut skip = false;
        let mut rem_cps: Vec<Cps> = Vec::new();
        // do all of the known columns not match the char in the guess?
        for c in g.to_chars() {
            if let Some(known_char) = sv.get_known_char(c.pos) {
                if c.c == known_char {
                    skip = true; // skip this guess
                    break;
                }
            } else {
                rem_cps.push(c); // add remaining for unKnown column
            }
        }

        if skip {
            continue;
        }

        // all known columns do not match guess chars, so does the remaining char cnt match its likeness?
        if rem_cps.len() as u8 == g.likeness {
            // the elements in remaining are known
            for r in &rem_cps {
                sv.update(Cps::new(r.c, r.pos, State::Known), true);
                sv.retain_known(r.pos);
            }
        }
    }
}

fn in_matches(c: Cps, ms: &Vec<(char, u8)>) -> bool {
    // check both char and position
    for cps in ms {
        if c.c == cps.0 && c.pos == cps.1 {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_general_pass() {
        let mut state_vec = StateVector::new();

        let guesses = vec![
            Guess::new("rew", 1),
            Guess::new("per", 2),
            Guess::new("ptk", 2),
        ];

        state_vec.pop_with_guess(guesses.get(0).unwrap());
        general_pass(&guesses, &mut state_vec);

        let out_state = format!("{}", &state_vec);

        assert_eq!(out_state, "P, E, K");
    }
}

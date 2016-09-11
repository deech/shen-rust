// [[file:../shen-rust.org::*Preamble][Preamble:1]]
#![feature(slice_patterns)]
#![feature(custom_derive)]
#![feature(try_from)]
#[macro_use]
extern crate nom;
extern crate uuid;
extern crate time;
extern crate core;
use std::str;
use nom::*;
use std::path::Path;
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;
use std::collections::HashMap;
use std::cell::RefCell;
use uuid::Uuid;
use std::io::{self, Error};
use std::convert::TryFrom;
use std::ops::{Add, Sub, Mul, Div};
// Preamble:1 ends here

// [[file:../shen-rust.org::*Token%20Types][Token\ Types:1]]
#[derive(Debug)]
pub enum KlToken {
    Symbol(String),
    Number(KlNumber),
    String(String),
    Sexp(Vec<KlToken>),
}

#[derive(Debug)]
pub enum KlNumber {
    Float(f64),
    Int(i64),
}

pub struct UniqueVector {
    uuid: Uuid,
    vector: RefCell<Vec<Rc<KlElement>>>
}

pub enum KlStreamDirection {
    In,
    Out
}

pub struct KlFileStream {
    direction : KlStreamDirection,
    file: RefCell<File>
}

pub enum KlStdStream {
    Stdout,
    Stdin
}

pub enum KlStream {
    FileStream(KlFileStream),
    Std(KlStdStream)
}

pub enum KlElement {
    Symbol(String),
    Number(KlNumber),
    String(String),
    Cons(Vec<Rc<KlElement>>),
    Closure(KlClosure),
    Vector(Rc<UniqueVector>),
    Stream(Rc<KlStream>)
}

#[derive(Debug)]
pub enum KlError {
    ErrorString(String)
}

pub enum KlClosure {
    FeedMe(Rc<Fn(Rc<KlElement>) -> KlClosure>),
    Thunk(Rc<Fn() -> Rc<KlElement>>),
    Done(Result<Option<Rc<KlElement>>,Rc<KlError>>)
}
// Token\ Types:1 ends here

// [[file:../shen-rust.org::*Constants][Constants:1]]
const CHARACTERS: &'static str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ=-*/+_?$!@~.><&%'#`;:{}";
const DIGITS: &'static str = "0123456789";
// Constants:1 ends here

// [[file:../shen-rust.org::*Parser][Parser:1]]
named!(klsymbol<KlToken>,
       chain!(
       initial: one_of!(CHARACTERS) ~
       remainder: many0!(
           alt_complete!(
               one_of!(DIGITS) |
               one_of!(CHARACTERS)
           )
       ),
       || {
           let mut res : Vec <char> = vec![initial];
           res.extend(remainder);
           KlToken::Symbol(res.into_iter().collect())
       })
);
// Parser:1 ends here

// [[file:../shen-rust.org::*Parsers][Parsers:1]]
named!(klnumber<KlToken>,
       alt_complete!(
           chain!(
               n: klfloat,
               || KlToken::Number(n)
           ) |
           chain!(
               n : klint,
               || KlToken::Number(n)
           )
       )
);

named!(klint<KlNumber>,
       chain!(
           sign: opt!(one_of!("-+")) ~
           numbers: many1!(one_of!(DIGITS)),
           || KlNumber::Int(make_int(sign,numbers))
       )
);

named!(klfloat<KlNumber>,
       chain!(
           sign: opt!(one_of!("-+")) ~
           before_decimal: many1!(one_of!(DIGITS)) ~
           one_of!(".") ~
           after_decimal: many1!(one_of!(DIGITS)),
           || KlNumber::Float(make_float(sign,before_decimal, after_decimal))
       )
);
// Parsers:1 ends here

// [[file:../shen-rust.org::*Helpers][Helpers:1]]
fn make_float(sign: Option<char>, before: Vec<char>, after: Vec<char> ) -> f64 {
    let mut float_char_vector : Vec<char> = Vec::new();
    match sign {
        Some(_sign) => float_char_vector.push(_sign),
        None => ()
    };
    float_char_vector.extend(before);
    float_char_vector.push('.');
    float_char_vector.extend(after);
    let float_string : String = float_char_vector.into_iter().collect();
    float_string.parse::<f64>().unwrap()
}

fn make_int(sign: Option<char>, numbers: Vec<char>) -> i64 {
    let mut int_char_vector : Vec<char> = Vec::new();
    match sign {
        Some(_sign) => int_char_vector.push(_sign),
        None => ()
    };
    int_char_vector.extend(numbers);
    let int_string : String = int_char_vector.into_iter().collect();
    let result : i64 = int_string.parse::<i64>().unwrap();
    result
}
// Helpers:1 ends here

// [[file:../shen-rust.org::*Parsers][Parsers:1]]
named!(klstring<KlToken>,
       chain!(
           char!('\"') ~
           contents:  many0!(klstringinnards) ~
           char!('\"'),
           || KlToken::String(make_quoted_string(contents))
       )
);

named!(klstringinnards< &[u8] >,
       escaped!(none_of!("\"\\"), '\\', one_of!("\"n\\"))
);
// Parsers:1 ends here

// [[file:../shen-rust.org::*Helpers][Helpers:1]]
fn make_quoted_string (contents:Vec<&[u8]>) -> String {
    let to_vectors : Vec< Vec<u8> > = contents.iter().map(|c| c.to_vec()).collect();
    let smushed : Vec<u8> = to_vectors.concat();
    let mut quoted : Vec<u8> = Vec::new();
    quoted.push('\"' as u8);
    quoted.extend(smushed);
    quoted.push('\"' as u8);
    let result : String = String::from_utf8(quoted).unwrap();
    result
}
// Helpers:1 ends here

// [[file:../shen-rust.org::*Many%20Until%20Combinator][Many\ Until\ Combinator:1]]
#[macro_export]
macro_rules! many0_until (
    ($input:expr, $stopmac:ident!( $($args:tt)* ), $submac:ident!( $($args2:tt)* )) => (
        {
            let mut res = Vec::new();
            let mut input = $input;
            let mut loop_result = Ok(());

            while input.input_len() != 0 {
                match $stopmac!(input, $($args)*) {
                    IResult::Error(_) => {
                        match $submac!(input, $($args2)*) {
                            IResult::Error(_) => {
                                break;
                            },
                            IResult::Incomplete(Needed::Unknown) => {
                                loop_result = Err(IResult::Incomplete(Needed::Unknown));
                                break;
                            },
                            IResult::Incomplete(Needed::Size(i)) => {
                                let size = i + ($input).input_len() - input.input_len();
                                loop_result = Err(IResult::Incomplete(Needed::Size(size)));
                                break;
                            },
                            IResult::Done(i, o) => {
                                res.push(o);
                                input = i;
                            }
                        }
                    },
                    IResult::Done(_,_) => {
                        break;
                    }
                    IResult::Incomplete(Needed::Unknown) => {
                        loop_result = Err(IResult::Incomplete(Needed::Unknown));
                        break;
                    },
                    IResult::Incomplete(Needed::Size(i)) => {
                        let size = i + ($input).input_len() - input.input_len();
                        loop_result = Err(IResult::Incomplete(Needed::Size(size)));
                        break;
                    },
                }
            }
            match loop_result {
                Ok(()) => IResult::Done(input,res),
                Err(e) => e
            }
        }
    );
    ($i:expr, $stopmac:ident!( $($args:tt)* ), $p:expr) => (
        many0_until!($i, $stopmac!($($args)*), call!($p));
    );
);
// Many\ Until\ Combinator:1 ends here

// [[file:../shen-rust.org::*Parsers][Parsers:1]]
named!(klsexps< Vec<KlToken> >,
       many0!(
           chain!(
               opt!(multispace) ~
               kl: alt_complete!(klsexp|klstring) ~
               opt!(multispace),
               || kl
           )
       )
);

named!(klsexp<KlToken>,
       chain!(
           char!('(') ~
           inner: many0_until!(char!(')'), klsexpinnards) ~
           char!(')'),
           || KlToken::Sexp(inner)
       )
);

named!(klsexpinnards<KlToken>,
       chain!(
           opt!(multispace) ~
           atom: alt_complete!(klsexp|klnumber|klstring|klsymbol) ~
           opt!(multispace),
           || atom
       )
);
// Parsers:1 ends here

// [[file:../shen-rust.org::*Collect][Collect:1]]
fn collect_sexps(kl: &[u8], kl_buffer: &mut Vec<Vec<KlToken>>) -> () {
    let mut parsed = match klsexps(kl) {
        IResult::Done(_, out) => out,
        IResult::Incomplete(x) => panic!("incomplete: {:?}", x),
        IResult::Error(e) => panic!("error: {:?}", e),
    };
    // remove toplevel strings
    parsed.retain(|expr| match expr { &KlToken::Sexp(_) => true, _ => false });
    kl_buffer.push(parsed)
}
// Collect:1 ends here

// [[file:../shen-rust.org::*Symbol%20Table][Symbol\ Table:1]]
thread_local!(static SYMBOL_TABLE: RefCell<HashMap<String, Rc<KlElement>>> = RefCell::new(HashMap::new()));
// Symbol\ Table:1 ends here

// [[file:../shen-rust.org::*Function%20Table][Function\ Table:1]]
thread_local!(static FUNCTION_TABLE: RefCell<HashMap<String, KlClosure>> = RefCell::new(HashMap::new()));
// Function\ Table:1 ends here

// [[file:../shen-rust.org::*Vector%20Table][Vector\ Table:1]]
thread_local!(static VECTOR_TABLE: RefCell<Vec<(Rc<UniqueVector>, RefCell<Vec<usize>>)>> = RefCell::new(Vec::new()));

pub fn shen_with_unique_vector (unique_vector: &UniqueVector, tx: Box<Fn(&RefCell<Vec<usize>>) -> ()>)
                                -> Option<()> {
    VECTOR_TABLE.with(| vector_table | {
        let vector_table = vector_table.borrow_mut();
        let mut iter = vector_table.iter().take_while(| &tuple | {
            match tuple {
                &(ref vector,_) => {
                    let uuid = vector.uuid;
                    uuid != unique_vector.uuid
                }
            }
        }).peekable();
        let found : Option<&&(Rc<UniqueVector>, RefCell<Vec<usize>>)> = iter.peek();
        match found {
            Some(&&(_, ref indices)) => Some(tx(indices)),
            None => None
        }
    })
}
// Vector\ Table:1 ends here

// [[file:../shen-rust.org::*Path%20Utilites][Path\ Utilites:1]]
pub fn add_path (old_path:&Vec<usize>, new_path:Vec<usize>) -> Vec<usize> {
    let mut p = old_path.clone();
    p.extend(new_path);
    p
}
// Path\ Utilites:1 ends here

// [[file:../shen-rust.org::*Getter][Getter:1]]
pub fn get_element_at (path : Vec<usize>, sexp: &KlToken)  -> Option<&KlToken> {
    let mut current_token = sexp;
    for index in path {
        if let &KlToken::Sexp(ref current) = current_token {
            if index < current.len() {
                current_token = &current[index];
            }
            else {
                return None;
            }
        }
        else {
            return None;
        }
    }
    Some(current_token)
}
// Getter:1 ends here

// [[file:../shen-rust.org::*Detect%20Possible%20Recursive%20Calls][Detect\ Possible\ Recursive\ Calls:1]]
pub fn find_recursive_calls (function_name: String, num_args: usize, sexp: &KlToken) -> Vec<Vec<usize>> {
    let mut found : Vec< Vec<usize> >= Vec::new();
    if let &KlToken::Sexp(_) = sexp {
        let mut pending : Vec <(Vec<usize>, &KlToken)> = vec![(Vec::new(), sexp)];
        while pending.len() > 0 {
            let mut newly_found = Vec::new();
            if let &mut [(ref path, &KlToken::Sexp(ref current)),_] = pending.as_mut_slice() {
                if let &[KlToken::Symbol(ref s), ref rest..] = current.as_slice() {
                    match (s.as_str(), rest) {
                        (name, rest) if (name == function_name.as_str()) && rest.len() == num_args => {
                            found.push(path.clone());
                        },
                        ("cond", rest) => {
                            let indexed : Vec<(usize, &KlToken)> = rest.iter().enumerate().collect();
                            for (index, sexp) in indexed {
                                if let &KlToken::Sexp(ref pair) = sexp {
                                    if let &[_, ref action @ KlToken::Sexp(_)] = pair.as_slice() {
                                        newly_found.push((add_path(path, vec![index,1]), action));
                                    }
                                }
                            };
                        },
                        ("if", &[ref if_true @ KlToken::Sexp(_), ref if_false @ KlToken::Sexp(_)]) => {
                            newly_found.push((add_path(path, vec![2]), if_true));
                            newly_found.push((add_path(path, vec![3]), if_false));
                        },
                        ("trap_error", &[ref to_try @ KlToken::Sexp(_), ref handler @ KlToken::Sexp(_)]) => {
                            newly_found.push((add_path(path, vec![1]), to_try));
                            newly_found.push((add_path(path, vec![2]), handler));
                        },
                        ("let", &[_ , _, ref body @ KlToken::Sexp(_)]) |
                        ("defun", &[_ , _, ref body @ KlToken::Sexp(_)]) =>
                            newly_found.push((add_path(path, vec![3]), body)),
                        ("lambda", &[_, ref body @ KlToken::Sexp(_)]) =>
                            newly_found.push((add_path(path, vec![2]), body)),
                        _ => match current.last() {
                            Some(ref tail @ &KlToken::Sexp(_)) =>
                                newly_found.push((add_path(path, vec![current.len() - 1]), tail)),
                            _ => ()
                        }
                    }
                }
                else {
                    match current.last() {
                        Some(ref tail @ &KlToken::Sexp(_)) =>
                            newly_found.push((add_path(path, vec![current.len() - 1]), tail)),
                        _ => ()
                    }
                }
            };
            pending.remove(0);
            newly_found.reverse();
            newly_found.extend(pending);
            pending = newly_found;
        }
    }
    found
}
// Detect\ Possible\ Recursive\ Calls:1 ends here

// [[file:../shen-rust.org::*Detect%20Function%20Application%20Context][Detect\ Function\ Application\ Context:1]]
pub fn start_of_function_chain (tail_call_path: Vec<usize>, sexp: &KlToken) -> Option<Vec<usize>> {
    let mut result = None;
    let mut i = 0;
    while i < tail_call_path.len() {
        let current_path : Vec<usize> = tail_call_path.iter().cloned().take(i).collect();
        match get_element_at(current_path.clone(), &sexp) {
            Some(current_element) => {
                if let &KlToken::Sexp(ref current) = current_element {
                    match current.as_slice() {
                        &[KlToken::Symbol(ref s), _] => {
                            match s.as_str() {
                                "if" | "defun" | "let" | "lambda" | "do" => {
                                    result = None;
                                    i = i + 1;
                                }
                                "cond" => {
                                    result = None;
                                    i = i + 2;
                                }
                                _ => {
                                    result = Some(current_path.clone());
                                    i = i + 1
                                }

                            }
                        }
                        _ => ()
                    }
                }
            },
            _ => return None
        }
    }
    result
}
// Detect\ Function\ Application\ Context:1 ends here

// [[file:../shen-rust.org::*Get%20Tail%20Calls][Get\ Tail\ Calls:1]]
pub fn get_all_tail_calls (sexp: &KlToken) -> Vec<Vec<usize>> {
    if let &KlToken::Sexp(ref defun) = sexp {
        match defun.as_slice() {
            &[KlToken::Symbol(ref defun), KlToken::Symbol(ref name), KlToken::Sexp(ref args), _]
                if defun.as_str() == "defun" => {
                    let mut recursive_calls = find_recursive_calls(name.clone(), args.len(), sexp);
                    recursive_calls.retain(
                        |ref path| {
                            let context = start_of_function_chain(path.iter().cloned().collect(), sexp);
                            match context {
                                Some(_) => false,
                                None => true
                            }
                        }
                    );
                    recursive_calls
                },
            _ => Vec::new()
        }
    }
    else {
        Vec::new()
    }
}
// Get\ Tail\ Calls:1 ends here

// [[file:../shen-rust.org::*Helpers][Helpers:1]]
pub fn shen_symbol_to_string(s : &KlElement) -> Result<Rc<&String>, Rc<String>> {
    match s {
        &KlElement::Symbol(ref s) => Ok(Rc::new(&s)),
        _ => Err(Rc::new(String::from("shen_symbol_to_string: Expecting a symbol.")))
    }
}

pub fn shen_string_to_symbol(s : &str) -> Rc<KlElement> {
    Rc::new(KlElement::Symbol(String::from(s)))
}

pub fn shen_is_bool (a: Rc<KlElement>) -> bool {
    match &*a {
        &KlElement::Symbol(ref s) if s.as_str() == "true" || s.as_str() == "false" => true,
        _ => false
    }
}

pub fn shen_is_thunk(a: Rc<KlElement>) -> bool {
    match &*a {
        &KlElement::Closure(KlClosure::Thunk(_)) => true,
        _ => false
    }
}

pub fn shen_force_thunk(a : Rc<KlElement>) -> Result<Option<Rc<KlElement>>,Rc<KlError>> {
    match &*a {
        &KlElement::Closure(KlClosure::Thunk(ref inner)) => Ok(Some(inner())),
        _ => shen_make_error("shen_force_thunk: Expected a thunk.")
    }
}

pub fn shen_make_error(s : &str) -> Result<Option<Rc<KlElement>>, Rc<KlError>> {
    Err(Rc::new((KlError::ErrorString(String::from(s)))))
}
// Helpers:1 ends here

// [[file:../shen-rust.org::*If][If:1]]
pub fn shen_if () -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | predicate | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | if_thunk | {
                            let predicate = predicate.clone();
                            KlClosure::FeedMe(
                                Rc::new(
                                    move | else_thunk | {
                                        if !shen_is_bool(predicate.clone()) {
                                            KlClosure::Done(shen_make_error("shen_if: the predicate must be 'true' or 'false'."))
                                        }
                                        else {
                                            if !shen_is_thunk(if_thunk.clone()) || !shen_is_thunk(else_thunk.clone()) {
                                                KlClosure::Done(shen_make_error("shen_if: Both the if and else branch must be thunks."))
                                            }
                                            else {
                                                match *predicate {
                                                    KlElement::Symbol(ref s) if s.as_str() == "true" => {
                                                        KlClosure::Done(shen_force_thunk(if_thunk.clone()))
                                                    },
                                                    KlElement::Symbol(ref s) if s.as_str() == "false" => {
                                                        KlClosure::Done(shen_force_thunk(else_thunk.clone()))
                                                    },
                                                    _ => KlClosure::Done(shen_make_error("Expecting predicate to be 'true' or 'false'."))
                                                }
                                            }
                                        }
                                    }
                                )
                            )
                        }
                    )
                )
            }
        )
    )
}
// If:1 ends here

// [[file:../shen-rust.org::*And][And:1]]
pub fn shen_and () -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | a_thunk | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | b_thunk | {
                            if !shen_is_thunk(a_thunk.clone()) || !shen_is_thunk(b_thunk.clone()) {
                                KlClosure::Done(shen_make_error("shen_and: Both arguments must be thunks."))
                            }
                            else {
                                let forced = shen_force_thunk(a_thunk.clone()).unwrap();
                                if forced.is_some() && !shen_is_bool(forced.clone().unwrap()) {
                                    KlClosure::Done(shen_make_error("shen_and: The first argument must evaluate to the symbol 'true' or 'false."))
                                }
                                else {
                                    let forced : Rc<KlElement> = forced.unwrap();
                                    match &*forced {
                                        &KlElement::Symbol(ref a)
                                            if a.as_str() == "false" =>
                                            KlClosure::Done(Ok(Some(shen_string_to_symbol("false")))),
                                        _ => {
                                            let forced = shen_force_thunk(b_thunk).unwrap();
                                            if forced.is_some() && !shen_is_bool(forced.clone().unwrap()) {
                                                KlClosure::Done(shen_make_error("shen_and: The second argument must evaluate to the symbol 'true' or 'false."))
                                            }
                                            else {
                                                let forced = forced.unwrap();
                                                match &*forced {
                                                    &KlElement::Symbol(ref b)
                                                        if b.as_str() == "false" =>
                                                        KlClosure::Done(Ok(Some(shen_string_to_symbol("false")))),
                                                    _ => KlClosure::Done(Ok(Some(shen_string_to_symbol("true"))))
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    )
                )
            }
        )
    )
}
// And:1 ends here

// [[file:../shen-rust.org::*Or][Or:1]]
pub fn shen_or () -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | a_thunk | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | b_thunk | {
                            if !shen_is_thunk(a_thunk.clone()) || !shen_is_thunk(b_thunk.clone()) {
                                KlClosure::Done(shen_make_error("shen_or: Both arguments must be thunks."))
                            }
                            else {
                                let forced = shen_force_thunk(a_thunk.clone()).unwrap();
                                if forced.is_some() && !shen_is_bool(forced.clone().unwrap()) {
                                    KlClosure::Done(shen_make_error("shen_or: The first argument must evaluate to the symbol 'true' or 'false."))
                                }
                                else {
                                    let forced : Rc<KlElement> = forced.unwrap();
                                    match &*forced {
                                        &KlElement::Symbol(ref a)
                                            if a.as_str() == "true" =>
                                            KlClosure::Done(Ok(Some(shen_string_to_symbol("true")))),
                                        _ => {
                                            let forced = shen_force_thunk(b_thunk).unwrap();
                                            if forced.is_some() && !shen_is_bool(forced.clone().unwrap()) {
                                                KlClosure::Done(shen_make_error("shen_or: The second argument must evaluate to the symbol 'true' or 'false."))
                                            }
                                            else {
                                                let forced = forced.unwrap();
                                                match &*forced {
                                                    &KlElement::Symbol(ref b)
                                                        if b.as_str() == "true" =>
                                                        KlClosure::Done(Ok(Some(shen_string_to_symbol("true")))),
                                                    _ => KlClosure::Done(Ok(Some(shen_string_to_symbol("false"))))
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    )
                )
            }
        )
    )
}
// Or:1 ends here

// [[file:../shen-rust.org::*Cond][Cond:1]]
pub fn shen_cond() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | cases | {
                match &*cases {
                    &KlElement::Cons(ref case_pairs) => {
                        let mut pairs : Vec<(Rc<KlElement>,Rc<KlElement>)>= Vec::new();
                        for case in case_pairs {
                            match &**case {
                                &KlElement::Cons(ref pair) if pair.len() == 2 => {
                                    let ref predicate = pair[1];
                                    let ref action = pair[0];
                                    if !shen_is_thunk(predicate.clone()) || !shen_is_thunk(action.clone()) {
                                        return KlClosure::Done(shen_make_error("shen_cond: All cases must be a pairs of thunks."))
                                    }
                                    else {
                                        pairs.push((predicate.clone(),action.clone()))
                                    }
                                },
                                _ => return KlClosure::Done(shen_make_error("shen_cond: All cases must be pairs."))
                            }
                        };
                        let mut result = None;
                        for &(ref predicate,ref action) in pairs.as_slice() {
                            let forced = shen_force_thunk(predicate.clone()).unwrap();
                            if forced.is_some() && !shen_is_bool(forced.clone().unwrap()) {
                                result = Some(KlClosure::Done(shen_make_error("shen_cond: All predicates must evaluate to 'true' or 'false'.")))
                            }
                            else {
                                let forced = forced.unwrap();
                                match &*forced {
                                    &KlElement::Symbol(ref s) if s.as_str() == "true" => {
                                        let forced = shen_force_thunk(action.clone()).unwrap();
                                        result = Some(KlClosure::Done(Ok(forced)));
                                    },
                                    _ => ()
                                }
                            }
                        }
                        match result {
                            Some(r) => r,
                            None => KlClosure::Done(shen_make_error("shen_cond: None of the predicates evaluated to 'true'."))

                        }
                    },
                    _ => KlClosure::Done(shen_make_error("shen_cond: All cases must be a pairs of thunks."))
                }
            }
        )
    )
}
// Cond:1 ends here

// [[file:../shen-rust.org::*Intern][Intern:1]]
pub fn shen_intern() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | string | {
                match &*string {
                    &KlElement::String(ref s) => {
                        KlClosure::Done(Ok(Some(Rc::new(KlElement::Symbol(s.clone())))))
                    },
                    _ => KlClosure::Done(shen_make_error("shen_intern: expecting a string."))
                }
            }
        )
    )
}
// Intern:1 ends here

// [[file:../shen-rust.org::*pos][pos:1]]
pub fn shen_pos() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | string | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | number | {
                            let string = string.clone();
                            match &*string {
                                &KlElement::String(ref s) => {
                                    let length = (&s).chars().count();
                                    match &*number {
                                        &KlElement::Number(KlNumber::Int(i)) if i > 0 && (i as usize) < length => {
                                            let char = (*s).chars().nth(i as usize).unwrap();
                                            let mut result = String::from("");
                                            result.push(char);
                                            KlClosure::Done(Ok(Some(Rc::new(KlElement::String(result)))))
                                        },
                                        _ => KlClosure::Done(shen_make_error("shen_pos: expecting a number between 0 and the length of the string."))
                                    }
                                },
                                _ => KlClosure::Done(shen_make_error("shen_pos: expecting a string."))
                            }
                        }
                    )
                )
            }
        )
    )
}
// pos:1 ends here

// [[file:../shen-rust.org::*tlstr][tlstr:1]]
pub fn shen_tlstr() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | string | {
                match &*string {
                    &KlElement::String(ref s) => {
                        let length = (&s).chars().count();
                        if length == 0 {
                            KlClosure::Done(shen_make_error("shen_tlstr: expecting non-empty string."))
                        }
                        else {
                            let (_, tail) = (&s).split_at(1);
                            KlClosure::Done(Ok(Some(Rc::new(KlElement::String(String::from(tail))))))
                        }
                    },
                    _ => KlClosure::Done(shen_make_error("shen_pos: expecting a string."))
                }

            }
        )
    )
}
// tlstr:1 ends here

// [[file:../shen-rust.org::*cn][cn:1]]
pub fn shen_cn () -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | string_a | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | string_b | {
                            let string_a = string_a.clone();
                            match (&*string_a, &*string_b) {
                                (&KlElement::String(ref a), &KlElement::String(ref b)) => {
                                    KlClosure::Done(Ok(Some(Rc::new(KlElement::String((*a).clone() + b)))))
                                },
                                _ => KlClosure::Done(shen_make_error("shen_cn: expecting two strings."))
                            }

                        }
                    )
                )
            }
        )
    )
}
// cn:1 ends here

// [[file:../shen-rust.org::*str][str:1]]
pub fn shen_str() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | atom | {
                match &*atom {
                    &KlElement::String(_) => KlClosure::Done(Ok(Some(atom.clone()))),
                    &KlElement::Number(KlNumber::Int(i)) =>
                        KlClosure::Done(Ok(Some(Rc::new(KlElement::String(format!("{}", i)))))),
                    &KlElement::Number(KlNumber::Float(f)) =>
                        KlClosure::Done(Ok(Some(Rc::new(KlElement::String(format!("{}", f)))))),
                    &KlElement::Symbol(ref s) =>
                        KlClosure::Done(Ok(Some(Rc::new(KlElement::String(s.clone()))))),
                    &KlElement::Stream(ref s) => {
                        match &**s {
                            &KlStream::FileStream(_) =>
                                KlClosure::Done(Ok(Some(Rc::new(KlElement::String(String::from("<file stream>")))))),
                            &KlStream::Std(KlStdStream::Stdout) =>
                                KlClosure::Done(Ok(Some(Rc::new(KlElement::String(String::from("<stdout>")))))),
                            &KlStream::Std(KlStdStream::Stdin) =>
                                KlClosure::Done(Ok(Some(Rc::new(KlElement::String(String::from("<stdin>")))))),
                        }
                    }
                    _ => KlClosure::Done(shen_make_error("Not an atom, stream or closure; str cannot convert it to a string."))
                }
            }
        )
    )
}
// str:1 ends here

// [[file:../shen-rust.org::*string?][string\?:1]]
pub fn shen_stringp() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | element | {
                match &*element {
                    &KlElement::String(_) =>
                        KlClosure::Done(Ok(Some(shen_string_to_symbol("true")))),
                    _ => KlClosure::Done(Ok(Some(shen_string_to_symbol("false"))))
                }
            }
        )
    )
}
// string\?:1 ends here

// [[file:../shen-rust.org::*n->string][n->string:1]]
pub fn shen_n_to_string() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | n | {
                match &*n {
                    &KlElement::Number(KlNumber::Int(i)) => {
                        let convert : Result<u8, _>= TryFrom::try_from(i);
                        match convert {
                            Ok(char) => {
                                match String::from_utf8(vec![char]) {
                                    Ok(string) => {
                                        KlClosure::Done(Ok(Some(Rc::new(KlElement::String(string)))))
                                    },
                                    Err(_) =>
                                        KlClosure::Done(shen_make_error("shen_n_to_string: number is not utf8."))
                                }
                            },
                            Err(_) => KlClosure::Done(shen_make_error("shen_n_to_string: number could not be converted to u8."))
                        }
                    },
                    _ => KlClosure::Done(shen_make_error("shen_n_to_string: expecting an integer."))
                }
            }
        )
    )
}
// n->string:1 ends here

// [[file:../shen-rust.org::*string->n][string->n:1]]
pub fn shen_string_to_n() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | string | {
                match &*string {
                    &KlElement::String(ref s) if s.len() == 1 => {
                        let v : Vec<u8> = (*s.clone()).into();
                        KlClosure::Done(Ok(Some(Rc::new(KlElement::Number(KlNumber::Int(v[0] as i64))))))
                    },
                    _ => KlClosure::Done(shen_make_error("shen_string_to_n: expecting a unit string."))

                }
            }
        )
    )
}
// string->n:1 ends here

// [[file:../shen-rust.org::*simple-error][simple-error:1]]
pub fn shen_simple_error () -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | error | {
                match *error {
                    KlElement::String(ref s) => {
                        KlClosure::Done(shen_make_error(&s.as_str()))
                    },
                    _ => KlClosure::Done(shen_make_error("shen_simple_error: Expecting a string."))
                }
            }
        )
    )
}
// simple-error:1 ends here

// [[file:../shen-rust.org::*trap-error][trap-error:1]]
pub fn shen_trap_error() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | to_try_thunk | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | handler | {
                            let to_try_thunk = to_try_thunk.clone();
                            if !shen_is_thunk(to_try_thunk.clone()) {
                                KlClosure::Done(shen_make_error("shen_trap_error: Expecting a thunk."))
                            }
                            else {
                                match &*handler {
                                    &KlElement::Closure(KlClosure::FeedMe(ref f)) => {
                                        let forced = shen_force_thunk(to_try_thunk.clone());
                                        match forced {
                                            Ok(r) => { KlClosure::Done(Ok(r)) },
                                            Err(s) => match &*s {
                                                &KlError::ErrorString(ref s) => {
                                                    let exception = Rc::new(KlElement::String(s.clone()));
                                                    (&f)(exception.clone())
                                                }
                                            }
                                        }
                                    },
                                    _ => KlClosure::Done(shen_make_error("Expecting a closure."))
                                }
                            }
                        }
                    )
                )
            }
        )
    )
}
// trap-error:1 ends here

// [[file:../shen-rust.org::*error-to-string][error-to-string:1]]
pub fn shen_error_to_string() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | exception | {
                match &*exception {
                    &KlElement::String(ref s) => {
                        KlClosure::Done(Ok(Some(Rc::new(KlElement::String(s.clone())))))
                    },
                    _ => KlClosure::Done(shen_make_error("shen_error_to_string: expecting a string."))
                }
            }
        )
    )
}
// error-to-string:1 ends here

// [[file:../shen-rust.org::*Set][Set:1]]
pub fn shen_set () -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | symbol | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | value | {
                            let symbol = symbol.clone();
                            SYMBOL_TABLE.with(| symbol_table | {
                                let mut map = symbol_table.borrow_mut();
                                let symbol_string = shen_symbol_to_string(&*symbol);
                                match symbol_string {
                                    Ok(s) => {
                                        map.insert((*s).clone(), value);
                                        return KlClosure::Done(Ok(None))
                                    }
                                    _ => return KlClosure::Done(shen_make_error("shen_set: expecting a symbol for a key."))
                                }
                            })
                        }
                    )
                )
            }
        )
    )
}
// Set:1 ends here

// [[file:../shen-rust.org::*Get][Get:1]]
pub fn shen_value() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | symbol | {
                SYMBOL_TABLE.with(| symbol_table| {
                    let map = symbol_table.borrow();
                    let symbol_string = shen_symbol_to_string(&*symbol);
                    match symbol_string {
                        Ok(s) => {
                            match map.get(*s) {
                                Some(v) => KlClosure::Done(Ok(Some(v.clone()))),
                                None => KlClosure::Done(shen_make_error(&*(format!("variable {} is unbound", (*s)))))
                            }
                        },
                        _ => return KlClosure::Done(shen_make_error("shen_value: expecting a symbol for a key."))
                    }
                })
            }
        )
    )
}
// Get:1 ends here

// [[file:../shen-rust.org::*Cons][Cons:1]]
pub fn shen_cons() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | new_head | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | list | {
                            let new_head = new_head.clone();
                            match *list {
                                KlElement::Cons(ref cons_cells) => {
                                    let mut new_cons_cells = cons_cells.clone();
                                    new_cons_cells.push(new_head.clone());
                                    KlClosure::Done(Ok(Some(Rc::new(KlElement::Cons(new_cons_cells)))))
                                },
                                _ => KlClosure::Done(shen_make_error("shen_cons: Expecting a list."))
                            }
                        }
                    )
                )
            }
        )
    )
}
// Cons:1 ends here

// [[file:../shen-rust.org::*Head][Head:1]]
pub fn shen_hd() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | list | {
                match *list {
                    KlElement::Cons(ref cons_cells) => {
                        let head = cons_cells.last();
                        match head {
                            Some(hd) => KlClosure::Done(Ok(Some(hd.clone()))),
                            None => KlClosure::Done(Ok(None))
                        }
                    },
                    _ => KlClosure::Done(shen_make_error("shen_hd: Expecting a list"))

                }
            }
        )
    )
}
// Head:1 ends here

// [[file:../shen-rust.org::*Tail][Tail:1]]
pub fn shen_tl() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | list | {
                match *list {
                    KlElement::Cons(ref cons_cells) => {
                        let mut new_cons_cells = cons_cells.clone();
                        let popped = new_cons_cells.pop();
                        match popped {
                            Some(_) => KlClosure::Done(Ok(Some(Rc::new(KlElement::Cons(new_cons_cells))))),
                            _ => KlClosure::Done(Ok(None))
                        }
                    },
                    _ => KlClosure::Done(shen_make_error("shen_tl: Expecting a list."))
                }
            }
        )
    )
}
// Tail:1 ends here

// [[file:../shen-rust.org::*Cons?][Cons\?:1]]
pub fn shen_consp() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | list | {
                match *list {
                    KlElement::Cons(_) => KlClosure::Done(Ok(Some(Rc::new(KlElement::Symbol(String::from("true")))))),
                    _ => KlClosure::Done(Ok(Some(Rc::new(KlElement::Symbol(String::from("false"))))))
                }
            }
        )
    )
}
// Cons\?:1 ends here

// [[file:../shen-rust.org::*absvector][absvector:1]]
pub fn shen_absvector() -> KlClosure {
    let v = Vec::new();
    let uuid = Uuid::new_v4();
    let unique_vector = Rc::new(UniqueVector{ uuid: uuid, vector: RefCell::new(v) });
    VECTOR_TABLE.with(| vector_map | {
        let mut vector_map = vector_map.borrow_mut();
        vector_map.push((unique_vector.clone(), RefCell::new(Vec::new())));
    });
    KlClosure::Done(Ok(Some(Rc::new(KlElement::Vector(unique_vector)))))
}
// absvector:1 ends here

// [[file:../shen-rust.org::*address->][address->:1]]
pub fn shen_insert_at_address() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | vector | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | index | {
                            let vector = vector.clone();
                            KlClosure::FeedMe(
                                Rc::new(
                                    move | value | {
                                        match &*vector {
                                            &KlElement::Vector(ref unique_vector) => {
                                                match *index {
                                                    KlElement::Number(KlNumber::Int(i)) if i >= 0 => {
                                                        let mut payload = (**unique_vector).vector.borrow_mut();
                                                        let length = payload.len();
                                                        if i as usize <= length {
                                                            payload[i as usize] = value.clone();
                                                            match &*value {
                                                                &KlElement::Vector(_) | &KlElement::Cons(_) => {
                                                                    let tx = Box::new(
                                                                        move | ref_cell : &RefCell<Vec<usize>> | {
                                                                            let mut v = (*ref_cell).borrow_mut();
                                                                            v.push(i.clone() as usize);
                                                                        }
                                                                    );
                                                                    shen_with_unique_vector(&unique_vector, tx);
                                                                },
                                                                _ => ()
                                                            };
                                                            KlClosure::Done(Ok(Some(vector.clone())))
                                                        }
                                                        else {
                                                            KlClosure::Done(shen_make_error("shen_insert_at_address: Expecting a positive integer less than the vector length."))
                                                        }
                                                    },
                                                    _ => KlClosure::Done(shen_make_error("shen_insert_at_address: Expecting a positive number."))
                                                }
                                            },
                                            _ => KlClosure::Done(shen_make_error("shen_insert_at_address: Expecting a vector."))
                                        }
                                    }
                                )
                            )
                        }
                    )
                )
            }
        )
    )
}
// address->:1 ends here

// [[file:../shen-rust.org::*<-address][<-address:1]]
pub fn shen_get_at_address() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | vector | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | index | {
                            let vector = vector.clone();
                            match &*vector {
                                &KlElement::Vector(ref unique_vector) => {
                                    match *index {
                                        KlElement::Number(KlNumber::Int(i)) if i > 0 => {
                                            let payload = (**unique_vector).vector.borrow();
                                            let length = payload.len();
                                            if i as usize <= length {
                                                let ref found = payload[i as usize];
                                                KlClosure::Done(Ok(Some((*found).clone())))
                                            }
                                            else {
                                                KlClosure::Done(Ok(None))
                                            }
                                        },
                                        _ => KlClosure::Done(shen_make_error("shen_insert_at_address: Expecting a positive number."))
                                    }
                                },
                                _ => KlClosure::Done(shen_make_error("shen_insert_at_address: Expecting a vector."))
                            }
                        }
                    )
                )
            }
        )
    )
}
// <-address:1 ends here

// [[file:../shen-rust.org::*absvector?][absvector\?:1]]
pub fn shen_absvectorp() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | vector | {
                match &*vector {
                    &KlElement::Vector(_) => KlClosure::Done(Ok(Some(Rc::new(KlElement::Symbol(String::from("true")))))),
                    _ => KlClosure::Done(Ok(Some(Rc::new(KlElement::Symbol(String::from("false")))))),
                }
            }
        )
    )
}
// absvector\?:1 ends here

// [[file:../shen-rust.org::*write-byte][write-byte:1]]
pub fn shen_write_byte () -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | to_write | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | stream | {
                            let byte = to_write.clone();
                            match &*byte {
                                &KlElement::Number(KlNumber::Int(i)) => {
                                    let converted = TryFrom::try_from(i);
                                    match converted {
                                        Ok(byte) => {
                                            match *stream {
                                                KlElement::Stream(ref stream) => {
                                                    let stream : &KlStream = &*stream;
                                                    match stream {
                                                        &KlStream::FileStream(KlFileStream { direction: KlStreamDirection::Out, file: ref handle }) => {
                                                            let mut file = (*handle).borrow_mut();
                                                            let written = file.write(&[byte]);
                                                            match written {
                                                                Ok(_) => KlClosure::Done(Ok(Some(to_write.clone()))),
                                                                Err(_) => KlClosure::Done(shen_make_error("shen_write_byte: Could not write byte to file."))
                                                            }
                                                        },
                                                        &KlStream::Std(KlStdStream::Stdout) => {
                                                            let written = io::stdout().write(&[byte]);
                                                            match written {
                                                                Ok(_) => KlClosure::Done(Ok(Some(to_write.clone()))),
                                                                Err(_) => KlClosure::Done(shen_make_error("shen_write_byte: Could not write byte to stdout."))
                                                            }
                                                        }
                                                        _ => KlClosure::Done(shen_make_error("shen_write_byte: Expecting a write-only stream or stdout."))
                                                    }
                                                },
                                                _ => KlClosure::Done(shen_make_error("shen_write_byte: Expecting a stream."))
                                            }
                                        },
                                        Err(_) => KlClosure::Done(shen_make_error("shen_write_byte: Expecting a byte."))
                                    }
                                },
                                _ => KlClosure::Done(shen_make_error("shen_write_byte: Expecting a number."))
                            }
                        }
                    )
                )
            }
        )
    )
}
// write-byte:1 ends here

// [[file:../shen-rust.org::*read-byte][read-byte:1]]
pub fn shen_read_byte () -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            move | stream | {
                match *stream {
                    KlElement::Stream(ref stream) => {
                        let stream : &KlStream = &*stream;
                        let mut buffer = [0; 1];
                        let read = match stream {
                            &KlStream::FileStream(KlFileStream { direction: KlStreamDirection::In, file: ref handle }) => {
                                let mut file = (*handle).borrow_mut();
                                let mut buffer = [0;1];
                                file.read(&mut buffer[..])
                            },
                            &KlStream::Std(KlStdStream::Stdin) => {
                                io::stdin().read(&mut buffer[..])
                            }
                            _ => Err(Error::new(std::io::ErrorKind::Other, "shen_write_byte: Expecting a write-only stream or stdout."))
                        };
                        match read {
                            Ok(_) => {
                                let read : Result<i64,_> = TryFrom::try_from(buffer[0]);
                                match read {
                                    Ok(i) => KlClosure::Done(Ok(Some(Rc::new(KlElement::Number(KlNumber::Int(i)))))),
                                    Err(_) => KlClosure::Done(shen_make_error("shen_read_byte: Could not read a byte."))
                                }
                            },
                            Err(_) => KlClosure::Done(shen_make_error("shen_write_byte: Could not read byte."))
                        }

                    },
                    _ => KlClosure::Done(shen_make_error("shen_write_byte: Expecting a stream."))
                }
            }
        )
    )
}
// read-byte:1 ends here

// [[file:../shen-rust.org::*Open][Open:1]]
pub fn shen_open() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | file_name | {
                KlClosure::FeedMe(
                    Rc::new(
                        move | direction | {
                            let file_name = file_name.clone();
                            match &*file_name {
                                &KlElement::String(ref path) => {
                                    let path = path.as_str();
                                    match &*direction {
                                        &KlElement::Symbol(ref direction) if direction.as_str() == "in" => {
                                            match File::open(path) {
                                                Ok(f) =>
                                                    KlClosure::Done(
                                                        Ok(Some(Rc::new(KlElement::Stream(Rc::new(
                                                            KlStream::FileStream(
                                                                KlFileStream {
                                                                    direction: KlStreamDirection::In,
                                                                    file: RefCell::new(f)}))))))),
                                                _ => KlClosure::Done(shen_make_error("shen_open: Could not open file."))
                                            }
                                        },
                                        _ => KlClosure::Done(shen_make_error("shen_open: Expecting direction 'in'."))
                                    }
                                },
                                _ => KlClosure::Done(shen_make_error("shen_open: Expecting a file path."))
                            }
                        }
                    )
                )
            }
        )
    )
}
// Open:1 ends here

// [[file:../shen-rust.org::*get-time][get-time:1]]
pub fn shen_get_time() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | time_type | {
                match &*time_type {
                    &KlElement::Symbol(ref s) if s.as_str() == "run" || s.as_str() == "real" => {
                        KlClosure::Done(Ok(Some(Rc::new(KlElement::Number(KlNumber::Float(time::precise_time_s()))))))
                    }
                    _ => KlClosure::Done(shen_make_error("shen_open: Expecting 'run' or 'real'."))
                }
            }
        )
    )
}
// get-time:1 ends here

// [[file:../shen-rust.org::*Macros][Macros:1]]
macro_rules! number_op {
    ($a:ident, $b:ident, $checked_op:ident, $float_op:ident, $fn_name:expr, $op_name:expr) => {
        KlClosure::FeedMe(
            Rc::new(
                | $a | {
                    KlClosure::FeedMe(
                        Rc::new(
                            move | $b | {
                                let $a = $a.clone();
                                match (&*$a, &*$b) {
                                    (&KlElement::Number(KlNumber::Int(a)), &KlElement::Number(KlNumber::Int(b))) => {
                                        match a.$checked_op(b) {
                                            Some(i) => KlClosure::Done(Ok(Some(Rc::new(KlElement::Number(KlNumber::Int(i.clone())))))),
                                            _ =>
                                                KlClosure::Done(shen_make_error(format!("{}: {} would cause overflow.", $fn_name, $op_name).as_str()))
                                        }
                                    },
                                    (&KlElement::Number(KlNumber::Float(a)), &KlElement::Number(KlNumber::Int(b))) => {
                                        KlClosure::Done(Ok(Some(Rc::new(KlElement::Number(KlNumber::Float(a.$float_op(b as f64)))))))
                                    }
                                    (&KlElement::Number(KlNumber::Int(a)), &KlElement::Number(KlNumber::Float(b))) => {
                                        KlClosure::Done(Ok(Some(Rc::new(KlElement::Number(KlNumber::Float((a as f64).$float_op(b)))))))
                                    }
                                    (&KlElement::Number(KlNumber::Float(a)), &KlElement::Number(KlNumber::Float(b))) => {
                                        KlClosure::Done(Ok(Some(Rc::new(KlElement::Number(KlNumber::Float(a.$float_op(b)))))))
                                    }
                                    _ => KlClosure::Done(shen_make_error(format!("{}: expecting two numbers.", $fn_name).as_str()))
                                }
                            }
                        )
                    )
                }
            )
        )
    }
}

macro_rules! number_test {
    ($a:ident, $b:ident, $test:ident, $fn_name:expr) => {
        KlClosure::FeedMe(
            Rc::new(
                | $a | {
                    KlClosure::FeedMe(
                        Rc::new(
                            move | $b | {
                                let $a = $a.clone();
                                let test_result =
                                    match (&*$a, &*$b) {
                                        (&KlElement::Number(KlNumber::Int(a)), &KlElement::Number(KlNumber::Int(b))) => Some($test(a,&b)),
                                        (&KlElement::Number(KlNumber::Float(a)), &KlElement::Number(KlNumber::Int(b))) => Some($test(a,&(b as f64))),
                                        (&KlElement::Number(KlNumber::Int(a)), &KlElement::Number(KlNumber::Float(b))) => Some($test((a as f64), &b)),
                                        (&KlElement::Number(KlNumber::Float(a)), &KlElement::Number(KlNumber::Float(b))) => Some($test(a,&b)),
                                        _ => None
                                    };
                                match test_result {
                                    Some(true) => KlClosure::Done(Ok(Some(shen_string_to_symbol("true")))),
                                    Some(false) => KlClosure::Done(Ok(Some(shen_string_to_symbol("false")))),
                                    None => KlClosure::Done(shen_make_error(format!("{}: expecting two numbers.", $fn_name).as_str()))
                                }
                            }
                        )
                    )
                }
            )
        )
    }
}
// Macros:1 ends here

// [[file:../shen-rust.org::*Helpers][Helpers:1]]
pub fn shen_le_shim<T: PartialEq + PartialOrd>(a: T, b: &T) -> bool {
    a.le(&b)
}
pub fn shen_ge_shim<T: PartialEq + PartialOrd>(a: T, b: &T) -> bool {
    a.ge(&b)
}
pub fn shen_eq_ge_shim<T: PartialEq + PartialOrd>(a: T, b: &T) -> bool {
    a.ge(&b) || a.eq(&b)
}
pub fn shen_eq_le_shim<T: PartialEq + PartialOrd>(a: T, b: &T) -> bool {
    a.le(&b) || a.eq(&b)
}
// Helpers:1 ends here

// [[file:../shen-rust.org::*+][+:1]]
pub fn shen_plus() -> KlClosure {
    number_op!(number_a, number_b, checked_add, add, "shen_plus", "adding")
}
// +:1 ends here

// [[file:../shen-rust.org::**][*:1]]
pub fn shen_mul() -> KlClosure {
    number_op!(number_a, number_b, checked_mul, mul, "shen_mul", "multiplying")
}
// *:1 ends here

// [[file:../shen-rust.org::*-][-:1]]
pub fn shen_sub() -> KlClosure {
    number_op!(number_a, number_b, checked_sub, sub, "shen_sub", "subtracting")
}
// -:1 ends here

// [[file:../shen-rust.org::*/][/:1]]
pub fn shen_div() -> KlClosure {
    number_op!(number_a, number_b, checked_div, div, "shen_div", "dividing")
}
// /:1 ends here

// [[file:../shen-rust.org::*>][>:1]]
pub fn shen_ge() -> KlClosure {
    number_test!(number_a, number_b, shen_ge_shim, "shen_ge")
}
// >:1 ends here

// [[file:../shen-rust.org::*<][<:1]]
pub fn shen_le() -> KlClosure {
    number_test!(number_a, number_b, shen_le_shim, "shen_le")
}
// <:1 ends here

// [[file:../shen-rust.org::*>=][>=:1]]
pub fn shen_eq_le() -> KlClosure {
    number_test!(number_a, number_b, shen_eq_le_shim, "shen_le")
}
// >=:1 ends here

// [[file:../shen-rust.org::*<=][<=:1]]
pub fn shen_eq_ge() -> KlClosure {
    number_test!(number_a, number_b, shen_eq_ge_shim, "shen_le")
}
// <=:1 ends here

// [[file:../shen-rust.org::*number?][number\?:1]]
pub fn shen_numberp() -> KlClosure {
    KlClosure::FeedMe(
        Rc::new(
            | number | {
                match &*number {
                    &KlElement::Number(_) => KlClosure::Done(Ok(Some(shen_string_to_symbol("true")))),
                    _ => KlClosure::Done(Ok(Some(shen_string_to_symbol("false"))))
                }
            }
        )
    )
}
// number\?:1 ends here

// [[file:../shen-rust.org::*Filling%20The%20Function%20Table][Filling\ The\ Function\ Table:1]]
pub fn shen_fill_function_table() {
    FUNCTION_TABLE.with(| function_table | {
        let mut map = function_table.borrow_mut();
        map.insert(String::from("if")              ,shen_if());
        map.insert(String::from("and")             ,shen_and());
        map.insert(String::from("or")              ,shen_or());
        map.insert(String::from("cond")            ,shen_cond());
        map.insert(String::from("intern")          ,shen_intern());
        map.insert(String::from("pos")             ,shen_pos());
        map.insert(String::from("tlstr")           ,shen_tlstr());
        map.insert(String::from("cn")              ,shen_cn());
        map.insert(String::from("str")             ,shen_str());
        map.insert(String::from("string?")         ,shen_stringp());
        map.insert(String::from("n_to_string")     ,shen_n_to_string());
        map.insert(String::from("string_to_n")     ,shen_string_to_n());
        map.insert(String::from("simple_error ")   ,shen_simple_error ());
        map.insert(String::from("trap_error")      ,shen_trap_error());
        map.insert(String::from("error_to_string") ,shen_error_to_string());
        map.insert(String::from("set")             ,shen_set());
        map.insert(String::from("value")           ,shen_value());
        map.insert(String::from("cons")            ,shen_cons());
        map.insert(String::from("hd")              ,shen_hd());
        map.insert(String::from("tl")              ,shen_tl());
        map.insert(String::from("cons?")           ,shen_consp());
        map.insert(String::from("absvector")       ,shen_absvector());
        map.insert(String::from("address->")       ,shen_insert_at_address());
        map.insert(String::from("<-address")       ,shen_get_at_address());
        map.insert(String::from("absvectorp")      ,shen_absvectorp());
        map.insert(String::from("write_byte")      ,shen_write_byte());
        map.insert(String::from("read_byte")       ,shen_read_byte());
        map.insert(String::from("open")            ,shen_open());
        map.insert(String::from("get_time")        ,shen_get_time());
        map.insert(String::from("+")               ,shen_plus());
        map.insert(String::from("*")               ,shen_mul());
        map.insert(String::from("-")               ,shen_sub());
        map.insert(String::from("/")               ,shen_div());
        map.insert(String::from(">")               ,shen_ge());
        map.insert(String::from("<")               ,shen_le());
        map.insert(String::from("<=")              ,shen_eq_le());
        map.insert(String::from(">=")              ,shen_eq_ge());
        map.insert(String::from("number?")         ,shen_numberp());
    })
}
// Filling\ The\ Function\ Table:1 ends here

// [[file:../shen-rust.org::*KLambda%20Files][KLambda\ Files:1]]
const KLAMBDAFILES: &'static [ &'static str ] = &[
    "toplevel.kl", "core.kl", "sys.kl", "sequent.kl", "yacc.kl",
    "reader.kl", "prolog.kl", "track.kl", "load.kl", "writer.kl",
    "macros.kl", "declarations.kl", "types.kl", "t-star.kl"
];
// KLambda\ Files:1 ends here

// [[file:../shen-rust.org::*KLambda%20Files][KLambda\ Files:2]]
fn main () {
    shen_fill_function_table();
    let with_klambda_path : Vec<String> = KLAMBDAFILES
        .into_iter()
        .map(|f| {"KLambda/".to_string() + f})
        .collect();
    for f in with_klambda_path {
        let path = Path::new(&f);
        let mut kl : Vec<Vec<KlToken>>= Vec::new();
        match File::open(path) {
            Ok(mut f) => {
                let mut buffer : Vec<u8> = Vec::new();
                match f.read_to_end(&mut buffer) {
                    Ok(_) => {
                        collect_sexps(&buffer, &mut kl);
                        println!("{:?}", kl);
                    },
                    Err(e) => panic!("error: {:?}", e)
                }
            },
            Err(e) => panic!("error: {:?}", e)
        }
    }
}
// KLambda\ Files:2 ends here

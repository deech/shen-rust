// [[file:../shen-rust.org::*Preamble][Preamble:1]]
#![feature(slice_patterns)]
#![feature(custom_derive)]
#[macro_use]
extern crate nom;
use std::str;
use nom::*;
use std::path::Path;
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;
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

pub enum KlCons {
    Cons(Box<KlElement> , Box<KlCons>),
    Nil
}

pub enum KlElement {
    Symbol(String),
    Number(KlNumber),
    String(String),
    Cons(KlCons),
    Closure(KlClosure),
    Vector(Vec<KlElement>)
}

#[derive(Debug)]
pub struct KlError { cause : String }

pub enum KlClosure {
    FeedMe(Rc<Fn(Rc<KlElement>) -> KlClosure>),
    Thunk(Rc<Fn() -> Rc<KlElement>>),
    Done(Result<Rc<KlElement>,Rc<String>>)
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
pub fn shen_cons_to_vec (cons_cells: &KlCons) -> Vec<&KlElement> {
    let mut result : Vec<&KlElement> = Vec::new();
    let mut so_far = cons_cells;
    loop {
        match so_far {
            &KlCons::Cons(ref car, ref cdr) => {
                result.push(&**car);
                so_far = cdr;
            },
            &KlCons::Nil => return result,
        }
    }
}

pub fn shen_is_bool (a: &KlElement) -> bool {
    match a {
        &KlElement::Symbol(ref s) if s.as_str() == "true" || s.as_str() == "false" => true,
        _ => false
    }
}

pub fn shen_is_thunk(a: &KlElement) -> bool {
    match a {
        &KlElement::Closure(KlClosure::Thunk(_)) => true,
        _ => false
    }
}

pub fn shen_extract_from_thunk(a : &KlElement) -> Option<&Rc<Fn() -> Rc<KlElement>>> {
    match a {
        &KlElement::Closure(KlClosure::Thunk(ref inner)) => Some(inner),
        _ => None
    }
}

pub fn shen_make_error(s : &str) -> Result<Rc<KlElement>, Rc<String>> {
    Err(Rc::new(String::from(s)))
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
                                        if !shen_is_bool(&*predicate) {
                                            KlClosure::Done(shen_make_error("shen_if: the predicate must be 'true' or 'false'."))
                                        }
                                        else {
                                            let extracted = shen_extract_from_thunk(&*if_thunk).and_then(
                                                | if_branch | {
                                                    shen_extract_from_thunk(&*else_thunk).and_then(
                                                        move | else_branch | {
                                                            Some((if_branch,else_branch))
                                                        }
                                                    )
                                                }
                                            );
                                            if !extracted.is_some() {
                                                KlClosure::Done(shen_make_error("shen_if: Both the if and else branch must be thunks."))
                                            }
                                            else {
                                                match extracted.unwrap() {
                                                    (if_branch, else_branch) => {
                                                        match *predicate {
                                                            KlElement::Symbol(ref s) if s.as_str() == "true" => {
                                                                let forced = if_branch();
                                                                KlClosure::Done(Ok(forced))
                                                            },
                                                            KlElement::Symbol(ref s) if s.as_str() == "false" => {
                                                                let forced = else_branch();
                                                                KlClosure::Done(Ok(forced))
                                                            },
                                                            _ => KlClosure::Done(Err(Rc::new(String::from("Expecting predicate to be 'true' or 'false'."))))
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
                            let extracted = shen_extract_from_thunk(&*a_thunk).and_then(
                                | a | {
                                    shen_extract_from_thunk(&*b_thunk).and_then(
                                        move | b | {
                                            Some((a,b))
                                        }
                                    )
                                }
                            );
                            if !extracted.is_some() {
                                KlClosure::Done(shen_make_error("shen_and: Both arguments must be thunks."))
                            }
                            else {
                                match extracted.unwrap() {
                                    (a,b) => {
                                        let forced = a();
                                        if !shen_is_bool(&forced) {
                                            KlClosure::Done(shen_make_error("shen_and: The first argument must evaluate to the symbol 'true' or 'false."))
                                        }
                                        else {
                                            match &*forced {
                                                &KlElement::Symbol(ref a)
                                                    if a.as_str() == "false" =>
                                                    KlClosure::Done(Ok(Rc::new(KlElement::Symbol(String::from("false"))))),
                                                _ => {
                                                    let forced = b();
                                                    if !shen_is_bool(&forced) {
                                                        KlClosure::Done(shen_make_error("shen_and: The second argument must evaluate to the symbol 'true' or 'false."))
                                                    }
                                                    else {
                                                        match &*forced {
                                                            &KlElement::Symbol(ref b)
                                                                if b.as_str() == "false" =>
                                                                KlClosure::Done(Ok(Rc::new(KlElement::Symbol(String::from("false"))))),
                                                            _ => KlClosure::Done(Ok(Rc::new(KlElement::Symbol(String::from("true")))))
                                                        }
                                                    }
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
                            let extracted = shen_extract_from_thunk(&*a_thunk).and_then(
                                | a | {
                                    shen_extract_from_thunk(&*b_thunk).and_then(
                                        move | b | {
                                            Some((a,b))
                                        }
                                    )
                                }
                            );
                            if !extracted.is_some() {
                                KlClosure::Done(shen_make_error("shen_or: Both arguments must be thunks."))
                            }
                            else {
                                match extracted.unwrap() {
                                    (a,b) => {
                                        let forced = a();
                                        if !shen_is_bool(&forced) {
                                            KlClosure::Done(shen_make_error("shen_or: The first argument must evaluate to the symbol 'true' or 'false."))
                                        }
                                        else {
                                            match &*forced {
                                                &KlElement::Symbol(ref a)
                                                    if a.as_str() == "true" =>
                                                    KlClosure::Done(Ok(Rc::new(KlElement::Symbol(String::from("true"))))),
                                                _ => {
                                                    let forced = b();
                                                    if !shen_is_bool(&forced) {
                                                        KlClosure::Done(shen_make_error("shen_or: The second argument must evaluate to the symbol 'true' or 'false."))
                                                    }
                                                    else {
                                                        match &*forced {
                                                            &KlElement::Symbol(ref b)
                                                                if b.as_str() == "true" =>
                                                                KlClosure::Done(Ok(Rc::new(KlElement::Symbol(String::from("true"))))),
                                                            _ => KlClosure::Done(Ok(Rc::new(KlElement::Symbol(String::from("false")))))
                                                        }
                                                    }
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
                match *cases {
                    KlElement::Cons(ref cases) => {
                        let cases_vec = shen_cons_to_vec(cases);
                        let mut pairs : Vec<(&KlElement,&KlElement)>= Vec::new();
                        for case in cases_vec.as_slice() {
                            match *case {
                                &KlElement::Cons(ref predicate_action) => {
                                    let pair = shen_cons_to_vec(predicate_action);
                                    match pair.as_slice() {
                                        &[predicate,action] => {
                                            if !shen_is_thunk(predicate) || !shen_is_thunk(action) {
                                                return KlClosure::Done(shen_make_error("shen_cond: All cases must be a pairs of thunks."))
                                            }
                                            else {
                                                pairs.push((predicate,action))
                                            }
                                        }
                                        _ => return KlClosure::Done(shen_make_error("shen_cond: All cases must be pairs."))
                                    }
                                },
                                _ => return KlClosure::Done(shen_make_error("shen_cond: All cases must be a pairs of thunks."))
                            }
                        }
                        let mut result = None;
                        for &(predicate,action) in pairs.as_slice() {
                            let predicate_thunk = shen_extract_from_thunk(predicate).unwrap();
                            let forced = predicate_thunk();
                            if !shen_is_bool(&forced) {
                                result = Some(KlClosure::Done(shen_make_error("shen_cond: All predicates must evaluate to 'true' or 'false'.")))
                            }
                            else {
                                match &*forced {
                                    &KlElement::Symbol(ref s) if s.as_str() == "true" => {
                                        let forced = shen_extract_from_thunk(action).unwrap()();
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
                    _ => KlClosure::Done(shen_make_error("shen_cond: All cases must be predicate/action pairs."))
                }
            }
        )
    )
}
// Cond:1 ends here

// [[file:../shen-rust.org::*KLambda%20Files][KLambda\ Files:1]]
const KLAMBDAFILES: &'static [ &'static str ] = &[
    "toplevel.kl", "core.kl", "sys.kl", "sequent.kl", "yacc.kl",
    "reader.kl", "prolog.kl", "track.kl", "load.kl", "writer.kl",
    "macros.kl", "declarations.kl", "types.kl", "t-star.kl"
];
// KLambda\ Files:1 ends here

// [[file:../shen-rust.org::*KLambda%20Files][KLambda\ Files:2]]
fn main () {
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

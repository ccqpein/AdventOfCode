#![feature(let_chains)]
use serde_json::Value;
use tools::*;

fn array(v: &Value, p2: bool) -> i64 {
    if !p2 {
        v.as_array().unwrap().iter().map(|vv| helper(&vv)).sum()
    } else {
        v.as_array().unwrap().iter().map(|vv| helper2(&vv)).sum()
    }
}

fn obj(v: &Value) -> i64 {
    v.as_object()
        .unwrap()
        .iter()
        .map(|(_, vv)| helper(vv))
        .sum()
}

fn obj2(v: &Value) -> i64 {
    if v.as_object()
        .unwrap()
        .iter()
        .filter(|(_, vv)| {
            //dbg!(vv);
            if let Some(ss) = vv.as_str()
                && *ss == *"red"
            {
                true
            } else {
                false
            }
        })
        .count()
        != 0
    {
        0
    } else {
        v.as_object()
            .unwrap()
            .iter()
            .map(|(_, vv)| helper2(vv))
            .sum()
    }
}

fn num(v: &Value) -> i64 {
    v.as_i64().unwrap()
}

fn helper(v: &Value) -> i64 {
    match v {
        Value::Null => 0,
        Value::Bool(_) => 0,
        Value::Number(_) => num(v),
        Value::String(_) => 0,
        Value::Array(_) => array(v, false),
        Value::Object(_) => obj(v),
    }
}

fn helper2(v: &Value) -> i64 {
    match v {
        Value::Null => 0,
        Value::Bool(_) => 0,
        Value::Number(_) => num(v),
        Value::String(_) => 0,
        Value::Array(_) => array(v, true),
        Value::Object(_) => obj2(v),
    }
}

fn day12(input: &str) -> i64 {
    let v: Value = serde_json::from_str(input).unwrap();
    helper(&v)
}

fn day12_2(input: &str) -> i64 {
    let v: Value = serde_json::from_str(input).unwrap();
    helper2(&v)
}

fn main() {
    let input = read_file_by_line("../inputs/day12.input");
    println!("1: {:?}", day12(&input[0]));

    // let input = r#"[1,2,3]"#;
    // println!("1: {:?}", day12(input));
    // let input = r#"{"a":2,"b":4}"#;
    // println!("1: {:?}", day12(input));

    // let input = r#"[[[3]]]"#;
    // println!("1: {:?}", day12(input));

    // let input = r#"{"a":{"b":4},"c":-1}"#;
    // println!("1: {:?}", day12(input));

    let input = read_file_by_line("../inputs/day12.input");
    println!("2: {:?}", day12_2(&input[0]));

    //let input = r#"[1,{"c":"red","b":2},3]"#;
    //println!("2: {:?}", day12_2(&input));
}

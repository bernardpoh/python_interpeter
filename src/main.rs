use anyhow::*;
use python_interpreter_2::interpreter::Interpreter;
use std::fs;

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let mut interpreter = Interpreter::new(&input).context("syntax error")?;
    dbg!(&interpreter);
    interpreter
        .run()
        .with_context(|| anyhow!("runtime error {:#?}", interpreter))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;
    use python_interpreter_2::interpreter::Variable;
    use python_interpreter_2::*;
    fn test_interpreter(code: &str, expected: IndexMap<String, Variable>) {
        let mut interpreter =
            Interpreter::new(code).expect(&format!("syntax error, code: {}", code));
        dbg!(&interpreter.statements);
        interpreter.run().unwrap();
        let variables = &interpreter.namespace[0];
        for (k, v) in &expected {
            let a = v;
            let b = &variables.get(k).expect(&format!("key '{}' not found", k));
            assert!(
                interpreter_operations::is_equal(a, b),
                "expected: {a}, got: {b}"
            );
        }
    }
    macro_rules! expected {
        ($($k:expr => $v:expr),* $(,)?) => {{
            [$(($k.to_string(), $v),)*].into_iter().collect()
        }};
    }
    fn to_list(v: impl IntoIterator<Item = Variable>) -> Variable {
        use std::cell::RefCell;
        use std::rc::Rc;
        Variable::List(Rc::new(RefCell::new(v.into_iter().collect())))
    }

    #[test]
    fn test_if() {
        let code = r#"
a = 1
if a == 1:
    b = 2
else:
    b = 3
"#;
        let expected = expected! {
            "a" => 1.into(),
            "b" => 2.into(),
        };
        test_interpreter(code, expected);
    }

    #[test]
    fn test_map() {
        let code = r#"
a = [1,2,3]
def f(x):
    return x * 2
b = list(map(f, a))

"#;
        let expected = expected!(
            "a" => to_list([1.into(),2.into(),3.into()]),
            "b" => to_list([2.into(),4.into(),6.into()]),
        );
        test_interpreter(code, expected);
    }

    #[test]
    fn test_filter() {
        let code = r#"
a = [1,2,3,4]
def f(x):
    return x % 2 == 0
b = list(filter(f, a))
"#;
        let expected = expected!(
            "a" => to_list([1.into(),2.into(),3.into(),4.into()]),
            "b" => to_list([2.into(),4.into()]),
        );
        test_interpreter(code, expected);
    }

    #[test]
    fn test_indexing() {
        let code = r#"
a = [1,2,3]
b = a[1]"#;
        let expected = expected!(
            "a" => to_list([1.into(),2.into(),3.into()]),
            "b" => 2.into(),
        );
        test_interpreter(code, expected);
    }

    #[test]
    fn test_add_strings() {
        let code = r#"
a = "Hello, "
b = "world!"
c = a + b
    "#;
        let expected = expected!(
            "a" => "Hello, ".into(),
            "b" => "world!".into(),
            "c" => "Hello, world!".into(),
        );
        test_interpreter(code, expected);
    }

    #[test]
    fn test_add_ints() {
        let code = r#"
a = 1
b = 2
c = a + b"#;
        let expected = expected! {
            "a" => 1.into(),
            "b" => 2.into(),
            "c" => 3.into(),
        };
        test_interpreter(code, expected);
    }

    #[test]
    fn test_basic() {
        let code = r#"a = 1"#;
        let expected = expected! {"a" => 1.into()};
        test_interpreter(code, expected);
    }

    #[test]
    fn test_comparison() {
        let code = r#"
a = 1
b = 2
c = a == b
d = a != b
e = a < b
f = a > b
g = a <= b
h = a >= b
"#;
        let expected = expected! {
            "a" => 1.into(),
            "b" => 2.into(),
            "c" => false.into(),
            "d" => true.into(),
            "e" => true.into(),
            "f" => false.into(),
            "g" => true.into(),
            "h" => false.into(),
        };
        test_interpreter(code, expected);
    }

    #[test]
    fn test_while() {
        let code = r#"
a = 0
while a < 10:
    a = a + 1
"#;
        let expected = expected! {
            "a" => 10.into(),
        };
        test_interpreter(code, expected);
    }
    #[test]
    fn test_while2() {
        let code = r#"
a = 0
while a < 10:
    a += 1
    y = 5
"#;
        let expected = expected! {
            "a" => 10.into(),
            "y" => 5.into(),
        };
        test_interpreter(code, expected);
    }

    #[test]
    fn test_for() {
        let code = r#"
a = 0
for i in range(10):
    a += i
"#;
        let expected = expected! {
            "i" => 9.into(),
            "a" => 45.into(),
        };
        test_interpreter(code, expected);
    }

    #[test]
    fn test_function() {
        let code = r#"
def f(x):
    return x * 2
a = f(2)
"#;
        let expected = expected! {
            "a" => 4.into(),
        };
        test_interpreter(code, expected);
    }

    #[test]
    fn test_function2() {
        let code = r#"
def f(x, y):    
    return x * y
a = f(2, 3)
"#;
        let expected = expected! {
            "a" => 6.into(),
        };
        test_interpreter(code, expected);
    }

    #[test]
    fn test_function3() {
        let code = r#"
x = 2
def f():
    x = 3
y = f()
"#;
        let expected = expected! {
            "x" => 2.into(),
            "y" => Variable::None,
        };
        test_interpreter(code, expected);
    }

    #[test]
    fn test_add_assignment() {
        let code = r#"
a = 1
a += 1
a = a + 1
"#;
        let expected = expected! {
            "a" => 3.into(),
        };
        test_interpreter(code, expected);
    }

    #[test]
    fn test_append_methods() {
        let code = r#"
a = [1,2,3]
a.append(4)
"#;
        let expected = expected! {
            "a" => to_list([1.into(),2.into(),3.into(),4.into()]),
        };
        test_interpreter(code, expected);
    }

}
use crate::get_args;
use crate::interpreter::*;
use crate::interpreter_operations;
use crate::interpreter_operations::*;
use anyhow::*;
use im::vector;
use im::Vector;
use indexmap::IndexMap;
use itertools::Itertools;
use num_bigint::BigInt;
use num_bigint::ToBigInt;
use num_traits::cast::ToPrimitive;
use num_traits::Signed;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;
use Variable::*;

impl Display for HashableVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", to_regular_variable(self.clone()))
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            None => write!(f, "None"),
            Int(i) => write!(f, "{}", i),
            Float(fl) => write!(f, "{}", fl),
            String(s) => write!(f, "{}", s),
            Bool(b) => write!(f, "{}", display_bool(*b)),
            List(l) => write!(f, "{}", display_list(l)),
            Dict(d) => write!(f, "{}", display_dict(d)),
            Tuple(t) => write!(f, "({})", t.iter().join(", ")),
            Function(fn_name, fn_type) => {
                write!(f, "<function type: {:?} name: '{}'>", fn_type, fn_name)
            }
            Iterable(i) => write!(f, "<iterable {}>", i),
        }
    }
}

impl Display for VariableIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableIterator::Indexable(i) => write!(f, "indexable({})", i),
            VariableIterator::Reversed(i) => write!(f, "reversed({})", i),
            VariableIterator::Map(_, i) => write!(f, "map({})", i),
            VariableIterator::Filter(_, i) => write!(f, "filter({})", i),
            VariableIterator::Zip(v) => write!(f, "zip({})", v.iter().join(", ")),
            VariableIterator::Enumerate(_, i) => write!(f, "enumerate({})", i),
        }
    }
}

impl Display for IndexableIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let IndexableIterator { iter, start, end } = self;
        write!(f, "iter: {}, start: {}, end: {}", iter, start, end)
    }
}

impl Display for Indexable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Indexable::List(l) => write!(f, "{}", display_list(l)),
            Indexable::Dict(d, _, dict_type) => write!(f, "{dict_type}({})", display_dict(d)),
            Indexable::Tuple(t) => write!(f, "({})", t.iter().join(", ")),
            Indexable::String(s) => write!(f, "'{}'", s),
            Indexable::Range(start, stop, step) => {
                write!(f, "range({}, {}, {})", start, stop, step)
            }
        }
    }
}

impl Display for DictIterType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn display_list(l: &Rc<RefCell<Vector<Variable>>>) -> std::string::String {
    let s = l.borrow().iter().join(", ");
    format!("[{}]", s)
}

fn display_dict(d: &Rc<RefCell<IndexMap<HashableVariable, Variable>>>) -> std::string::String {
    let s = d
        .borrow()
        .iter()
        .map(|(k, v)| format!("{}: {}", k, v))
        .join(", ");
    format!("{{{}}}", s)
}

fn display_bool(b: bool) -> &'static str {
    match b {
        true => "True",
        false => "False",
    }
}

pub fn abs(v: Vector<Variable>) -> Result<Variable> {
    ensure!(v.len() == 1, "wrong number of arguments");
    let n = v.into_iter().next().unwrap();
    let result = match n {
        Int(i) => Int(i.abs()),
        Float(f) => Float(f.abs()),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn max(v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    reduce(v, interpreter, max_variable)?.ok_or(anyhow!("empty sequence"))
}

pub fn min(v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    reduce(v, interpreter, min_variable)?.ok_or(anyhow!("empty sequence"))
}

pub fn sum(v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    ensure!(
        v.len() == 1,
        "wrong number of arguments: expected 1, got {}",
        v.len()
    );
    Ok(reduce(v, interpreter, sum_function)?.unwrap_or(0.into()))
}

fn reduce(
    v: Vector<Variable>,
    interpreter: &mut Interpreter,
    f: fn(Variable, Variable) -> Result<Variable>,
) -> Result<Option<Variable>> {
    let items = match v.len() {
        0 => bail!("wrong number of arguments"),
        1 => collect(v.into_iter().next().unwrap(), interpreter)?,
        _ => v,
    };
    let f = |a, b| f(a?, b?);
    let result = items.into_iter().map(|a| Ok(a)).reduce(f).transpose()?;
    Ok(result)
}

fn max_variable(a: Variable, b: Variable) -> Result<Variable> {
    if compare(&a, &b)?.is_gt() {
        Ok(a)
    } else {
        Ok(b)
    }
}

fn min_variable(a: Variable, b: Variable) -> Result<Variable> {
    if compare(&a, &b)?.is_lt() {
        Ok(a)
    } else {
        Ok(b)
    }
}

fn sum_function(a: Variable, b: Variable) -> Result<Variable> {
    if matches!(a, String(_)) {
        bail!("not allowed to sum strings")
    }
    add(&a, &b)
}

pub fn round(v: Vector<Variable>) -> Result<Variable> {
    let (n, digits) = match v.len() {
        1 => (get_args!(v, 1), 0.into()),
        2 => get_args!(v, 2),
        _ => bail!("wrong number of arguments"),
    };
    let digits = match digits {
        Int(i) => i,
        None => 0.into(),
        _ => bail!("type error"),
    };
    let result = match n {
        Int(i) => {
            if digits >= 0.into() {
                return Ok(Int(i));
            }
            let digits = -digits;
            let sf = BigInt::from(10).pow(digits.to_u32().ok_or(anyhow!("int conversion error"))?);
            let remainder = &i % &sf;
            if remainder >= &sf / 2 {
                Int(i + sf - remainder)
            } else {
                Int(i - remainder)
            }
        }
        Float(f) => {
            let sf = 10f64.powf(digits.to_f64().ok_or(anyhow!("int conversion error"))?);
            let remainder = f % sf;
            if remainder >= (sf / 2.0) {
                Float(f + sf - remainder)
            } else {
                Float(f - remainder)
            }
        }
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn len(v: Vector<Variable>) -> Result<Variable> {
    let n = get_args!(v, 1);
    let result = match n {
        String(s) => Int(s.len().to_bigint().unwrap()),
        List(l) => Int(l.borrow().len().to_bigint().unwrap()),
        Dict(d) => Int(d.borrow().len().to_bigint().unwrap()),
        Tuple(t) => Int(t.len().to_bigint().unwrap()),
        _ => bail!("type error"),
    };
    Ok(result)
}

fn to_int(variable: Variable) -> Result<BigInt> {
    match variable {
        Int(i) => Ok(i),
        _ => bail!("type error"),
    }
}

pub fn range(mut v: Vector<Variable>) -> Result<Variable> {
    let mut start = 0.into();
    let stop;
    let mut step = 1.into();
    match v.len() {
        1 => stop = to_int(v.into_iter().next().unwrap())?,
        2 => {
            start = to_int(v.pop_front().unwrap())?;
            stop = to_int(v.pop_front().unwrap())?;
        }
        3 => {
            start = to_int(v.pop_front().unwrap())?;
            stop = to_int(v.pop_front().unwrap())?;
            step = to_int(v.pop_front().unwrap())?;
        }
        _ => bail!("wrong number of arguments: expected 1-3, got {}", v.len()),
    };
    Ok(Iterable(VariableIterator::Indexable(IndexableIterator {
        start: 0,
        end: ((&stop - &start) / &step)
            .to_usize()
            .ok_or(anyhow!("int conversion error"))?,
        iter: Indexable::Range(start, stop, step),
    })))
}

pub fn bool(v: Vector<Variable>) -> Result<Variable> {
    let n = get_args!(v, 1);
    Ok(truth_value(n).into())
}

pub fn int(v: Vector<Variable>) -> Result<Variable> {
    let n = get_args!(v, 1);
    let result = match n {
        Int(i) => Int(i),
        Float(f) => Int(f
            .floor()
            .to_bigint()
            .ok_or(anyhow!("error converting float to int"))?),
        String(s) => Int(s.parse()?),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn float(v: Vector<Variable>) -> Result<Variable> {
    let n = get_args!(v, 1);
    let result = match n {
        Int(i) => Float(i.to_f64().unwrap()),
        Float(f) => Float(f),
        String(s) => Float(s.parse()?),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn print(v: Vector<Variable>) -> Result<Variable> {
    println!("{}", v.iter().join(" "));
    Ok(None)
}

pub fn input(v: Vector<Variable>) -> Result<Variable> {
    let prompt = match v.len() {
        0 => "".to_string(),
        1 => format!("{}", get_args!(v, 1)),
        _ => bail!("wrong number of arguments: expected 0-1, got {}", v.len()),
    };
    let mut input = std::string::String::new();
    print!("{}", prompt);
    std::io::stdin().read_line(&mut input).unwrap();
    Ok(input.trim().into())
}

pub fn str(v: Vector<Variable>) -> Result<Variable> {
    ensure!(v.len() == 1, "wrong number of arguments");
    Ok(format!("{}", v[0]).into())
}

pub fn type_(v: Vector<Variable>) -> Result<Variable> {
    let n = get_args!(v, 1);
    let result = match n {
        None => String("NoneType".to_string()),
        Int(_) => String("int".to_string()),
        Float(_) => String("float".to_string()),
        String(_) => String("str".to_string()),
        Bool(_) => String("bool".to_string()),
        List(_) => String("list".to_string()),
        Dict(_) => String("dict".to_string()),
        Tuple(_) => String("tuple".to_string()),
        Function(..) => String("function".to_string()),
        Iterable(_) => String("iterable".to_string()),
    };
    Ok(result)
}

pub fn zip(v: Vector<Variable>) -> Result<Variable> {
    ensure!(v.len() >= 2, "wrong number of arguments");
    let iterables: Vector<_> = v.into_iter().map(into_iter).try_collect()?;
    Ok(Iterable(VariableIterator::Zip(iterables)))
}

pub fn enumerate(v: Vector<Variable>) -> Result<Variable> {
    let iterable = into_iter(get_args!(v, 1))?;
    Ok(Iterable(VariableIterator::Enumerate(0, Box::new(iterable))))
}

pub fn filter(v: Vector<Variable>) -> Result<Variable> {
    let (function, iterable) = get_args!(v, 2);
    let iter = into_iter(iterable)?;
    let function = get_function_value(function)?;
    let result = VariableIterator::Filter(function, Box::new(iter));
    Ok(Iterable(result))
}

pub fn map(v: Vector<Variable>) -> Result<Variable> {
    let (function, iterable) = get_args!(v, 2);
    let iter = into_iter(iterable)?;
    let function = get_function_value(function)?;
    let result = VariableIterator::Map(function, Box::new(iter));
    Ok(Iterable(result))
}

pub fn any(v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    let iter = collect(get_args!(v, 1), interpreter)?;
    let result = iter.into_iter().any(truth_value);
    Ok(result.into())
}

pub fn all(v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    let iter = collect(get_args!(v, 1), interpreter)?;
    let result = iter.into_iter().all(truth_value);
    Ok(result.into())
}

pub fn collect(iter: Variable, interpreter: &mut Interpreter) -> Result<Vector<Variable>> {
    let mut iter = into_iter(iter)?;
    let result =
        std::iter::from_fn(|| interpreter.next_item(&mut iter).transpose()).try_collect()?;
    Ok(result)
}

pub fn list(v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    let iter = get_args!(v, 1);
    let result = match iter {
        List(l) => l.borrow().clone(),
        Tuple(t) => t,
        _ => collect(iter, interpreter)?,
    };
    Ok(List(Rc::new(RefCell::new(result))))
}

pub fn dict(v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    let iter = get_args!(v, 1);
    let iter = collect(iter, interpreter)?;
    let dict = Rc::new(RefCell::new(IndexMap::new()));
    crate::dict_methods::update(dict, iter, interpreter)
}

pub fn tuple(v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    ensure!(v.len() == 1, "wrong number of arguments");
    let iter = v.into_iter().next().unwrap();
    match iter {
        t @ Tuple(_) => return Ok(t),
        List(l) => return Ok(Tuple(l.borrow().clone())),
        _ => (),
    }
    let result = collect(iter, interpreter)?;
    Ok(Tuple(result))
}

pub fn into_iter(variable: Variable) -> Result<VariableIterator> {
    let (len, iter) = match variable {
        Tuple(v) => (v.len(), Indexable::Tuple(v)),
        List(l) => {
            let len = l.borrow().len();
            (len, Indexable::List(l))
        }
        String(s) => (s.len(), Indexable::String(s)),
        Dict(d) => {
            let len = d.borrow().len();
            (len, Indexable::Dict(d, len, DictIterType::Keys))
        }
        Iterable(i) => return Ok(i),
        _ => bail!("type error"),
    };
    Ok(VariableIterator::Indexable(IndexableIterator {
        start: 0,
        end: len,
        iter,
    }))
}

pub fn reversed(v: Vector<Variable>) -> Result<Variable> {
    let iter = into_iter(get_args!(v, 1))?;
    let result = match iter {
        VariableIterator::Indexable(i) => VariableIterator::Reversed(i),
        VariableIterator::Reversed(i) => VariableIterator::Indexable(i),
        _ => bail!("type error"),
    };
    Ok(Iterable(result))
}

pub fn chr(v: Vector<Variable>) -> Result<Variable> {
    let c = get_args!(v, 1);
    let result = match c {
        Int(i) => String(
            std::char::from_u32(i.to_u32().ok_or(anyhow!("int conversion error"))?)
                .ok_or(anyhow!("invalid unicode"))?
                .to_string(),
        ),
        _ => bail!("type error: expected int"),
    };
    Ok(result)
}

pub fn ord(v: Vector<Variable>) -> Result<Variable> {
    let c = get_args!(v, 1);
    let result = match c {
        String(s) => {
            ensure!(
                s.len() == 1,
                "ord() expected a character, but string of length {} found",
                s.len()
            );
            s.chars().next().unwrap() as u32
        }
        _ => bail!("type error: expected string"),
    };
    Ok(result.into())
}

pub fn pow(v: Vector<Variable>) -> Result<Variable> {
    let (a, b) = get_args!(v, 2);
    interpreter_operations::pow(&a, &b)
}

pub fn divmod(v: Vector<Variable>) -> Result<Variable> {
    let (a, b) = get_args!(v, 2);
    let (quotient, remainder) = (floor_div(&a, &b)?, mod_(&a, &b)?);
    Ok(Variable::Tuple(vector![quotient, remainder]))
}

pub fn next(v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    let iter = get_args!(v, 1);
    let mut iter = match iter {
        Iterable(i) => i,
        _ => bail!("type error: expected iterable"),
    };
    let result = interpreter.next_item(&mut iter)?;
    result.ok_or(anyhow!("error: StopIteration"))?;
    unimplemented!()
}
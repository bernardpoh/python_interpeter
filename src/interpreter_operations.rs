use crate::interpreter::*;
use anyhow::*;
use itertools::{izip, Itertools};
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use num_traits::{Signed, Zero};
use std::cmp::Ordering;
use std::rc::Rc;
use std::cell::RefCell;

pub fn eval_binary_operator(operator: &str, left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match operator {
        "+" => add(left, right)?,
        "-" => sub(left, right)?,
        "*" => mul(left, right)?,
        "/" => div(left, right)?,
        "%" => mod_(left,right)?,
        "**" => pow(left,right)?,
        "//" => floor_div(left, right)?,
        "<<" => shl(left, right)?,
        ">>" => shr(left, right)?,
        "==" => is_equal(left, right).into(),
        "!=" => (!is_equal(left, right)).into(),
        "<" => compare(left, right)?.is_lt().into(),
        ">" => compare(left, right)?.is_gt().into(),
        "<=" => compare(left, right)?.is_le().into(),
        ">=" => compare(left, right)?.is_ge().into(),
        "and" => (get_bool_value(left)? && get_bool_value(right)?).into(),
        "or" => (get_bool_value(left)? || get_bool_value(right)?).into(),
        "is" => is_identical(left, right).into(),
        "is not" => (!is_identical(left, right)).into(),
        "in" => contains(right, left)?.into(),
        "not in" => (!contains(right, left)?).into(),
        "&" => match (left, right) {
            (Int(l), Int(r)) => Int(l & r),
            (Bool(l), Bool(r)) => Bool(l & r),
            _ => bail!("type error"),
        },
        "^" => match (left, right) {
            (Int(l), Int(r)) => Int(l ^ r),
            (Bool(l), Bool(r)) => Bool(l ^ r),
            _ => bail!("type error"),
        },
        "|" => match (left, right) {
            (Int(l), Int(r)) => Int(l | r),
            (Bool(l), Bool(r)) => Bool(l | r),
            _ => bail!("type error"),
        },
        _ => bail!("unknown binary operator {operator}"),
    };
    Ok(result)
}

macro_rules! int_or_float {
    () => {
        (Float(_) | Int(_), Float(_) | Int(_))
    };
}

pub fn is_equal(left: &Variable, right: &Variable) -> bool {
    use Variable::*;
    match (left, right) {
        (Int(l), Int(r)) => l == r,
        n @ int_or_float!() => {
            calc_float(n, |a, b| a == b).unwrap_or(false)
        }
        (String(l), String(r)) => l == r,
        (Bool(l), Bool(r)) => l == r,
        (None, None) => true,
        (Tuple(l), Tuple(r)) => l.len() == r.len() && izip!(l, r).all(|(a, b)| is_equal(a, b)),
        (List(l), List(r)) => {
            let l = l.borrow();
            let r = r.borrow();
            l.len() == r.len() && izip!(l.iter(), r.iter()).all(|(a, b)| is_equal(a, b))
        }
        (Dict(l), Dict(r)) => {
            let l = l.borrow();
            let r = r.borrow();
            l.len() == r.len()
                && izip!(l.iter(), r.iter())
                    .all(|((l0, l1), (r0, r1))| l0 == r0 && is_equal(l1, r1))
        }
        (Function(l, ..), Function(r, ..)) => l == r,
        _ => false,
    }
}

pub fn compare(left: &Variable, right: &Variable) -> Result<Ordering> {
    use Variable::*;
    let result = match (left, right) {
        (Int(l), Int(r)) => l.cmp(r),
        n @ int_or_float!() => calc_float(n, |a, b| {
            a.partial_cmp(&b).ok_or(anyhow!("could not compare floats"))
        })??,
        (String(l), String(r)) => l.cmp(&r),
        (Bool(l), Bool(r)) => l.cmp(&r),
        _ => bail!("type error"),
    };
    Ok(result)
}

fn is_identical(left: &Variable, right: &Variable) -> bool {
    use Variable::*;
    match (left, right) {
        (Int(l), Int(r)) => l == r,
        (Float(l), Float(r)) => l == r,
        (String(l), String(r)) => l == r,
        (Bool(l), Bool(r)) => l == r,
        (None, None) => true,
        (Tuple(l), Tuple(r)) => l.len() == r.len() && izip!(l, r).all(|(a, b)| is_identical(a, b)),
        (List(l), List(r)) => Rc::ptr_eq(&l, &r),
        (Dict(l), Dict(r)) => Rc::ptr_eq(&l, &r),
        (Function(l, ..), Function(r, ..)) => l == r,
        _ => false,
    }
}

fn calc_float<F, U>(n: (&Variable, &Variable), f: F) -> Result<U>
where
    F: Fn(f64, f64) -> U,
{
    Ok(f(int_to_float(&n.0)?, int_to_float(&n.1)?))
}

pub fn add(left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match (left, right) {
        (Int(l), Int(r)) => Int(l + r),
        n @ int_or_float!() => calc_float(n, |a, b| a + b)?.into(),
        (String(l), String(r)) => String(l.clone() + r),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn sub(left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match (left, right) {
        (Int(l), Int(r)) => Int(l - r),
        n @ int_or_float!() => calc_float(n, |a, b| a - b)?.into(),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn mul(left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match (left, right) {
        (Int(l), Int(r)) => Int(l * r),
        n @ int_or_float!() => calc_float(n, |a, b| a * b)?.into(),
        (String(s), Int(i)) | (Int(i), String(s)) => {
            let n = get_repeat_amount(i)?;
            String(s.repeat(n))
        }
        (List(l), Int(i)) | (Int(i), List(l)) => {
            let l = l.borrow();
            let n = get_repeat_amount(i)?;
            let new_list = std::iter::repeat(l.clone()).take(n).flatten().collect();
            List(Rc::new(RefCell::new(new_list)))
        }
        (Tuple(l), Int(i)) | (Int(i), Tuple(l)) => {
            let n = get_repeat_amount(i)?;
            let new_list = std::iter::repeat(l.clone()).take(n).flatten().collect();
            Tuple(new_list)
        }
        
        _ => bail!("type error"),
    };
    Ok(result)
}

fn get_repeat_amount(i: &BigInt) -> Result<usize> {
    let repeat_amount = i.max(&BigInt::zero()).to_usize().ok_or(anyhow!("int conversion"))?;
    Ok(repeat_amount)
}

pub fn div(left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match (left, right) {
        n @ int_or_float!() => {
            let x = calc_float(n, |a, b| a / b)?;
            ensure!(x.is_normal(), "division by zero");
            Float(x)
        }
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn mod_(left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match (left, right) {
        (Int(l), Int(r)) => Int(l % r),
        n @ int_or_float!() => calc_float(n, |a, b| a % b)?.into(),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn pow(left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match (left, right) {
        (Int(l), Int(r)) => Int(l.pow(r.try_into()?)),
        n @ int_or_float!() => calc_float(n, |a, b| a.powf(b))?.into(),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn floor_div(left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match (left, right) {
        (Int(l), Int(r)) => Int(l / r),
        n @ int_or_float!() => calc_float(n, |a, b| (a / b).floor())?.into(),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn shl(left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match (left, right) {
        (Int(l), Int(r)) => Int(l * BigInt::from(2).pow(r.try_into()?)),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn shr(left: &Variable, right: &Variable) -> Result<Variable> {
    use Variable::*;
    let result = match (left, right) {
        (Int(l), Int(r)) => Int(l / BigInt::from(2).pow(r.try_into()?)),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn eval_unary_operator(operator: String, right: Variable) -> Result<Variable> {
    use Variable::*;
    let result = match operator.as_str() {
        "-" => match right {
            Int(i) => Int(-i),
            Float(f) => Float(-f),
            _ => bail!("type error"),
        },
        "+" => match right {
            Int(i) => Int(i),
            Float(f) => Float(f),
            _ => bail!("type error"),
        },
        "~" => match right {
            Int(i) => Int(-i - 1),
            Bool(b) => Bool(!b),
            _ => bail!("type error"),
        },
        "not" => (!truth_value(right)).into(),
        _ => bail!("unknown unary operator {operator}"),
    };
    Ok(result)
}

fn int_to_float(v: &Variable) -> Result<f64> {
    use Variable::*;
    let result = match v {
        Int(i) => i
            .to_f64()
            .ok_or(anyhow!("int {i} cannot be converted to float"))?,
        Float(f) => *f,
        _ => unreachable!(),
    };
    Ok(result)
}

pub fn truth_value(v: Variable) -> bool {
    use Variable::*;
    match v {
        None => false,
        Bool(b) => b,
        String(s) => !s.is_empty(),
        Int(i) => i != BigInt::from(0),
        Float(f) => f != 0.0,
        Tuple(t) => !t.is_empty(),
        List(l) => !l.borrow().is_empty(),
        Dict(d) => !d.borrow().is_empty(),
        Function(..) => true,
        Iterable(..) => true,
    }
}

pub fn get_bool_value(v: &Variable) -> Result<bool> {
    use Variable::*;
    let result = match v {
        Bool(b) => *b,
        _ => bail!("type error"),
    };
    Ok(result)
}

fn get_string_value(v: &Variable) -> Result<&str> {
    use Variable::*;
    let result = match v {
        String(s) => s,
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn get_function_value(function: Variable) -> Result<FunctionType> {
    let function = match function {
        Variable::Function(_, f) => f,
        _ => bail!("type error"),
    };
    Ok(function)
}

fn contains(left: &Variable, right: &Variable) -> Result<bool> {
    Ok(match left {
        Variable::String(s) => s.contains(&get_string_value(right)?).into(),
        Variable::List(l) => l.borrow().iter().any(|item| is_equal(&item, &right)).into(),
        Variable::Dict(d) => d
            .borrow()
            .contains_key(&to_hashable_variable(right.clone())?)
            .into(),
        _ => bail!("type error"),
    })
}

pub fn to_hashable_variable(variable: Variable) -> Result<HashableVariable> {
    let result = match variable {
        Variable::Int(i) => HashableVariable::Int(i),
        Variable::String(s) => HashableVariable::String(s),
        Variable::Bool(b) => HashableVariable::Bool(b),
        Variable::None => HashableVariable::None,
        Variable::Tuple(t) => HashableVariable::Tuple(
            t.into_iter().map(to_hashable_variable).try_collect()?,
        ),
        _ => bail!("type error"),
    };
    Ok(result)
}

pub fn to_regular_variable(variable: HashableVariable) -> Variable {
    match variable {
        HashableVariable::Int(i) => Variable::Int(i),
        HashableVariable::String(s) => Variable::String(s),
        HashableVariable::Bool(b) => Variable::Bool(b),
        HashableVariable::None => Variable::None,
        HashableVariable::Tuple(t) => {
            Variable::Tuple(t.into_iter().map(to_regular_variable).collect())
        }
    }
}

pub fn get_positive_index(index_no: Variable, list_len: usize) -> Result<usize> {
    let mut index_no = match index_no {
        Variable::Int(i) => i,
        _ => bail!("type error"),
    };
    if index_no.is_negative() {
        index_no = list_len + index_no - 1;
    }
    let index_no = index_no.to_usize().ok_or(anyhow!("int conversion"))?;
    Ok(index_no)
}

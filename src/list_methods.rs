use crate::default_functions::collect;
use crate::interpreter::*;
use crate::interpreter_operations::*;
use anyhow::*;
use im::Vector;
use itertools::Itertools;
use num_traits::ToPrimitive;
use std::{cell::RefCell, rc::Rc};
type List = Rc<RefCell<Vector<Variable>>>;

#[macro_export]
macro_rules! get_args {
    ($v:expr, 0) => {{
        ensure!(
            $v.len() == 0,
            "wrong number of arguments: expected 0, got {}",
            $v.len()
        );
    }};
    ($v:expr, 1) => {{
        ensure!(
            $v.len() == 1,
            "wrong number of arguments: expected 1, got {}",
            $v.len()
        );
        $v.into_iter().next().unwrap()
    }};
    ($v:expr, $n:expr) => {{
        ensure!(
            $v.len() == $n,
            "wrong number of arguments: expected {}, got {}",
            $n,
            $v.len()
        );
        $v.into_iter().next_tuple().unwrap()
    }};
}

pub fn append(list: List, v: Vector<Variable>) -> Result<Variable> {
    let value = get_args!(v, 1);
    list.borrow_mut().push_back(value);
    Ok(Variable::None)
}

pub fn clear(list: List, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    list.borrow_mut().clear();
    Ok(Variable::None)
}

pub fn copy(list: List, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    Ok(Variable::List(Rc::new(RefCell::new(list.borrow().clone()))))
}

pub fn count(list: List, v: Vector<Variable>) -> Result<Variable> {
    let value = get_args!(v, 1);
    let count = list.borrow().iter().filter(|x| is_equal(x, &value)).count();
    Ok(Variable::Int(count.into()))
}

pub fn extend(list: List, v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    let iter = get_args!(v, 1);
    let iter = collect(iter, interpreter)?;
    list.borrow_mut().append(iter);
    Ok(Variable::None)
}

pub fn index(list: List, v: Vector<Variable>) -> Result<Variable> {
    let value = get_args!(v, 1);
    let index = list.borrow().iter().position(|x| is_equal(x, &value));
    if let Some(index) = index {
        Ok(Variable::Int(index.into()))
    } else {
        bail!("ValueError: {} is not in list", value)
    }
}

pub fn insert(list: List, v: Vector<Variable>) -> Result<Variable> {
    let (position, value) = get_args!(v, 2);
    if let Variable::Int(i) = position {
        list.borrow_mut()
            .insert(i.to_usize().ok_or(anyhow!("int conversion"))?, value);
        Ok(Variable::None)
    } else {
        bail!("TypeError")
    }
}

pub fn pop(list: List, v: Vector<Variable>) -> Result<Variable> {
    let position = get_args!(v, 1);
    if let Variable::Int(i) = position {
        let i = i.to_usize().ok_or(anyhow!("int conversion"))?;
        let v = list.borrow_mut().remove(i);
        Ok(v)
    } else {
        bail!("TypeError")
    }
}

pub fn remove(list: List, v: Vector<Variable>) -> Result<Variable> {
    let value = get_args!(v, 1);
    let list = list;
    let index = list.borrow().iter().position(|x| is_equal(x, &value));
    if let Some(index) = index {
        let v = list.borrow_mut().remove(index);
        Ok(v)
    } else {
        bail!("ValueError: {} is not in list", value)
    }
}

pub fn reverse(list: List, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    *list.borrow_mut() = list.borrow().iter().cloned().rev().collect();
    Ok(Variable::None)
}

use try_partialord::*;
struct SortableVariable(Variable);

impl PartialEq for SortableVariable {
    fn eq(&self, other: &Self) -> bool {
        is_equal(&self.0, &other.0)
    }
}

impl PartialOrd for SortableVariable {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        compare(&self.0, &other.0).ok()
    }
}

pub fn sort(list: List, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    let mut sortable = list
        .borrow()
        .iter()
        .cloned()
        .map(SortableVariable)
        .collect_vec();
    if let Err(e) = sortable.try_sort() {
        bail!("sort error: {}", e)
    }
    let l = sortable.into_iter().map(|x| x.0).collect();
    *list.borrow_mut() = l;
    Ok(Variable::None)
}

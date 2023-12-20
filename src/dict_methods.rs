use crate::default_functions::collect;
use crate::interpreter::*;
use crate::interpreter_operations::*;
use crate::get_args;
use anyhow::*;
use im::Vector;
use im::vector;
use indexmap::IndexMap;
use itertools::Itertools;
use std::{cell::RefCell, rc::Rc};
type Dict = Rc<RefCell<IndexMap<HashableVariable, Variable>>>;

pub fn clear(dict: Dict, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    dict.borrow_mut().clear();
    Ok(Variable::None)
}

pub fn copy(dict: Dict, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    Ok(Variable::Dict(Rc::new(RefCell::new(dict.borrow().clone()))))
}

pub fn get(dict: Dict, v: Vector<Variable>) -> Result<Variable> {
    let key = get_args!(v, 1);
    let key = to_hashable_variable(key)?;
    let value = dict
        .borrow()
        .get(&key)
        .ok_or_else(|| anyhow!("key '{}' not found", key))?.clone();
    Ok(value)
}

pub fn get_dict_iterator(dict: Dict, iter_type: DictIterType) -> Variable{
    let len = dict.borrow().len();
    Variable::Iterable(VariableIterator::Indexable(IndexableIterator { start: 0, end: len, iter: Indexable::Dict(dict, len, iter_type) }))
}

pub fn keys(dict: Dict, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    Ok(get_dict_iterator(dict, DictIterType::Keys))
}

pub fn values(dict: Dict, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    Ok(get_dict_iterator(dict, DictIterType::Values))
}

pub fn items(dict: Dict, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    Ok(get_dict_iterator(dict, DictIterType::Items))
}

pub fn pop(dict: Dict, v: Vector<Variable>) -> Result<Variable> {
    let key = get_args!(v, 1);
    let key = to_hashable_variable(key)?;
    let value = dict
        .borrow_mut()
        .remove(&key)
        .ok_or_else(|| anyhow!("key '{}' not found", key))?;
    Ok(value)
}

pub fn pop_item(dict: Dict, v: Vector<Variable>) -> Result<Variable> {
    get_args!(v, 0);
    let (key, value) = dict
        .borrow_mut()
        .pop()
        .ok_or_else(|| anyhow!("dict is empty"))?;
    Ok(Variable::Tuple(vector![key.into(), value.into()]))
}

pub fn set_default(dict: Dict, v: Vector<Variable>) -> Result<Variable> {
    let (key, default) = match v.len() {
        1 => (get_args!(v, 1), Variable::None),
        2 => get_args!(v, 2),
        _ => bail!("wrong number of arguments: expected 1-2, got {}", v.len()),
    };
    let key = to_hashable_variable(key)?;
    let value = dict.borrow_mut().entry(key).or_insert(default).clone();
    Ok(value)
}

pub fn update(dict:Dict, v: Vector<Variable>, interpreter: &mut Interpreter) -> Result<Variable> {
    let iter = get_args!(v, 1);
    match iter {
        Variable::Dict(other_dict) => {
            let other_dict = other_dict.borrow().clone();
            dict.borrow_mut().extend(other_dict);
        }
        _ => {
            let iter = collect(iter, interpreter)?;
            let iter: Vec<_> = iter.into_iter().map(to_pair).try_collect()?;
            dict.borrow_mut().extend(iter);
        }
    }
    Ok(Variable::None)
}

fn to_pair(v: Variable) -> Result<(HashableVariable, Variable)> {
    let list = match v {
        Variable::Tuple(v) => v,
        Variable::List(v) => v.borrow().clone(),
        _ => bail!("TypeError: cannot convert {} to tuple", v),
    };
    let (k,v) = get_args!(list, 2);
    Ok((to_hashable_variable(k)?, v))
}
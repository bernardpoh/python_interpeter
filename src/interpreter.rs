use crate::default_functions;
use crate::default_functions::into_iter;
use crate::dict_methods;
use crate::interpreter_operations::*;
use crate::lexer::*;
use crate::lexer2::*;
use crate::lexer4::*;
use crate::list_methods;
use crate::parser::*;
use crate::parser_assign::*;
use crate::parser_expr::*;
use anyhow::*;
use im::{vector, Vector};
use indexmap::IndexMap;
use itertools::chain;
use itertools::{izip, Itertools};
use num_bigint::BigInt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Variable {
    String(String),
    Int(BigInt),
    Float(f64),
    Bool(bool),
    None,
    Tuple(Vector<Self>),
    List(Rc<RefCell<Vector<Self>>>),
    Dict(Rc<RefCell<IndexMap<HashableVariable, Self>>>),
    Function(String, FunctionType),
    Iterable(VariableIterator),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HashableVariable {
    Tuple(Vector<Self>),
    String(String),
    Int(BigInt),
    Bool(bool),
    None,
}

#[derive(Clone)]
pub enum FunctionType {
    UserDefined(Vector<String>, Vector<Stmt>),
    Standard(fn(Vector<Variable>) -> Result<Variable>),
    Standard2(fn(Vector<Variable>, &mut Interpreter) -> Result<Variable>),
    Lambda(Vector<String>, Expr),
}

impl Debug for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionType::UserDefined(..) => write!(f, "UserDefined"),
            FunctionType::Standard(..) => write!(f, "Standard"),
            FunctionType::Standard2(..) => write!(f, "Standard2"),
            FunctionType::Lambda(..) => write!(f, "Lambda"),
        }
    }
}

impl From<bool> for Variable {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<f64> for Variable {
    fn from(f: f64) -> Self {
        Self::Float(f)
    }
}

impl From<String> for Variable {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<&str> for Variable {
    fn from(s: &str) -> Self {
        Self::String(s.to_string())
    }
}

impl From<usize> for Variable {
    fn from(i: usize) -> Self {
        Self::Int(i.into())
    }
}

impl From<BigInt> for Variable {
    fn from(i: BigInt) -> Self {
        Self::Int(i)
    }
}

impl From<HashableVariable> for Variable {
    fn from(h: HashableVariable) -> Self {
        to_regular_variable(h)
    }
}

impl From<i32> for Variable {
    fn from(i: i32) -> Self {
        Self::Int(i.into())
    }
}

impl From<u32> for Variable {
    fn from(i: u32) -> Self {
        Self::Int(i.into())
    }
}

#[derive(Debug, Clone)]
pub enum VariableIterator {
    Indexable(IndexableIterator),
    Reversed(IndexableIterator),
    Map(FunctionType, Box<Self>),
    Filter(FunctionType, Box<Self>),
    Zip(Vector<Self>),
    Enumerate(usize, Box<Self>),
}

#[derive(Debug, Clone)]
pub struct IndexableIterator {
    pub start: usize,
    pub end: usize,
    pub iter: Indexable,
}

#[derive(Debug, Clone)]
pub enum Indexable {
    Tuple(Vector<Variable>),
    List(Rc<RefCell<Vector<Variable>>>),
    String(String),
    Dict(
        Rc<RefCell<IndexMap<HashableVariable, Variable>>>,
        usize,
        DictIterType,
    ),
    Range(BigInt, BigInt, BigInt),
}
#[derive(Debug, Clone)]
pub enum DictIterType {
    Keys,
    Values,
    Items,
}

impl Indexable {
    fn get_item(&self, index: usize) -> Result<Option<Variable>> {
        let result = match self {
            Indexable::Tuple(t) => t.get(index).cloned(),
            Indexable::String(s) => s.get(index..=index).map(|s| s.to_string().into()),
            Indexable::List(l) => l.borrow().get(index).cloned(),
            Indexable::Dict(dict, dict_len, iter_type) => {
                let dict = dict.borrow();
                ensure!(
                    dict.len() == *dict_len,
                    "dictionary changed size during iteration"
                );
                let (k, v) = match dict.get_index(index) {
                    Some((k, v)) => (k, v),
                    None => return Ok(None),
                };
                let result = match iter_type {
                    DictIterType::Keys => k.clone().into(),
                    DictIterType::Values => v.clone(),
                    DictIterType::Items => Variable::Tuple(vector![k.clone().into(), v.clone()]),
                };
                Some(result)
            }
            Indexable::Range(start, end, step) => {
                let index = { start + index * step };
                if &index >= start && &index < end {
                    Some(index.into())
                } else {
                    None
                }
            }
        };
        Ok(result)
    }
}

#[derive(Debug, Clone)]
enum EvaluatedIdentifierData {
    Index(Variable),
    Attribute(String),
    Slice([Variable; 3]),
}
#[derive(Debug, Clone)]
enum StatementResult {
    Return(Variable),
    Break,
    Continue,
    None,
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub statements: Vector<Stmt>,
    pub namespace: Vector<HashMap<String, Variable>>,
}

impl Interpreter {
    pub fn new(code: &str) -> Result<Self> {
        let tokens: Vector<_> = Lexer::new(code)
            .try_collect()
            .context("parsing into basic tokens")?;
        let lines = convert_tokens_to_lines(tokens.into_iter()).context("parsing into lines")?;
        let block_lines = lines.into_iter().map(to_block_line).try_collect()?;
        let blocks = group_lines(block_lines).context("parsing into blocks")?;

        let error_message = anyhow!("blocks: {:#?}", blocks);
        let statements = parse_grouped_blocks(blocks)
            .context("parsing into statements")
            .with_context(|| error_message)?;
        Ok(Self {
            statements,
            namespace: vector![Self::get_default_namespace()],
        })
    }

    fn get_default_namespace() -> HashMap<String, Variable> {
        const FUNCTIONS: &[(&str, fn(Vector<Variable>) -> Result<Variable>)] = &[
            ("bool", default_functions::bool),
            ("int", default_functions::int),
            ("float", default_functions::float),
            ("range", default_functions::range),
            ("len", default_functions::len),
            ("str", default_functions::str),
            ("type", default_functions::type_),
            ("map", default_functions::map),
            ("zip", default_functions::zip),
            ("filter", default_functions::filter),
            ("enumerate", default_functions::enumerate),
            ("reversed", default_functions::reversed),
            ("abs", default_functions::abs),
            ("round", default_functions::round),
            ("ord", default_functions::ord),
            ("chr", default_functions::chr),
            ("divmod", default_functions::divmod),
            ("pow", default_functions::pow),
            ("input", default_functions::input),
            ("print", default_functions::print),
        ];
        const FUNCTIONS2: &[(
            &str,
            fn(Vector<Variable>, &mut Interpreter) -> Result<Variable>,
        )] = &[
            ("list", default_functions::list),
            ("sum", default_functions::sum),
            ("max", default_functions::max),
            ("min", default_functions::min),
        ];

        use FunctionType::*;
        use Variable::*;
        let namespace = chain!(
            FUNCTIONS
                .into_iter()
                .map(|(s, f)| (s.to_string(), Function(s.to_string(), Standard(*f)))),
            FUNCTIONS2
                .into_iter()
                .map(|(s, f)| (s.to_string(), Function(s.to_string(), Standard2(*f)))),
        )
        .collect();
        namespace
    }

    pub fn run(&mut self) -> Result<()> {
        while let Some(statement) = self.statements.pop_front() {
            self.run_statement(statement)?;
        }
        Ok(())
    }

    fn run_statements(&mut self, statements: Vector<Stmt>) -> Result<StatementResult> {
        for statement in statements {
            let result = self.run_statement(statement)?;
            match result {
                StatementResult::None => (),
                _ => return Ok(result),
            }
        }
        Ok(StatementResult::None)
    }

    fn try_run_statement(&mut self, statement: StmtType) -> Result<StatementResult> {
        use StmtType::*;
        match statement {
            Expr(expr) => {
                let variable = self.eval(expr)?;
                dbg!(variable);
            }
            Assign(assign_value, assign_op, expr) => {
                let variable = self.eval(expr)?;
                macro_rules! assign {
                    ($f:expr) => {{
                        let old_value = self.read_assignment(assign_value.clone())?;
                        let new_value = $f(&old_value, &variable)?;
                        self.write_assignment(assign_value, new_value)?;
                    }};
                }
                match assign_op.as_str() {
                    "=" => self.write_assignment(assign_value, variable)?,
                    "+=" => assign!(add),
                    "-=" => assign!(sub),
                    "*=" => assign!(mul),
                    "/=" => assign!(div),
                    "%=" => assign!(mod_),
                    "**=" => assign!(pow),
                    "//=" => assign!(floor_div),
                    "<<=" => assign!(shl),
                    ">>=" => assign!(shr),
                    _ => bail!("unknown assignment operator"),
                }
            }
            If {
                condition,
                body,
                elif,
                else_,
            } => {
                let condition = self.eval(condition)?;
                if truth_value(condition) {
                    return self.run_statements(body);
                }
                for (condition, body) in elif {
                    let condition = self.eval(condition)?;
                    if truth_value(condition) {
                        return self.run_statements(body);
                    }
                }
                if let Some(body) = else_ {
                    return self.run_statements(body);
                }
            }
            While { condition, body } => {
                'conditional_loop: while truth_value(self.eval(condition.clone())?) {
                    'stmt_loop: for statement in body.clone() {
                        let statement_result = self.run_statement(statement)?;
                        use StatementResult::*;
                        match statement_result {
                            r @ Return(_) => return Ok(r),
                            Break => break 'conditional_loop,
                            Continue => break 'stmt_loop,
                            None => (),
                        }
                    }
                }
            }
            Def {
                fn_name,
                parameters,
                body,
            } => {
                let function = Variable::Function(
                    fn_name.clone(),
                    FunctionType::UserDefined(parameters, body),
                );
                self.namespace
                    .iter_mut()
                    .last()
                    .unwrap()
                    .insert(fn_name, function);
            }
            For { target, iter, body } => {
                let mut iter = into_iter(self.eval(iter)?)?;
                'iter_loop: while let Some(item) = self.next_item(&mut iter)? {
                    self.write_assignment(target.clone(), item)?;

                    'stmt_loop: for statement in body.clone() {
                        let statement_result = self.run_statement(statement)?;
                        use StatementResult::*;
                        match statement_result {
                            r @ Return(_) => return Ok(r),
                            Break => break 'iter_loop,
                            Continue => break 'stmt_loop,
                            None => (),
                        }
                    }
                }
            }
            Return(r) => return Ok(StatementResult::Return(self.eval(r)?)),
            Break => return Ok(StatementResult::Break),
            Continue => return Ok(StatementResult::Continue),
            Pass => (),
            Del(_) => todo!(),
            Assert(_, _) => todo!(),
            Raise(_) => todo!(),
            Global(_) => todo!(),
            Nonlocal(_) => todo!(),
            Try { .. } => todo!(),
            Import(_) => todo!(),
            FromImport(_, _) => todo!(),
        }
        Ok(StatementResult::None)
    }

    fn run_statement(&mut self, statement: Stmt) -> Result<StatementResult> {
        let Stmt { stmt, line_number } = statement;
        let result = self
            .try_run_statement(stmt)
            .with_context(|| anyhow!("error on line {}", line_number))?;
        Ok(result)
    }

    fn write_single_assignment(
        assign: &mut Variable,
        mut identifier_data: Vector<EvaluatedIdentifierData>,
        new_value: Variable,
    ) -> Result<()> {
        if identifier_data.is_empty() {
            *assign = new_value;
            return Ok(());
        }
        let next_data = identifier_data.pop_front().unwrap();
        use EvaluatedIdentifierData::*;
        match next_data {
            Index(variable) => {
                macro_rules! assign_index {
                    ($list:expr) => {
                        let index_no = get_positive_index(variable, $list.len())?;
                        let assign = $list
                            .get_mut(index_no)
                            .ok_or(anyhow!("index out of range"))?;
                        Self::write_single_assignment(assign, identifier_data, new_value)?;
                    };
                }

                match assign {
                    Variable::Tuple(list) => {
                        assign_index!(list);
                    }
                    Variable::List(l) => {
                        let mut list = l.borrow_mut();
                        assign_index!(list);
                    }
                    Variable::Dict(d) => {
                        let mut d = d.borrow_mut();
                        let assign = d
                            .get_mut(&to_hashable_variable(variable)?)
                            .ok_or(anyhow!("index out of range"))?;
                        Self::write_single_assignment(assign, identifier_data, new_value)?;
                    }
                    _ => bail!("type error"),
                }
            }
            Slice(s) => {
                if let Variable::List(v) = assign {
                    todo!()
                } else {
                    bail!("type error");
                }
            }
            Attribute(..) => {
                todo!()
            }
        }
        Ok(())
    }

    fn evaluate_identifier_data(
        &mut self,
        data: Vector<IdentifierData>,
    ) -> Result<Vector<EvaluatedIdentifierData>> {
        let evaluate = |data| {
            use EvaluatedIdentifierData::*;
            let result = match data {
                IdentifierData::Index(e) => self.eval(e).map(Index)?,
                IdentifierData::Attribute(s) => Attribute(s),
                IdentifierData::Slice(s) => {
                    let items: Vec<_> = s
                        .into_iter()
                        .map(|e| e.map(|e| self.eval(e)).unwrap_or(Ok(Variable::None)))
                        .try_collect()?;
                    Slice(items.try_into().unwrap())
                }
            };
            Ok(result)
        };
        data.into_iter().map(evaluate).try_collect()
    }

    fn write_assignment(&mut self, assign: AssignValue, new_value: Variable) -> Result<()> {
        use AssignValue::*;
        match assign {
            Identifier(indentifer, identifier_data) => {
                let identifier_data = self.evaluate_identifier_data(identifier_data)?;

                let namespace = self.namespace.iter_mut().last().unwrap();
                let assign = namespace.entry(indentifer).or_insert(Variable::None);
                Self::write_single_assignment(assign, identifier_data, new_value)?;
            }
            List(v) => match new_value {
                Variable::List(l) => {
                    let l = l.borrow();
                    ensure!(l.len() == v.len(), "length mismatch");
                    for (a, b) in izip!(v, l.iter().cloned()) {
                        self.write_assignment(a, b)?;
                    }
                }
                Variable::Tuple(t) => {
                    ensure!(t.len() == v.len(), "length mismatch");
                    for (a, b) in izip!(v, t) {
                        self.write_assignment(a, b)?;
                    }
                }
                _ => bail!("type error"),
            },
        };
        Ok(())
    }

    fn read_assignment(&mut self, assign: AssignValue) -> Result<Variable> {
        self.eval(to_expr(assign))
    }

    fn eval(&mut self, expr: Expr) -> Result<Variable> {
        use crate::lexer3::Literal as L;
        use Expr::*;
        let result = match expr {
            Literal(literal) => match literal {
                L::Int(i) => Variable::Int(i),
                L::Float(f) => Variable::Float(f),
                L::String(s) => Variable::String(s),
                L::Bool(b) => Variable::Bool(b),
                L::None => Variable::None,
            },
            BinaryOp(left, operator, right) => {
                let l = format!("{:?}", left);
                let r = format!("{:?}", right);
                eval_binary_operator(&operator, &self.eval(*left)?, &self.eval(*right)?)
                    .with_context(|| {
                        anyhow!("failed to perform operation '{operator}' on operands {l} and {r}")
                    })?
            }
            UnaryOp(operator, right) => eval_unary_operator(operator, self.eval(*right)?)?,
            Identifier(s) => self.get_identifier(s)?,
            Tuple(list) => Variable::Tuple(list.into_iter().map(|e| self.eval(e)).try_collect()?),
            List(list) => Variable::List(Rc::new(RefCell::new(
                list.into_iter().map(|e| self.eval(e)).try_collect()?,
            ))),
            Dict(list) => {
                let convert_pair = |pair| {
                    let (k, v) = pair;
                    Ok((to_hashable_variable(self.eval(k)?)?, self.eval(v)?))
                };
                Variable::Dict(Rc::new(RefCell::new(
                    list.into_iter().map(convert_pair).try_collect()?,
                )))
            }
            FunctionCall(function, args) => {
                let args = args.into_iter().map(|e| self.eval(e)).try_collect()?;
                let function = self.eval(*function)?;
                let function = get_function_value(function)?;
                let error_message =
                    anyhow!("failed to run function {:?} on args {:?}", function, args);
                self.eval_function(function, args).context(error_message)?
            }
            Index(expr, index) => {
                let expr = self.eval(*expr)?;
                let index = self.eval(*index)?;
                index_variable(expr, index)?
            }
            MethodCall(variable, method_name, args) => {
                let args = args.into_iter().map(|e| self.eval(e)).try_collect()?;
                let result = match self.eval(*variable)? {
                    Variable::List(l) => match method_name.as_str() {
                        "append" => list_methods::append(l, args)?,
                        "pop" => list_methods::pop(l, args)?,
                        "remove" => list_methods::remove(l, args)?,
                        "reverse" => list_methods::reverse(l, args)?,
                        "sort" => list_methods::sort(l, args)?,
                        "index" => list_methods::index(l, args)?,
                        "count" => list_methods::count(l, args)?,
                        "clear" => list_methods::clear(l, args)?,
                        "copy" => list_methods::copy(l, args)?,
                        "extend" => list_methods::extend(l, args, self)?,
                        _ => bail!("unknown method {}", method_name),
                    },
                    Variable::Dict(d) => match method_name.as_str() {
                        "clear" => dict_methods::clear(d, args)?,
                        "copy" => dict_methods::copy(d, args)?,
                        "get" => dict_methods::get(d, args)?,
                        "items" => dict_methods::items(d, args)?,
                        "keys" => dict_methods::keys(d, args)?,
                        "pop" => dict_methods::pop(d, args)?,
                        "popitem" => dict_methods::pop_item(d, args)?,
                        "setdefault" => dict_methods::set_default(d, args)?,
                        "update" => dict_methods::update(d, args, self)?,
                        "values" => dict_methods::values(d, args)?,
                        _ => bail!("unknown method {}", method_name),
                    },
                    _ => todo!(),
                };
                result
            }
            Slice(expr, slice) => {
                todo!()
            }
            Lambda(parameters, expr) => {
                let fn_ = FunctionType::Lambda(parameters, *expr);
                Variable::Function("lambda".to_string(), fn_)
            },
            Attribute(..) => todo!(),
            ListComprehension(_, _, _) => todo!(),
            DictComprehension(_, _, _) => todo!(),
        };
        Ok(result)
    }

    fn eval_function(
        &mut self,
        function: FunctionType,
        args: Vector<Variable>,
    ) -> Result<Variable> {
        Ok(match function {
            FunctionType::Standard(f) => f(args)?,
            FunctionType::Standard2(f) => f(args, self)?,
            FunctionType::UserDefined(parameters, body) => {
                self.execute_function(parameters, args, body, Self::run_user_defined_function)?
            }
            FunctionType::Lambda(parameters, expr) => {
                self.execute_function(parameters, args, expr, Self::eval)?
            }
        })
    }

    fn execute_function<F, C>(
        &mut self,
        parameters: Vector<String>,
        args: Vector<Variable>,
        code: C,
        f: F,
    ) -> Result<Variable>
    where
        F: Fn(&mut Interpreter, C) -> Result<Variable>,
    {
        let mut namespace = HashMap::new();
        for (parameter, value) in parameters.into_iter().zip(args) {
            namespace.insert(parameter, value);
        }
        self.namespace.push_back(namespace);
        let result = f(self, code)?;
        self.namespace.pop_back();
        Ok(result)
    }

    fn run_user_defined_function(&mut self, code: Vector<Stmt>) -> Result<Variable> {
        let result = self.run_statements(code)?;
        let result = match result {
            StatementResult::Return(v) => v,
            StatementResult::None => Variable::None,
            _ => bail!("unexpected function result"),
        };
        Ok(result)
    }

    fn get_identifier(&mut self, s: String) -> Result<Variable> {
        let variable = self.namespace.iter().rev().find_map(|n| n.get(&s));
        let result = match variable {
            Some(v) => v.clone(),
            None => bail!("name '{}' is not defined", s),
        };
        Ok(result)
    }

    pub fn next_item(&mut self, iter: &mut VariableIterator) -> Result<Option<Variable>> {
        match iter {
            VariableIterator::Indexable(i) => {
                if i.start == i.end {
                    return Ok(None);
                }
                let next = i.iter.get_item(i.start)?;
                i.start += 1;
                Ok(next)
            }
            VariableIterator::Reversed(i) => {
                if i.start == i.end {
                    return Ok(None);
                }
                let next = i.iter.get_item(i.end)?;
                i.end -= 1;
                Ok(next)
            }
            VariableIterator::Map(f, iter) => {
                let next = self.next_item(iter)?;
                let next = match next {
                    Some(v) => v,
                    None => return Ok(None),
                };
                let args = vector![next];
                let result = self.eval_function(f.clone(), args)?;
                Ok(Some(result))
            }
            VariableIterator::Filter(f, iter) => {
                while let Some(next) = self.next_item(iter)? {
                    let args = vector![next.clone()];
                    let filter = self.eval_function(f.clone(), args)?;
                    if get_bool_value(&filter)? {
                        return Ok(Some(next));
                    }
                }
                Ok(None)
            }
            VariableIterator::Zip(v) => {
                let result: Vector<_> = v.iter_mut().map(|i| self.next_item(i)).try_collect()?;
                if result.iter().any(|i| i.is_none()) {
                    return Ok(None);
                }
                let result: Vector<_> = result.into_iter().map(|i| i.unwrap()).collect();
                Ok(Some(Variable::Tuple(result)))
            }
            VariableIterator::Enumerate(index, iter) => {
                let next = self.next_item(iter)?;
                let next = match next {
                    Some(v) => v,
                    None => return Ok(None),
                };
                let result = Variable::Tuple(vector![(*index).into(), next]);
                *index += 1;
                Ok(Some(result))
            }
        }
    }
}

fn index_variable(expr: Variable, index: Variable) -> Result<Variable> {
    let result = match expr {
        Variable::Tuple(v) => {
            let index_no = get_positive_index(index, v.len())?;
            v.get(index_no)
                .cloned()
                .ok_or(anyhow!("index out of range"))?
        }
        Variable::List(l) => {
            let l = l.borrow();
            let index_no = get_positive_index(index, l.len())?;
            l.get(index_no)
                .cloned()
                .ok_or(anyhow!("index out of range"))?
        }
        Variable::Dict(d) => {
            let d = d.borrow();
            let index = to_hashable_variable(index)?;
            d.get(&index)
                .cloned()
                .ok_or(anyhow!("key not in dictionary"))?
        }
        _ => bail!("type error"),
    };
    Ok(result)
}

fn to_expr(assign: AssignValue) -> Expr {
    use AssignValue::*;
    match assign {
        Identifier(s, v) => {
            let mut expr = Expr::Identifier(s);
            for i in v {
                use IdentifierData::*;
                match i {
                    Index(e) => expr = Expr::Index(Box::new(expr), Box::new(e)),
                    Attribute(s) => expr = Expr::Attribute(Box::new(expr), s),
                    Slice(s) => {
                        expr = Expr::Slice(Box::new(expr), s.map(|a| a.map(|a| Box::new(a))))
                    }
                }
            }
            expr
        }
        List(v) => Expr::Tuple(v.into_iter().map(to_expr).collect()),
    }
}

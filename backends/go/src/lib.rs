#![allow(clippy::too_many_arguments, clippy::type_complexity)]

use itertools::Itertools;
use std::{
    borrow::Cow,
    cell::RefCell,
    collections::VecDeque,
    io::{self, Write},
};
use wipple_frontend::ir;

#[derive(Debug, Clone)]
enum StackItem {
    Temporary(usize),
    Var(usize),
    Ident(Cow<'static, str>),
}

type Stack<'a> = Vec<(&'a ir::Type, StackItem)>;

pub struct Codegen<'a> {
    program: &'a ir::Program,
    temporaries: RefCell<Vec<&'a ir::Type>>,
}

impl<'a> Codegen<'a> {
    pub fn new(program: &'a ir::Program) -> Self {
        Codegen {
            program,
            temporaries: Default::default(),
        }
    }
}

impl<'a> Codegen<'a> {
    pub fn write_to(mut self, mut w: impl io::Write) -> io::Result<()> {
        w.write_all(include_bytes!("./runtime.go"))?;

        writeln!(w, "func main() {{")?;
        self.write_label(&mut w, "entrypoint", self.program.entrypoint)?;
        writeln!(w, "()")?;
        writeln!(w, "}}")?;

        for (index, (kind, vars, blocks)) in self.program.labels.iter().enumerate() {
            let mut stack = Stack::new();

            let label = match kind {
                ir::LabelKind::Entrypoint(_) => "entrypoint",
                ir::LabelKind::Constant(_) => "constant",
                ir::LabelKind::Function(_, _) => "function",
                ir::LabelKind::Closure(_, _, _) => {
                    continue; // closures are declared inline
                }
            };

            if let ir::LabelKind::Constant(ty) = kind {
                write!(w, "var ")?;
                self.write_thunk(&mut w, index)?;
                write!(w, " *")?;

                if self.program.wrapped_entrypoint == Some(index) {
                    self.write_type(
                        &mut w,
                        &ir::Type::FunctionReference(
                            Box::new(ir::Type::Tuple(Vec::new())),
                            Box::new(ty.clone()),
                        ),
                    )?;
                } else {
                    self.write_type(&mut w, ty)?;
                }

                writeln!(w, " = nil")?;
            }

            write!(w, "func ")?;
            self.write_label(&mut w, label, index)?;
            write!(w, "(")?;

            if let ir::LabelKind::Function(input, _) = kind {
                write!(w, "__wpl_function_input ")?;
                self.write_type(&mut w, input)?;
                stack.push((input, StackItem::Ident("__wpl_function_input".into())));
            }

            write!(w, ") ")?;

            match kind {
                ir::LabelKind::Constant(ty) if self.program.wrapped_entrypoint == Some(index) => {
                    self.write_type(
                        &mut w,
                        &ir::Type::FunctionReference(
                            Box::new(ir::Type::Tuple(Vec::new())),
                            Box::new(ty.clone()),
                        ),
                    )?;

                    write!(w, " ")?;
                }
                ir::LabelKind::Entrypoint(ty)
                | ir::LabelKind::Constant(ty)
                | ir::LabelKind::Function(_, ty) => {
                    self.write_type(&mut w, ty)?;
                    write!(w, " ")?;
                }
                _ => {}
            }

            writeln!(w, "{{")?;

            if let ir::LabelKind::Constant(ty) = kind {
                write!(w, "if ")?;
                self.write_thunk(&mut w, index)?;
                writeln!(w, " != nil {{")?;
                write!(w, "return *")?;
                self.write_thunk(&mut w, index)?;
                writeln!(w, "}}")?;
                write!(w, "__wpl_constant_value := func() ")?;
                if self.program.wrapped_entrypoint == Some(index) {
                    self.write_type(
                        &mut w,
                        &ir::Type::FunctionReference(
                            Box::new(ir::Type::Tuple(Vec::new())),
                            Box::new(ty.clone()),
                        ),
                    )?;
                } else {
                    self.write_type(&mut w, ty)?;
                }
                writeln!(w, " {{")?;
            }

            self.write_body(&mut w, vars, None, blocks, stack)?;

            if let ir::LabelKind::Constant(_) = kind {
                writeln!(w, "}}()")?;
                self.write_thunk(&mut w, index)?;
                writeln!(w, " = &__wpl_constant_value")?;
                writeln!(w, "return __wpl_constant_value")?
            }

            writeln!(w, "}}")?;
        }

        Ok(())
    }

    fn write_body(
        &mut self,
        w: &mut impl io::Write,
        vars: &'a [ir::Type],
        captures: Option<&'a ir::CaptureList>,
        blocks: &'a [ir::BasicBlock],
        stack: Stack<'a>,
    ) -> Result<(), io::Error> {
        let (captured, local): (Vec<_>, Vec<_>) =
            vars.iter().enumerate().partition_map(|(index, ty)| {
                use itertools::Either::*;

                match captures.and_then(|captures| {
                    captures
                        .0
                        .iter()
                        .find_map(|(&captured, &var)| (var == index).then_some(captured))
                }) {
                    Some(captured) => Left((index, captured)),
                    None => Right((index, ty)),
                }
            });

        for &(index, captured) in &captured {
            write!(w, "__wpl_capture_{index} := ")?;
            self.write_var(w, captured)?;
            writeln!(w)?;
        }

        for (index, _) in captured {
            self.write_var(w, index)?;
            writeln!(w, " := __wpl_capture_{index}")?;
        }

        for (index, ty) in local {
            write!(w, "var ")?;
            self.write_var(w, index)?;
            write!(w, " ")?;
            self.write_type(w, ty)?;
            writeln!(w)?;
        }

        let first_temporary_index = self.temporaries.borrow().len();

        let mut buf = Vec::new();

        write!(&mut buf, "goto ")?;
        self.write_block_label(&mut buf, 0)?;
        writeln!(&mut buf)?;

        let mut cache = vec![false; blocks.len()];

        let mut queue = VecDeque::new();
        queue.push_back((0, stack));

        while let Some((block, stack)) = queue.pop_front() {
            self.write_block(
                &mut buf,
                block,
                vars,
                blocks,
                stack,
                &mut cache,
                &mut queue,
                first_temporary_index,
            )?;
        }

        for (index, ty) in self.temporaries.borrow()[first_temporary_index..]
            .iter()
            .enumerate()
        {
            let index = index + first_temporary_index;

            write!(w, "var ")?;
            self.write_temporary(w, index)?;
            write!(w, " ")?;
            self.write_type(w, ty)?;
            writeln!(w)?;
        }

        w.write_all(&buf)?;

        Ok(())
    }

    fn write_block(
        &mut self,
        w: &mut impl io::Write,
        index: usize,
        vars: &'a [ir::Type],
        blocks: &'a [ir::BasicBlock],
        mut stack: Stack<'a>,
        cache: &mut [bool],
        queue: &mut VecDeque<(usize, Stack<'a>)>,
        first_temporary_index: usize,
    ) -> io::Result<()> {
        if cache[index] {
            return writeln!(w);
        }

        cache[index] = true;

        self.write_block_label(w, index)?;
        writeln!(w, ": // {}", blocks[index].description)?;

        for statement in &blocks[index].statements {
            self.write_statement(w, statement, &mut stack)?;
        }

        // HACK: Work around Go's errors for unused variables
        if let Some(terminator) = &blocks[index].terminator {
            for index in first_temporary_index..self.temporaries.borrow().len() {
                write!(w, "_ = ")?;
                self.write_temporary(w, index)?;
                writeln!(w)?;
            }

            for (var, _) in vars.iter().enumerate() {
                write!(w, "_ = ")?;
                self.write_var(w, var)?;
                writeln!(w)?;
            }

            self.write_terminator(w, terminator, stack, queue)?;
        }

        Ok(())
    }

    fn write_statement(
        &mut self,
        w: &mut impl io::Write,
        statement: &'a ir::Statement,
        stack: &mut Stack<'a>,
    ) -> io::Result<()> {
        match statement {
            ir::Statement::Copy => {
                let (ty, top) = stack.last().unwrap().clone();
                let next = self.next_temporary(ty);

                self.write_temporary(w, next)?;
                writeln!(w, " = {}", self.use_stack_item(top))?;

                stack.push((ty, StackItem::Temporary(next)));
            }
            ir::Statement::Drop => {
                let (_, top) = stack.pop().unwrap();
                writeln!(w, "_ = {}", self.use_stack_item(top))?;
            }
            ir::Statement::Initialize(var) => {
                let (_, top) = stack.pop().unwrap();
                self.write_var(w, *var)?;
                writeln!(w, " = {}", self.use_stack_item(top))?;
            }
            ir::Statement::Free(var) => {
                write!(w, "_ = ")?;
                self.write_var(w, *var)?;
                writeln!(w)?;
            }
            ir::Statement::WithContext(_) => todo!(),
            ir::Statement::ResetContext(_) => todo!(),
            ir::Statement::Unpack(_) => {
                // Closures are defined inline in Go, so we don't need to do
                // anything (see below)
            }
            ir::Statement::Phi(phi) => {
                let (ty, top) = stack.pop().unwrap();
                self.write_var(w, *phi)?;
                writeln!(w, " = {} // phi", self.use_stack_item(top))?;
                stack.push((ty, StackItem::Var(*phi)));
            }
            ir::Statement::Expression(ty, expr) => {
                let next = self.next_temporary(ty);

                self.write_temporary(w, next)?;
                write!(w, " = ")?;

                match expr {
                    ir::Expression::Marker => {
                        self.write_type(w, ty)?;
                        writeln!(w, "{{}}")?;
                    }
                    ir::Expression::Text(s) => {
                        writeln!(w, "{:?}", s)?;
                    }
                    ir::Expression::Number(n) => {
                        writeln!(w, "decimal.RequireFromString({:?})", n.to_string())?;
                    }
                    ir::Expression::Integer(n) => {
                        self.write_type(w, ty)?;
                        writeln!(w, "({n})")?;
                    }
                    ir::Expression::Natural(n) => {
                        self.write_type(w, ty)?;
                        writeln!(w, "({n})")?;
                    }
                    ir::Expression::Byte(n) => {
                        self.write_type(w, ty)?;
                        writeln!(w, "({n})")?;
                    }
                    ir::Expression::Signed(n) => {
                        self.write_type(w, ty)?;
                        writeln!(w, "({n})")?;
                    }
                    ir::Expression::Unsigned(n) => {
                        self.write_type(w, ty)?;
                        writeln!(w, "({n})")?;
                    }
                    ir::Expression::Float(n) => {
                        self.write_type(w, ty)?;
                        writeln!(w, "({n})")?;
                    }
                    ir::Expression::Double(n) => {
                        self.write_type(w, ty)?;
                        writeln!(w, "({n})")?;
                    }
                    ir::Expression::Variable(var) => {
                        self.write_var(w, *var)?;
                        writeln!(w)?;
                    }
                    ir::Expression::Constant(id) => {
                        self.write_label(w, "constant", *id)?;
                        writeln!(w, "()")?;
                    }
                    ir::Expression::Function(id) => {
                        self.write_label(w, "function", *id)?;
                        writeln!(w)?;
                    }
                    ir::Expression::Closure(captures, id) => {
                        // Closures are defined inline in Go

                        let (kind, vars, blocks) = &self.program.labels[*id];

                        let (input, output) = match kind {
                            ir::LabelKind::Closure(_, input, output) => (input, output),
                            _ => panic!("not a closure"),
                        };

                        write!(w, "func(__wpl_function_input ")?;
                        self.write_type(w, input)?;
                        write!(w, ") ")?;
                        self.write_type(w, output)?;
                        writeln!(w, " {{")?;

                        let stack = vec![(input, StackItem::Ident("__wpl_function_input".into()))];
                        self.write_body(w, vars, Some(captures), blocks, stack)?;

                        writeln!(w, "}}")?;
                    }
                    ir::Expression::Call => {
                        let (_, input) = stack.pop().unwrap();
                        let (_, func) = stack.pop().unwrap();

                        writeln!(
                            w,
                            "{}({})",
                            self.use_stack_item(func),
                            self.use_stack_item(input)
                        )?;
                    }
                    ir::Expression::External(_, _, _) => todo!(),
                    ir::Expression::Runtime(intrinsic, inputs) => {
                        let mut inputs = (0..*inputs)
                            .map(|_| stack.pop().unwrap())
                            .collect::<Vec<_>>();

                        inputs.reverse();

                        let id = intrinsic.to_string().replace('-', "_");

                        if self.write_instrinsic_represented_inline(w, *intrinsic, ty)? {
                            writeln!(w)?;
                        } else if self.intrinsic_returns_maybe_represented_as_pointer(*intrinsic) {
                            let payload_ty = match ty {
                                ir::Type::Enumeration(id) => {
                                    let variants = self.program.enumerations.get(id).unwrap();
                                    &variants[1][0]
                                }
                                _ => unreachable!(),
                            };

                            writeln!(w, "func() __wpl_type_enumeration {{")?;

                            write!(w, "maybe := __wpl_intrinsic_{id}(")?;
                            for (_, input) in inputs {
                                write!(w, "{},", self.use_stack_item(input))?;
                            }
                            writeln!(w, ")")?;

                            writeln!(w, "if maybe == nil {{")?;
                            writeln!(w, "return __wpl_type_enumeration{{0,struct{{}}{{}}}}")?;
                            writeln!(w, "}} else {{")?;
                            write!(
                                w,
                                "return __wpl_type_enumeration{{1,struct{{__wpl_variant_element_0 "
                            )?;
                            self.write_type(w, payload_ty)?;
                            writeln!(w, "}}{{*maybe}}}}")?;
                            writeln!(w, "}}")?;

                            writeln!(w, "}}()")?;
                        } else {
                            write!(w, "__wpl_intrinsic_{id}")?;

                            if self.intrinsic_returns_type_of_expr(*intrinsic) {
                                write!(w, "[")?;
                                self.write_type(w, ty)?;
                                write!(w, "]")?;
                            }

                            write!(w, "(")?;
                            for (_, input) in inputs {
                                write!(w, "{},", self.use_stack_item(input))?;
                            }
                            writeln!(w, ")")?;
                        }
                    }
                    ir::Expression::Tuple(elements) => {
                        let mut elements = (0..*elements)
                            .map(|_| stack.pop().unwrap())
                            .collect::<Vec<_>>();

                        elements.reverse();

                        self.write_type(w, ty)?;

                        write!(w, "{{")?;
                        for (_, element) in elements {
                            write!(w, "{},", self.use_stack_item(element))?;
                        }
                        writeln!(w, "}}")?;
                    }
                    ir::Expression::Format(segments, end) => {
                        let mut inputs = (0..segments.len())
                            .map(|_| stack.pop().unwrap())
                            .collect::<Vec<_>>();

                        inputs.reverse();

                        for (index, (segment, (_, input))) in
                            segments.iter().zip(inputs).enumerate()
                        {
                            if index > 0 {
                                write!(w, " + ")?;
                            }

                            write!(w, "{:?} + {}", segment, self.use_stack_item(input))?;
                        }

                        if let Some(end) = end {
                            if !segments.is_empty() {
                                write!(w, " + ")?;
                            }

                            write!(w, "{:?}", end)?;
                        }

                        writeln!(w)?;
                    }
                    ir::Expression::Structure(fields) => {
                        let mut fields = (0..*fields)
                            .map(|_| stack.pop().unwrap())
                            .collect::<Vec<_>>();

                        fields.reverse();

                        self.write_type(w, ty)?;

                        write!(w, "{{")?;
                        for (_, field) in fields {
                            write!(w, "{},", self.use_stack_item(field))?;
                        }
                        writeln!(w, "}}")?;
                    }
                    ir::Expression::Variant(index, elements) => {
                        let mut elements = (0..*elements)
                            .map(|_| stack.pop().unwrap())
                            .collect::<Vec<_>>();

                        elements.reverse();

                        self.write_type(w, ty)?;

                        write!(w, "{{{},", index.into_inner())?;

                        write!(w, "struct{{")?;
                        for (index, (ty, _)) in elements.iter().enumerate() {
                            write!(w, "__wpl_variant_element_{} ", index)?;
                            self.write_type(w, ty)?;
                            write!(w, ";")?;
                        }
                        write!(w, "}}")?;

                        write!(w, "{{")?;
                        for (_, element) in elements {
                            write!(w, "{},", self.use_stack_item(element))?;
                        }
                        write!(w, "}}")?;

                        writeln!(w, "}}")?;
                    }
                    ir::Expression::TupleElement(index) => {
                        let (_, tuple) = stack.pop().unwrap();

                        writeln!(
                            w,
                            "{}.__wpl_tuple_element_{}",
                            self.use_stack_item(tuple),
                            index
                        )?;
                    }
                    ir::Expression::StructureElement(index) => {
                        let (_, structure) = stack.pop().unwrap();

                        writeln!(
                            w,
                            "{}.__wpl_structure_element_{}",
                            self.use_stack_item(structure),
                            index.into_inner()
                        )?;
                    }
                    ir::Expression::VariantElement((_, elements), index) => {
                        let (_, variant) = stack.pop().unwrap();

                        write!(
                            w,
                            "{}.__wpl_enumeration_payload.(",
                            self.use_stack_item(variant)
                        )?;

                        write!(w, "struct{{")?;
                        for (index, ty) in elements.iter().enumerate() {
                            write!(w, "__wpl_variant_element_{} ", index)?;
                            self.write_type(w, ty)?;
                            write!(w, ";")?;
                        }
                        writeln!(w, "}}).__wpl_variant_element_{}", index)?;
                    }
                    ir::Expression::Reference => todo!(),
                    ir::Expression::Dereference => todo!(),
                    ir::Expression::Context(_) => todo!(),
                    ir::Expression::Extend(_) => todo!(),
                }

                stack.push((ty, StackItem::Temporary(next)));
            }
        }

        Ok(())
    }

    fn write_terminator(
        &mut self,
        w: &mut impl io::Write,
        terminator: &'a ir::Terminator,
        mut stack: Stack<'a>,
        queue: &mut VecDeque<(usize, Stack<'a>)>,
    ) -> io::Result<()> {
        match terminator {
            ir::Terminator::Unreachable => {
                writeln!(w, "panic(\"unreachable\")")?;
            }
            ir::Terminator::Return => {
                let (_, top) = stack.pop().unwrap();
                assert!(stack.is_empty());
                writeln!(w, "return {}", self.use_stack_item(top))?;
            }
            ir::Terminator::Jump(block) => {
                write!(w, "goto ")?;
                self.write_block_label(w, *block)?;
                writeln!(w)?;

                queue.push_back((*block, stack));
            }
            ir::Terminator::If(variant, then_block, else_block) => {
                let (_, input) = stack.pop().unwrap();

                writeln!(
                    w,
                    "if {}.__wpl_enumeration_discriminant == {} {{",
                    self.use_stack_item(input),
                    variant.into_inner()
                )?;

                write!(w, "goto ")?;
                self.write_block_label(w, *then_block)?;
                writeln!(w)?;

                writeln!(w, "}} else {{")?;

                write!(w, "goto ")?;
                self.write_block_label(w, *else_block)?;
                writeln!(w)?;

                writeln!(w, "}}")?;

                queue.push_back((*then_block, stack.clone()));
                queue.push_back((*else_block, stack));
            }
            ir::Terminator::TailCall => {
                let (_, input) = stack.pop().unwrap();
                let (_, func) = stack.pop().unwrap();

                // Go performs tail call optimization automatically
                writeln!(
                    w,
                    "return {}({})",
                    self.use_stack_item(func),
                    self.use_stack_item(input)
                )?;
            }
        }

        Ok(())
    }

    fn write_label(&self, w: &mut impl io::Write, kind: &str, n: usize) -> io::Result<()> {
        write!(w, "__wpl_{kind}_{n}")
    }

    fn write_block_label(&self, w: &mut impl io::Write, n: usize) -> io::Result<()> {
        write!(w, "__wpl_block_{n}")
    }

    fn write_var(&self, w: &mut impl io::Write, n: usize) -> io::Result<()> {
        write!(w, "__wpl_var_{n}")
    }

    fn write_thunk(&self, w: &mut impl io::Write, n: usize) -> io::Result<()> {
        write!(w, "__wpl_constant_{n}_thunk")
    }

    fn write_temporary(&self, w: &mut impl io::Write, n: usize) -> io::Result<()> {
        write!(w, "__wpl_temp_{n}")
    }

    fn next_temporary(&self, ty: &'a ir::Type) -> usize {
        let mut temporaries = self.temporaries.borrow_mut();
        let n = temporaries.len();
        temporaries.push(ty);
        n
    }

    // This is its own function so that we can track whether stack items are
    // used, if needed in the future
    fn use_stack_item(&self, item: StackItem) -> Cow<'static, str> {
        match item {
            StackItem::Temporary(n) => format!("__wpl_temp_{n}").into(),
            StackItem::Var(n) => format!("__wpl_var_{n}").into(),
            StackItem::Ident(identifier) => identifier,
        }
    }

    fn write_type(&self, w: &mut impl io::Write, ty: &ir::Type) -> io::Result<()> {
        match ty {
            ir::Type::Marker => write!(w, "struct{{}}")?,
            ir::Type::Number => write!(w, "__wpl_type_number")?,
            ir::Type::Integer => write!(w, "int64")?,
            ir::Type::Natural => write!(w, "uint64")?,
            ir::Type::Byte => write!(w, "uint8")?,
            ir::Type::Signed => write!(w, "int")?,
            ir::Type::Unsigned => write!(w, "uint")?,
            ir::Type::Float => write!(w, "float32")?,
            ir::Type::Double => write!(w, "float64")?,
            ir::Type::Ui => write!(w, "__wpl_type_ui")?,
            ir::Type::TaskGroup => write!(w, "__wpl_type_taskgroup")?,
            ir::Type::Tuple(elements) => {
                write!(w, "struct{{")?;

                for (index, ty) in elements.iter().enumerate() {
                    write!(w, "__wpl_tuple_element_{} ", index)?;
                    self.write_type(w, ty)?;
                    write!(w, ";")?;
                }

                write!(w, "}}")?;
            }
            ir::Type::Structure(id) => {
                let elements = self.program.structures.get(id).unwrap();

                write!(w, "struct{{")?;

                for (index, ty) in elements.iter().enumerate() {
                    write!(w, "__wpl_structure_element_{} ", index)?;
                    self.write_type(w, ty)?;
                    write!(w, ";")?;
                }

                write!(w, "}}")?;
            }
            ir::Type::Enumeration(_) => {
                // FIXME: Store payload on the stack
                write!(w, "__wpl_type_enumeration")?;
            }
            ir::Type::TextReference => {
                write!(w, "string")?;
            }
            ir::Type::ListReference(ty) => {
                write!(w, "[]")?;
                self.write_type(w, ty)?;
            }
            ir::Type::MutableReference(ty) => {
                write!(w, "*")?;
                self.write_type(w, ty)?;
            }
            ir::Type::FunctionReference(input, output) => {
                write!(w, "func(")?;
                self.write_type(w, input)?;
                write!(w, ") ")?;
                self.write_type(w, output)?;
            }
            ir::Type::StructureReference(id) => {
                write!(w, "*")?;
                self.write_type(w, &ir::Type::Structure(*id))?;
            }
            ir::Type::EnumerationReference(id) => {
                write!(w, "*")?;
                self.write_type(w, &ir::Type::Enumeration(*id))?;
            }
        }

        Ok(())
    }

    fn write_instrinsic_represented_inline(
        &self,
        w: &mut impl io::Write,
        intrinsic: ir::Intrinsic,
        ty: &ir::Type,
    ) -> io::Result<bool> {
        match intrinsic {
            ir::Intrinsic::MakeEmptyList => {
                let element_ty = match ty {
                    ir::Type::ListReference(element_ty) => element_ty.as_ref(),
                    _ => unreachable!(),
                };

                write!(w, "[]")?;
                self.write_type(w, element_ty)?;
                write!(w, "{{}}")?;

                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn intrinsic_returns_type_of_expr(&self, intrinsic: ir::Intrinsic) -> bool {
        matches!(intrinsic, ir::Intrinsic::Crash | ir::Intrinsic::Prompt)
    }

    fn intrinsic_returns_maybe_represented_as_pointer(&self, intrinsic: ir::Intrinsic) -> bool {
        match intrinsic {
            ir::Intrinsic::ListFirst
            | ir::Intrinsic::ListLast
            | ir::Intrinsic::ListInitial
            | ir::Intrinsic::ListTail
            | ir::Intrinsic::ListNth
            | ir::Intrinsic::TextToNumber
            | ir::Intrinsic::TextToInteger
            | ir::Intrinsic::TextToNatural
            | ir::Intrinsic::TextToByte
            | ir::Intrinsic::TextToSigned
            | ir::Intrinsic::TextToUnsigned
            | ir::Intrinsic::TextToFloat
            | ir::Intrinsic::TextToDouble => true,
            ir::Intrinsic::Crash
            | ir::Intrinsic::Display
            | ir::Intrinsic::Prompt
            | ir::Intrinsic::Choice
            | ir::Intrinsic::WithUi
            | ir::Intrinsic::MessageUi
            | ir::Intrinsic::WithContinuation
            | ir::Intrinsic::WithTaskGroup
            | ir::Intrinsic::Task
            | ir::Intrinsic::InBackground
            | ir::Intrinsic::Delay
            | ir::Intrinsic::NumberToText
            | ir::Intrinsic::IntegerToText
            | ir::Intrinsic::NaturalToText
            | ir::Intrinsic::ByteToText
            | ir::Intrinsic::SignedToText
            | ir::Intrinsic::UnsignedToText
            | ir::Intrinsic::FloatToText
            | ir::Intrinsic::DoubleToText
            | ir::Intrinsic::NaturalToNumber
            | ir::Intrinsic::NumberToNatural
            | ir::Intrinsic::NaturalToInteger
            | ir::Intrinsic::IntegerToNatural
            | ir::Intrinsic::AddNumber
            | ir::Intrinsic::SubtractNumber
            | ir::Intrinsic::MultiplyNumber
            | ir::Intrinsic::DivideNumber
            | ir::Intrinsic::ModuloNumber
            | ir::Intrinsic::PowerNumber
            | ir::Intrinsic::FloorNumber
            | ir::Intrinsic::CeilNumber
            | ir::Intrinsic::SqrtNumber
            | ir::Intrinsic::NegateNumber
            | ir::Intrinsic::AddInteger
            | ir::Intrinsic::SubtractInteger
            | ir::Intrinsic::MultiplyInteger
            | ir::Intrinsic::DivideInteger
            | ir::Intrinsic::ModuloInteger
            | ir::Intrinsic::PowerInteger
            | ir::Intrinsic::NegateInteger
            | ir::Intrinsic::AddNatural
            | ir::Intrinsic::SubtractNatural
            | ir::Intrinsic::MultiplyNatural
            | ir::Intrinsic::DivideNatural
            | ir::Intrinsic::ModuloNatural
            | ir::Intrinsic::PowerNatural
            | ir::Intrinsic::AddByte
            | ir::Intrinsic::SubtractByte
            | ir::Intrinsic::MultiplyByte
            | ir::Intrinsic::DivideByte
            | ir::Intrinsic::ModuloByte
            | ir::Intrinsic::PowerByte
            | ir::Intrinsic::AddSigned
            | ir::Intrinsic::SubtractSigned
            | ir::Intrinsic::MultiplySigned
            | ir::Intrinsic::DivideSigned
            | ir::Intrinsic::ModuloSigned
            | ir::Intrinsic::PowerSigned
            | ir::Intrinsic::NegateSigned
            | ir::Intrinsic::AddUnsigned
            | ir::Intrinsic::SubtractUnsigned
            | ir::Intrinsic::MultiplyUnsigned
            | ir::Intrinsic::DivideUnsigned
            | ir::Intrinsic::ModuloUnsigned
            | ir::Intrinsic::PowerUnsigned
            | ir::Intrinsic::AddFloat
            | ir::Intrinsic::SubtractFloat
            | ir::Intrinsic::MultiplyFloat
            | ir::Intrinsic::DivideFloat
            | ir::Intrinsic::ModuloFloat
            | ir::Intrinsic::PowerFloat
            | ir::Intrinsic::FloorFloat
            | ir::Intrinsic::CeilFloat
            | ir::Intrinsic::SqrtFloat
            | ir::Intrinsic::NegateFloat
            | ir::Intrinsic::AddDouble
            | ir::Intrinsic::SubtractDouble
            | ir::Intrinsic::MultiplyDouble
            | ir::Intrinsic::DivideDouble
            | ir::Intrinsic::ModuloDouble
            | ir::Intrinsic::PowerDouble
            | ir::Intrinsic::FloorDouble
            | ir::Intrinsic::CeilDouble
            | ir::Intrinsic::SqrtDouble
            | ir::Intrinsic::NegateDouble
            | ir::Intrinsic::TextEquality
            | ir::Intrinsic::NumberEquality
            | ir::Intrinsic::IntegerEquality
            | ir::Intrinsic::NaturalEquality
            | ir::Intrinsic::ByteEquality
            | ir::Intrinsic::SignedEquality
            | ir::Intrinsic::UnsignedEquality
            | ir::Intrinsic::FloatEquality
            | ir::Intrinsic::DoubleEquality
            | ir::Intrinsic::TextOrdering
            | ir::Intrinsic::NumberOrdering
            | ir::Intrinsic::IntegerOrdering
            | ir::Intrinsic::NaturalOrdering
            | ir::Intrinsic::ByteOrdering
            | ir::Intrinsic::SignedOrdering
            | ir::Intrinsic::UnsignedOrdering
            | ir::Intrinsic::FloatOrdering
            | ir::Intrinsic::DoubleOrdering
            | ir::Intrinsic::MakeReference
            | ir::Intrinsic::GetReference
            | ir::Intrinsic::SetReference
            | ir::Intrinsic::MakeEmptyList
            | ir::Intrinsic::TextCharacters
            | ir::Intrinsic::RandomNumber
            | ir::Intrinsic::RandomInteger
            | ir::Intrinsic::RandomNatural
            | ir::Intrinsic::RandomByte
            | ir::Intrinsic::RandomSigned
            | ir::Intrinsic::RandomUnsigned
            | ir::Intrinsic::RandomFloat
            | ir::Intrinsic::RandomDouble
            | ir::Intrinsic::ListAppend
            | ir::Intrinsic::ListPrepend
            | ir::Intrinsic::ListInsertAt
            | ir::Intrinsic::ListRemoveAt
            | ir::Intrinsic::ListCount
            | ir::Intrinsic::ListSlice
            | ir::Intrinsic::UndefinedNumber
            | ir::Intrinsic::NumberIsUndefined => false,
        }
    }
}

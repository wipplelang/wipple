//! Parse a token tree into an abstract syntax tree.

use crate::{
    tokenize::{self, Keyword, NonAssociativeOperator, Operator, TokenTree, VariadicOperator},
    BinaryOperator, Driver, Location,
};
use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::{fmt::Debug, hash::Hash, rc::Rc};
use wipple_util::{DefaultFromInfo, WithInfo};

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug, T: Debug"),
    Clone(bound = "T: Clone"),
    PartialEq(bound = "D::Info: PartialEq, T: PartialEq"),
    Eq(bound = "D::Info: Eq, T: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize<'de>"))]
pub(crate) struct Attributed<D: Driver, T> {
    pub(crate) attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
    pub(crate) value: WithInfo<D::Info, T>,
}

impl<D: Driver, T> DefaultFromInfo<D::Info> for Attributed<D, T>
where
    T: DefaultFromInfo<D::Info>,
{
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: Attributed {
                attributes: Vec::new(),
                value: T::default_from_info(info),
            },
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) enum Attribute<D: Driver> {
    Error,
    Name(WithInfo<D::Info, String>),
    Valued {
        name: WithInfo<D::Info, String>,
        value: WithInfo<D::Info, AttributeValue<D>>,
    },
}

impl<D: Driver> DefaultFromInfo<D::Info> for Attribute<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Attribute::Error,
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase", tag = "type", content = "value")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) enum AttributeValue<D: Driver> {
    Error,
    Name(WithInfo<D::Info, String>),
    Number(WithInfo<D::Info, String>),
    Text(WithInfo<D::Info, String>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for AttributeValue<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: AttributeValue::Error,
        }
    }
}

#[allow(missing_docs)]
#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub struct TopLevel<D: Driver> {
    pub(crate) statements: Vec<WithInfo<D::Info, Statement<D>>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for TopLevel<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: TopLevel {
                statements: Vec::new(),
            },
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) enum Statement<D: Driver> {
    Error,
    #[serde(rename_all = "camelCase")]
    TypeDeclaration {
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
        name: WithInfo<D::Info, Option<String>>,
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        representation: WithInfo<D::Info, TypeRepresentation<D>>,
    },
    #[serde(rename_all = "camelCase")]
    TypeAliasDeclaration {
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
        name: WithInfo<D::Info, Option<String>>,
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        r#type: WithInfo<D::Info, Type<D>>,
    },
    #[serde(rename_all = "camelCase")]
    TraitDeclaration {
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
        name: WithInfo<D::Info, Option<String>>,
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        r#type: Option<WithInfo<D::Info, Type<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    DefaultInstanceDeclaration {
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        instance: WithInfo<D::Info, Option<Instance<D>>>,
        body: Option<WithInfo<D::Info, Expression<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    InstanceDeclaration {
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        instance: WithInfo<D::Info, Option<Instance<D>>>,
        body: Option<WithInfo<D::Info, Expression<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    ConstantDeclaration {
        attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
        name: WithInfo<D::Info, Option<String>>,
        parameters: WithInfo<D::Info, TypeFunction<D>>,
        r#type: WithInfo<D::Info, Type<D>>,
    },
    #[serde(rename_all = "camelCase")]
    Assignment {
        pattern: WithInfo<D::Info, Pattern<D>>,
        value: WithInfo<D::Info, Expression<D>>,
    },
    Expression(WithInfo<D::Info, Expression<D>>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for Statement<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Statement::Error,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
#[strum(serialize_all = "kebab-case")]
pub(crate) enum LanguageDeclarationKind {
    Type,
    Trait,
    Constant,
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) struct TypeFunction<D: Driver> {
    pub(crate) parameters: Vec<WithInfo<D::Info, TypeParameter<D>>>,
    pub(crate) bounds: Vec<WithInfo<D::Info, Instance<D>>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeFunction<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: TypeFunction {
                parameters: Vec::new(),
                bounds: Vec::new(),
            },
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) struct TypeParameter<D: Driver> {
    pub(crate) name: WithInfo<D::Info, Option<String>>,
    pub(crate) infer: Option<WithInfo<D::Info, ()>>,
    pub(crate) default: Option<WithInfo<D::Info, Type<D>>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeParameter<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: TypeParameter {
                name: Option::default_from_info(info),
                infer: None,
                default: None,
            },
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) struct Instance<D: Driver> {
    pub(crate) r#trait: WithInfo<D::Info, Option<String>>,
    pub(crate) parameters: Vec<WithInfo<D::Info, Type<D>>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for Instance<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: Instance {
                r#trait: Option::default_from_info(info),
                parameters: Vec::new(),
            },
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) enum TypeRepresentation<D: Driver> {
    Marker,
    Compound(Vec<WithInfo<D::Info, TypeMember<D>>>),
    Wrapper(WithInfo<D::Info, Type<D>>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeRepresentation<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: TypeRepresentation::Marker,
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) struct TypeMember<D: Driver> {
    pub(crate) attributes: Vec<WithInfo<D::Info, Attribute<D>>>,
    pub(crate) name: WithInfo<D::Info, Option<String>>,
    pub(crate) kind: TypeMemberKind<D>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeMember<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: TypeMember {
                attributes: Vec::new(),
                name: Option::default_from_info(info),
                kind: TypeMemberKind::Error,
            },
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) enum TypeMemberKind<D: Driver> {
    Error,
    Field(WithInfo<D::Info, Type<D>>),
    Variant(Vec<WithInfo<D::Info, Type<D>>>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for TypeMemberKind<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: TypeMemberKind::Error,
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) enum Expression<D: Driver> {
    Error,
    #[serde(rename_all = "camelCase")]
    Annotate {
        value: WithInfo<D::Info, Box<Expression<D>>>,
        r#type: WithInfo<D::Info, Type<D>>,
    },
    Name(String),
    Number(String),
    Text(String),
    Unit,
    Block(Vec<WithInfo<D::Info, Statement<D>>>),
    Do(WithInfo<D::Info, Box<Expression<D>>>),
    #[serde(rename_all = "camelCase")]
    Function {
        inputs: Vec<WithInfo<D::Info, Pattern<D>>>,
        body: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Call {
        function: WithInfo<D::Info, Box<Expression<D>>>,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Apply {
        input: WithInfo<D::Info, Box<Expression<D>>>,
        function: WithInfo<D::Info, Box<Expression<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    BinaryOperator {
        operator: WithInfo<D::Info, BinaryOperator>,
        left: WithInfo<D::Info, Box<Expression<D>>>,
        right: WithInfo<D::Info, Box<Expression<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    As {
        value: WithInfo<D::Info, Box<Expression<D>>>,
        r#type: WithInfo<D::Info, Type<D>>,
    },
    #[serde(rename_all = "camelCase")]
    Is {
        value: WithInfo<D::Info, Box<Expression<D>>>,
        pattern: WithInfo<D::Info, Pattern<D>>,
    },
    #[serde(rename_all = "camelCase")]
    When {
        input: WithInfo<D::Info, Box<Expression<D>>>,
        arms: Vec<WithInfo<D::Info, Arm<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    Intrinsic {
        name: WithInfo<D::Info, Option<String>>,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Tuple(Vec<WithInfo<D::Info, Expression<D>>>),
    Collection(Vec<WithInfo<D::Info, Expression<D>>>),
    Structure(Vec<WithInfo<D::Info, FieldValue<D>>>),
}

impl<D: Driver> DefaultFromInfo<D::Info> for Expression<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Expression::Error,
        }
    }
}

#[allow(missing_docs)]
#[derive(Serialize, Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub enum Type<D: Driver> {
    Error,
    Placeholder,
    Unit,
    #[serde(rename_all = "camelCase")]
    Declared {
        name: WithInfo<D::Info, Option<String>>,
        parameters: Vec<WithInfo<D::Info, Type<D>>>,
    },
    #[serde(rename_all = "camelCase")]
    Function {
        inputs: Vec<WithInfo<D::Info, Type<D>>>,
        output: WithInfo<D::Info, Box<Type<D>>>,
    },
    Tuple(Vec<WithInfo<D::Info, Type<D>>>),
    Block(WithInfo<D::Info, Box<Type<D>>>),
    Intrinsic,
    Message {
        message: WithInfo<D::Info, String>,
        inputs: Vec<WithInfo<D::Info, Type<D>>>,
    },
}

impl<D: Driver> DefaultFromInfo<D::Info> for Type<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Type::Error,
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) enum Pattern<D: Driver> {
    Error,
    Wildcard,
    Unit,
    Number(String),
    Text(String),
    Name(String),
    VariantOrName(WithInfo<D::Info, Option<String>>),
    Destructure(Vec<WithInfo<D::Info, FieldPattern<D>>>),
    #[serde(rename_all = "camelCase")]
    Variant {
        variant: WithInfo<D::Info, Option<String>>,
        value_patterns: Vec<WithInfo<D::Info, Pattern<D>>>,
    },
    Tuple(Vec<WithInfo<D::Info, Pattern<D>>>),
    Or {
        left: WithInfo<D::Info, Box<Pattern<D>>>,
        right: WithInfo<D::Info, Box<Pattern<D>>>,
    },
    Mutate(WithInfo<D::Info, Option<String>>),
    Annotate {
        pattern: WithInfo<D::Info, Box<Pattern<D>>>,
        r#type: WithInfo<D::Info, Type<D>>,
    },
}

impl<D: Driver> DefaultFromInfo<D::Info> for Pattern<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info,
            item: Pattern::Error,
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) struct FieldPattern<D: Driver> {
    pub(crate) name: WithInfo<D::Info, Option<String>>,
    pub(crate) pattern: WithInfo<D::Info, Pattern<D>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for FieldPattern<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: FieldPattern {
                name: Option::default_from_info(info.clone()),
                pattern: Pattern::default_from_info(info),
            },
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) struct Arm<D: Driver> {
    pub(crate) pattern: WithInfo<D::Info, Pattern<D>>,
    pub(crate) body: WithInfo<D::Info, Expression<D>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for Arm<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: Arm {
                pattern: Pattern::default_from_info(info.clone()),
                body: Expression::default_from_info(info),
            },
        }
    }
}

#[derive(Deserialize, Derivative)]
#[derivative(
    Debug(bound = "D::Info: Debug"),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq")
)]
#[serde(rename_all = "camelCase")]
#[serde(bound(serialize = "", deserialize = ""))]
pub(crate) struct FieldValue<D: Driver> {
    pub(crate) name: WithInfo<D::Info, Option<String>>,
    pub(crate) value: WithInfo<D::Info, Expression<D>>,
}

impl<D: Driver> DefaultFromInfo<D::Info> for FieldValue<D> {
    fn default_from_info(info: D::Info) -> WithInfo<D::Info, Self> {
        WithInfo {
            info: info.clone(),
            item: FieldValue {
                name: Option::default_from_info(info.clone()),
                value: Expression::default_from_info(info),
            },
        }
    }
}

/// A diagnostic generated by the parser.
#[derive(Derivative, Serialize, Deserialize)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq"),
    Hash(bound = "D::Info: Hash")
)]
pub struct Diagnostic<D: Driver> {
    /// The expected piece of syntax at this location.
    pub expected: SyntaxKind,

    /// Whether the diagnostic refers to the token before or after the expected
    /// syntax.
    pub direction: Option<Direction>,

    /// The syntax rules matched so far.
    pub stack: Vec<WithInfo<D::Info, SyntaxKind>>,
}

/// Whether a [`Diagnostic`] refers to the token before of after the expected
/// syntax.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Direction {
    Before,
    After,
}

#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum::Display, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[strum(serialize_all = "kebab-case")]
pub enum SyntaxKind {
    TopLevel,
    Attribute,
    AttributeValue,
    Name,
    Number,
    Text,
    Statement,
    Keyword,
    Operator,
    Instance,
    TypeParameter,
    Pattern,
    WildcardPattern,
    NumberPattern,
    TextPattern,
    VariantPattern,
    DestructurePattern,
    TuplePattern,
    OrPattern,
    MutatePattern,
    AnnotatePattern,
    Expression,
    Type,
    PlaceholderType,
    DeclaredType,
    FunctionType,
    TupleType,
    BlockType,
    IntrinsicType,
    MessageType,
    TypeMember,
    FieldDeclaration,
    VariantDeclaration,
    Arm,
    TypeFunction,
    TypeRepresentation,
    TypeDeclaration,
    TypeAliasDeclaration,
    TraitDeclaration,
    InstanceDeclaration,
    ConstantDeclaration,
    LanguageDeclaration,
    Assignment,
    AnnotateExpression,
    NameExpression,
    NumberExpression,
    TextExpression,
    DoExpression,
    CallExpression,
    ApplyExpression,
    BinaryOperatorExpression,
    AsExpression,
    IsExpression,
    WhenExpression,
    IntrinsicExpression,
    TupleExpression,
    CollectionExpression,
    StructureExpression,
    StructureField,
    WhenBody,
    WhenArm,
    BlockExpression,
    FunctionExpression,
    FunctionInputs,
    Nothing,
}

#[allow(missing_docs)]
#[derive(Debug)]
pub struct Result<D: Driver, T> {
    pub parsed: WithInfo<D::Info, T>,
    pub diagnostics: Vec<WithInfo<D::Info, Diagnostic<D>>>,
}

/// Parse a token tree into a concrete top-level syntax tree.
pub fn parse_top_level<D: Driver>(
    driver: &D,
    tree: WithInfo<D::Info, &TokenTree<'_, D>>,
) -> Result<D, TopLevel<D>>
where
    D::Info: From<Location>,
{
    parse_rule(driver, tree, rules::top_level())
}

/// Parse a token tree into a type.
pub fn parse_type<D: Driver>(
    driver: &D,
    tree: WithInfo<D::Info, &TokenTree<'_, D>>,
) -> Result<D, Type<D>>
where
    D::Info: From<Location>,
{
    parse_rule(driver, tree, rules::r#type())
}

fn parse_rule<D: Driver, T>(
    driver: &D,
    tree: WithInfo<D::Info, &TokenTree<'_, D>>,
    rule: base::Rule<D, T>,
) -> Result<D, T>
where
    D::Info: From<Location>,
    T: DefaultFromInfo<D::Info> + 'static,
{
    let mut parser = base::Parser::new(driver);

    let stack = base::ParseStack::<D>::new(WithInfo {
        info: tree.info.clone(),
        item: SyntaxKind::TopLevel,
    });

    let parsed = rule.parse(&mut parser, tree, &stack);

    Result {
        parsed,
        diagnostics: parser.into_diagnostics(),
    }
}

/// An HTML-rendered version of the parser's grammar.
pub struct Grammar {
    rules: Vec<(&'static str, SyntaxKind, RuleToRender)>,
}

#[derive(Clone)]
enum RuleToRender {
    Ellipsis,
    Token(&'static str),
    Keyword(String),
    Terminal(SyntaxKind),
    List(Vec<Rc<dyn Fn() -> RuleToRender>>),
    Block(Vec<Rc<dyn Fn() -> RuleToRender>>),
    Switch(Vec<Rc<dyn Fn() -> RuleToRender>>),
}

impl RuleToRender {
    const NAME: Self = RuleToRender::Token("name");
    const NUMBER: Self = RuleToRender::Token("number");
    const TEXT: Self = RuleToRender::Token("text");
    const UNDERSCORE: Self = RuleToRender::Token("_");
}

impl Grammar {
    /// Render the grammar to HTML.
    pub fn render_to_html(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        write!(f, "<ul class=\"grammar\">")?;

        for (doc, kind, rule) in &self.rules {
            write!(f, "<li id=\"{}\">", kind)?;
            write!(f, "<a href=\"#{}\"><em>{}</em></a> &rarr; ", kind, kind)?;
            rule.render_to_html(f)?;
            write!(f, "<br>")?;
            write!(f, "<p>{}</p>", doc)?;
            write!(f, "</li>")?;
        }

        write!(f, "</ul>")?;

        Ok(())
    }

    /// Render the grammar to an HTML string.
    pub fn render_to_html_string(&self) -> String {
        let mut s = String::new();
        self.render_to_html(&mut s).unwrap();
        s
    }
}

#[cfg(test)]
mod render_grammar_to_html {
    use super::*;
    use std::{fs, path::PathBuf, sync::Arc};

    #[test]
    fn render_grammar_to_html() {
        struct TestDriver;

        impl Driver for TestDriver {
            type Info = ();

            fn file_path(&self) -> Arc<str> {
                unimplemented!()
            }

            fn visible_path(&self) -> Arc<str> {
                unimplemented!()
            }

            fn file_size(&self) -> u32 {
                unimplemented!()
            }

            fn merge_info(_left: Self::Info, _right: Self::Info) -> Self::Info {
                unimplemented!()
            }
        }

        let html = grammar(&TestDriver).render_to_html_string();

        let output_path = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap())
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("target/debug/grammar.html");

        fs::write(output_path, html).unwrap();
    }
}

impl RuleToRender {
    fn render_to_html(&self, f: &mut dyn std::fmt::Write) -> std::fmt::Result {
        match self {
            RuleToRender::Ellipsis => write!(f, "<em>...</em>")?,
            RuleToRender::Token(token) => write!(f, "<em>{}</em>", token)?,
            RuleToRender::Keyword(keyword) => write!(f, "<code>{}</code>", keyword)?,
            RuleToRender::Terminal(kind) => {
                write!(f, "<a href=\"#{}\"><em>{}</em></a>", kind, kind)?
            }
            RuleToRender::List(rules) => {
                write!(f, "<code>(</code>")?;

                for rule in rules {
                    write!(f, " ")?;
                    rule().render_to_html(f)?;
                }

                if !rules.is_empty() {
                    write!(f, " ")?;
                }

                write!(f, "<code>)</code>")?;
            }
            RuleToRender::Block(rules) => {
                write!(f, "<code>{{</code>")?;

                for rule in rules {
                    write!(f, " ")?;
                    rule().render_to_html(f)?;
                }

                if !rules.is_empty() {
                    write!(f, " ")?;
                }

                write!(f, "<code>}}</code>")?;
            }
            RuleToRender::Switch(rules) => {
                write!(f, "(")?;

                for (index, rule) in rules.iter().enumerate() {
                    if index > 0 {
                        write!(f, "&emsp;|&emsp;")?;
                    }

                    rule().render_to_html(f)?;
                }

                write!(f, ")")?;
            }
        }

        Ok(())
    }
}

/// The parser's syntax rules.
pub fn grammar<D: Driver>(_driver: &D) -> Grammar {
    let mut rules = rules::render::<D>();
    rules.sort_by_key(|(_, kind, _)| kind.to_string());

    Grammar { rules }
}

mod rules {
    use super::*;
    use base::Rule;

    pub fn render<D: Driver>() -> Vec<(&'static str, SyntaxKind, RuleToRender)> {
        vec![
            attribute::<D>().render(),
            attribute_value::<D>().render(),
            top_level::<D>().render(),
            statement::<D>().render(),
            type_declaration::<D>().render(),
            type_alias_declaration::<D>().render(),
            trait_declaration::<D>().render(),
            default_instance_declaration::<D>().render(),
            instance_declaration::<D>().render(),
            constant_declaration::<D>().render(),
            assignment::<D>().render(),
            r#type::<D>().render(),
            placeholder_type::<D>().render(),
            declared_type::<D>().render(),
            function_type::<D>().render(),
            tuple_type::<D>().render(),
            block_type::<D>().render(),
            intrinsic_type::<D>().render(),
            message_type::<D>().render(),
            type_function::<D>().render(),
            type_parameter::<D>().render(),
            type_representation::<D>().render(),
            instance::<D>().render(),
            pattern::<D>().render(),
            wildcard_pattern::<D>().render(),
            number_pattern::<D>().render(),
            text_pattern::<D>().render(),
            variant_pattern::<D>().render(),
            destructure_pattern::<D>().render(),
            tuple_pattern::<D>().render(),
            or_pattern::<D>().render(),
            expression::<D>().render(),
            annotate_expression::<D>().render(),
            name_expression::<D>().render(),
            number_expression::<D>().render(),
            text_expression::<D>().render(),
            apply_expression::<D>().render(),
            binary_operator_expression::<D>().render(),
            as_expression::<D>().render(),
            is_expression::<D>().render(),
            when_expression::<D>().render(),
            when_arm::<D>().render(),
            intrinsic_expression::<D>().render(),
            tuple_expression::<D>().render(),
            collection_expression::<D>().render(),
            structure_expression::<D>().render(),
            block_expression::<D>().render(),
            function_expression::<D>().render(),
            do_expression::<D>().render(),
            call_expression::<D>().render(),
        ]
    }

    pub fn attribute<D: Driver>() -> Rule<D, Attribute<D>> {
        Rule::switch(
            SyntaxKind::Attribute,
            [
                || {
                    name()
                        .wrapped()
                        .map(SyntaxKind::Attribute, |name| {
                            Attribute::Name(name.map(Option::unwrap))
                        })
                        .named("A name.")
                },
                || {
                    Rule::non_associative_operator(
                        SyntaxKind::Attribute,
                        NonAssociativeOperator::Assign,
                        || name().wrapped().in_list(),
                        || attribute_value().in_list(),
                        |_, info, name, value, _| WithInfo {
                            info,
                            item: Attribute::Valued {
                                name: name.map(Option::unwrap),
                                value,
                            },
                        },
                    )
                },
            ],
        )
        .named("An attribute.")
    }

    pub fn attribute_value<D: Driver>() -> Rule<D, AttributeValue<D>> {
        Rule::switch(
            SyntaxKind::AttributeValue,
            [
                || {
                    name().wrapped().map(SyntaxKind::AttributeValue, |name| {
                        AttributeValue::Name(name.map(Option::unwrap))
                    })
                },
                || {
                    number()
                        .wrapped()
                        .map(SyntaxKind::AttributeValue, |number| {
                            AttributeValue::Number(number.map(Option::unwrap))
                        })
                },
                || {
                    text().wrapped().map(SyntaxKind::AttributeValue, |text| {
                        AttributeValue::Text(text.map(Option::unwrap))
                    })
                },
            ],
        )
        .named("An attribute value.")
    }

    pub fn top_level<D: Driver>() -> Rule<D, TopLevel<D>> {
        Rule::block(SyntaxKind::TopLevel, statement, |_, info, statements, _| {
            WithInfo {
                info,
                item: TopLevel { statements },
            }
        })
        .named("A file or code box.")
    }

    pub fn statement<D: Driver>() -> Rule<D, Statement<D>> {
        Rule::switch(
            SyntaxKind::Statement,
            [
                type_declaration,
                type_alias_declaration,
                trait_declaration,
                default_instance_declaration,
                instance_declaration,
                constant_declaration,
                assignment,
                || expression().map(SyntaxKind::Statement, Statement::Expression),
            ],
        )
        .no_backtrack()
        .named("A statement.")
    }

    pub fn type_declaration<D: Driver>() -> Rule<D, Statement<D>> {
        Rule::non_associative_operator(
            SyntaxKind::TypeDeclaration,
            NonAssociativeOperator::Assign,
            || name().wrapped().attributed_with(attribute()).in_list(),
            || {
                Rule::switch(
                    SyntaxKind::TypeDeclaration,
                    [
                        || {
                            Rule::keyword0(
                                SyntaxKind::TypeRepresentation,
                                Keyword::Type,
                                |_, info, _| <(_, _)>::default_from_info(info),
                            )
                        },
                        || {
                            Rule::keyword1(
                                SyntaxKind::TypeRepresentation,
                                Keyword::Type,
                                type_representation,
                                |_, info: D::Info, representation, _| WithInfo {
                                    info: info.clone(),
                                    item: (TypeFunction::default_from_info(info), representation),
                                },
                            )
                        },
                        || {
                            Rule::non_associative_operator(
                                SyntaxKind::TypeRepresentation,
                                NonAssociativeOperator::TypeFunction,
                                type_function,
                                || {
                                    Rule::keyword0(
                                        SyntaxKind::TypeRepresentation,
                                        Keyword::Type,
                                        |_, info, _| TypeRepresentation::default_from_info(info),
                                    )
                                },
                                |_, info, type_function, representation, _| WithInfo {
                                    info,
                                    item: (type_function, representation),
                                },
                            )
                        },
                        || {
                            Rule::non_associative_operator(
                                SyntaxKind::TypeRepresentation,
                                NonAssociativeOperator::TypeFunction,
                                type_function,
                                || {
                                    Rule::keyword1(
                                        SyntaxKind::TypeRepresentation,
                                        Keyword::Type,
                                        type_representation,
                                        |_, _, representation, _| representation,
                                    )
                                },
                                |_, info, type_function, representation, _| WithInfo {
                                    info,
                                    item: (type_function, representation),
                                },
                            )
                        },
                    ],
                )
            },
            |_, info, name, declaration, _| {
                let (parameters, representation) = declaration.item;

                WithInfo {
                    info,
                    item: Statement::TypeDeclaration {
                        attributes: name.item.attributes,
                        name: name.item.value,
                        parameters,
                        representation,
                    },
                }
            },
        )
        .named("A type declaration.")
    }

    pub fn type_alias_declaration<D: Driver>() -> Rule<D, Statement<D>> {
        Rule::non_associative_operator(
            SyntaxKind::TypeAliasDeclaration,
            NonAssociativeOperator::Assign,
            || name().wrapped().attributed_with(attribute()).in_list(),
            || {
                Rule::switch(
                    SyntaxKind::TypeAliasDeclaration,
                    [
                        || {
                            Rule::keyword2(
                                SyntaxKind::TypeAliasDeclaration,
                                Keyword::Type,
                                || {
                                    Rule::match_terminal(
                                        SyntaxKind::InstanceDeclaration,
                                        RuleToRender::Keyword(String::from("alias")),
                                        |_, tree, _| match tree.item {
                                            TokenTree::Name(name) if name == "alias" => {
                                                Some(WithInfo {
                                                    info: tree.info,
                                                    item: (),
                                                })
                                            }
                                            _ => None,
                                        },
                                    )
                                },
                                r#type,
                                |_, info: D::Info, _, r#type, _| WithInfo {
                                    info: info.clone(),
                                    item: (TypeFunction::default_from_info(info), r#type),
                                },
                            )
                        },
                        || {
                            Rule::non_associative_operator(
                                SyntaxKind::TypeAliasDeclaration,
                                NonAssociativeOperator::TypeFunction,
                                type_function,
                                || {
                                    Rule::keyword2(
                                        SyntaxKind::Type,
                                        Keyword::Type,
                                        || {
                                            Rule::match_terminal(
                                                SyntaxKind::InstanceDeclaration,
                                                RuleToRender::Keyword(String::from("alias")),
                                                |_, tree, _| match tree.item {
                                                    TokenTree::Name(name) if name == "alias" => {
                                                        Some(WithInfo {
                                                            info: tree.info,
                                                            item: (),
                                                        })
                                                    }
                                                    _ => None,
                                                },
                                            )
                                        },
                                        r#type,
                                        |_, _, _, r#type, _| r#type,
                                    )
                                },
                                |_, info, type_function, r#type, _| WithInfo {
                                    info,
                                    item: (type_function, r#type),
                                },
                            )
                        },
                    ],
                )
            },
            |_, info, name, r#type, _| {
                let (parameters, r#type) = r#type.item;

                WithInfo {
                    info,
                    item: Statement::TypeAliasDeclaration {
                        attributes: name.item.attributes,
                        name: name.item.value,
                        parameters,
                        r#type,
                    },
                }
            },
        )
        .named("A type alias declaration.")
    }

    pub fn trait_declaration<D: Driver>() -> Rule<D, Statement<D>> {
        Rule::non_associative_operator(
            SyntaxKind::TraitDeclaration,
            NonAssociativeOperator::Assign,
            || name().wrapped().attributed_with(attribute()).in_list(),
            || {
                Rule::switch(
                    SyntaxKind::TraitDeclaration,
                    [
                        || {
                            Rule::keyword0(
                                SyntaxKind::TraitDeclaration,
                                Keyword::Trait,
                                |_, info: D::Info, _| WithInfo {
                                    info: info.clone(),
                                    item: (
                                        TypeFunction::default_from_info(info.clone()),
                                        Option::default_from_info(info),
                                    ),
                                },
                            )
                        },
                        || {
                            Rule::keyword1(
                                SyntaxKind::TraitDeclaration,
                                Keyword::Trait,
                                || r#type().no_backtrack(),
                                |_, info: D::Info, r#type, _| WithInfo {
                                    info: info.clone(),
                                    item: (TypeFunction::default_from_info(info), r#type.map(Some)),
                                },
                            )
                        },
                        || {
                            Rule::non_associative_operator(
                                SyntaxKind::TraitDeclaration,
                                NonAssociativeOperator::TypeFunction,
                                type_function,
                                || {
                                    Rule::switch(
                                        SyntaxKind::TraitDeclaration,
                                        [
                                            || {
                                                Rule::keyword0(
                                                    SyntaxKind::TraitDeclaration,
                                                    Keyword::Trait,
                                                    |_, info: D::Info, _| {
                                                        Option::default_from_info(info)
                                                    },
                                                )
                                            },
                                            || {
                                                Rule::keyword1(
                                                    SyntaxKind::TraitDeclaration,
                                                    Keyword::Trait,
                                                    || r#type().no_backtrack(),
                                                    |_, _, r#type, _| r#type.map(Some),
                                                )
                                            },
                                        ],
                                    )
                                },
                                |_, info, type_function, r#type, _| WithInfo {
                                    info,
                                    item: (type_function, r#type),
                                },
                            )
                        },
                    ],
                )
            },
            |_, info, name, declaration, _| {
                let (parameters, r#type) = declaration.item;

                WithInfo {
                    info,
                    item: Statement::TraitDeclaration {
                        attributes: name.item.attributes,
                        name: name.item.value,
                        parameters,
                        r#type: r#type.try_unwrap(),
                    },
                }
            },
        )
        .named("A trait declaration.")
    }

    pub fn default_instance_declaration<D: Driver>() -> Rule<D, Statement<D>> {
        Rule::switch(
            SyntaxKind::InstanceDeclaration,
            [
                || {
                    Rule::non_associative_operator(
                        SyntaxKind::InstanceDeclaration,
                        NonAssociativeOperator::Assign,
                        || {
                            Rule::switch(
                                SyntaxKind::InstanceDeclaration,
                                [
                                    || {
                                        Rule::non_associative_operator(
                                            SyntaxKind::InstanceDeclaration,
                                            NonAssociativeOperator::TypeFunction,
                                            type_function,
                                            || {
                                                Rule::contextual_keyword2(
                                                    SyntaxKind::InstanceDeclaration,
                                                    "default",
                                                    || {
                                                        Rule::match_terminal(
                                                            SyntaxKind::InstanceDeclaration,
                                                            RuleToRender::Keyword(
                                                                Keyword::Instance.to_string(),
                                                            ),
                                                            |_, info, _| Some(info.replace(())),
                                                        )
                                                    },
                                                    || instance().wrapped(),
                                                    |_, _, _, instance, _| instance,
                                                )
                                            },
                                            |_, info, type_function, instance, _| WithInfo {
                                                info,
                                                item: (type_function, instance),
                                            },
                                        )
                                    },
                                    || {
                                        Rule::contextual_keyword2(
                                            SyntaxKind::InstanceDeclaration,
                                            "default",
                                            || {
                                                Rule::match_terminal(
                                                    SyntaxKind::InstanceDeclaration,
                                                    RuleToRender::Keyword(
                                                        Keyword::Instance.to_string(),
                                                    ),
                                                    |_, info, _| Some(info.replace(())),
                                                )
                                            },
                                            || instance().wrapped(),
                                            |_, info: D::Info, _, instance, _| WithInfo {
                                                info: info.clone(),
                                                item: (
                                                    TypeFunction::default_from_info(info),
                                                    instance,
                                                ),
                                            },
                                        )
                                    },
                                ],
                            )
                        },
                        expression,
                        |_, info, declaration, body, _| {
                            let (parameters, instance) = declaration.item;

                            WithInfo {
                                info,
                                item: Statement::DefaultInstanceDeclaration {
                                    parameters,
                                    instance,
                                    body: Some(body),
                                },
                            }
                        },
                    )
                },
                || {
                    Rule::switch(
                        SyntaxKind::InstanceDeclaration,
                        [
                            || {
                                Rule::non_associative_operator(
                                    SyntaxKind::InstanceDeclaration,
                                    NonAssociativeOperator::TypeFunction,
                                    type_function,
                                    || {
                                        Rule::contextual_keyword2(
                                            SyntaxKind::InstanceDeclaration,
                                            "default",
                                            || {
                                                Rule::match_terminal(
                                                    SyntaxKind::InstanceDeclaration,
                                                    RuleToRender::Keyword(
                                                        Keyword::Instance.to_string(),
                                                    ),
                                                    |_, info, _| Some(info.replace(())),
                                                )
                                            },
                                            || instance().wrapped(),
                                            |_, _, _, instance, _| instance,
                                        )
                                    },
                                    |_, info, type_function, instance, _| WithInfo {
                                        info,
                                        item: Statement::DefaultInstanceDeclaration {
                                            parameters: type_function,
                                            instance,
                                            body: None,
                                        },
                                    },
                                )
                            },
                            || {
                                Rule::contextual_keyword2(
                                    SyntaxKind::InstanceDeclaration,
                                    "default",
                                    || {
                                        Rule::match_terminal(
                                            SyntaxKind::InstanceDeclaration,
                                            RuleToRender::Keyword(Keyword::Instance.to_string()),
                                            |_, info, _| Some(info.replace(())),
                                        )
                                    },
                                    || instance().wrapped(),
                                    |_, info: D::Info, _, instance, _| WithInfo {
                                        info: info.clone(),
                                        item: Statement::DefaultInstanceDeclaration {
                                            parameters: TypeFunction::default_from_info(info),
                                            instance,
                                            body: None,
                                        },
                                    },
                                )
                            },
                        ],
                    )
                },
            ],
        )
        .named("A default instance declaration.")
    }

    pub fn instance_declaration<D: Driver>() -> Rule<D, Statement<D>> {
        Rule::switch(
            SyntaxKind::InstanceDeclaration,
            [
                || {
                    Rule::non_associative_operator(
                        SyntaxKind::InstanceDeclaration,
                        NonAssociativeOperator::Assign,
                        || {
                            Rule::switch(
                                SyntaxKind::InstanceDeclaration,
                                [
                                    || {
                                        Rule::non_associative_operator(
                                            SyntaxKind::InstanceDeclaration,
                                            NonAssociativeOperator::TypeFunction,
                                            type_function,
                                            || {
                                                Rule::keyword1(
                                                    SyntaxKind::InstanceDeclaration,
                                                    Keyword::Instance,
                                                    || instance().wrapped(),
                                                    |_, _, instance, _| instance,
                                                )
                                            },
                                            |_, info, type_function, instance, _| WithInfo {
                                                info,
                                                item: (type_function, instance),
                                            },
                                        )
                                    },
                                    || {
                                        Rule::keyword1(
                                            SyntaxKind::InstanceDeclaration,
                                            Keyword::Instance,
                                            || instance().wrapped(),
                                            |_, info: D::Info, instance, _| WithInfo {
                                                info: info.clone(),
                                                item: (
                                                    TypeFunction::default_from_info(info),
                                                    instance,
                                                ),
                                            },
                                        )
                                    },
                                ],
                            )
                        },
                        expression,
                        |_, info, declaration, body, _| {
                            let (parameters, instance) = declaration.item;

                            WithInfo {
                                info,
                                item: Statement::InstanceDeclaration {
                                    parameters,
                                    instance,
                                    body: Some(body),
                                },
                            }
                        },
                    )
                },
                || {
                    Rule::switch(
                        SyntaxKind::InstanceDeclaration,
                        [
                            || {
                                Rule::non_associative_operator(
                                    SyntaxKind::InstanceDeclaration,
                                    NonAssociativeOperator::TypeFunction,
                                    type_function,
                                    || {
                                        Rule::keyword1(
                                            SyntaxKind::InstanceDeclaration,
                                            Keyword::Instance,
                                            || instance().wrapped(),
                                            |_, _, instance, _| instance,
                                        )
                                    },
                                    |_, info, type_function, instance, _| WithInfo {
                                        info,
                                        item: Statement::InstanceDeclaration {
                                            parameters: type_function,
                                            instance,
                                            body: None,
                                        },
                                    },
                                )
                            },
                            || {
                                Rule::keyword1(
                                    SyntaxKind::InstanceDeclaration,
                                    Keyword::Instance,
                                    || instance().wrapped(),
                                    |_, info: D::Info, instance, _| WithInfo {
                                        info: info.clone(),
                                        item: Statement::InstanceDeclaration {
                                            parameters: TypeFunction::default_from_info(info),
                                            instance,
                                            body: None,
                                        },
                                    },
                                )
                            },
                        ],
                    )
                },
            ],
        )
        .named("An instance declaration.")
    }

    pub fn constant_declaration<D: Driver>() -> Rule<D, Statement<D>> {
        Rule::non_associative_operator(
            SyntaxKind::ConstantDeclaration,
            NonAssociativeOperator::Annotate,
            || {
                name()
                    .wrapped()
                    .attributed_with(attribute())
                    .in_list()
                    .no_backtrack()
            },
            || {
                Rule::switch(
                    SyntaxKind::ConstantDeclaration,
                    [
                        || {
                            Rule::non_associative_operator(
                                SyntaxKind::TypeFunction,
                                NonAssociativeOperator::TypeFunction,
                                type_function,
                                r#type,
                                |_, info, type_function, r#type, _| WithInfo {
                                    info,
                                    item: (type_function, r#type),
                                },
                            )
                        },
                        || {
                            r#type().map(SyntaxKind::Type, |r#type| {
                                (
                                    TypeFunction::default_from_info(D::Info::clone(&r#type.info)),
                                    r#type,
                                )
                            })
                        },
                    ],
                )
            },
            |_, info, name, declaration, _| {
                let (parameters, r#type) = declaration.item;

                WithInfo {
                    info,
                    item: Statement::ConstantDeclaration {
                        attributes: name.item.attributes,
                        name: name.item.value,
                        parameters,
                        r#type,
                    },
                }
            },
        )
        .named("A constant declaration.")
    }

    pub fn assignment<D: Driver>() -> Rule<D, Statement<D>> {
        Rule::non_associative_operator(
            SyntaxKind::Assignment,
            NonAssociativeOperator::Assign,
            pattern,
            expression,
            |_, info, pattern, value, _| WithInfo {
                info,
                item: Statement::Assignment { pattern, value },
            },
        )
        .named("Assign a value to a pattern.")
    }

    pub fn r#type<D: Driver>() -> Rule<D, Type<D>> {
        Rule::switch(
            SyntaxKind::Type,
            [
                placeholder_type,
                function_type,
                tuple_type,
                block_type,
                intrinsic_type,
                message_type,
                declared_type,
            ],
        )
        .unwrap_parentheses()
        .no_backtrack()
        .named("A type.")
    }

    pub fn placeholder_type<D: Driver>() -> Rule<D, Type<D>> {
        Rule::match_terminal(
            SyntaxKind::PlaceholderType,
            RuleToRender::UNDERSCORE,
            |_, tree, _| match tree.item {
                TokenTree::Keyword(Keyword::Underscore) => Some(tree.replace(Type::Placeholder)),
                _ => None,
            },
        )
        .named("An inferred type.")
    }

    pub fn declared_type<D: Driver>() -> Rule<D, Type<D>> {
        Rule::switch(
            SyntaxKind::DeclaredType,
            [
                || {
                    Rule::empty_list(SyntaxKind::DeclaredType, |info| WithInfo {
                        info,
                        item: Type::Unit,
                    })
                },
                || {
                    name()
                        .wrapped()
                        .map(SyntaxKind::DeclaredType, |name| Type::Declared {
                            name,
                            parameters: Vec::new(),
                        })
                },
                || {
                    Rule::list_prefix(
                        SyntaxKind::DeclaredType,
                        || name().wrapped(),
                        r#type,
                        |_, info, name, types, _| WithInfo {
                            info,
                            item: Type::Declared {
                                name,
                                parameters: types,
                            },
                        },
                    )
                },
            ],
        )
        .named("A declared type, optionally with parameters.")
    }

    pub fn function_type<D: Driver>() -> Rule<D, Type<D>> {
        Rule::operator(
            SyntaxKind::FunctionType,
            Operator::Function,
            || {
                Rule::switch(
                    SyntaxKind::FunctionInputs,
                    [
                        || {
                            Rule::list(SyntaxKind::FunctionInputs, r#type, |_, info, types, _| {
                                WithInfo { info, item: types }
                            })
                        },
                        || r#type().map(SyntaxKind::FunctionInputs, |r#type| vec![r#type]),
                    ],
                )
            },
            r#type,
            |_, info, inputs, output, _| WithInfo {
                info,
                item: Type::Function {
                    inputs: inputs.item,
                    output: output.boxed(),
                },
            },
        )
        .named("A function type.")
    }

    pub fn tuple_type<D: Driver>() -> Rule<D, Type<D>> {
        Rule::variadic_operator(
            SyntaxKind::TupleType,
            VariadicOperator::Tuple,
            r#type,
            |_, info, types, _| WithInfo {
                info,
                item: Type::Tuple(types),
            },
        )
        .named("A tuple type.")
    }

    pub fn block_type<D: Driver>() -> Rule<D, Type<D>> {
        Rule::switch(
            SyntaxKind::BlockType,
            [
                || {
                    Rule::empty_block(SyntaxKind::BlockType, |info| WithInfo {
                        info: D::Info::clone(&info),
                        item: Type::Block(
                            WithInfo {
                                info,
                                item: Type::Unit,
                            }
                            .boxed(),
                        ),
                    })
                },
                || {
                    Rule::block(
                        SyntaxKind::BlockType,
                        r#type,
                        |parser, info, types, stack| {
                            let mut types = types.into_iter();

                            let r#type = match types.next() {
                                Some(r#type) => r#type,
                                None => {
                                    parser.add_diagnostic(stack.error_expected(
                                        WithInfo {
                                            info: D::Info::clone(&info),
                                            item: SyntaxKind::Type,
                                        },
                                        None,
                                    ));

                                    WithInfo {
                                        info: info.clone(),
                                        item: Type::Error,
                                    }
                                }
                            };

                            for r#type in types {
                                parser.add_diagnostic(
                                    stack.error_expected(r#type.replace(SyntaxKind::Nothing), None),
                                );
                            }

                            WithInfo {
                                info,
                                item: Type::Block(r#type.boxed()),
                            }
                        },
                    )
                },
            ],
        )
        .named("A type whose value is computed from a block expression.")
    }

    pub fn intrinsic_type<D: Driver>() -> Rule<D, Type<D>> {
        Rule::match_terminal(
            SyntaxKind::IntrinsicType,
            RuleToRender::Keyword(Keyword::Intrinsic.to_string()),
            |_, tree, _| match tree.item {
                TokenTree::Keyword(Keyword::Intrinsic) => Some(tree.replace(Type::Intrinsic)),
                _ => None,
            },
        )
        .named("An intrinsic type provided by the runtime.")
    }

    pub fn message_type<D: Driver>() -> Rule<D, Type<D>> {
        Rule::switch(
            SyntaxKind::MessageType,
            [
                || {
                    text()
                        .wrapped()
                        .map(SyntaxKind::MessageType, |text| Type::Message {
                            message: text.try_unwrap().unwrap(),
                            inputs: Vec::new(),
                        })
                },
                || {
                    Rule::list_prefix(
                        SyntaxKind::MessageType,
                        || text().wrapped(),
                        r#type,
                        |_, info, message, inputs, _| WithInfo {
                            info,
                            item: Type::Message {
                                message: message.try_unwrap().unwrap(),
                                inputs,
                            },
                        },
                    )
                },
            ],
        )
        .named("A type-level piece of text used to generate compiler errors.")
    }

    pub fn type_function<D: Driver>() -> Rule<D, TypeFunction<D>> {
        Rule::switch(
            SyntaxKind::TypeFunction,
            [
                || {
                    Rule::list(
                        SyntaxKind::TypeParameter,
                        || type_parameter().no_backtrack(),
                        |_, info, parameters, _| WithInfo {
                            info,
                            item: TypeFunction {
                                parameters,
                                bounds: Vec::new(),
                            },
                        },
                    )
                },
                || {
                    Rule::non_associative_operator(
                        SyntaxKind::TypeFunction,
                        NonAssociativeOperator::Where,
                        || {
                            Rule::switch(
                                SyntaxKind::TypeParameter,
                                [
                                    || {
                                        Rule::empty_list(
                                            SyntaxKind::TypeParameter,
                                            Vec::default_from_info,
                                        )
                                        .in_list()
                                    },
                                    || {
                                        Rule::list(
                                            SyntaxKind::TypeParameter,
                                            || type_parameter().no_backtrack(),
                                            |_, info, parameters, _| WithInfo {
                                                info,
                                                item: parameters,
                                            },
                                        )
                                    },
                                ],
                            )
                        },
                        || {
                            Rule::list(
                                SyntaxKind::Instance,
                                || instance().no_backtrack(),
                                |_, info, bounds, _| WithInfo { info, item: bounds },
                            )
                        },
                        |_, info, parameters, bounds, _| WithInfo {
                            info,
                            item: TypeFunction {
                                parameters: parameters.item,
                                bounds: bounds.item,
                            },
                        },
                    )
                },
            ],
        )
        .named("Provides generic type parameters and bounds to a declaration.")
    }

    pub fn type_parameter<D: Driver>() -> Rule<D, TypeParameter<D>> {
        Rule::switch(
            SyntaxKind::TypeParameter,
            [
                || {
                    name()
                        .wrapped()
                        .map(SyntaxKind::TypeParameter, |name| TypeParameter {
                            name,
                            default: None,
                            infer: None,
                        })
                },
                || {
                    Rule::contextual_keyword1(
                        SyntaxKind::TypeParameter,
                        "infer",
                        || name().wrapped(),
                        |_, info, name, _| WithInfo {
                            info: D::Info::clone(&info),
                            item: TypeParameter {
                                name,
                                default: None,
                                infer: Some(WithInfo { info, item: () }),
                            },
                        },
                    )
                },
                || {
                    Rule::non_associative_operator(
                        SyntaxKind::TypeParameter,
                        NonAssociativeOperator::Assign,
                        || {
                            Rule::switch(
                                SyntaxKind::TypeParameter,
                                [
                                    || {
                                        name().wrapped().in_list().map(
                                            SyntaxKind::TypeParameter,
                                            |name| {
                                                (
                                                    name.clone(),
                                                    WithInfo {
                                                        info: name.info,
                                                        item: None,
                                                    },
                                                )
                                            },
                                        )
                                    },
                                    || {
                                        Rule::contextual_keyword1(
                                            SyntaxKind::TypeParameter,
                                            "infer",
                                            || name().wrapped(),
                                            |_, info, name, _| WithInfo {
                                                info: D::Info::clone(&info),
                                                item: (
                                                    name,
                                                    WithInfo {
                                                        info,
                                                        item: Some(()),
                                                    },
                                                ),
                                            },
                                        )
                                    },
                                ],
                            )
                        },
                        r#type,
                        |_, info, name, r#type, _| {
                            let (name, default) = name.item;

                            WithInfo {
                                info,
                                item: TypeParameter {
                                    name,
                                    default: Some(r#type),
                                    infer: default.try_unwrap(),
                                },
                            }
                        },
                    )
                },
            ],
        )
        .no_backtrack()
        .named("A type parameter.")
    }

    pub fn type_representation<D: Driver>() -> Rule<D, TypeRepresentation<D>> {
        Rule::switch(
            SyntaxKind::TypeRepresentation,
            [
                || {
                    Rule::block(
                        SyntaxKind::TypeRepresentation,
                        || {
                            Rule::switch(
                                SyntaxKind::TypeMember,
                                [
                                    || {
                                        Rule::non_associative_operator(
                                            SyntaxKind::FieldDeclaration,
                                            NonAssociativeOperator::Annotate,
                                            || {
                                                name()
                                                    .wrapped()
                                                    .attributed_with(attribute())
                                                    .in_list()
                                                    .no_backtrack()
                                            },
                                            r#type,
                                            |_, info, name, r#type, _| WithInfo {
                                                info,
                                                item: TypeMember {
                                                    attributes: name.item.attributes,
                                                    name: name.item.value,
                                                    kind: TypeMemberKind::Field(r#type),
                                                },
                                            },
                                        )
                                    },
                                    || {
                                        Rule::list_prefix(
                                            SyntaxKind::VariantDeclaration,
                                            || {
                                                name()
                                                    .wrapped()
                                                    .attributed_with(attribute())
                                                    .no_backtrack()
                                            },
                                            r#type,
                                            |_, info, name, types, _| WithInfo {
                                                info,
                                                item: TypeMember {
                                                    attributes: name.item.attributes,
                                                    name: name.item.value,
                                                    kind: TypeMemberKind::Variant(types),
                                                },
                                            },
                                        )
                                    },
                                ],
                            )
                        },
                        |_, info, members, _| WithInfo {
                            info,
                            item: TypeRepresentation::Compound(members),
                        },
                    )
                },
                || {
                    r#type().map(SyntaxKind::Type, |r#type| {
                        TypeRepresentation::Wrapper(r#type)
                    })
                },
            ],
        )
        .no_backtrack()
        .named("A set of fields or variants in a type.")
    }

    pub fn instance<D: Driver>() -> Rule<D, Instance<D>> {
        Rule::list_prefix(
            SyntaxKind::Instance,
            || name().wrapped(),
            r#type,
            |_, info, r#trait, parameters, _| WithInfo {
                info,
                item: Instance {
                    r#trait,
                    parameters,
                },
            },
        )
        .no_backtrack()
        .named("An instance.")
    }

    pub fn pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        Rule::switch(
            SyntaxKind::Pattern,
            [
                wildcard_pattern,
                number_pattern,
                text_pattern,
                destructure_pattern,
                tuple_pattern,
                or_pattern,
                mutate_pattern,
                variant_pattern,
                annotate_pattern,
            ],
        )
        .unwrap_parentheses()
        .no_backtrack()
        .named("A pattern.")
    }

    pub fn wildcard_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        Rule::match_terminal(
            SyntaxKind::WildcardPattern,
            RuleToRender::UNDERSCORE,
            |_, tree, _| match tree.item {
                TokenTree::Keyword(Keyword::Underscore) => Some(tree.replace(Pattern::Wildcard)),
                _ => None,
            },
        )
        .named("A pattern that matches any value.")
    }

    pub fn number_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        number()
            .wrapped()
            .map(SyntaxKind::NumberPattern, |number| {
                Pattern::Number(number.item.unwrap())
            })
            .named("A pattern that matches a number.")
    }

    pub fn text_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        text()
            .wrapped()
            .map(SyntaxKind::TextPattern, |text| {
                Pattern::Text(text.item.unwrap())
            })
            .named("A pattern that matches a piece of text.")
    }

    pub fn variant_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        Rule::switch(
            SyntaxKind::VariantPattern,
            [
                || {
                    Rule::empty_list(SyntaxKind::VariantPattern, |info| WithInfo {
                        info,
                        item: Pattern::Unit,
                    })
                },
                || {
                    name()
                        .wrapped()
                        .map(SyntaxKind::VariantPattern, Pattern::VariantOrName)
                },
                || {
                    Rule::list_prefix(
                        SyntaxKind::VariantPattern,
                        || name().wrapped(),
                        pattern,
                        |_, info, variant, value_patterns, _| WithInfo {
                            info,
                            item: if value_patterns.is_empty() {
                                Pattern::VariantOrName(variant)
                            } else {
                                Pattern::Variant {
                                    variant,
                                    value_patterns,
                                }
                            },
                        },
                    )
                },
            ],
        )
        .named("A pattern that matches a variant or binds to a variable.")
    }

    pub fn destructure_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        Rule::block(
            SyntaxKind::DestructurePattern,
            || {
                Rule::non_associative_operator(
                    SyntaxKind::StructureField,
                    NonAssociativeOperator::Assign,
                    || name().wrapped().in_list().no_backtrack(),
                    pattern,
                    |_, info, name, pattern, _| WithInfo {
                        info,
                        item: FieldPattern { name, pattern },
                    },
                )
            },
            |_, info, patterns, _| WithInfo {
                info,
                item: Pattern::Destructure(patterns),
            },
        )
        .named("A pattern that matches the fields of a structure.")
    }

    pub fn tuple_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        Rule::variadic_operator(
            SyntaxKind::TuplePattern,
            VariadicOperator::Tuple,
            pattern,
            |_, info, patterns, _| WithInfo {
                info,
                item: Pattern::Tuple(patterns),
            },
        )
        .named("A pattern that matches a tuple.")
    }

    pub fn or_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        Rule::operator(
            SyntaxKind::OrPattern,
            Operator::Or,
            pattern,
            pattern,
            |_, info, left, right, _| WithInfo {
                info,
                item: Pattern::Or {
                    left: left.boxed(),
                    right: right.boxed(),
                },
            },
        )
        .named("A pattern that matches either one of its subpatterns.")
    }

    pub fn mutate_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        Rule::mutate(
            SyntaxKind::MutatePattern,
            || name().wrapped(),
            |_, info, name, _| WithInfo {
                info,
                item: Pattern::Mutate(name),
            },
        )
        .named("A pattern that changes the value of an existing variable.")
    }

    pub fn annotate_pattern<D: Driver>() -> Rule<D, Pattern<D>> {
        Rule::non_associative_operator(
            SyntaxKind::AnnotatePattern,
            NonAssociativeOperator::Annotate,
            pattern,
            r#type,
            |_, info, pattern, r#type, _| WithInfo {
                info,
                item: Pattern::Annotate {
                    pattern: pattern.boxed(),
                    r#type,
                },
            },
        )
        .named("Annotate a pattern with a type.")
    }

    pub fn expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::switch(
            SyntaxKind::Expression,
            [
                annotate_expression,
                name_expression,
                number_expression,
                text_expression,
                apply_expression,
                binary_operator_expression,
                as_expression,
                is_expression,
                when_expression,
                intrinsic_expression,
                tuple_expression,
                collection_expression,
                structure_expression,
                block_expression,
                function_expression,
                do_expression,
                call_expression,
            ],
        )
        .unwrap_parentheses()
        .no_backtrack()
        .named("An expression.")
    }

    pub fn annotate_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::non_associative_operator(
            SyntaxKind::AnnotateExpression,
            NonAssociativeOperator::Annotate,
            expression,
            r#type,
            |_, info, value, r#type, _| WithInfo {
                info,
                item: Expression::Annotate {
                    value: value.boxed(),
                    r#type,
                },
            },
        )
        .named("Annotate an expression with a type.")
    }

    pub fn name_expression<D: Driver>() -> Rule<D, Expression<D>> {
        name()
            .wrapped()
            .map(SyntaxKind::NameExpression, |name| {
                Expression::Name(name.item.unwrap())
            })
            .named("A name.")
    }

    pub fn number_expression<D: Driver>() -> Rule<D, Expression<D>> {
        number()
            .wrapped()
            .map(SyntaxKind::NumberExpression, |number| {
                Expression::Number(number.item.unwrap())
            })
            .named("A number.")
    }

    pub fn text_expression<D: Driver>() -> Rule<D, Expression<D>> {
        text()
            .wrapped()
            .map(SyntaxKind::TextExpression, |text| {
                Expression::Text(text.item.unwrap())
            })
            .named("A piece of text.")
    }

    pub fn call_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::list(
            SyntaxKind::CallExpression,
            expression,
            |_, info, expressions, _| match expressions.len() {
                0 => WithInfo {
                    info,
                    item: Expression::Unit,
                },
                1 => expressions.into_iter().next().unwrap(),
                _ => {
                    let mut expressions = expressions.into_iter();

                    WithInfo {
                        info,
                        item: Expression::Call {
                            function: expressions.next().unwrap().boxed(),
                            inputs: expressions.collect(),
                        },
                    }
                }
            },
        )
        .named("Call a function with at least one input.")
    }

    pub fn apply_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::operator(
            SyntaxKind::ApplyExpression,
            Operator::Apply,
            expression,
            expression,
            |_, info, input, function, _| WithInfo {
                info,
                item: Expression::Apply {
                    input: input.boxed(),
                    function: function.boxed(),
                },
            },
        )
        .named("Function application using the <code>.</code> operator.")
    }

    pub fn binary_operator_expression<D: Driver>() -> Rule<D, Expression<D>> {
        macro_rules! binary_operator {
            ($op:ident) => {
                Rule::operator(
                    SyntaxKind::BinaryOperatorExpression,
                    Operator::$op,
                    expression,
                    expression,
                    |_, info, left, right, _| WithInfo {
                        info: D::Info::clone(&info),
                        item: Expression::BinaryOperator {
                            operator: WithInfo {
                                info,
                                item: BinaryOperator::$op,
                            },
                            left: left.boxed(),
                            right: right.boxed(),
                        },
                    },
                )
            };
            ($($op:ident),* $(,)?) => {
                [$(|| binary_operator!($op),)*]
            };
        }

        Rule::switch(
            SyntaxKind::BinaryOperatorExpression,
            binary_operator!(
                To,
                By,
                Power,
                Multiply,
                Divide,
                Remainder,
                Add,
                Subtract,
                LessThan,
                LessThanOrEqual,
                GreaterThan,
                GreaterThanOrEqual,
                Equal,
                NotEqual,
                And,
                Or,
            ),
        )
        .named("An expression involving a binary operator.")
    }

    pub fn as_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::operator(
            SyntaxKind::AsExpression,
            Operator::As,
            expression,
            r#type,
            |_, info, value, r#type, _| WithInfo {
                info,
                item: Expression::As {
                    value: value.boxed(),
                    r#type,
                },
            },
        )
        .named("Convert a value of one type to a value of a different type.")
    }

    pub fn is_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::operator(
            SyntaxKind::IsExpression,
            Operator::Is,
            expression,
            pattern,
            |_, info, value, pattern, _| WithInfo {
                info,
                item: Expression::Is {
                    value: value.boxed(),
                    pattern,
                },
            },
        )
        .named("Check if a value matches a pattern.")
    }

    pub fn when_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::keyword2(
            SyntaxKind::WhenExpression,
            Keyword::When,
            expression,
            || {
                Rule::block(SyntaxKind::WhenBody, when_arm, |_, info, arms, _| {
                    WithInfo { info, item: arms }
                })
                .no_backtrack()
            },
            |_, info, input, arms, _| WithInfo {
                info,
                item: Expression::When {
                    input: input.boxed(),
                    arms: arms.item,
                },
            },
        )
        .named("Match a value against a set of patterns.")
    }

    pub fn when_arm<D: Driver>() -> Rule<D, Arm<D>> {
        Rule::require_operator(
            SyntaxKind::WhenArm,
            Operator::Function,
            pattern,
            expression,
            |_, info, pattern, body, _| WithInfo {
                info,
                item: Arm { pattern, body },
            },
        )
        .no_backtrack()
        .named("An arm in a <code>when</code> expression.")
    }

    pub fn intrinsic_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::keyword_prefixn(
            SyntaxKind::IntrinsicExpression,
            Keyword::Intrinsic,
            || text().wrapped().no_backtrack(),
            expression,
            |_, info, name, inputs, _| WithInfo {
                info,
                item: Expression::Intrinsic { name, inputs },
            },
        )
        .named("Call an intrinsic function provided by the runtime.")
    }

    pub fn do_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::keyword1(
            SyntaxKind::DoExpression,
            Keyword::Do,
            expression,
            |_, info, block, _| WithInfo {
                info,
                item: Expression::Do(block.boxed()),
            },
        )
        .named("Call an intrinsic function provided by the runtime.")
    }

    pub fn tuple_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::variadic_operator(
            SyntaxKind::TupleExpression,
            VariadicOperator::Tuple,
            expression,
            |_, info, expressions, _| WithInfo {
                info,
                item: Expression::Tuple(expressions),
            },
        )
        .named("A tuple.")
    }

    pub fn collection_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::variadic_operator(
            SyntaxKind::CollectionExpression,
            VariadicOperator::Collection,
            expression,
            |_, info, expressions, _| WithInfo {
                info,
                item: Expression::Collection(expressions),
            },
        )
        .named("A collection.")
    }

    pub fn structure_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::block(
            SyntaxKind::StructureExpression,
            || {
                Rule::non_associative_operator(
                    SyntaxKind::StructureField,
                    NonAssociativeOperator::Assign,
                    || name().wrapped().in_list(),
                    expression,
                    |_, info, name, value, _| WithInfo {
                        info,
                        item: FieldValue { name, value },
                    },
                )
            },
            |_, info, fields, _| WithInfo {
                info,
                item: Expression::Structure(fields),
            },
        )
        .named("A structure.")
    }

    pub fn block_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::block(
            SyntaxKind::BlockExpression,
            statement,
            |_, info, statements, _| WithInfo {
                info,
                item: Expression::Block(statements),
            },
        )
        .named("A block expression.")
    }

    pub fn function_expression<D: Driver>() -> Rule<D, Expression<D>> {
        Rule::operator(
            SyntaxKind::FunctionExpression,
            Operator::Function,
            || {
                Rule::switch(
                    SyntaxKind::FunctionInputs,
                    [
                        || {
                            Rule::list(
                                SyntaxKind::FunctionInputs,
                                pattern,
                                |_, info, patterns, _| WithInfo {
                                    info,
                                    item: patterns,
                                },
                            )
                        },
                        || pattern().map(SyntaxKind::FunctionInputs, |pattern| vec![pattern]),
                    ],
                )
            },
            expression,
            |_, info, inputs, body, _| WithInfo {
                info,
                item: Expression::Function {
                    inputs: inputs.item,
                    body: body.boxed(),
                },
            },
        )
        .named("A function expression.")
    }

    pub fn name<D: Driver>() -> Rule<D, String> {
        Rule::match_terminal(SyntaxKind::Name, RuleToRender::NAME, |_, tree, _| {
            tree.filter_map(|tree| match tree {
                TokenTree::Name(name) => Some(name.clone().into_owned()),
                _ => None,
            })
        })
    }

    pub fn text<D: Driver>() -> Rule<D, String> {
        Rule::match_terminal(SyntaxKind::Text, RuleToRender::TEXT, |_, tree, _| {
            tree.filter_map(|tree| match tree {
                TokenTree::Text(text) => Some(text.clone().into_owned()),
                _ => None,
            })
        })
    }

    pub fn number<D: Driver>() -> Rule<D, String> {
        Rule::match_terminal(SyntaxKind::Number, RuleToRender::NUMBER, |_, tree, _| {
            tree.filter_map(|tree| match tree {
                TokenTree::Number(number) => Some(number.clone().into_owned()),
                _ => None,
            })
        })
    }
}

mod base {
    use super::*;
    use std::result::Result;

    /// Tracks the parser's progress.
    pub struct ParseStack<D: Driver> {
        parent: Option<Rc<ParseStack<D>>>,
        current: WithInfo<D::Info, SyntaxKind>,
    }

    impl<D: Driver> ParseStack<D> {
        pub fn new(root: WithInfo<D::Info, SyntaxKind>) -> Rc<Self> {
            Rc::new(ParseStack {
                parent: None,
                current: root,
            })
        }

        pub fn push(self: &Rc<Self>, child: WithInfo<D::Info, SyntaxKind>) -> Rc<Self> {
            // Prevent duplicate stack entries
            if self.current.item == child.item {
                return self.clone();
            }

            Rc::new(ParseStack {
                parent: Some(self.clone()),
                current: child,
            })
        }

        pub fn len(&self) -> usize {
            std::iter::successors(Some(self), |stack| stack.parent.as_deref()).count()
        }

        pub fn error_expected(
            &self,
            expected: WithInfo<D::Info, SyntaxKind>,
            direction: impl Into<Option<Direction>>,
        ) -> WithInfo<D::Info, Diagnostic<D>> {
            let mut stack = std::iter::successors(Some(self), |stack| stack.parent.as_deref())
                .map(|stack| stack.current.clone())
                .collect::<Vec<_>>();

            stack.reverse();

            WithInfo {
                info: expected.info,
                item: Diagnostic {
                    expected: expected.item,
                    direction: direction.into(),
                    stack,
                },
            }
        }
    }

    pub struct Parser<'a, D: Driver> {
        _driver: &'a D,
        debug: bool,
        diagnostics: Vec<WithInfo<D::Info, Diagnostic<D>>>,
    }

    impl<'a, D: Driver + 'a> Parser<'a, D> {
        pub fn new(driver: &'a D) -> Self {
            Parser {
                _driver: driver,
                debug: false,
                diagnostics: Vec::new(),
            }
        }

        #[allow(unused)]
        #[deprecated(note = "use of `debug`")]
        pub fn debug(mut self) -> Self {
            self.debug = true;
            self
        }

        pub fn add_diagnostic(&mut self, diagnostic: WithInfo<D::Info, Diagnostic<D>>) {
            self.diagnostics.push(diagnostic);
        }

        pub fn into_diagnostics(self) -> Vec<WithInfo<D::Info, Diagnostic<D>>> {
            self.diagnostics
        }
    }

    #[derive(Derivative)]
    #[derivative(Clone(bound = ""))]
    pub struct ParseFn<D: Driver, Output> {
        try_parse: Rc<
            dyn Fn(
                    &mut Parser<'_, D>,
                    WithInfo<D::Info, &TokenTree<'_, D>>,
                    &Rc<ParseStack<D>>,
                ) -> Option<Result<WithInfo<D::Info, Output>, usize>>
                + 'static,
        >,
        parse: Rc<
            dyn Fn(
                    &mut Parser<'_, D>,
                    WithInfo<D::Info, &TokenTree<'_, D>>,
                    &Rc<ParseStack<D>>,
                ) -> Option<WithInfo<D::Info, Output>>
                + 'static,
        >,
    }

    impl<D: Driver, Output> ParseFn<D, Output> {
        pub fn new(
            try_parse: impl Fn(
                    &mut Parser<'_, D>,
                    WithInfo<D::Info, &TokenTree<'_, D>>,
                    &Rc<ParseStack<D>>,
                ) -> Option<Result<WithInfo<D::Info, Output>, usize>>
                + 'static,
            parse: impl Fn(
                    &mut Parser<'_, D>,
                    WithInfo<D::Info, &TokenTree<'_, D>>,
                    &Rc<ParseStack<D>>,
                ) -> Option<WithInfo<D::Info, Output>>
                + 'static,
        ) -> Self {
            ParseFn {
                try_parse: Rc::new(try_parse),
                parse: Rc::new(parse),
            }
        }

        pub fn try_parse(
            &self,
            parser: &mut Parser<'_, D>,
            tree: WithInfo<D::Info, &TokenTree<'_, D>>,
            stack: &Rc<ParseStack<D>>,
        ) -> Option<Result<WithInfo<D::Info, Output>, usize>> {
            (self.try_parse)(parser, tree, stack)
        }

        pub fn parse(
            &self,
            parser: &mut Parser<'_, D>,
            tree: WithInfo<D::Info, &TokenTree<'_, D>>,
            stack: &Rc<ParseStack<D>>,
        ) -> Option<WithInfo<D::Info, Output>> {
            (self.parse)(parser, tree, stack)
        }
    }

    #[derive(Derivative)]
    #[derivative(Clone(bound = ""))]
    pub struct Rule<D: Driver, Output> {
        doc: Option<&'static str>,
        syntax_kind: SyntaxKind,
        rendered: RuleToRender,
        backtracks: Rc<dyn Fn() -> bool>,
        parse: ParseFn<D, Output>,
    }

    impl<D: Driver, Output: 'static> Rule<D, Output> {
        fn nonterminal(
            syntax_kind: SyntaxKind,
            rendered: RuleToRender,
            backtracks: impl Fn() -> bool + 'static,
            parse: ParseFn<D, Output>,
        ) -> Self {
            Rule {
                doc: None,
                syntax_kind,
                rendered,
                backtracks: Rc::new(backtracks),
                parse,
            }
        }

        fn terminal(
            syntax_kind: SyntaxKind,
            rendered: RuleToRender,
            backtracks: impl Fn() -> bool + 'static,
            parse: ParseFn<D, Output>,
        ) -> Self {
            Rule {
                doc: None,
                syntax_kind,
                rendered,
                backtracks: Rc::new(backtracks),
                parse,
            }
        }

        pub fn render(self) -> (&'static str, SyntaxKind, RuleToRender) {
            let doc = self.doc.expect("rule must be named");
            (doc, self.syntax_kind, self.rendered)
        }

        fn render_nested(&self) -> RuleToRender {
            if self.doc.is_some() {
                RuleToRender::Terminal(self.syntax_kind)
            } else {
                self.rendered.clone()
            }
        }

        #[allow(unused)]
        pub fn todo(syntax_kind: SyntaxKind) -> Self {
            Rule {
                doc: None,
                syntax_kind,
                rendered: RuleToRender::Token("TODO"),
                backtracks: Rc::new(|| true),
                parse: ParseFn::new(|_, _, _| todo!(), |_, _, _| todo!()),
            }
        }

        pub fn in_list(self) -> Rule<D, Output>
        where
            Output: DefaultFromInfo<D::Info>,
        {
            Rule {
                doc: self.doc,
                syntax_kind: self.syntax_kind,
                rendered: RuleToRender::List(vec![Rc::new({
                    let rendered = self.rendered.clone();
                    move || rendered.clone()
                })]),
                backtracks: self.backtracks.clone(),
                parse: ParseFn::new(
                    {
                        let rule = self.clone();

                        move |parser, tree, stack| {
                            let mut elements = match &tree.item {
                                TokenTree::List(_, elements) => elements.iter(),
                                _ => return None,
                            };

                            let output = rule.try_parse(
                                parser,
                                elements.next().map(WithInfo::as_ref)?,
                                stack,
                            )?;

                            if elements.next().is_some() {
                                return Some(Err(stack.len()));
                            }

                            Some(output)
                        }
                    },
                    move |parser, tree, stack| {
                        let mut elements = match &tree.item {
                            TokenTree::List(_, elements) => elements.iter(),
                            _ => return None,
                        };

                        let output =
                            self.parse(parser, elements.next().map(WithInfo::as_ref)?, stack);

                        for element in elements {
                            parser.add_diagnostic(stack.error_expected(
                                WithInfo {
                                    info: element.info.clone(),
                                    item: SyntaxKind::Nothing,
                                },
                                None,
                            ));
                        }

                        Some(output)
                    },
                ),
            }
        }

        pub fn unwrap_parentheses(self) -> Rule<D, Output>
        where
            Output: DefaultFromInfo<D::Info>,
        {
            Rule {
                doc: self.doc,
                syntax_kind: self.syntax_kind,
                rendered: self.rendered.clone(),
                backtracks: self.backtracks.clone(),
                parse: ParseFn::new(
                    {
                        let rule = self.clone();

                        move |parser, mut tree, stack| {
                            while let TokenTree::List(_, elements) = &tree.item {
                                if elements.len() != 1 {
                                    break;
                                }

                                tree = elements.iter().next().unwrap().as_ref();
                            }

                            rule.try_parse(parser, tree, stack)
                        }
                    },
                    move |parser, mut tree, stack| {
                        while let TokenTree::List(_, elements) = &tree.item {
                            if elements.len() != 1 {
                                break;
                            }

                            tree = elements.iter().next().unwrap().as_ref();
                        }

                        Some(self.parse(parser, tree, stack))
                    },
                ),
            }
        }

        pub fn map<T>(
            self,
            syntax_kind: SyntaxKind,
            f: impl Fn(WithInfo<D::Info, Output>) -> T + Clone + 'static,
        ) -> Rule<D, T>
        where
            Output: DefaultFromInfo<D::Info>,
        {
            Rule {
                doc: self.doc,
                syntax_kind,
                rendered: self.rendered.clone(),
                backtracks: self.backtracks.clone(),
                parse: ParseFn::new(
                    {
                        let rule = self.clone();
                        let f = f.clone();

                        move |parser, tree, stack| {
                            let output = rule.try_parse(parser, tree, stack)?;

                            Some(output.map(|output| WithInfo {
                                info: output.info.clone(),
                                item: f(output),
                            }))
                        }
                    },
                    move |parser, tree, stack| {
                        let output = self.parse(parser, tree, stack);

                        Some(WithInfo {
                            info: output.info.clone(),
                            item: f(output),
                        })
                    },
                ),
            }
        }

        pub fn wrapped(self) -> Rule<D, Option<Output>> {
            Rule {
                doc: self.doc,
                syntax_kind: self.syntax_kind,
                rendered: self.rendered.clone(),
                backtracks: self.backtracks.clone(),
                parse: ParseFn::new(
                    {
                        let rule = self.clone();
                        move |parser, tree, stack| {
                            let output = rule.try_parse(parser, tree, stack)?;
                            Some(output.map(|output| output.map(Some)))
                        }
                    },
                    move |parser, tree, stack| Some(self.parse_option(parser, tree, stack)),
                ),
            }
        }

        pub fn named(mut self, doc: &'static str) -> Self {
            self.doc = Some(doc);
            self
        }

        pub fn no_backtrack(mut self) -> Self {
            self.backtracks = Rc::new(|| false);
            self
        }
    }

    impl<D: Driver, Output: 'static> Rule<D, Output> {
        pub fn parse(
            &self,
            parser: &mut Parser<'_, D>,
            tree: WithInfo<D::Info, &TokenTree<'_, D>>,
            stack: &Rc<ParseStack<D>>,
        ) -> WithInfo<D::Info, Output>
        where
            Output: DefaultFromInfo<D::Info>,
        {
            let info = tree.info.clone();

            let stack = stack.push(WithInfo {
                info: tree.info.clone(),
                item: self.syntax_kind,
            });

            self.parse.parse(parser, tree, &stack).unwrap_or_else(|| {
                parser.add_diagnostic(stack.error_expected(
                    WithInfo {
                        info: info.clone(),
                        item: self.syntax_kind,
                    },
                    None,
                ));

                Output::default_from_info(info)
            })
        }

        pub fn parse_option(
            &self,
            parser: &mut Parser<'_, D>,
            tree: WithInfo<D::Info, &TokenTree<'_, D>>,
            stack: &Rc<ParseStack<D>>,
        ) -> WithInfo<D::Info, Option<Output>> {
            let info = tree.info.clone();

            let stack = stack.push(WithInfo {
                info: tree.info.clone(),
                item: self.syntax_kind,
            });

            let result = self.parse.parse(parser, tree, &stack);
            if result.is_none() {
                parser.add_diagnostic(stack.error_expected(
                    WithInfo {
                        info: info.clone(),
                        item: self.syntax_kind,
                    },
                    None,
                ));
            }

            result
                .map(|result| result.map(Some))
                .unwrap_or_else(|| Option::default_from_info(info))
        }

        pub fn try_parse(
            &self,
            parser: &mut Parser<'_, D>,
            tree: WithInfo<D::Info, &TokenTree<'_, D>>,
            stack: &Rc<ParseStack<D>>,
        ) -> Option<Result<WithInfo<D::Info, Output>, usize>> {
            let stack = stack.push(WithInfo {
                info: tree.info.clone(),
                item: self.syntax_kind,
            });

            if self.backtracks() {
                self.parse.try_parse(parser, tree, &stack)
            } else {
                self.parse.parse(parser, tree, &stack).map(Ok)
            }
        }

        pub fn backtracks(&self) -> bool {
            (self.backtracks)()
        }
    }

    impl<D: Driver, Output: 'static> Rule<D, Output> {
        pub fn match_terminal(
            syntax_kind: SyntaxKind,
            rendered: RuleToRender,
            f: impl Fn(
                    &mut Parser<'_, D>,
                    WithInfo<D::Info, &TokenTree<'_, D>>,
                    &Rc<ParseStack<D>>,
                ) -> Option<WithInfo<D::Info, Output>>
                + Clone
                + 'static,
        ) -> Self {
            Rule::terminal(
                syntax_kind,
                rendered,
                || true,
                ParseFn::new(
                    {
                        let f = f.clone();
                        move |parser, tree, stack| f(parser, tree, stack).map(Ok)
                    },
                    move |parser, tree, stack| f(parser, tree, stack),
                ),
            )
        }

        pub fn mutate<E>(
            syntax_kind: SyntaxKind,
            parse_element: fn() -> Rule<D, E>,
            output: impl Fn(
                    &mut Parser<'_, D>,
                    D::Info,
                    WithInfo<D::Info, E>,
                    &Rc<ParseStack<D>>,
                ) -> WithInfo<D::Info, Output>
                + Clone
                + 'static,
        ) -> Self
        where
            E: DefaultFromInfo<D::Info> + 'static,
        {
            Rule::terminal(
                syntax_kind,
                RuleToRender::List(vec![
                    Rc::new(move || parse_element().render_nested()),
                    Rc::new(|| RuleToRender::Keyword(Keyword::Mutate.to_string())),
                ]),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();
                        move |parser, tree, stack| {
                            let element = match &tree.item {
                                TokenTree::Mutate(element) => element,
                                _ => return None,
                            };

                            let element = match parse_element().try_parse(
                                parser,
                                element.as_deref(),
                                stack,
                            )? {
                                Ok(element) => element,
                                Err(progress) => return Some(Err(progress)),
                            };

                            Some(Ok(output(parser, tree.info, element, stack)))
                        }
                    },
                    move |parser, tree, stack| {
                        let element = match &tree.item {
                            TokenTree::Mutate(element) => element,
                            _ => return None,
                        };

                        let element = parse_element().parse(parser, element.as_deref(), stack);

                        Some(output(parser, tree.info, element, stack))
                    },
                ),
            )
        }

        pub fn operator<L, R>(
            syntax_kind: SyntaxKind,
            expected: tokenize::Operator,
            parse_left: fn() -> Rule<D, L>,
            parse_right: fn() -> Rule<D, R>,
            output: impl Fn(
                    &mut Parser<'_, D>,
                    D::Info,
                    WithInfo<D::Info, L>,
                    WithInfo<D::Info, R>,
                    &Rc<ParseStack<D>>,
                ) -> WithInfo<D::Info, Output>
                + Clone
                + 'static,
        ) -> Self
        where
            L: DefaultFromInfo<D::Info> + 'static,
            R: DefaultFromInfo<D::Info> + 'static,
        {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::List(vec![
                    Rc::new(move || parse_left().render_nested()),
                    Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                    Rc::new(move || parse_right().render_nested()),
                ]),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();
                        move |parser, tree, stack| {
                            let (found, left, right) = match &tree.item {
                                TokenTree::Operator(operator, left, right) => {
                                    (operator, left, right)
                                }
                                _ => return None,
                            };

                            if found.item != expected {
                                return None;
                            }

                            let left =
                                match parse_left().try_parse(parser, left.as_deref(), stack)? {
                                    Ok(left) => left,
                                    Err(progress) => return Some(Err(progress)),
                                };

                            let right =
                                match parse_right().try_parse(parser, right.as_deref(), stack)? {
                                    Ok(right) => right,
                                    Err(progress) => return Some(Err(progress)),
                                };

                            Some(Ok(output(parser, tree.info, left, right, stack)))
                        }
                    },
                    move |parser, tree, stack| {
                        let (found, left, right) = match &tree.item {
                            TokenTree::Operator(operator, left, right) => (operator, left, right),
                            _ => return None,
                        };

                        if found.item != expected {
                            return None;
                        }

                        let left = parse_left().parse(parser, left.as_deref(), stack);
                        let right = parse_right().parse(parser, right.as_deref(), stack);

                        Some(output(parser, tree.info, left, right, stack))
                    },
                ),
            )
        }

        pub fn require_operator<L, R>(
            syntax_kind: SyntaxKind,
            expected: tokenize::Operator,
            parse_left: fn() -> Rule<D, L>,
            parse_right: fn() -> Rule<D, R>,
            output: impl Fn(
                    &mut Parser<'_, D>,
                    D::Info,
                    WithInfo<D::Info, L>,
                    WithInfo<D::Info, R>,
                    &Rc<ParseStack<D>>,
                ) -> WithInfo<D::Info, Output>
                + Clone
                + 'static,
        ) -> Self
        where
            L: DefaultFromInfo<D::Info> + 'static,
            R: DefaultFromInfo<D::Info> + 'static,
        {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::List(vec![
                    Rc::new(move || parse_left().render_nested()),
                    Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                    Rc::new(move || parse_right().render_nested()),
                ]),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();
                        move |parser, tree, stack| {
                            let (found, left, right) = match &tree.item {
                                TokenTree::Operator(operator, left, right) => {
                                    (operator, left, right)
                                }
                                _ => return Some(Err(stack.len())),
                            };

                            if found.item != expected {
                                return Some(Err(stack.len()));
                            }

                            let left =
                                match parse_left().try_parse(parser, left.as_deref(), stack)? {
                                    Ok(left) => left,
                                    Err(progress) => return Some(Err(progress)),
                                };

                            let right =
                                match parse_right().try_parse(parser, right.as_deref(), stack)? {
                                    Ok(right) => right,
                                    Err(progress) => return Some(Err(progress)),
                                };

                            Some(Ok(output(parser, tree.info, left, right, stack)))
                        }
                    },
                    move |parser, tree, stack| {
                        let (found, left, right) = match &tree.item {
                            TokenTree::Operator(operator, left, right) => (operator, left, right),
                            _ => {
                                parser.add_diagnostic(stack.error_expected(
                                    WithInfo {
                                        info: tree.info,
                                        item: syntax_kind,
                                    },
                                    None,
                                ));

                                return None;
                            }
                        };

                        if found.item != expected {
                            parser.add_diagnostic(stack.error_expected(
                                WithInfo {
                                    info: tree.info,
                                    item: syntax_kind,
                                },
                                None,
                            ));

                            return None;
                        }

                        let left = parse_left().parse(parser, left.as_deref(), stack);
                        let right = parse_right().parse(parser, right.as_deref(), stack);

                        Some(output(parser, tree.info, left, right, stack))
                    },
                ),
            )
        }

        pub fn variadic_operator<E>(
            syntax_kind: SyntaxKind,
            expected: tokenize::VariadicOperator,
            parse_element: fn() -> Rule<D, E>,
            output: impl Fn(
                    &mut Parser<'_, D>,
                    D::Info,
                    Vec<WithInfo<D::Info, E>>,
                    &Rc<ParseStack<D>>,
                ) -> WithInfo<D::Info, Output>
                + Clone
                + 'static,
        ) -> Self
        where
            E: DefaultFromInfo<D::Info> + 'static,
        {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::List(vec![
                    Rc::new(move || parse_element().render_nested()),
                    Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                    Rc::new(|| RuleToRender::Ellipsis),
                ]),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();

                        move |parser, tree, stack| {
                            let (found, elements) = match &tree.item {
                                TokenTree::VariadicOperator(operator, children) => {
                                    (operator, children)
                                }
                                _ => return None,
                            };

                            if found.item != expected {
                                return None;
                            }

                            let mut result = Vec::with_capacity(elements.len());
                            for element in elements {
                                match parse_element().try_parse(parser, element.as_ref(), stack)? {
                                    Ok(element) => result.push(element),
                                    Err(progress) => return Some(Err(progress)),
                                }
                            }

                            Some(Ok(output(parser, tree.info, result, stack)))
                        }
                    },
                    move |parser, tree, stack| {
                        let (found, elements) = match &tree.item {
                            TokenTree::VariadicOperator(operator, children) => (operator, children),
                            _ => return None,
                        };

                        if found.item != expected {
                            return None;
                        }

                        let elements = elements
                            .iter()
                            .map(|element| parse_element().parse(parser, element.as_ref(), stack))
                            .collect();

                        Some(output(parser, tree.info, elements, stack))
                    },
                ),
            )
        }

        pub fn non_associative_operator<L, R>(
            syntax_kind: SyntaxKind,
            expected: tokenize::NonAssociativeOperator,
            parse_left: fn() -> Rule<D, L>,
            parse_right: fn() -> Rule<D, R>,
            output: impl Fn(
                    &mut Parser<'_, D>,
                    D::Info,
                    WithInfo<D::Info, L>,
                    WithInfo<D::Info, R>,
                    &Rc<ParseStack<D>>,
                ) -> WithInfo<D::Info, Output>
                + Clone
                + 'static,
        ) -> Self
        where
            L: DefaultFromInfo<D::Info> + 'static,
            R: DefaultFromInfo<D::Info> + 'static,
        {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::List(vec![
                    Rc::new(move || parse_left().render_nested()),
                    Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                    Rc::new(move || parse_right().render_nested()),
                ]),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();

                        move |parser, tree, stack| {
                            let (found, left, right) = match &tree.item {
                                TokenTree::NonAssociativeOperator(operator, left, right) => {
                                    (operator, left, right)
                                }
                                _ => return None,
                            };

                            if found.item != expected {
                                return None;
                            }

                            let left =
                                match parse_left().try_parse(parser, left.as_deref(), stack)? {
                                    Ok(left) => left,
                                    Err(progress) => return Some(Err(progress)),
                                };

                            let right =
                                match parse_right().try_parse(parser, right.as_deref(), stack)? {
                                    Ok(right) => right,
                                    Err(progress) => return Some(Err(progress)),
                                };

                            Some(Ok(output(parser, tree.info, left, right, stack)))
                        }
                    },
                    move |parser, tree, stack| {
                        let (found, left, right) = match &tree.item {
                            TokenTree::NonAssociativeOperator(operator, left, right) => {
                                (operator, left, right)
                            }
                            _ => return None,
                        };

                        if found.item != expected {
                            return None;
                        }

                        let left = parse_left().parse(parser, left.as_deref(), stack);
                        let right = parse_right().parse(parser, right.as_deref(), stack);

                        Some(output(parser, tree.info, left, right, stack))
                    },
                ),
            )
        }

        pub fn empty_list(
            syntax_kind: SyntaxKind,
            output: impl Fn(D::Info) -> WithInfo<D::Info, Output> + Clone + 'static,
        ) -> Self {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::List(Vec::new()),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();

                        move |_, tree, _| match tree.item {
                            TokenTree::List(_, elements) if elements.is_empty() => {
                                Some(Ok(output(tree.info)))
                            }
                            _ => None,
                        }
                    },
                    move |_, tree, _| match tree.item {
                        TokenTree::List(_, elements) if elements.is_empty() => {
                            Some(output(tree.info))
                        }
                        _ => None,
                    },
                ),
            )
        }

        pub fn list<E>(
            syntax_kind: SyntaxKind,
            parse_element: fn() -> Rule<D, E>,
            output: impl Fn(
                    &mut Parser<'_, D>,
                    D::Info,
                    Vec<WithInfo<D::Info, E>>,
                    &Rc<ParseStack<D>>,
                ) -> WithInfo<D::Info, Output>
                + Clone
                + 'static,
        ) -> Self
        where
            E: DefaultFromInfo<D::Info> + 'static,
        {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::List(vec![
                    Rc::new(move || parse_element().render_nested()),
                    Rc::new(|| RuleToRender::Ellipsis),
                ]),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();

                        move |parser, tree, stack| {
                            let elements = match &tree.item {
                                TokenTree::List(_, elements) => elements,
                                _ => return None,
                            };

                            let mut result = Vec::with_capacity(elements.len());
                            for element in elements {
                                match parse_element().try_parse(parser, element.as_ref(), stack)? {
                                    Ok(element) => result.push(element),
                                    Err(progress) => return Some(Err(progress)),
                                }
                            }

                            Some(Ok(output(parser, tree.info, result, stack)))
                        }
                    },
                    move |parser, tree, stack| {
                        let elements = match &tree.item {
                            TokenTree::List(_, elements) => elements,
                            _ => return None,
                        };

                        let elements = elements
                            .iter()
                            .map(|element| parse_element().parse(parser, element.as_ref(), stack))
                            .collect();

                        Some(output(parser, tree.info, elements, stack))
                    },
                ),
            )
        }

        pub fn list_prefix<P, E>(
            syntax_kind: SyntaxKind,
            parse_prefix: fn() -> Rule<D, P>,
            parse_element: fn() -> Rule<D, E>,
            output: impl Fn(
                    &mut Parser<'_, D>,
                    D::Info,
                    WithInfo<D::Info, P>,
                    Vec<WithInfo<D::Info, E>>,
                    &Rc<ParseStack<D>>,
                ) -> WithInfo<D::Info, Output>
                + Clone
                + 'static,
        ) -> Self
        where
            P: DefaultFromInfo<D::Info> + 'static,
            E: DefaultFromInfo<D::Info> + 'static,
        {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::List(vec![
                    Rc::new(move || parse_prefix().render_nested()),
                    Rc::new(move || parse_element().render_nested()),
                    Rc::new(|| RuleToRender::Ellipsis),
                ]),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();

                        move |parser, tree, stack| {
                            let elements = match &tree.item {
                                TokenTree::List(_, elements) => elements,
                                _ => return None,
                            };

                            let mut elements = elements.iter();

                            let prefix = match parse_prefix().try_parse(
                                parser,
                                elements.next()?.as_ref(),
                                stack,
                            )? {
                                Ok(prefix) => prefix,
                                Err(progress) => return Some(Err(progress)),
                            };

                            let mut result = Vec::with_capacity(elements.len());
                            for element in elements {
                                match parse_element().try_parse(parser, element.as_ref(), stack)? {
                                    Ok(element) => result.push(element),
                                    Err(progress) => return Some(Err(progress)),
                                }
                            }

                            Some(Ok(output(parser, tree.info, prefix, result, stack)))
                        }
                    },
                    move |parser, tree, stack| {
                        let elements = match &tree.item {
                            TokenTree::List(_, elements) => elements,
                            _ => return None,
                        };

                        let mut elements = elements.iter();

                        let prefix = match elements.next() {
                            Some(prefix) => parse_prefix().parse(parser, prefix.as_ref(), stack),
                            None => {
                                parser.add_diagnostic(stack.error_expected(
                                    WithInfo {
                                        info: tree.info.clone(),
                                        item: parse_prefix().syntax_kind,
                                    },
                                    None,
                                ));

                                P::default_from_info(tree.info.clone())
                            }
                        };

                        let elements = elements
                            .map(|element| parse_element().parse(parser, element.as_ref(), stack))
                            .collect();

                        Some(output(parser, tree.info, prefix, elements, stack))
                    },
                ),
            )
        }

        pub fn block<E>(
            syntax_kind: SyntaxKind,
            parse_statement: fn() -> Rule<D, E>,
            output: impl Fn(
                    &mut Parser<'_, D>,
                    D::Info,
                    Vec<WithInfo<D::Info, E>>,
                    &Rc<ParseStack<D>>,
                ) -> WithInfo<D::Info, Output>
                + Clone
                + 'static,
        ) -> Self
        where
            E: DefaultFromInfo<D::Info> + 'static,
        {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::Block(vec![
                    Rc::new(move || parse_statement().render_nested()),
                    Rc::new(|| RuleToRender::Ellipsis),
                ]),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();

                        move |parser, tree, stack| {
                            let statements = match &tree.item {
                                TokenTree::Block(statements) => statements,
                                _ => return None,
                            };

                            let mut result = Vec::with_capacity(statements.len());
                            for statement in statements {
                                match parse_statement().try_parse(
                                    parser,
                                    statement.as_ref(),
                                    stack,
                                )? {
                                    Ok(statement) => result.push(statement),
                                    Err(progress) => return Some(Err(progress)),
                                }
                            }

                            Some(Ok(output(parser, tree.info, result, stack)))
                        }
                    },
                    move |parser, tree, stack| {
                        let statements = match &tree.item {
                            TokenTree::Block(statements) => statements,
                            _ => return None,
                        };

                        let statements = statements
                            .iter()
                            .map(|statement| {
                                parse_statement().parse(parser, statement.as_ref(), stack)
                            })
                            .collect();

                        Some(output(parser, tree.info, statements, stack))
                    },
                ),
            )
        }

        pub fn empty_block(
            syntax_kind: SyntaxKind,
            output: impl Fn(D::Info) -> WithInfo<D::Info, Output> + Clone + 'static,
        ) -> Self {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::Block(Vec::new()),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();

                        move |_, tree, _| match tree.item {
                            TokenTree::Block(statements) if statements.is_empty() => {
                                Some(Ok(output(tree.info)))
                            }
                            _ => None,
                        }
                    },
                    move |_, tree, _| match tree.item {
                        TokenTree::Block(statements) if statements.is_empty() => {
                            Some(output(tree.info))
                        }
                        _ => None,
                    },
                ),
            )
        }

        pub fn switch<const N: usize>(
            syntax_kind: SyntaxKind,
            alternatives: [fn() -> Rule<D, Output>; N],
        ) -> Self
        where
            Output: DefaultFromInfo<D::Info> + 'static,
        {
            assert!(!alternatives.is_empty());

            Rule::nonterminal(
                syntax_kind,
                RuleToRender::Switch(
                    alternatives
                        .iter()
                        .cloned()
                        .map(|alternative| {
                            Rc::new(move || alternative().render_nested()) as Rc<dyn Fn() -> _>
                        })
                        .collect(),
                ),
                || true,
                ParseFn::new(
                    move |parser, tree, stack| {
                        for alternative in alternatives {
                            let alternative = alternative();

                            match alternative.try_parse(parser, tree.as_deref(), stack) {
                                Some(Ok(result)) => return Some(Ok(result)),
                                Some(Err(progress)) if !alternative.backtracks() => {
                                    return Some(Err(progress))
                                }
                                _ => continue,
                            }
                        }

                        None
                    },
                    move |parser, tree, stack| {
                        for alternative in alternatives {
                            let alternative = alternative();

                            match alternative.try_parse(parser, tree.as_deref(), stack) {
                                Some(Ok(result)) => return Some(result),
                                Some(Err(_)) if !alternative.backtracks() => {
                                    // Calling `parse` here should produce a diagnostic
                                    return Some(alternative.parse(parser, tree.as_deref(), stack));
                                }
                                _ => continue,
                            }
                        }

                        // Produce a diagnostic inside the alternative that made the most progress
                        let alternative = alternatives
                            .iter()
                            .filter_map(|alternative| {
                                let alternative = alternative();

                                match alternative.try_parse(parser, tree.as_deref(), stack) {
                                    Some(Ok(_)) => panic!("rule was expected to fail"),
                                    Some(Err(progress)) => {
                                        Some((alternative, Some(std::cmp::Reverse(progress))))
                                    }
                                    None => None,
                                }
                            })
                            .min_by_key(|(_, progress)| *progress)
                            .map(|(alternative, _)| alternative)?;

                        // Calling `parse` here should produce a diagnostic
                        Some(alternative.parse(parser, tree.as_deref(), stack))
                    },
                ),
            )
        }
    }

    macro_rules! impl_keyword_rule {
        ($pattern:ident($ty:ty), $name:ident($($n:ident),*)) => {
            impl<_D: Driver, Output: 'static> Rule<_D, Output> {
                #[allow(unused, non_snake_case, clippy::redundant_clone, clippy::too_many_arguments)]
                pub fn $name<$($n),*>(
                    syntax_kind: SyntaxKind,
                    expected: $ty,
                    $($n: fn() -> Rule<_D, $n>,)*
                    output: impl Fn(&mut Parser<'_, _D>, _D::Info, $(WithInfo<_D::Info, $n>, )* &Rc<ParseStack<_D>>) -> WithInfo<_D::Info, Output> + Clone + 'static,
                ) -> Rule<_D, Output>
                where
                    $($n: DefaultFromInfo<_D::Info> + 'static,)*
                {
                    Rule::nonterminal(
                        syntax_kind,
                        RuleToRender::List(vec![
                            Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                            $(Rc::new(move || $n().render_nested()),)*
                        ]),
                        || true,
                        ParseFn::new(
                            {
                                let output = output.clone();

                                move |parser, tree, stack| {
                                    let mut elements = match &tree.item {
                                        TokenTree::List(_, elements) => elements.iter(),
                                        _ => return None,
                                    };

                                    let info = match elements.next()? {
                                        WithInfo {
                                            item: TokenTree::$pattern(found),
                                            info,
                                        } if *found == expected => info,
                                        _ => return None,
                                    };

                                    $(
                                        let $n = match $n().try_parse(parser, elements.next()?.as_ref(), stack)? {
                                            Ok($n) => $n,
                                            Err(progress) => return Some(Err(progress)),
                                        };
                                    )*

                                    if elements.next().is_some() {
                                        return None;
                                    }

                                    Some(Ok(output(parser, tree.info, $($n,)* stack)))
                                }
                            },
                            move |parser, tree, stack| {
                                let mut elements = match &tree.item {
                                    TokenTree::List(_, elements) => elements.iter(),
                                    _ => return None,
                                };

                                let info = match elements.next()? {
                                    WithInfo {
                                        item: TokenTree::$pattern(found),
                                        info,
                                    } if *found == expected => info,
                                    _ => return None,
                                };

                                $(
                                    let $n = match elements.next() {
                                        Some(input) => $n().parse(parser, input.as_ref(), stack),
                                        None => {
                                            parser.add_diagnostic(
                                                stack.error_expected(
                                                    WithInfo {
                                                        info: info.clone(),
                                                        item: $n().syntax_kind,
                                                    },
                                                    None,
                                                ),
                                            );

                                            $n::default_from_info(info.clone())
                                        }
                                    };
                                )*

                                for element in elements {
                                    parser.add_diagnostic(
                                        stack.error_expected(
                                            WithInfo {
                                                info: _D::Info::clone(&element.info),
                                                item: SyntaxKind::Nothing,
                                            },
                                            Direction::After,
                                        )
                                    );
                                }

                                Some(output(parser, tree.info, $($n,)* stack))
                            },
                        ),
                    )
                }
            }
        };
    }

    impl_keyword_rule!(Keyword(tokenize::Keyword), keyword0());
    impl_keyword_rule!(Keyword(tokenize::Keyword), keyword1(A));
    impl_keyword_rule!(Keyword(tokenize::Keyword), keyword2(A, B));
    impl_keyword_rule!(Keyword(tokenize::Keyword), keyword3(A, B, C));

    impl_keyword_rule!(Name(&'static str), contextual_keyword0());
    impl_keyword_rule!(Name(&'static str), contextual_keyword1(A));
    impl_keyword_rule!(Name(&'static str), contextual_keyword2(A, B));
    impl_keyword_rule!(Name(&'static str), contextual_keyword3(A, B, C));

    impl<D: Driver, Output: 'static> Rule<D, Output> {
        pub fn keyword_prefixn<P, E>(
            syntax_kind: SyntaxKind,
            expected: tokenize::Keyword,
            parse_prefix: fn() -> Rule<D, P>,
            parse_element: fn() -> Rule<D, E>,
            output: impl Fn(
                    &mut Parser<'_, D>,
                    D::Info,
                    WithInfo<D::Info, P>,
                    Vec<WithInfo<D::Info, E>>,
                    &Rc<ParseStack<D>>,
                ) -> WithInfo<D::Info, Output>
                + Clone
                + 'static,
        ) -> Rule<D, Output>
        where
            P: DefaultFromInfo<D::Info> + 'static,
            E: DefaultFromInfo<D::Info> + 'static,
        {
            Rule::nonterminal(
                syntax_kind,
                RuleToRender::List(vec![
                    Rc::new(move || RuleToRender::Keyword(expected.to_string())),
                    Rc::new(move || parse_prefix().render_nested()),
                    Rc::new(move || parse_element().render_nested()),
                    Rc::new(|| RuleToRender::Ellipsis),
                ]),
                || true,
                ParseFn::new(
                    {
                        let output = output.clone();

                        move |parser, tree, stack| {
                            let mut elements = match &tree.item {
                                TokenTree::List(_, elements) => elements.iter(),
                                _ => return None,
                            };

                            if !matches!(elements.next()?, WithInfo {
                                item: TokenTree::Keyword(found),
                                ..
                            } if *found == expected)
                            {
                                return None;
                            }

                            let prefix = match parse_prefix().try_parse(
                                parser,
                                elements.next()?.as_ref(),
                                stack,
                            )? {
                                Ok(prefix) => prefix,
                                Err(progress) => return Some(Err(progress)),
                            };

                            let mut result = Vec::with_capacity(elements.len());
                            for element in elements {
                                match parse_element().try_parse(parser, element.as_ref(), stack)? {
                                    Ok(element) => result.push(element),
                                    Err(progress) => return Some(Err(progress)),
                                }
                            }

                            Some(Ok(output(parser, tree.info, prefix, result, stack)))
                        }
                    },
                    move |parser, tree, stack| {
                        let mut elements = match &tree.item {
                            TokenTree::List(_, elements) => elements.iter(),
                            _ => return None,
                        };

                        let info = match elements.next()? {
                            WithInfo {
                                item: TokenTree::Keyword(found),
                                info,
                            } if *found == expected => info,
                            _ => return None,
                        };

                        let prefix = match elements.next() {
                            Some(prefix) => parse_prefix().parse(parser, prefix.as_ref(), stack),
                            None => {
                                parser.add_diagnostic(stack.error_expected(
                                    WithInfo {
                                        info: info.clone(),
                                        item: parse_prefix().syntax_kind,
                                    },
                                    Direction::After,
                                ));

                                P::default_from_info(info.clone())
                            }
                        };

                        let elements = elements
                            .map(|element| parse_element().parse(parser, element.as_ref(), stack))
                            .collect();

                        Some(output(parser, tree.info, prefix, elements, stack))
                    },
                ),
            )
        }
    }

    impl<D: Driver, Output: 'static> Rule<D, Output>
    where
        Output: DefaultFromInfo<D::Info> + 'static,
    {
        pub fn attributed_with(
            self,
            parse_attribute: Rule<D, Attribute<D>>,
        ) -> Rule<D, Attributed<D, Output>> {
            Rule {
                doc: None,
                syntax_kind: self.syntax_kind,
                rendered: self.rendered.clone(),
                backtracks: self.backtracks.clone(),
                parse: ParseFn::new(
                    {
                        let parse_attribute = parse_attribute.clone();
                        let parse_value = self.clone();

                        move |parser, mut tree, stack| {
                            let mut attributes = Vec::new();
                            while let TokenTree::Attribute(attribute, contents) = tree.item {
                                let attribute =
                                    parse_attribute.parse(parser, attribute.as_deref(), stack);

                                attributes.push(attribute);

                                tree = contents.as_deref();
                            }

                            let output = match parse_value.try_parse(parser, tree, stack)? {
                                Ok(prefix) => prefix,
                                Err(progress) => return Some(Err(progress)),
                            };

                            let info = attributes.first().map_or_else(
                                || output.info.clone(),
                                |attribute| {
                                    D::merge_info(attribute.info.clone(), output.info.clone())
                                },
                            );

                            Some(Ok(WithInfo {
                                info,
                                item: Attributed {
                                    attributes,
                                    value: output,
                                },
                            }))
                        }
                    },
                    move |parser, mut tree, stack| {
                        let mut attributes = Vec::new();
                        while let TokenTree::Attribute(attribute, contents) = tree.item {
                            let attribute =
                                parse_attribute.parse(parser, attribute.as_deref(), stack);

                            attributes.push(attribute);

                            tree = contents.as_deref();
                        }

                        let output = self.parse(parser, tree, stack);

                        let info = attributes.first().map_or_else(
                            || output.info.clone(),
                            |attribute| D::merge_info(attribute.info.clone(), output.info.clone()),
                        );

                        Some(WithInfo {
                            info,
                            item: Attributed {
                                attributes,
                                value: output,
                            },
                        })
                    },
                ),
            }
        }
    }
}

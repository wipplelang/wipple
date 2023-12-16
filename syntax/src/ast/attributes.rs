use crate::{
    ast::{self, format::Format, SyntaxError},
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct FileAttributes<D: Driver> {
    pub raw: Vec<parse::Attribute<D>>,
    pub help_url: Option<ast::HelpUrlFileAttribute<D>>,
    pub no_implicit_use: Option<ast::NoImplicitUseFileAttribute<D>>,
    pub recursion_limit: Option<ast::RecursionLimitFileAttribute<D>>,
}

impl<D: Driver> Default for FileAttributes<D> {
    fn default() -> Self {
        Self {
            raw: Default::default(),
            help_url: Default::default(),
            no_implicit_use: Default::default(),
            recursion_limit: Default::default(),
        }
    }
}

impl<D: Driver> Format<D> for FileAttributes<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        self.raw
            .into_iter()
            .map(|attribute| {
                Ok(format!(
                    "[[{}]]\n",
                    attribute
                        .exprs
                        .into_iter()
                        .map(Format::format)
                        .collect::<Result<Vec<_>, _>>()?
                        .join(" ")
                ))
            })
            .collect::<Result<String, _>>()
    }
}

#[derive(Debug, Clone)]
pub struct StatementAttributes<D: Driver> {
    pub raw: Vec<parse::Attribute<D>>,
    pub language_item: Option<ast::LanguageItemStatementAttribute<D>>,
    pub diagnostic_item: Option<ast::DiagnosticItemStatementAttribute<D>>,
    pub help_aliases: Vec<ast::HelpAliasStatementAttribute<D>>,
    pub help_alternatives: Vec<ast::HelpAlternativeStatementAttribute<D>>,
    pub help: Vec<ast::HelpStatementAttribute<D>>,
    pub help_group: Option<ast::HelpGroupStatementAttribute<D>>,
    pub help_playground: Option<ast::HelpPlaygroundStatementAttribute<D>>,
    pub on_unimplemented: Option<ast::OnUnimplementedStatementAttribute<D>>,
    pub on_mismatch: Option<ast::OnMismatchStatementAttribute<D>>,
    pub specialize: Option<ast::SpecializeStatementAttribute<D>>,
    pub allow_overlapping_instances: Option<ast::AllowOverlappingInstancesStatementAttribute<D>>,
    pub operator_precedence: Option<ast::OperatorPrecedenceStatementAttribute<D>>,
    pub keyword: Option<ast::KeywordStatementAttribute<D>>,
    pub contextual: Option<ast::ContextualStatementAttribute<D>>,
    pub help_convert_from: Vec<ast::HelpConvertFromStatementAttribute<D>>,
    pub private: Option<ast::PrivateStatementAttribute<D>>,
    pub sealed: Option<ast::SealedStatementAttribute<D>>,
    pub entrypoint: Option<ast::EntrypointStatementAttribute<D>>,
    pub help_show_code: Option<ast::HelpShowCodeStatementAttribute<D>>,
    pub on_unresolved: Option<ast::OnUnresolvedStatementAttribute<D>>,
    pub resolve: Option<ast::ResolveStatementAttribute<D>>,
}

impl<D: Driver> Default for StatementAttributes<D> {
    fn default() -> Self {
        Self {
            raw: Default::default(),
            language_item: Default::default(),
            diagnostic_item: Default::default(),
            help_aliases: Default::default(),
            help_alternatives: Default::default(),
            help: Default::default(),
            help_group: Default::default(),
            help_playground: Default::default(),
            on_unimplemented: Default::default(),
            on_mismatch: Default::default(),
            specialize: Default::default(),
            allow_overlapping_instances: Default::default(),
            operator_precedence: Default::default(),
            keyword: Default::default(),
            contextual: Default::default(),
            help_convert_from: Default::default(),
            private: Default::default(),
            sealed: Default::default(),
            entrypoint: Default::default(),
            help_show_code: Default::default(),
            on_unresolved: Default::default(),
            resolve: Default::default(),
        }
    }
}

impl<D: Driver> Format<D> for StatementAttributes<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        self.raw
            .into_iter()
            .map(|attribute| {
                Ok(format!(
                    "[{}]\n",
                    attribute
                        .exprs
                        .into_iter()
                        .map(Format::format)
                        .collect::<Result<Vec<_>, _>>()?
                        .join(" ")
                ))
            })
            .collect::<Result<String, _>>()
    }
}

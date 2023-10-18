use crate::{
    ast::{
        format::Format, AllowOverlappingInstancesStatementAttribute, ContextualStatementAttribute,
        ConvertFromStatementAttribute, DeriveStatementAttribute, DiagnosticAliasStatementAttribute,
        DiagnosticItemStatementAttribute, EntrypointStatementAttribute,
        HelpGroupStatementAttribute, HelpPlaygroundStatementAttribute,
        HelpShowCodeStatementAttribute, HelpStatementAttribute, HelpUrlFileAttribute,
        KeywordStatementAttribute, LanguageItemStatementAttribute, NoImplicitUseFileAttribute,
        NoReuseStatementAttribute, OnMismatchStatementAttribute, OnUnimplementedStatementAttribute,
        OnUnresolvedStatementAttribute, OperatorPrecedenceStatementAttribute,
        PrivateStatementAttribute, RecursionLimitFileAttribute, ResolveStatementAttribute,
        SealedStatementAttribute, SpecializeStatementAttribute, SyntaxError,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct FileAttributes<D: Driver> {
    pub raw: Vec<parse::Attribute<D>>,
    pub help_url: Option<HelpUrlFileAttribute<D>>,
    pub no_implicit_use: Option<NoImplicitUseFileAttribute<D>>,
    pub recursion_limit: Option<RecursionLimitFileAttribute<D>>,
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
    pub language_item: Option<LanguageItemStatementAttribute<D>>,
    pub diagnostic_item: Option<DiagnosticItemStatementAttribute<D>>,
    pub diagnostic_aliases: Vec<DiagnosticAliasStatementAttribute<D>>,
    pub help: Vec<HelpStatementAttribute<D>>,
    pub help_group: Option<HelpGroupStatementAttribute<D>>,
    pub help_playground: Option<HelpPlaygroundStatementAttribute<D>>,
    pub on_unimplemented: Option<OnUnimplementedStatementAttribute<D>>,
    pub on_mismatch: Option<OnMismatchStatementAttribute<D>>,
    pub specialize: Option<SpecializeStatementAttribute<D>>,
    pub allow_overlapping_instances: Option<AllowOverlappingInstancesStatementAttribute<D>>,
    pub operator_precedence: Option<OperatorPrecedenceStatementAttribute<D>>,
    pub keyword: Option<KeywordStatementAttribute<D>>,
    pub contextual: Option<ContextualStatementAttribute<D>>,
    pub convert_from: Vec<ConvertFromStatementAttribute<D>>,
    pub derive: Option<DeriveStatementAttribute<D>>,
    pub private: Option<PrivateStatementAttribute<D>>,
    pub sealed: Option<SealedStatementAttribute<D>>,
    pub no_reuse: Option<NoReuseStatementAttribute<D>>,
    pub entrypoint: Option<EntrypointStatementAttribute<D>>,
    pub help_show_code: Option<HelpShowCodeStatementAttribute<D>>,
    pub on_unresolved: Option<OnUnresolvedStatementAttribute<D>>,
    pub resolve: Option<ResolveStatementAttribute<D>>,
}

impl<D: Driver> Default for StatementAttributes<D> {
    fn default() -> Self {
        Self {
            raw: Default::default(),
            language_item: Default::default(),
            diagnostic_item: Default::default(),
            diagnostic_aliases: Default::default(),
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
            convert_from: Default::default(),
            derive: Default::default(),
            private: Default::default(),
            sealed: Default::default(),
            no_reuse: Default::default(),
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

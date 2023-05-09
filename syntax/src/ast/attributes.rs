use crate::{
    ast::{
        AllowOverlappingInstancesStatementAttribute, ContextualStatementAttribute,
        DiagnosticAliasStatementAttribute, DiagnosticItemStatementAttribute,
        HelpGroupStatementAttribute, HelpStatementAttribute, KeywordStatementAttribute,
        LanguageItemStatementAttribute, NoStdFileAttribute, OnMismatchStatementAttribute,
        OnUnimplementedStatementAttribute, OperatorPrecedenceStatementAttribute,
        RecursionLimitFileAttribute, SpecializeStatementAttribute,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct FileAttributes<D: Driver> {
    pub no_std: Option<NoStdFileAttribute<D>>,
    pub recursion_limit: Option<RecursionLimitFileAttribute<D>>,
}

impl<D: Driver> Default for FileAttributes<D> {
    fn default() -> Self {
        Self {
            no_std: Default::default(),
            recursion_limit: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StatementAttributes<D: Driver> {
    pub language_item: Option<LanguageItemStatementAttribute<D>>,
    pub diagnostic_item: Option<DiagnosticItemStatementAttribute<D>>,
    pub diagnostic_aliases: Vec<DiagnosticAliasStatementAttribute<D>>,
    pub help: Vec<HelpStatementAttribute<D>>,
    pub help_group: Option<HelpGroupStatementAttribute<D>>,
    pub on_unimplemented: Option<OnUnimplementedStatementAttribute<D>>,
    pub on_mismatch: Option<OnMismatchStatementAttribute<D>>,
    pub specialize: Option<SpecializeStatementAttribute<D>>,
    pub allow_overlapping_instances: Option<AllowOverlappingInstancesStatementAttribute<D>>,
    pub operator_precedence: Option<OperatorPrecedenceStatementAttribute<D>>,
    pub keyword: Option<KeywordStatementAttribute<D>>,
    pub contextual: Option<ContextualStatementAttribute<D>>,
}

impl<D: Driver> Default for StatementAttributes<D> {
    fn default() -> Self {
        Self {
            language_item: Default::default(),
            diagnostic_item: Default::default(),
            diagnostic_aliases: Default::default(),
            help: Default::default(),
            help_group: Default::default(),
            on_unimplemented: Default::default(),
            on_mismatch: Default::default(),
            specialize: Default::default(),
            allow_overlapping_instances: Default::default(),
            operator_precedence: Default::default(),
            keyword: Default::default(),
            contextual: Default::default(),
        }
    }
}

use crate::analysis::ast::{
    AllowOverlappingInstancesStatementAttribute, ContextualStatementAttribute,
    DiagnosticAliasStatementAttribute, DiagnosticItemStatementAttribute,
    HelpGroupStatementAttribute, HelpStatementAttribute, KeywordStatementAttribute,
    LanguageItemStatementAttribute, NoStdFileAttribute, OnMismatchStatementAttribute,
    OnUnimplementedStatementAttribute, OperatorPrecedenceStatementAttribute,
    RecursionLimitFileAttribute, SpecializeStatementAttribute,
};

#[derive(Debug, Clone, Default)]
pub struct FileAttributes {
    pub no_std: Option<NoStdFileAttribute>,
    pub recursion_limit: Option<RecursionLimitFileAttribute>,
}

#[derive(Debug, Clone, Default)]
pub struct StatementAttributes {
    pub language_item: Option<LanguageItemStatementAttribute>,
    pub diagnostic_item: Option<DiagnosticItemStatementAttribute>,
    pub diagnostic_aliases: Vec<DiagnosticAliasStatementAttribute>,
    pub help: Vec<HelpStatementAttribute>,
    pub help_group: Option<HelpGroupStatementAttribute>,
    pub on_unimplemented: Option<OnUnimplementedStatementAttribute>,
    pub on_mismatch: Option<OnMismatchStatementAttribute>,
    pub specialize: Option<SpecializeStatementAttribute>,
    pub allow_overlapping_instances: Option<AllowOverlappingInstancesStatementAttribute>,
    pub operator_precedence: Option<OperatorPrecedenceStatementAttribute>,
    pub keyword: Option<KeywordStatementAttribute>,
    pub contextual: Option<ContextualStatementAttribute>,
}

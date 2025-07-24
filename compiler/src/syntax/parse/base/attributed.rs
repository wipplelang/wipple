use crate::{
    syntax::{
        Location,
        parse::{
            Attribute, Attributed,
            base::{ParseFn, Rule},
        },
        tokenize::TokenTree,
    },
    util::{DefaultFromInfo, WithInfo},
};

impl<Output: 'static> Rule<Output>
where
    Output: DefaultFromInfo + 'static,
{
    pub fn attributed_with(self, parse_attribute: Rule<Attribute>) -> Rule<Attributed<Output>> {
        Rule {
            doc: None,
            syntax_kind: self.syntax_kind.clone(),
            rendered: self.rendered.clone(),
            backtracks: self.backtracks.clone(),
            parse: ParseFn::new(
                {
                    let parse_attribute = parse_attribute.clone();
                    let parse_value = self.clone();

                    move |parser, mut tree, stack, _| {
                        let mut attributes = Vec::new();
                        while let TokenTree::Attribute(attribute, contents) = tree.item {
                            let attribute =
                                parse_attribute.parse(parser, attribute.as_deref(), stack, None);

                            attributes.push(attribute);

                            tree = contents.as_deref();
                        }

                        let output = match parse_value.try_parse(parser, tree, stack, None)? {
                            Ok(prefix) => prefix,
                            Err(progress) => return Some(Err(progress)),
                        };

                        let info = attributes.first().map_or_else(
                            || output.info.clone(),
                            |attribute| {
                                Location::merge(attribute.info.clone(), output.info.clone())
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
                move |parser, mut tree, stack, _| {
                    let mut attributes = Vec::new();
                    while let TokenTree::Attribute(attribute, contents) = tree.item {
                        let attribute =
                            parse_attribute.parse(parser, attribute.as_deref(), stack, None);

                        attributes.push(attribute);

                        tree = contents.as_deref();
                    }

                    let output = self.parse(parser, tree, stack, None);

                    let info = attributes.first().map_or_else(
                        || output.info.clone(),
                        |attribute| Location::merge(attribute.info.clone(), output.info.clone()),
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

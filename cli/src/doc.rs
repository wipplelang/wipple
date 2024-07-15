pub fn json(
    title: &str,
    interface: wipple_driver::Interface,
    filter: Option<glob::Pattern>,
) -> serde_json::Value {
    let render = wipple_render::Render::new();

    render.update(interface, Vec::new(), None);

    let constant_declaration = |interface: &wipple_driver::Interface,
                                name: &str,
                                path: &wipple_driver::lower::Path| {
        let declaration = interface
            .constant_declarations
            .get(path)?
            .clone()
            .map(|declaration| wipple_render::AnyDeclaration {
                name: Some(name.to_string()),
                path: path.clone(),
                kind: wipple_render::AnyDeclarationKind::Constant(declaration),
            });

        let declaration_string = render.render_declaration(&declaration)?;
        let documentation = render.render_documentation(&declaration)?;

        Some(serde_json::json!({
            "type": "constant",
            "name": name,
            "declaration": declaration_string,
            "documentation": markdown::to_html(&documentation.docs),
            "example": documentation.example,
        }))
    };

    let type_declaration =
        |interface: &wipple_driver::Interface, name: &str, path: &wipple_driver::lower::Path| {
            let declaration = interface
                .type_declarations
                .get(path)?
                .clone()
                .map(|declaration| wipple_render::AnyDeclaration {
                    name: Some(name.to_string()),
                    path: path.clone(),
                    kind: wipple_render::AnyDeclarationKind::Type(declaration),
                });

            let declaration_string = render.render_declaration(&declaration)?;
            let documentation = render.render_documentation(&declaration)?;

            Some(serde_json::json!({
                "type": "type",
                "name": name,
                "declaration": declaration_string,
                "documentation": markdown::to_html(&documentation.docs),
                "example": documentation.example,
            }))
        };

    let trait_declaration =
        |interface: &wipple_driver::Interface, name: &str, path: &wipple_driver::lower::Path| {
            let declaration = interface
                .trait_declarations
                .get(path)?
                .clone()
                .map(|declaration| wipple_render::AnyDeclaration {
                    name: Some(name.to_string()),
                    path: path.clone(),
                    kind: wipple_render::AnyDeclarationKind::Trait(declaration),
                });

            let declaration_string = render.render_declaration(&declaration)?;
            let documentation = render.render_documentation(&declaration)?;

            Some(serde_json::json!({
                "type": "trait",
                "name": name,
                "declaration": declaration_string,
                "documentation": markdown::to_html(&documentation.docs),
                "example": documentation.example,
            }))
        };

    let mut items = render
        .with_interface(|interface| {
            interface
                .top_level
                .iter()
                .filter_map(move |(name, paths)| {
                    let item = paths
                        .iter()
                        .filter(|path| {
                            filter.as_ref().map_or(true, |filter| {
                                let file = match path.item.first() {
                                    Some(wipple_driver::lower::PathComponent::File(file)) => file,
                                    _ => return false,
                                };

                                filter.matches(file)
                            })
                        })
                        .find_map(|path| {
                            constant_declaration(interface, name, &path.item)
                                .or_else(|| type_declaration(interface, name, &path.item))
                                .or_else(|| trait_declaration(interface, name, &path.item))
                        })?;

                    Some((name.clone(), item))
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    items.sort_by_key(|(name, _)| name.clone().to_lowercase());

    serde_json::json!({
        "title": title,
        "items": items.into_iter().map(|(_, item)| item).collect::<Vec<_>>(),
    })
}

pub fn html(
    title: &str,
    interface: wipple_driver::Interface,
    template: &str,
    filter: Option<glob::Pattern>,
) -> anyhow::Result<String> {
    let model = json(title, interface, filter);

    handlebars::Handlebars::new()
        .render_template(template, &model)
        .map_err(Into::into)
}

TODO:

- add optional section-name expansion
- templates? can we add support here?
  maybe like "alias", but instead "template".
  then within the struct, "template" means "use defaults from that other entry"
  on the struct itself we can use #[serde(default = "path")] with SomeTrait::some_default
  #[serde(default = "Template::template")]  ?

DONE:
- add globbing to include.
- document 'include'.


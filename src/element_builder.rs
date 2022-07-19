use xmltree::{Element, XMLNode};

pub trait Builder {
  fn builder(name: &str) -> ElementBuilder;
}

impl Builder for Element {
  fn builder(name: &str) -> ElementBuilder {
    ElementBuilder(Element::new(name))
  }
}

pub struct ElementBuilder(Element);

impl ElementBuilder {
  pub fn text(mut self, text: impl Into<String>) -> Self {
    self.0.children.push(XMLNode::Text(text.into()));
    self
  }
  pub fn comment(mut self, text: impl Into<String>) -> Self {
    self.0.children.push(XMLNode::Comment(text.into()));
    self
  }
  pub fn prefix(mut self, prefix: impl Into<String>) -> Self {
    self.0.prefix = Some(prefix.into());
    self
  }

  /// Sets an attribute.
  pub fn attr(mut self, name: impl Into<String>, value: impl Into<String>) -> ElementBuilder {
    self.0.attributes.insert(name.into(), value.into());
    self
  }

  pub fn attrs(mut self, iter: impl IntoIterator<Item = (String, String)>) -> ElementBuilder {
    for (name, value) in iter {
      self.0.attributes.insert(name, value);
    }
    self
  }

  /// Appends anything implementing `Into<XMLNode>` into the tree.
  pub fn append(mut self, node: impl Into<XMLNode>) -> ElementBuilder {
    self.0.children.push(node.into());
    self
  }

  pub fn optional_append(mut self, node: Option<impl Into<XMLNode>>) -> ElementBuilder {
    if let Some(node) = node {
      self.0.children.push(node.into());
    }
    self
  }

  /// Appends an iterator of things implementing `Into<XMLNode>` into the tree.
  pub fn append_all<T: Into<XMLNode>>(
    mut self,
    iter: impl IntoIterator<Item = T>,
  ) -> ElementBuilder {
    for node in iter {
      self.0.children.push(node.into());
    }
    self
  }

  /// Builds the `Element`.
  pub fn build(self) -> Element {
    self.0
  }
  pub fn to_node(self) -> XMLNode {
    XMLNode::Element(self.0)
  }
}

impl From<ElementBuilder> for XMLNode {
  fn from(b: ElementBuilder) -> Self {
    b.to_node()
  }
}

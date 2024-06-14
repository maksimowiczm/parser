use crate::query::Query;

#[derive(Default)]
pub enum ResultType {
    Single(String),
    #[default]
    Boolean,
}

#[derive(Default)]
pub struct QueryBuilder {
    declarations: Vec<(String, Vec<String>)>,
    result_type: ResultType,
    follows: Vec<(String, String)>,
    parent: Vec<(String, String)>,
}

impl QueryBuilder {
    pub(crate) fn add_declaration(&mut self, declaration: (String, Vec<String>)) {
        self.declarations.push(declaration);
    }

    pub(crate) fn set_result(&mut self, result: ResultType) {
        self.result_type = result;
    }

    pub(crate) fn add_follows(&mut self, predecessor: String, follower: String) {
        self.follows.push((predecessor, follower));
    }

    pub(crate) fn add_parent(&mut self, parent: String, child: String) {
        self.parent.push((parent, child));
    }

    pub(crate) fn build(self) -> Query {
        Query {}
    }
}

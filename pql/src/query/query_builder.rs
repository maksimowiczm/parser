use crate::query::Query;
#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
pub trait QueryBuilder {
    fn add_declaration(&mut self, declaration: (String, Vec<String>));
    fn set_result(&mut self, result: ResultType);
    fn add_follows(&mut self, predecessor: String, follower: String);
    fn add_parent(&mut self, parent: String, child: String);
    fn build(&self) -> Query;
}

#[derive(Default)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum ResultType {
    Single(String),
    #[default]
    Boolean,
}

#[derive(Default)]
pub struct QueryBuilderImpl {
    declarations: Vec<(String, Vec<String>)>,
    result_type: ResultType,
    follows: Vec<(String, String)>,
    parent: Vec<(String, String)>,
}

impl QueryBuilder for QueryBuilderImpl {
    fn add_declaration(&mut self, declaration: (String, Vec<String>)) {
        self.declarations.push(declaration);
    }

    fn set_result(&mut self, result: ResultType) {
        self.result_type = result;
    }

    fn add_follows(&mut self, predecessor: String, follower: String) {
        self.follows.push((predecessor, follower));
    }

    fn add_parent(&mut self, parent: String, child: String) {
        self.parent.push((parent, child));
    }

    fn build(&self) -> Query {
        Query {}
    }
}

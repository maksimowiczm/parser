use crate::query::query_argument::{Argument, Declaration};
use crate::query::query_builder::ResultType;
use crate::query::query_declaration::QueryDeclaration;
use crate::query::{Query, QueryDeclarationVariant};
use pkb::pkb_context::PkbContext;
use std::collections::HashMap;
use std::rc::Rc;

impl Query {
    pub fn execute(&self) {
        let select = match &self.result_type {
            ResultType::Single(declaration) => self.declarations.get(declaration).unwrap(),
            ResultType::Boolean => {
                unimplemented!()
            }
        };

        self.execute_select(select)
    }

    fn execute_select(&self, select: &Declaration) {
        let input = match select {
            Declaration::Statement(_) => self.pkb_context.get_statements(),
            Declaration::Assign(_) => self.pkb_context.get_assigns(),
            Declaration::While(_) => self.pkb_context.get_whiles(),
            Declaration::If(_) => self.pkb_context.get_ifs(),
        };

        let concrete_queries = input
            .iter()
            .map(|statement| {
                let argument = Argument::transform(statement);
                let mut table = HashMap::new();
                table.insert(select.clone(), argument);

                let xd = self.queries.iter().cloned().collect::<Vec<_>>();

                ConcreteQuery::new(self.pkb_context.clone(), xd, table)
            })
            .collect::<Vec<_>>();

        let xd = concrete_queries
            .iter()
            .map(|cq| cq.execute())
            .collect::<Vec<_>>();
        dbg!(xd);

        todo!()
    }
}

struct ConcreteQuery {
    context: Rc<PkbContext>,
    queries: Vec<QueryDeclarationVariant>,
    table: HashMap<Declaration, Argument>,
}

#[allow(dead_code, unused_variables)]
impl ConcreteQuery {
    fn new(
        context: Rc<PkbContext>,
        queries: Vec<QueryDeclarationVariant>,
        table: HashMap<Declaration, Argument>,
    ) -> Self {
        let mut queries = queries.iter().cloned().collect::<Vec<_>>();
        queries.iter_mut().for_each(|q| match q {
            QueryDeclarationVariant::Parent(ref mut query_declaration) => {
                for (k, v) in table.iter() {
                    query_declaration.set_argument(k.clone(), v.clone());
                }
            }
        });

        Self {
            context,
            queries,
            table,
        }
    }

    fn execute(&self) -> Vec<HashMap<Declaration, Argument>> {
        let executable_queries = self
            .queries
            .iter()
            .filter(|q| q.is_executable())
            .collect::<Vec<_>>();

        if executable_queries.iter().any(|q| !q.execute(&self.context)) {
            return vec![];
        }

        let queries = self
            .queries
            .iter()
            .filter(|q| !executable_queries.contains(q))
            .collect::<Vec<_>>();

        if queries.is_empty() {
            return vec![self.table.clone()];
        }

        let rest_declarations = queries.iter().map(|q| q.get_declaration());

        todo!()
    }
}

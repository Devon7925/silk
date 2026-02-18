use silk::syntax::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn user_implementations_work_on_type_aliases() {
    let program = "
        Meters := i32 @ {
            square = (self: i32) => self * self,
        };

        foo: Meters := 5;
        foo.square
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 25),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn user_implementations_work_on_struct_types() {
    let program = "
        Pair := { first = i32, second = i32 } @ {
            sum = (self: { first = i32, second = i32 }) => self.first + self.second,
        };

        pair: Pair := { first = 3, second = 4 };
        pair.sum
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 7),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn user_implementations_can_return_structs() {
    let program = "
        Swapped := { first = i32, second = i32 } @ {
            swap = (self: { first = i32, second = i32 }) => (
                {
                    first = self.second,
                    second = self.first,
                }
            ),
        };

        pair: Swapped := { first = 1, second = 2 };
        pair.swap.first
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 2),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}


use silk::parsing::{ExpressionKind, ExpressionLiteral};
use silk::test_support::evaluate_text_to_expression;

#[test]
fn trait_constraints_allow_generic_implementations() {
    let program = "
Adder := (T: type) => (
    { add = (T) -> ((T) -> T) }
);

Meters := i32 @ {
    add = (self: i32) => ((other: i32) => self + other),
};

sum := (T: type @ Adder) => (
    ({ left: T, right: T }) => left.add(right)
);

sum(Meters){ 3, 4 }
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 7),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn trait_constraints_reject_missing_implementations() {
    let program = "
Adder := (T: type) => (
    { add = (T) -> ((T) -> T) }
);

noop := (T: type @ Adder) => (
    (value: T) => value
);

noop(i32)(1)
    ";

    let err = evaluate_text_to_expression(program).expect_err("expected trait error");
    assert_eq!(
        err.message,
        "Type does not implement trait: missing field add"
    );
}

#[test]
fn trait_constraints_support_multiple_types() {
    let program = "
Adder := (T: type) => (
    { add = (T) -> ((T) -> T) }
);

Meters := i32 @ {
    add = (self: i32) => ((other: i32) => self + other),
};

Seconds := i32 @ {
    add = (self: i32) => ((other: i32) => self + other),
};

add_pair := (T: type @ Adder) => (
    ({ left: T, right: T }) => left.add(right)
);

add_pair(Meters){ 3, 4 } + add_pair(Seconds){ 5, 9 }
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 21),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

#[test]
fn trait_constraints_support_struct_implementations() {
    let program = "
Adder := (T: type) => (
    { add = (T) -> ((T) -> T) }
);

Pair := { left = i32, right = i32 } @ {
    add = (self: { left = i32, right = i32 }) => (
        (other: { left = i32, right = i32 }) => (
            { left = self.left + other.left, right = self.right + other.right }
        )
    ),
};

add_pair := (T: type @ Adder) => (
    ({ left: T, right: T }) => left.add(right)
);

adder := add_pair(Pair);
result := adder{
    { left = 1, right = 2 },
    { left = 3, right = 4 },
};
result.left
    ";

    let (expr, _) =
        evaluate_text_to_expression(program).unwrap_or_else(|err| panic!("{}", err.message));

    match expr.kind {
        ExpressionKind::Literal(ExpressionLiteral::Number(value)) => assert_eq!(value, 4),
        other => panic!("Expected numeric literal, got {:?}", other),
    }
}

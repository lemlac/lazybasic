export enum TokenTag {
    SYMBOL = "SYMBOL",
    WORD = "WORD",
    NUMBER_LITERAL = "NUMBER_LITERAL",
    STRING_LITERAL = "STRING_LITERAL",
    FUNCTION = "FUNCTION",
    END = "END",
    RETURN = "RETURN",
    PARAMETER = "PARAMETER",
    OP = "OP",
    B_START = "B_START",
    B_END = "B_END",
    B_SEQUENCE = "B_SEQUENCE",
    LINE_BREAK = "LINE_BREAK",
}

export enum Op {
    SET = "SET",
    ACCESS = "ACCESS",
    INDEX = "INDEX",
    CALL = "CALL",
    DELIM = "DELIM",
    KEY_DELIM = "DELIM",
    ADD = "ADD",
    SUBTRACT = "SUBTRACT",
    MULTIPLY = "MULTIPLY",
    DIVIDE = "DIVIDE",
    MODULO = "MODULO",
    POS = "POS",
    NEG = "NEG",
    EQ = "EQ",
    NEQ = "NEQ",
    LT = "LT",
    GT = "GT",
    LTE = "LTE",
    GTE = "GTE",
    AND = "AND",
    OR = "OR",
    NOT = "NOT",
    B_AND = "B_AND",
    B_OR = "B_OR",
    X_OR = "X_OR",
    B_NOT = "B_NOT",
    L_SHIFT = "L_SHIFT",
    R_SHIFT = "R_SHIFT",
    SET_ADD = "SET_ADD",
    SET_SUBTRACT = "SET_SUBTRACT",
    SET_MULTIPLY = "SET_MULTIPLY",
    SET_DIVIDE = "SET_DIVIDE",
}

export enum BracketType {
    ROUND = "()",
    SQUARE = "[]",
    CURLY = "{}",
}

export type Token = 
    | { tag: TokenTag.SYMBOL, symbol?: string }
    | { tag: TokenTag.WORD, word?: string }
    | { tag: TokenTag.NUMBER_LITERAL, number?: number }
    | { tag: TokenTag.STRING_LITERAL, string?: string }
    | { tag: TokenTag.B_START, bracketStart?: '('|'['|'{' }
    | { tag: TokenTag.B_END, bracketEnd?: ')'|']'|'}' }
    | { tag: TokenTag.B_SEQUENCE, bracket?: BracketType, sequrence?: TokenWithTracking[] }
    | { tag: TokenTag.FUNCTION, name?: string, body?: TokenWithTracking[] }
    | { tag: TokenTag.OP, op?: Op, lhs?: TokenWithTracking, rhs?: TokenWithTracking }
    | { tag: TokenTag.END | TokenTag.RETURN | TokenTag.PARAMETER | TokenTag.LINE_BREAK }

export type Position = {
    line: number,
    col: number,
    idx: number,
};

export type TokenWithTracking = Token & { pos: Position };

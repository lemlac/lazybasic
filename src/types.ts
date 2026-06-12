export enum TokenTag {
    SYMBOL = "SYMBOL",
    WORD = "WORD",
    NUMBER_LITERAL = "NUMBER_LITERAL",
    STRING_LITERAL = "STRING_LITERAL",
    FUNCTION = "FUNCTION",
    END = "END",
    RETURN = "RETURN",
    PARAMETER = "PARAMETER",
    B_START = "B_START",
    B_END = "B_END",
    LINE_BREAK = "LINE_BREAK",
}

export type Token = 
    | { tag: TokenTag.SYMBOL, symbol?: string }
    | { tag: TokenTag.WORD, word?: string }
    | { tag: TokenTag.NUMBER_LITERAL, number?: number }
    | { tag: TokenTag.STRING_LITERAL, string?: string }
    | { tag: TokenTag.B_START, bracketStart?: '('|'['|'{' }
    | { tag: TokenTag.B_END, bracketEnd?: ')'|']'|'}' }
    | { tag: TokenTag.FUNCTION | TokenTag.END | TokenTag.RETURN | TokenTag.PARAMETER | TokenTag.LINE_BREAK }

export type TokenWithTracking = {
    token: Token,
    pos: {
        line: number,
        col: number,
        idx: number,
    },
};

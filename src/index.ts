enum TokenTag {
    SYMBOL,
    WORD,
    NUMBER_LITERAL,
    STRING_LITERAL,
    FUNCTION,
    END,
    RETURN,
    B_START,
    B_END,
    LINE_BREAK,
}

type Token = 
    | { tag: TokenTag.SYMBOL, symbol?: string }
    | { tag: TokenTag.WORD, word?: string }
    | { tag: TokenTag.NUMBER_LITERAL, number?: number }
    | { tag: TokenTag.STRING_LITERAL, string?: string }
    | { tag: TokenTag.B_START, bracketStart?: '('|'['|'{' }
    | { tag: TokenTag.B_END, bracketEnd?: ')'|']'|'}' }
    | { tag: TokenTag.FUNCTION | TokenTag.END | TokenTag.RETURN | TokenTag.LINE_BREAK }

export function tokenize(input: string): Token[] {
    let tokens: Token[] = [];
    let chunk = '';
    let isComment = false;
    let token: Token | null = null;
    for (let c of input) {
        if (isComment) {
            if (c === '\n') {
                isComment = false;
                tokens.push({ tag: TokenTag.LINE_BREAK });
            }
            continue;
        }
        if (c === "'") {
            isComment = true;
            if (token != null) {
                switch (token.tag) {
                    case TokenTag.WORD:
                        token.word = chunk;
                        break;
                    case TokenTag.SYMBOL:
                        token.symbol = chunk;
                }
                tokens.push(token);
                chunk = '';
                token = null;
            }
            continue;
        }
        if (token != null) {
            switch (token.tag) {
                case TokenTag.WORD:
                    if (isAlphanumeric(c)) {
                        chunk += c;
                        continue;
                    }
                    token.word = chunk;
                    break;
                case TokenTag.SYMBOL:
                    if (isSymbol(c)) {
                        chunk += c;
                        continue;
                    }
                    token.symbol = chunk;
            }
            tokens.push(token);
            chunk = '';
            token = null;
        }
        if (isAlphanumeric(c)) {
            token = { tag: TokenTag.WORD };
            chunk = c;
        } else if (isSymbol(c)) {
            token = { tag: TokenTag.SYMBOL };
            chunk = c;
        } else if (c === '\n') {
            tokens.push({ tag: TokenTag.LINE_BREAK });
        } else if (!isWhitespace(c)) {
            throw new Error(`Unexpected token: ${c}`);
        }
    }
    return tokens;
}

function isWhitespace(s: string): boolean {
    return /\s/.test(s);
}

function isAlphanumeric(s: string): boolean {
    return /\w/.test(s);
}

function isSymbol(s: string): boolean {
    return /[~!@#$%^&*-+=\\|,<.>/?]/.test(s);
}

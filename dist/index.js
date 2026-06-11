"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.tokenize = tokenize;
var TokenTag;
(function (TokenTag) {
    TokenTag[TokenTag["SYMBOL"] = 0] = "SYMBOL";
    TokenTag[TokenTag["WORD"] = 1] = "WORD";
    TokenTag[TokenTag["NUMBER_LITERAL"] = 2] = "NUMBER_LITERAL";
    TokenTag[TokenTag["STRING_LITERAL"] = 3] = "STRING_LITERAL";
    TokenTag[TokenTag["FUNCTION"] = 4] = "FUNCTION";
    TokenTag[TokenTag["END"] = 5] = "END";
    TokenTag[TokenTag["RETURN"] = 6] = "RETURN";
    TokenTag[TokenTag["B_START"] = 7] = "B_START";
    TokenTag[TokenTag["B_END"] = 8] = "B_END";
    TokenTag[TokenTag["LINE_BREAK"] = 9] = "LINE_BREAK";
})(TokenTag || (TokenTag = {}));
function tokenize(input) {
    let tokens = [];
    let chunk = '';
    let isComment = false;
    let token = null;
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
        }
        else if (isSymbol(c)) {
            token = { tag: TokenTag.SYMBOL };
            chunk = c;
        }
        else if (c === '\n') {
            tokens.push({ tag: TokenTag.LINE_BREAK });
        }
        else if (!isWhitespace(c)) {
            throw new Error(`Unexpected token: ${c}`);
        }
    }
    return tokens;
}
function isWhitespace(s) {
    return /\s/.test(s);
}
function isAlphanumeric(s) {
    return /\w/.test(s);
}
function isSymbol(s) {
    return /[~!@#$%^&*-+=\\|,<.>/?]/.test(s);
}
//# sourceMappingURL=index.js.map
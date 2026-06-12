"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.tokenize = tokenize;
var TokenTag;
(function (TokenTag) {
    TokenTag["SYMBOL"] = "SYMBOL";
    TokenTag["WORD"] = "WORD";
    TokenTag["NUMBER_LITERAL"] = "NUMBER_LITERAL";
    TokenTag["STRING_LITERAL"] = "STRING_LITERAL";
    TokenTag["FUNCTION"] = "FUNCTION";
    TokenTag["END"] = "END";
    TokenTag["RETURN"] = "RETURN";
    TokenTag["PARAMETER"] = "PARAMETER";
    TokenTag["B_START"] = "B_START";
    TokenTag["B_END"] = "B_END";
    TokenTag["LINE_BREAK"] = "LINE_BREAK";
})(TokenTag || (TokenTag = {}));
const keywords = new Map([
    ['function', TokenTag.FUNCTION],
    ['end', TokenTag.END],
    ['return', TokenTag.RETURN],
    ['parameter', TokenTag.PARAMETER],
]);
function tokenize(input) {
    let tokens = [];
    let chunk = '';
    let isComment = false;
    let token = null;
    let line = 0;
    let col = 0;
    let idx = 0;
    let ignoreLineBreaks = false;
    let contexts = [ignoreLineBreaks];
    for (let c of input.replace(/\n?$/, '\n')) {
        if (isComment) {
            if (c === '\n') {
                isComment = false;
                if (!ignoreLineBreaks) {
                    tokens.push({
                        token: { tag: TokenTag.LINE_BREAK },
                        pos: { line, col, idx },
                    });
                }
                col = 0;
                line += 1;
            }
            idx += 1;
            continue;
        }
        if (c === "'") {
            isComment = true;
            if (token != null) {
                switch (token.tag) {
                    case TokenTag.WORD:
                        const tag = keywords.get(chunk.toLowerCase());
                        if (tag != null) {
                            token.tag = tag;
                        }
                        else {
                            token.word = chunk;
                        }
                        break;
                    case TokenTag.SYMBOL:
                        token.symbol = chunk;
                }
                tokens.push({ token, pos: { line, col, idx } });
                chunk = '';
                token = null;
            }
            col += 1;
            idx += 1;
            continue;
        }
        if (token != null) {
            switch (token.tag) {
                case TokenTag.WORD:
                    if (isAlphanumeric(c)) {
                        chunk += c;
                        col += 1;
                        idx += 1;
                        continue;
                    }
                    const tag = keywords.get(chunk.toLowerCase());
                    if (tag != null) {
                        token.tag = tag;
                        if (tag === TokenTag.FUNCTION) {
                            ignoreLineBreaks = false;
                            contexts.push(ignoreLineBreaks);
                        }
                        else if (tag === TokenTag.END) {
                            if (contexts.length <= 1) {
                                throw new Error(`Unmatched token: "${chunk}" @ ${line}:${col + 1 - chunk.length}`);
                            }
                            contexts.pop();
                            ignoreLineBreaks = contexts[contexts.length - 1];
                        }
                    }
                    else {
                        token.word = chunk;
                    }
                    break;
                case TokenTag.SYMBOL:
                    if (isSymbol(c)) {
                        chunk += c;
                        col += 1;
                        idx += 1;
                        continue;
                    }
                    token.symbol = chunk;
            }
            tokens.push({ token, pos: { line, col, idx } });
            chunk = '';
            token = null;
        }
        if (c === '\n') {
            if (!ignoreLineBreaks) {
                tokens.push({
                    token: { tag: TokenTag.LINE_BREAK },
                    pos: { line, col, idx },
                });
            }
            col = 0;
            line += 1;
            idx += 1;
            continue;
        }
        if (c === '(' || c === '[' || c === '{') {
            tokens.push({
                token: { tag: TokenTag.B_START, bracketStart: c },
                pos: { line, col, idx },
            });
            ignoreLineBreaks = true;
            contexts.push(ignoreLineBreaks);
        }
        else if (c === ')' || c === ']' || c === '}') {
            let foundBracket = false;
            for (let i = tokens.length - 1; i >= 0; i--) {
                const token_i = tokens[i].token;
                if (token_i.tag === TokenTag.B_START) {
                    let start = token_i.bracketStart;
                    let end = c;
                    if ((start === '(' && end !== ')') ||
                        (start === '[' && end !== ']') ||
                        (start === '{' && end !== '}')) {
                        throw new Error(`Unmatched bracket: "${start}" -> "${end}" @ ${line}:${col}`);
                    }
                    foundBracket = true;
                    break;
                }
            }
            if (!foundBracket || contexts.length <= 1) {
                throw new Error(`Unmatched bracket: "${c}" @ ${line}:${col}`);
            }
            contexts.pop();
            ignoreLineBreaks = contexts[contexts.length - 1];
            tokens.push({
                token: { tag: TokenTag.B_END, bracketEnd: c },
                pos: { line, col, idx },
            });
        }
        else if (isAlphanumeric(c)) {
            token = { tag: TokenTag.WORD };
            chunk = c;
        }
        else if (isSymbol(c)) {
            token = { tag: TokenTag.SYMBOL };
            chunk = c;
        }
        else if (!isWhitespace(c)) {
            throw new Error(`Unexpected token: ${c} @ ${line}:${col}`);
        }
        col += 1;
        idx += 1;
    }
    return tokens;
}
function isWhitespace(s) {
    return /^\s+$/.test(s);
}
function isAlphanumeric(s) {
    return /^\w+$/.test(s);
}
function isSymbol(s) {
    return /^[~!@#$%^&*-+=\\|,<.>/?]+$/.test(s);
}
//# sourceMappingURL=index.js.map
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Tokenizer = void 0;
exports.tokenize = tokenize;
const types_1 = require("./types");
const keywords = new Map([
    ['function', types_1.TokenTag.FUNCTION],
    ['end', types_1.TokenTag.END],
    ['return', types_1.TokenTag.RETURN],
    ['parameter', types_1.TokenTag.PARAMETER],
]);
class Tokenizer {
    constructor() {
        this.tokens = [];
        this.chunk = '';
        this.isComment = false;
        this.token = null;
        this.pos = {
            line: 0,
            col: 0,
            idx: 0,
        };
        this.ignoreLineBreaks = false;
        this.contexts = [this.ignoreLineBreaks];
    }
    copyPos() {
        return {
            line: this.pos.line,
            col: this.pos.col,
            idx: this.pos.idx,
        };
    }
    tokenize(input) {
        const info = this;
        for (let c of input) {
            if (info.isComment) {
                if (c === '\n') {
                    info.isComment = false;
                    if (!info.ignoreLineBreaks) {
                        info.tokens.push({
                            token: { tag: types_1.TokenTag.LINE_BREAK },
                            pos: info.copyPos(),
                        });
                    }
                    info.pos.col = 0;
                    info.pos.line += 1;
                }
                info.pos.idx += 1;
                continue;
            }
            if (c === "'") {
                info.isComment = true;
                if (info.token != null) {
                    switch (info.token.tag) {
                        case types_1.TokenTag.WORD:
                            const tag = keywords.get(info.chunk.toLowerCase());
                            if (tag != null) {
                                info.token.tag = tag;
                            }
                            else {
                                info.token.word = info.chunk;
                            }
                            break;
                        case types_1.TokenTag.SYMBOL:
                            info.token.symbol = info.chunk;
                    }
                    info.tokens.push({ token: info.token, pos: info.copyPos() });
                    info.chunk = '';
                    info.token = null;
                }
                info.pos.col += 1;
                info.pos.idx += 1;
                continue;
            }
            if (info.token != null) {
                switch (info.token.tag) {
                    case types_1.TokenTag.WORD:
                        if (isAlphanumeric(c)) {
                            info.chunk += c;
                            info.pos.col += 1;
                            info.pos.idx += 1;
                            continue;
                        }
                        const tag = keywords.get(info.chunk.toLowerCase());
                        if (tag != null) {
                            info.token.tag = tag;
                            if (tag === types_1.TokenTag.FUNCTION) {
                                info.ignoreLineBreaks = false;
                                info.contexts.push(info.ignoreLineBreaks);
                            }
                            else if (tag === types_1.TokenTag.END) {
                                if (info.contexts.length <= 1) {
                                    throw new Error(`Unmatched token: "${info.chunk}" @ ${info.pos.line}:${info.pos.col + 1 - info.chunk.length}`);
                                }
                                info.contexts.pop();
                                info.ignoreLineBreaks = info.contexts[info.contexts.length - 1];
                            }
                        }
                        else {
                            info.token.word = info.chunk;
                        }
                        break;
                    case types_1.TokenTag.SYMBOL:
                        if (isSymbol(c)) {
                            info.chunk += c;
                            info.pos.col += 1;
                            info.pos.idx += 1;
                            continue;
                        }
                        info.token.symbol = info.chunk;
                }
                info.tokens.push({ token: info.token, pos: info.copyPos() });
                info.chunk = '';
                info.token = null;
            }
            if (c === '\n') {
                if (!info.ignoreLineBreaks) {
                    info.tokens.push({
                        token: { tag: types_1.TokenTag.LINE_BREAK },
                        pos: info.copyPos(),
                    });
                }
                info.pos.col = 0;
                info.pos.line += 1;
                info.pos.idx += 1;
                continue;
            }
            if (c === '(' || c === '[' || c === '{') {
                info.tokens.push({
                    token: { tag: types_1.TokenTag.B_START, bracketStart: c },
                    pos: info.copyPos(),
                });
                info.ignoreLineBreaks = true;
                info.contexts.push(info.ignoreLineBreaks);
            }
            else if (c === ')' || c === ']' || c === '}') {
                let foundBracket = false;
                for (let i = info.tokens.length - 1; i >= 0; i--) {
                    const token_i = info.tokens[i].token;
                    if (token_i.tag === types_1.TokenTag.B_START) {
                        let start = token_i.bracketStart;
                        let end = c;
                        if ((start === '(' && end !== ')') ||
                            (start === '[' && end !== ']') ||
                            (start === '{' && end !== '}')) {
                            throw new Error(`Unmatched bracket: "${start}" -> "${end}" @ ${info.pos.line}:${info.pos.col}`);
                        }
                        foundBracket = true;
                        break;
                    }
                }
                if (!foundBracket || info.contexts.length <= 1) {
                    throw new Error(`Unmatched bracket: "${c}" @ ${info.pos.line}:${info.pos.col}`);
                }
                info.contexts.pop();
                info.ignoreLineBreaks = info.contexts[info.contexts.length - 1];
                info.tokens.push({
                    token: { tag: types_1.TokenTag.B_END, bracketEnd: c },
                    pos: info.copyPos(),
                });
            }
            else if (isAlphanumeric(c)) {
                info.token = { tag: types_1.TokenTag.WORD };
                info.chunk = c;
            }
            else if (isSymbol(c)) {
                info.token = { tag: types_1.TokenTag.SYMBOL };
                info.chunk = c;
            }
            else if (!isWhitespace(c)) {
                throw new Error(`Unexpected token: ${c} @ ${info.pos.line}:${info.pos.col}`);
            }
            info.pos.col += 1;
            info.pos.idx += 1;
        }
        return info.tokens;
    }
}
exports.Tokenizer = Tokenizer;
function tokenize(input) {
    return new Tokenizer().tokenize(input.replace(/\n?$/, '\n'));
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
//# sourceMappingURL=tokenizer.js.map
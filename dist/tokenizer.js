"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.Tokenizer = exports.TokenizerError = void 0;
exports.tokenize = tokenize;
exports.tokenizeStream = tokenizeStream;
exports.tokenizeFile = tokenizeFile;
const node_fs_1 = __importDefault(require("node:fs"));
const types_1 = require("./types");
const keywords = new Map([
    ['function', types_1.TokenTag.FUNCTION],
    ['end', types_1.TokenTag.END],
    ['return', types_1.TokenTag.RETURN],
    ['parameter', types_1.TokenTag.PARAMETER],
]);
class TokenizerError extends Error {
    constructor(message, pos, filename) {
        super();
        this.message = message;
        this.pos = pos;
        this.filename = filename ?? null;
    }
    toString() {
        return `${this.message} @ ${this.filename ?? ''}:${this.pos.line}:${this.pos.col}`;
    }
}
exports.TokenizerError = TokenizerError;
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
        this.filename = null;
    }
    copyPos() {
        return {
            line: this.pos.line,
            col: this.pos.col,
            idx: this.pos.idx,
        };
    }
    makeError(message) {
        return new TokenizerError(message, this.copyPos(), this.filename);
    }
    tokenize(input) {
        this.tokenizeChunk(input);
        return this.finish();
    }
    async tokenizeStream(readable) {
        for await (const chunk of readable) {
            this.tokenizeChunk(chunk);
        }
        return this.finish();
    }
    tokenizeChunk(input) {
        try {
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
                                        throw info.makeError(`Unmatched token: ${addQuotes(info.chunk)}`);
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
                                throw info.makeError(`Unmatched bracket: ${addQuotes(start)} -> ${addQuotes(end)}`);
                            }
                            foundBracket = true;
                            break;
                        }
                    }
                    if (!foundBracket || info.contexts.length <= 1) {
                        throw info.makeError(`Unmatched bracket: ${addQuotes(c)}`);
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
                    throw info.makeError(`Unexpected token: ${addQuotes(c)}`);
                }
                info.pos.col += 1;
                info.pos.idx += 1;
            }
        }
        catch (e) {
            this.reset();
            throw e;
        }
        return this;
    }
    finish() {
        try {
            const info = this;
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
            }
            if (this.contexts.length != 1) {
                throw info.makeError(`Unexepected end of script`);
            }
            return info.tokens;
        }
        finally {
            this.reset();
        }
    }
    reset() {
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
        this.filename = null;
    }
}
exports.Tokenizer = Tokenizer;
function tokenize(input) {
    return new Tokenizer().tokenize(input);
}
function tokenizeStream(stream) {
    return new Tokenizer().tokenizeStream(stream);
}
function tokenizeFile(filename, encoding = 'utf8') {
    const tokenizer = new Tokenizer();
    tokenizer.filename = filename;
    const stream = node_fs_1.default.createReadStream(filename);
    stream.setEncoding(encoding);
    return tokenizer.tokenizeStream(stream);
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
function addQuotes(s) {
    return s.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/^|$/g, '"');
}
//# sourceMappingURL=tokenizer.js.map
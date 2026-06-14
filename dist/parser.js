"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Parser = exports.ParsingError = void 0;
exports.parseTokens = parseTokens;
const types_1 = require("./types");
const helpers_1 = require("./helpers");
class ParsingError extends Error {
    constructor(message, pos, filename) {
        super();
        this.message = message;
        this.pos = Object.assign({}, pos);
        this.filename = filename ?? null;
    }
    toString() {
        return `${this.message} @ ${this.filename ?? ''}:${this.pos.line}:${this.pos.col}`;
    }
}
exports.ParsingError = ParsingError;
const ops = new Map([
    ['=', types_1.Op.SET],
    ['.', types_1.Op.ACCESS],
    [',', types_1.Op.DELIM],
    [':', types_1.Op.KEY_DELIM],
    ['+', types_1.Op.ADD],
    ['-', types_1.Op.SUBTRACT],
    ['*', types_1.Op.MULTIPLY],
    ['/', types_1.Op.DIVIDE],
    ['==', types_1.Op.EQ],
    ['<>', types_1.Op.NEQ],
    ['>', types_1.Op.GT],
    ['<', types_1.Op.LT],
    ['>=', types_1.Op.GTE],
    ['<=', types_1.Op.LTE],
    ['+=', types_1.Op.SET_ADD],
    ['-=', types_1.Op.SET_SUBTRACT],
    ['*=', types_1.Op.SET_MULTIPLY],
    ['/=', types_1.Op.SET_DIVIDE],
]);
const prefixOps = new Map([
    ['+', types_1.Op.POS],
    ['-', types_1.Op.NEG],
    ['not', types_1.Op.NOT],
    ['bnot', types_1.Op.B_NOT],
]);
// a + b * c
// (a + (b * c))
// (+ a b) * c
// (+ a (* b c))
// a + - b * c
// (+ a -) b * c
// (+ a (- _ b)) * c
// (* (+ a (- _ b)) c)
class Parser {
    constructor(tokens, filename) {
        this.tokens = tokens;
        this.filename = filename;
    }
    parse() {
        let [i, ret] = this.parsePart(0);
        if (i < this.tokens.length) {
            throw new ParsingError(`Ended unexpectedly`, ret[ret.length - 1]?.pos ?? { line: 0, col: i, idx: i }, this.filename);
        }
        return ret;
    }
    parsePart(i, tag) {
        let { tokens, filename } = this;
        let ret = [];
        loop: for (; i < tokens.length; i++) {
            let info = tokens[i];
            switch (info.tag) {
                case types_1.TokenTag.SYMBOL:
                    {
                        let symbol = info.symbol ?? '';
                        let pos = info.pos;
                        let isPrefix = ret.length <= 0;
                        let prev;
                        if (!isPrefix) {
                            prev = ret[ret.length - 1];
                            if (prev.tag === types_1.TokenTag.OP && prev.rhs == null) {
                                isPrefix = true;
                            }
                        }
                        let ops = Array.from(this.splitSymbols(symbol, pos, isPrefix));
                        if (!isPrefix) {
                            let infix;
                            [infix, ...ops] = ops;
                            ret.push(infix);
                        }
                        for (let next of ops) {
                            ret.push(next);
                        }
                    }
                    break;
                case types_1.TokenTag.B_START:
                    {
                        let sequence;
                        let tag = types_1.TokenTag.B_SEQUENCE;
                        i++;
                        [i, sequence] = this.parsePart(i, tag);
                        ret.push({
                            tag,
                            sequence,
                            bracket: info.bracketStart === '(' ? types_1.BracketType.ROUND :
                                info.bracketStart === '[' ? types_1.BracketType.SQUARE :
                                    info.bracketStart === '{' ? types_1.BracketType.CURLY : types_1.BracketType.ROUND,
                            pos: info.pos,
                        });
                    }
                    break;
                case types_1.TokenTag.LINE_BREAK:
                    {
                        if (ret.length > 0) {
                            let prev = ret[ret.length - 1];
                            if (prev.tag === types_1.TokenTag.LINE_BREAK) {
                                continue;
                            }
                        }
                        ret.push(info);
                    }
                    break;
                case types_1.TokenTag.B_END:
                    if (tag !== types_1.TokenTag.B_SEQUENCE) {
                        throw new ParsingError(`Unexpected bracket end`, info.pos, filename);
                    }
                    break loop;
                case types_1.TokenTag.FUNCTION:
                    {
                        let name;
                        if (info.name != null) {
                            name = info.name;
                        }
                        else {
                            i++;
                            let next = tokens[i];
                            if (next.tag === types_1.TokenTag.WORD) {
                                name = next.word;
                                i++;
                                next = tokens[i];
                            }
                            if (next.tag !== types_1.TokenTag.LINE_BREAK) {
                                throw new ParsingError(`Expected line break, found ${next.tag}`, next.pos, filename);
                            }
                        }
                        let tag = types_1.TokenTag.FUNCTION;
                        let body;
                        if (info.body != null) {
                            body = info.body;
                        }
                        else {
                            i++;
                            [i, body] = this.parsePart(i, tag);
                        }
                        ret.push({
                            tag,
                            name,
                            body,
                            pos: info.pos,
                        });
                    }
                    break;
                case types_1.TokenTag.END:
                    if (tag !== types_1.TokenTag.FUNCTION) {
                        throw new ParsingError(`Unexpected body end`, info.pos, filename);
                    }
                    break loop;
                default:
                    ret.push(info);
            }
        }
        while (ret.length > 0 && ret[0].tag === types_1.TokenTag.LINE_BREAK) {
            ret.splice(0, 1);
        }
        while (ret.length > 0 && ret[ret.length - 1].tag === types_1.TokenTag.LINE_BREAK) {
            ret.splice(ret.length - 1, 1);
        }
        return [i, ret];
    }
    *splitSymbols(symbol, pos, isPrefix = false) {
        let spill = '';
        let line = pos.line;
        let col = pos.col;
        let idx = pos.idx;
        while (symbol.length > 0) {
            let op = (isPrefix ? prefixOps : ops).get(symbol);
            if (op != null) {
                yield {
                    tag: types_1.TokenTag.OP,
                    op,
                    pos: { line, col, idx },
                };
                col += symbol.length;
                idx += symbol.length;
                symbol = spill;
                isPrefix = true;
            }
            else {
                spill = symbol[symbol.length - 1] + spill;
                symbol = symbol.slice(0, -1);
            }
        }
        if (spill.length > 0) {
            throw new ParsingError(`Expected symbol: ${(0, helpers_1.addQuotes)(spill)}`, { line, col, idx }, this.filename);
        }
    }
}
exports.Parser = Parser;
function parseTokens(tokens, filename) {
    return new Parser(tokens, filename).parse();
}
//# sourceMappingURL=parser.js.map
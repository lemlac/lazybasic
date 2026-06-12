"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ParsingError = void 0;
exports.parse = parse;
const types_1 = require("./types");
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
function parse(tokens, filename) {
    let [i, ret] = parsePart(0, tokens, filename);
    if (i < tokens.length) {
        throw new ParsingError(`Ended unexpectedly`, ret[ret.length - 1]?.pos ?? { line: 0, col: i, idx: i }, filename);
    }
    return ret;
}
function parsePart(i, tokens, filename, tag) {
    let ret = [];
    for (; i < tokens.length; i++) {
        let info = tokens[i];
        switch (info.tag) {
            case types_1.TokenTag.SYMBOL:
                {
                    let symbol = info.symbol ?? '';
                    let pos = info.pos;
                    for (let next of splitSymbols(symbol, pos, filename)) {
                        ret.push(next);
                    }
                }
                break;
            case types_1.TokenTag.B_START:
                {
                    let sequrence;
                    let tag = types_1.TokenTag.B_SEQUENCE;
                    i++;
                    [i, sequrence] = parsePart(i, tokens, filename, tag);
                    ret.push({
                        tag,
                        sequrence,
                        bracket: info.bracketStart === '(' ? types_1.BracketType.ROUND :
                            info.bracketStart === '[' ? types_1.BracketType.SQUARE :
                                info.bracketStart === '{' ? types_1.BracketType.CURLY : types_1.BracketType.ROUND,
                        pos: info.pos,
                    });
                }
                break;
            case types_1.TokenTag.B_END:
                if (tag !== types_1.TokenTag.B_SEQUENCE) {
                    throw new ParsingError(`Unexpected bracket end`, info.pos, filename);
                }
                return [i, ret];
            case types_1.TokenTag.FUNCTION:
                {
                    let name;
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
                    let body;
                    let tag = types_1.TokenTag.FUNCTION;
                    i++;
                    [i, body] = parsePart(i, tokens, filename, tag);
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
                return [i, ret];
            default:
                ret.push(info);
        }
    }
    return [i, ret];
}
function* splitSymbols(symbol, pos, filename) {
    let spill = '';
    let line = pos.line;
    let col = pos.col;
    let idx = pos.idx;
    while (symbol.length > 0) {
        let op = ops.get(symbol);
        if (op != null) {
            yield {
                tag: types_1.TokenTag.OP,
                op,
                pos: { line, col, idx },
            };
            col += symbol.length;
            idx += symbol.length;
            symbol = spill;
        }
        else {
            spill = symbol.charAt(-1) + spill;
            symbol = symbol.slice(0, -1);
        }
    }
    if (spill.length > 0) {
        throw new ParsingError(`Expected symbol: ${addQuotes(spill)}`, { line, col, idx }, filename);
    }
}
function addQuotes(s) {
    return s.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/^|$/g, '"');
}
//# sourceMappingURL=parser.js.map
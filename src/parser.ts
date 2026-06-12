import { TokenTag, Op, BracketType } from './types';
import type { TokenWithTracking, Position } from './types';
import { addQuotes } from './helpers';

export class ParsingError extends Error {
    message: string;
    filename: string | null;
    pos: Position;
    
    constructor(message: string, pos: Position, filename?: string | null | undefined) {
        super();
        this.message = message;
        this.pos = Object.assign({}, pos);
        this.filename = filename ?? null;
    }

    toString() {
        return `${this.message} @ ${this.filename ?? ''}:${this.pos.line}:${this.pos.col}`;
    }
}

const ops = new Map<string, Op>([
    ['=', Op.SET],
    ['.', Op.ACCESS],
    [',', Op.DELIM],
    [':', Op.KEY_DELIM],
    ['+', Op.ADD],
    ['-', Op.SUBTRACT],
    ['*', Op.MULTIPLY],
    ['/', Op.DIVIDE],
    ['==', Op.EQ],
    ['<>', Op.NEQ],
    ['>', Op.GT],
    ['<', Op.LT],
    ['>=', Op.GTE],
    ['<=', Op.LTE],
    ['+=', Op.SET_ADD],
    ['-=', Op.SET_SUBTRACT],
    ['*=', Op.SET_MULTIPLY],
    ['/=', Op.SET_DIVIDE],
]);

export class Parser {
    tokens: TokenWithTracking[];
    filename: string | null | undefined;

    constructor(tokens: TokenWithTracking[], filename?: string | null | undefined) {
        this.tokens = tokens;
        this.filename = filename;
    }

    parse(): TokenWithTracking[] {
        let [i, ret] = this.parsePart(0);
        if (i < this.tokens.length) {
            throw new ParsingError(`Ended unexpectedly`, ret[ret.length - 1]?.pos ?? { line: 0, col: i, idx: i }, this.filename);
        }
        return ret;
    }

    private parsePart(i: number, tag?: TokenTag): [number, TokenWithTracking[]] {
        let { tokens, filename } = this;
        let ret: TokenWithTracking[] = [];
        for (; i < tokens.length; i++) {
            let info = tokens[i];
            switch (info.tag) {
                case TokenTag.SYMBOL: {
                    let symbol = info.symbol ?? '';
                    let pos = info.pos;
                    for (let next of this.splitSymbols(symbol, pos)) {
                        ret.push(next);
                    }
                } break;
                case TokenTag.B_START: {
                    let sequrence: TokenWithTracking[];
                    let tag = TokenTag.B_SEQUENCE;
                    i++;
                    [i, sequrence] = this.parsePart(i, tag);
                    ret.push({
                        tag,
                        sequrence,
                        bracket:
                            info.bracketStart === '(' ? BracketType.ROUND : 
                            info.bracketStart === '[' ? BracketType.SQUARE : 
                            info.bracketStart === '{' ? BracketType.CURLY : BracketType.ROUND,
                        pos: info.pos,
                    });
                } break;
                case TokenTag.B_END:
                    if (tag !== TokenTag.B_SEQUENCE) {
                        throw new ParsingError(`Unexpected bracket end`, info.pos, filename);
                    }
                    return [i, ret];
                case TokenTag.FUNCTION: {
                    let name: string | undefined;
                    if (info.name != null) {
                        name = info.name;
                    } else {
                        i++;
                        let next = tokens[i];
                        if (next.tag === TokenTag.WORD) {
                            name = next.word;
                            i++;
                            next = tokens[i];
                        }
                        if (next.tag !== TokenTag.LINE_BREAK) {
                            throw new ParsingError(`Expected line break, found ${next.tag}`, next.pos, filename);
                        }
                    }
                    let tag = TokenTag.FUNCTION;
                    let body: TokenWithTracking[];
                    if (info.body != null) {
                        body = info.body;
                    } else {
                        i++;
                        [i, body] = this.parsePart(i, tag);
                    }
                    ret.push({
                        tag,
                        name,
                        body,
                        pos: info.pos,
                    });
                } break;
                case TokenTag.END:
                    if (tag !== TokenTag.FUNCTION) {
                        throw new ParsingError(`Unexpected body end`, info.pos, filename);
                    }
                    return [i, ret];
                default:
                    ret.push(info);
            }
        }
        return [i, ret];
    }

    private* splitSymbols(symbol: string, pos: Position): Generator<TokenWithTracking, void, unknown> {
        let spill = '';
        let line = pos.line;
        let col = pos.col;
        let idx = pos.idx;
        while (symbol.length > 0) {
            let op = ops.get(symbol);
            if (op != null) {
                yield {
                    tag: TokenTag.OP,
                    op,
                    pos: { line, col, idx },
                };
                col += symbol.length;
                idx += symbol.length;
                symbol = spill;
            } else {
                spill = symbol.charAt(-1) + spill;
                symbol = symbol.slice(0, -1);
            }
        }
        if (spill.length > 0) {
            throw new ParsingError(`Expected symbol: ${addQuotes(spill)}`, { line, col, idx }, this.filename);
        }
    }
}

export function parse(tokens: TokenWithTracking[], filename?: string | null | undefined): TokenWithTracking[] {
    return new Parser(tokens, filename).parse();
}

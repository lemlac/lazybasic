import fs from 'node:fs';
import type { ReadStream } from 'node:fs';

import { TokenTag } from './types';
import type { Token, TokenWithTracking, Position } from './types';

const keywords = new Map<string, TokenTag>([
    ['function', TokenTag.FUNCTION],
    ['end', TokenTag.END],
    ['return', TokenTag.RETURN],
    ['parameter', TokenTag.PARAMETER],
]);

export class TokenizerError extends Error {
    message: string;
    filename: string | null;
    pos: Position;
    
    constructor(message: string, pos: Position, filename?: string | null) {
        super();
        this.message = message;
        this.pos = pos;
        this.filename = filename ?? null;
    }

    toString() {
        return `${this.message} @ ${this.filename ?? ''}:${this.pos.line}:${this.pos.col}`;
    }
}

export class Tokenizer {
    tokens: TokenWithTracking[] = [];
    chunk = '';
    isComment = false;
    token: Token | null = null;
    pos: Position = {
        line: 0,
        col: 0,
        idx: 0,
    };
    ignoreLineBreaks = false;
    contexts = [this.ignoreLineBreaks];
    filename: string | null = null;

    copyPos(): Position {
        return {
            line: this.pos.line,
            col: this.pos.col,
            idx: this.pos.idx,
        };
    }

    makeError(message: string): TokenizerError {
        return new TokenizerError(message, this.copyPos(), this.filename); 
    }

    tokenize(input: string): TokenWithTracking[] {
        this.tokenizeChunk(input);
        return this.finish();
    }

    async tokenizeStream(readable: AsyncGenerator<string> | ReadStream): Promise<TokenWithTracking[]> {
        for await (const chunk of readable) {
            this.tokenizeChunk(chunk);
        }
        return this.finish();
    }

    tokenizeChunk(input: string): this {
        try {
            const info = this;

            for (let c of input) {
                if (info.isComment) {
                    if (c === '\n') {
                        info.isComment = false;

                        if (!info.ignoreLineBreaks) {
                            info.tokens.push({
                                tag: TokenTag.LINE_BREAK,
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
                            case TokenTag.WORD:
                                const tag = keywords.get(info.chunk.toLowerCase());
                                if (tag != null) {
                                    (info.token as Token).tag = tag;
                                } else {
                                    info.token.word = info.chunk;
                                }
                                break;
                            case TokenTag.SYMBOL:
                                info.token.symbol = info.chunk;
                        }

                        info.tokens.push(Object.assign({ pos: info.copyPos() }, info.token));
                        info.chunk = '';
                        info.token = null;
                    }

                    info.pos.col += 1;
                    info.pos.idx += 1;
                    continue;
                }

                if (info.token != null) {
                    switch (info.token.tag) {
                        case TokenTag.WORD:
                            if (isAlphanumeric(c)) {
                                info.chunk += c;
                                info.pos.col += 1;
                                info.pos.idx += 1;
                                continue;
                            }

                            const tag = keywords.get(info.chunk.toLowerCase());

                            if (tag != null) {
                                (info.token as Token).tag = tag;
                                if (tag === TokenTag.FUNCTION) {
                                    info.ignoreLineBreaks = false;
                                    info.contexts.push(info.ignoreLineBreaks);
                                } else if (tag === TokenTag.END) {
                                    if (info.contexts.length <= 1) {
                                        throw info.makeError(`Unmatched token: ${addQuotes(info.chunk)}`);
                                    }
                                    info.contexts.pop();
                                    info.ignoreLineBreaks = info.contexts[info.contexts.length - 1];
                                }
                            } else {
                                info.token.word = info.chunk;
                            }

                            break;
                        case TokenTag.SYMBOL:
                            if (isSymbol(c)) {
                                info.chunk += c;
                                info.pos.col += 1;
                                info.pos.idx += 1;
                                continue;
                            }
                            info.token.symbol = info.chunk;
                    }
                    info.tokens.push(Object.assign({ pos: info.copyPos() }, info.token));
                    info.chunk = '';
                    info.token = null;
                }

                if (c === '\n') {
                    if (!info.ignoreLineBreaks) {
                        info.tokens.push({
                            tag: TokenTag.LINE_BREAK,
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
                        tag: TokenTag.B_START,
                        bracketStart: c,
                        pos: info.copyPos(),
                    });
                    info.ignoreLineBreaks = true;
                    info.contexts.push(info.ignoreLineBreaks);
                } else if (c === ')' || c === ']' || c === '}') {
                    let foundBracket = false;

                    for (let i = info.tokens.length - 1; i >= 0; i--) {
                        const token_i = info.tokens[i];
                        if (token_i.tag === TokenTag.B_START) {
                            let start = token_i.bracketStart;
                            let end = c;
                            if (
                                (start === '(' && end !== ')') ||
                                (start === '[' && end !== ']') ||
                                (start === '{' && end !== '}')
                            ) {
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
                        tag: TokenTag.B_END,
                        bracketEnd: c,
                        pos: info.copyPos(),
                    });
                } else if (isAlphanumeric(c)) {
                    info.token = { tag: TokenTag.WORD };
                    info.chunk = c;
                } else if (isSymbol(c)) {
                    info.token = { tag: TokenTag.SYMBOL };
                    info.chunk = c;
                } else if (!isWhitespace(c)) {
                    throw info.makeError(`Unexpected token: ${addQuotes(c)}`);
                }

                info.pos.col += 1;
                info.pos.idx += 1;
            }
        } catch (e) {
            this.reset();
            throw e;
        }
        return this;
    }

    finish(): TokenWithTracking[] {
        try {
            const info = this;

            if (info.token != null) {
                switch (info.token.tag) {
                    case TokenTag.WORD:
                        const tag = keywords.get(info.chunk.toLowerCase());
                        if (tag != null) {
                            (info.token as Token).tag = tag;
                        } else {
                            info.token.word = info.chunk;
                        }
                        break;
                    case TokenTag.SYMBOL:
                        info.token.symbol = info.chunk;
                }

                info.tokens.push(Object.assign({ pos: info.copyPos() }, info.token));
            }

            if (this.contexts.length != 1) {
                throw info.makeError(`Unexepected end of script`);
            }

            return info.tokens;
        } finally {
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

export function tokenize(input: string): TokenWithTracking[] {
    return new Tokenizer().tokenize(input);
}

export function tokenizeStream(stream: AsyncGenerator<string> | ReadStream): Promise<TokenWithTracking[]> {
    return new Tokenizer().tokenizeStream(stream);
}

export function tokenizeFile(filename: string, encoding: BufferEncoding = 'utf8'): Promise<TokenWithTracking[]> {
    const tokenizer = new Tokenizer();
    tokenizer.filename = filename;
    const stream = fs.createReadStream(filename);
    stream.setEncoding(encoding);
    return tokenizer.tokenizeStream(stream);
}

function isWhitespace(s: string): boolean {
    return /^\s+$/.test(s);
}

function isAlphanumeric(s: string): boolean {
    return /^\w+$/.test(s);
}

function isSymbol(s: string): boolean {
    return /^[~!@#$%^&*-+=\\|,<.>/?]+$/.test(s);
}

function addQuotes(s: string): string {
    return s.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/^|$/g, '"');
}

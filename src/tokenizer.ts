import { TokenTag } from './types';
import type { Token, TokenWithTracking } from './types';

const keywords = new Map<string, TokenTag>([
    ['function', TokenTag.FUNCTION],
    ['end', TokenTag.END],
    ['return', TokenTag.RETURN],
    ['parameter', TokenTag.PARAMETER],
]);

export class Tokenizer {
    tokens: TokenWithTracking[] = [];
    chunk = '';
    isComment = false;
    token: Token | null = null;
    pos = {
        line: 0,
        col: 0,
        idx: 0,
    };
    ignoreLineBreaks = false;
    contexts = [this.ignoreLineBreaks];

    copyPos() {
        return {
            line: this.pos.line,
            col: this.pos.col,
            idx: this.pos.idx,
        };
    }

    tokenize(input: string): TokenWithTracking[] {
        const info = this;

        for (let c of input) {
            if (info.isComment) {
                if (c === '\n') {
                    info.isComment = false;

                    if (!info.ignoreLineBreaks) {
                        info.tokens.push({
                            token: { tag: TokenTag.LINE_BREAK },
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
                                    throw new Error(`Unmatched token: "${info.chunk}" @ ${info.pos.line}:${info.pos.col + 1 - info.chunk.length}`);
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
                info.tokens.push({ token: info.token, pos: info.copyPos() });
                info.chunk = '';
                info.token = null;
            }

            if (c === '\n') {
                if (!info.ignoreLineBreaks) {
                    info.tokens.push({
                        token: { tag: TokenTag.LINE_BREAK },
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
                    token: { tag: TokenTag.B_START, bracketStart: c },
                    pos: info.copyPos(),
                });
                info.ignoreLineBreaks = true;
                info.contexts.push(info.ignoreLineBreaks);
            } else if (c === ')' || c === ']' || c === '}') {
                let foundBracket = false;

                for (let i = info.tokens.length - 1; i >= 0; i--) {
                    const token_i = info.tokens[i].token;
                    if (token_i.tag === TokenTag.B_START) {
                        let start = token_i.bracketStart;
                        let end = c;
                        if (
                            (start === '(' && end !== ')') ||
                            (start === '[' && end !== ']') ||
                            (start === '{' && end !== '}')
                        ) {
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
                    token: { tag: TokenTag.B_END, bracketEnd: c }, 
                    pos: info.copyPos(),
                });
            } else if (isAlphanumeric(c)) {
                info.token = { tag: TokenTag.WORD };
                info.chunk = c;
            } else if (isSymbol(c)) {
                info.token = { tag: TokenTag.SYMBOL };
                info.chunk = c;
            } else if (!isWhitespace(c)) {
                throw new Error(`Unexpected token: ${c} @ ${info.pos.line}:${info.pos.col}`);
            }

            info.pos.col += 1;
            info.pos.idx += 1;
        }

        return info.tokens;
    }
}

export function tokenize(input: string): TokenWithTracking[] {
    return new Tokenizer().tokenize(input.replace(/\n?$/, '\n'));
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

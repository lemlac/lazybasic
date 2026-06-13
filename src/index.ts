import type { TokenWithTracking } from './types';

import { tokenize, tokenizeStream, tokenizeFile } from './tokenizer';
import { parseTokens } from './parser';

export { tokenize, tokenizeStream, tokenizeFile, parseTokens };

export function parse(input: string): TokenWithTracking[] {
    return parseTokens(tokenize(input));
}

export async function parseFile(filename: string): Promise<TokenWithTracking[]> {
    return parseTokens(await tokenizeFile(filename), filename);
}

export async function parseFileAndPrint(filename: string): Promise<void> {
    console.log(JSON.stringify(await parseFile(filename), null, 2));
}
